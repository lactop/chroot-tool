#!/bin/bash

# Устанавливаем режим работы "поломка на первой команде, вернувшей не 0". Так
# удобнее, можно каждый раз не проверять коды возврата
set -e

echo_trap () { echo trap $$: $1 >> /tmp/siglog.txt; }

readonly script_full_name="$(readlink -f "$0")"
readonly base="$(dirname "$script_full_name")"

# Скрипт поддерживает несколько команд: pack, unpack, on, off. Подпрограммы,
# которые относятся к той или иной команде в этом исходном тексте, обычно,
# содержат имя команды на первом месте. Так оформлен и этот список подпрограмм,
# которые выводят краткую справку по использованию той или иной команды. Первым
# аргументом они все принимают то, что трактуют как имя файла, содержащего сам
# скрипт, чтобы вставить это имя в выдаваемую справку.
#
# Пример вызова: off_usage /path/to/chroot-tool

pack_usage () {
  echo -e "\n$1 pack [-h] [-d dir] [-f file] [-c config-dir] [-i ignore-list]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tsource chroot directory\n" \
    "\t-f\tdestination archive file. DEFAULT: ./name-of-chroot.tar.xz\n" \
    "\t-i\tignore these files and directories\n" \
    "\t-c\tconfiguration directory. DEFAULT: chroot-path/chroot.d"
}

unpack_usage () {
  echo -e "\n$1 unpack [-h] -f file [-d dir]\n" \
    "\t-h\tthis help\n" \
    "\t-f\tsource archive file\n" \
    "\t-d\tdestination chroot directory. DEFAULT: ./"
}

on_usage () {
  echo -e "\n$1 on [-h] [-d dir] [-c config-dir] [-b bindings]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tchroot directory to mount on. DEFAULT: ./\n" \
    "\t-b\tmount points to bind\n" \
    "\t-c\tconfiguration directory. DEFAULT: chroot-path/chroot.d"
}

off_usage () {
  echo -e "\n$1 off [-h] [-S] [-d dir]\n" \
    "\t-h\tthis help\n" \
    "\t-S\tdo not try to stop chroot\n" \
    "\t-d\tchroot directory to mount off. DEFAULT: ./"
}

create_usage () {
  echo -e "\n$1 create [-h] [-d dir]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tchroot directory to create. DEFAULT: ./"
}

start_usage () {
  echo -e "\n$1 start [-h] [-d dir] [-c config-dir]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tchroot directory to start. DEFAULT: ./\n" \
    "\t-c\tpath to configuration directory. DEFAULT: chroot-path/chroot.d"
}

stop_usage () {
  echo -e "\n$1 stop [-h] [-d dir] [-c config-dir]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tchroot directory to start. DEFAULT: ./\n" \
    "\t-c\tpath to configuration directory. DEFAULT: chroot-path/chroot.d"
}

restart_usage () {
  echo -e "\n$1 restart [-h] [-d dir] [-c config-dir]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tchroot directory to start. DEFAULT: ./\n" \
    "\t-c\tpath to configuration directory. DEFAULT: chroot-path/chroot.d"
}

run_usage () {
  echo -e "\n$1 run bash-script-in-chroot.d-dir [-h] [-d dir] [-c config-dir]\n" \
    "\t-h\tthis help\n" \
    "\t-d\tchroot directory to start. DEFAULT: ./\n" \
    "\t-c\tpath to configuration directory. DEFAULT: chroot-path/chroot.d"
}

# Процедура, которая пробегает по всем командам и вызывает соответствующую
# справку, передавая в неё параметром путь до файла, который сейчас исполняется.
# Это специальная переменная $0.
usage () {
  for i in pack unpack on off create start 'stop' restart run
  do
    ${i}_usage "$0"
  done
}

# Функция для запуска одной из процедур (первый параметр) в режиме root с
# перенаправлением в 5-ый дескриптор содержимого файла (второй аргумент).
# Пример использования: su_run cat "/etc/passwd"

su_run () {
  if test $(id -u) -eq 0
  then
    # Если уже работаем от имени root, ничего особенного не предпринимаем
    "$1" 5<"$2"
  else
    # Иначе хитрый запуск с переключением пользователя. su пропускает не все
    # сигналы к своему дочернему процессу, поэтому, если текущая оболочка
    # завершается по каким-то причинам (текущий bugfix-случай: при закрытии
    # графического терминала), мы явно посылаем сигнал sigterm, на который su
    # реагирует. Чтобы аккуратно контролировать этот процесс, нужно запустить su
    # в новой под-оболочке

    ( trap "kill -s TERM 0" EXIT
      su -c "source '$0' source-mode; $1 5<'$2'" )
  fi
}

# Процедура, извлекающая список строк из конфигурационного файла (второй
# аргумент), который записан в соответствующей секции (первый аргумент). Секции
# начинаются с записи [секция]
#
# Пример вызова: grab_config bind file/name.conf 

grab_config () {
  local config="$2"
  local key="$1"
  readonly config key

  # Если файл с конфигураций не найден просто завершаемся
  if test ! -f "$config"
  then
    echo "no configuration "$config": SKIPPING" >&2
    exit
  fi

  # Если же файл найден, то он обрабатывается при помощи программы на языке awk.
  # Эта программа состоит из частей.
  #
  #   I - инициализация: mode = 0. Это означает, что ключевая секция пока не
  #   обнаружена и строки надо пропускать (см. часть C).
  #
  #   D - обнаружение ключа: когда секция с нужным ключём обнаружена,
  #   устанавливаем mode = 1 и переходим к следующей строке.
  #
  #   R - обнаружение другой секции: когда найдена не интересная нам секция,
  #   сбрасываем mode и переходим к следующей строке.
  #
  #   C - собственно, ядро обработки: если сейчас интересная секция печатаем
  #   текущую строку, $0, на stdout
  local I='BEGIN { mode = 0 }'
  local D="/^\[$key\]\$/ { mode = 1; next }"
  local R='/^\[.*\]$/ { mode = 0; next }'
  local C='/^.+$/ { if (mode) print $0 }'
  readonly I D R C

  awk "$I $D $R $C" "$config"
}

# Функция собирает список исключаемых файлов и директорий, которые не войду в
# архив с упакованной chroot-директорией. В этот список входят: (1) переданный
# через командную строку список исключений (начиная с 3-го аргумента); (2)
# список обнаруженных точек монтирования в указанном каталоге (задаётся 1-м
# аргументом); (3) список исключений из секции exclude в указанном
# конфигурационном файле (2-й аргумент).
#
# Пример использования: collect_exclude path/to/chroot some/config.conf a b c d

collect_exclude () {
  local wrkdir="$1"
  local config="$2"
  readonly wrkdir config
  shift 2

  # После того, как список исключений собран из разных источников он, для того,
  # чтобы избежать повторений, сортируется (sort), из него выбрасываются
  # повторения (uniq), а затем каждый полученный элемент выдаётся на stdout в
  # кавычках (команда echo). Последнее необходимо для работы с пробельными
  # символами
  local i a
  {
    for i in "$@"; do echo "$i"; done
    "$base"/gen-bindings.scm down "$wrkdir"
    grab_config 'exclude' "$config"
  } | sort \
    | uniq \
    | while read a; do echo "'$a'"; done
}

# Процедура разборки параметров команды pack. Стандартный конечный автомат,
# который считывает переданные аргументы через встроенную в Bash функцию
# getopts. Список исключений начинает накапливаться, когда автомат встречает
# опцию -i (ignore). Собранную из параметров информацию pack_parse дополняет, в
# случае необходимости значениями по умолчанию и выводит на stdout в виде списка
# команд, присваивающих значения некоторым переменным. Подразумевается, что
# результат работы этой процедуры передаётся в функцию eval, после выполнения
# которой переменные, сформированные pack_parse, попадут в локальную область
# видимости и могут быть использованы напрямую.
#
# Пример использования: eval "$(pack_parse -d some/chroot/dir -i a b c)"

pack_parse () {
  local cfg_dir=''
  local src_dir=''
  local dst_archive=''
  local ignore=(proc sys dev var/run lib/init/rw)
  local ARGS=("$@")

  local go=''
  local mode=''
  local opt
  while test -z "$go"
  do
    if getopts :hd:f:i:c: opt "${ARGS[@]}"
    then
      case $opt in
        'h') pack_usage "$0" >&2; exit 1;;

        'd')
          mode=''

          if [ -z "$src_dir" ]
          then
            src_dir="$OPTARG"
          else
            {
              echo "ERROR: source dir already selected: $src_dir";
              pack_usage "$0";
            } >&2

            exit -1
          fi;;

        'f')
          mode=''

          if [ -z "$dst_archive" ]
          then
            dst_archive="$OPTARG"
          else
            {
              echo "ERROR: destination archive already selected: $dst_archive"
              pack_usage "$0"
            } >&2 

            exit -1
          fi;;

        'c')
          mode=''

          if [ -z "$cfg_dir" ]
          then
            cfg_dir="$OPTARG"
          else
            {
              echo "ERROR: configuration directory already selected: $cfg_dir"
              pack_usage "$0"
            } >&2

            exit -1
          fi;;

        'i')
          mode='ignore'
          ignore+=("$OPTARG") ;;

        *)
          pack_usage "$0" >&2; exit -1;;
      esac
    else
      if test $OPTIND -gt ${#ARGS[@]}
      then
        go='stop'
      else
        case $mode in
          'ignore')
            ignore+=("${ARGS[$(($OPTIND-1))]}")
            OPTIND=$(($OPTIND+1));;

          *)
            pack_usage "$0" >&2; exit -1;;
        esac
      fi
    fi
  done

  # После отработки автомата по разбору переменных уточняем некоторые необходимые
  # параметры значениями по-умолчанию.

  # Исходная директория по-умолчанию.
  if test -z "$src_dir"
  then
    src_dir='.'
  fi

  # Файл с конфигурацией по-умолчанию.
  if test -z "$cfg_dir"
  then
    cfg_dir="$src_dir/chroot.d"
  fi

  # Имя архива по-умолчанию.
  if test -z "$dst_archive"
  then
    if test "$src_dir" = '.'
    then
      dst_archive="chroot-$(date +%Y%m%d%H%M).tar.xz"
    else
      dst_archive="$(basename "$src_dir")-$(date +%Y%m%d%H%M).tar.xz"
    fi
  fi

  SRC_DIR='$src_dir'
  DST_ARCHIVE='$dst_archive'
  CFG_DIR='$cfg_dir'
  IGNORE=($(collect_exclude "$src_dir" "$cfg_dir/tool.conf" "${ignore[@]}"))
  readonly SRC_DIR DST_ARCHIVE CFG_DIR IGNORE
}

# Процедура формирует по имени исходной директории (2-й арнумент), имени файла с
# архивом (1-й аргумент) и списку исключений (аргументы, начиная с 3-го) команду
# архивации. Процедура старается быть аккуратной и различать файлы и директории,
# так как для них необходимо формировать разные шаблоны для параметра --exclude
# программы tar. Процедура старается не включать файл с архивом
# chroot-директории в этот архив
#
# Пример использования: pack_mkcmd some-arhchive.tar.xz some/chroot/dir a b c d

pack_mkcmd () {
  local src_dir="$2"
  local dst_archive="$1"
  readonly src_dir dst_archive
  shift 2

  echo -n "tar -vcJSf '$dst_archive' -C '$src_dir'"

  local i
  for i in "$@"
  do
    if test -f "$src_dir/$i"
    then
      echo -n " --exclude='./$i'"
    else
      if test -d "$src_dir/$i"
      then
        echo -n " --exclude='./$i/*'"
      fi
    fi
  done

  echo -n " --exclude='*/$dst_archive' ./"
}

# Работа всех команд разбита на два этапа. (1) сбор информации и формирование
# разных значений, которые можно осуществить без прав root (это
# over engineering, но так уж получилось, не хотелось мне запускать свои корявые
# скрипты от имени root на рабочей машине), необходимых для исполнения команды.
# (2) собственно исполнение команды от имени root. Эти исполняющие этапы
# оформлены в процедуры с названием имя-команды_execute. Стандартный интерфейс к
# этим процедурам таков: они запускаются без параметров, но получают список
# всего необходимого для своей работы через канал, привязанный к файловому
# дескриптору 5.

# Процедура, выполняющая упаковку chroot от имени root. Она просто читает
# команду из дескриптора 5 и запускает её.
pack_execute () {
  local cmd
  read cmd <&5
  echo -e "\t$cmd\n"
  eval "$cmd"
}

# Процедура - точка входа для обработки команды pack. На вход принимает
# аргументы из командной строки. В результате работы создаёт файл с архивом
# указанной chroot с учётом исключений из командной строки, из списка точек
# монтирования и конфигурационного файла
#
# Пример использования: pack -d some/chroot/dir -f archive.tar.xz -i a b c d

function pack () {
  local SRC_DIR DST_ARCHIVE CFG_DIR IGNORE
  pack_parse "$@"

  # Немного информации для пользователя, чтобы он был в курсе
  echo -e \
    "\nsource directory: $SRC_DIR" \
    "\ndestination archive: $DST_ARCHIVE" \
    "\nconfiguration directory: $CFG_DIR" \
    "\nignore list:"

  for i in "${IGNORE[@]}"
  do
    echo -e "\t$i"
  done

  echo

  # Проверка существования директории с chroot
  if ! test -d "$SRC_DIR"
  then
    echo "ERROR: no source dir: $SRC_DIR" >&2
    exit -1
  fi

  # Проверка возможности создать файл с архивом. Файл создаётся именно на этой
  # стадии, чтобы его владельцем был не root, а текущий пользователь.
  if ! touch "$DST_ARCHIVE"
  then
    echo "ERROR: can't touch this: $DST_ARCHIVE" >&2
    exit -1
  fi

  # Формирование команды для передачи на стадию execute
  local cmd="$(pack_mkcmd "$DST_ARCHIVE" "$SRC_DIR" "${IGNORE[@]}")"
  readonly cmd

  echo -e "proceeding as root:"

  # Переключение в режим root. Файл с текущим скриптом считывается в
  # интерпретаторе, запущенном от имени root (команда source), затем вызывается
  # процедура pack_execute. Перед этим всем дескриптор 5 связывается с анонимным
  # каналом (pipe), куда записывается одна строка с командой архивирования
  su_run pack_execute <(echo "$cmd")
}

# Процедура разбора параметров команды unpack. Стандартный автомат по аргументам
# командной строки. С проверками и подстановками значений по-умолчанию. На
# выходе процедуры - программа с назначениями переменных для eval. Это
# стандартная техника для имя-команды_parse процедур в этом скрипте.
#
# Пример использования: eval "$(unpack_parse -d path/to/new-chroot -f some.tar.xz)"

unpack_parse () {
  dst_dir=''
  src_archive=''

  # Автомат разбора командной строки через getopts
  local opt
  while getopts :hd:f: opt "$@"
  do
    case $opt in
      'h') pack_usage "$0" >&2; exit 1;;

      'f')
        if test -z "$src_archive"
        then
          src_archive="$OPTARG"
        else
          {
            echo "ERROR: source archive already selected: $src_archive"
            pack_usage "$0"
          } >&2

          exit -1
        fi;;

      'd')
        if test -z "$dst_dir"
        then
          dst_dir="$OPTARG"
        else
          {
            echo "ERROR: destination directory already selected: $dst_dir"
            pack_usage "$0"
          } >&2

          exit -1
        fi;;

      *) pack_usage "$0" >&2; exit -1;
    esac
  done

  # Корректировка пути до целевого chroot, если нужно
  if test -z "$dst_dir"
  then
    dst_dir='.'
  fi

  # Исходный архив должен быть указан
  if test -z "$src_archive"
  then
    {
      echo "ERROR: source archive is not specified"
      pack_usage "$0"
    } >&2

    exit -1
  fi

  DST_DIR="$dst_dir"
  SRC_ARCHIVE="$src_archive"
  readonly DST_DIR SRC_ARCHIVE
}

# Процедура формирования команды распаковки архива с chroot. Всего два
# аргумента: что распаковываем и куда распаковываем.
#
# Пример использования: unpack_execute archive.tar.xz new/chroot/dir

unpack_mkcmd () {
  local src="$2"
  local dst="$1"
  readonly src dst

  echo "tar -vxJSf '$src' -C '$dst'"
}

# Процедура выполняющая распаковку архива. Все параметры передаются через
# дескриптор 5: в первой строке целевая директория, во второй - команда
# распаковки. Целевая директория может не существовать, поэтому выполняется
# проверка и её создание в случае необходимости.

function unpack_execute () {
  local dst cmd
  read dst <&5
  read cmd <&5

  echo -ne "\tchecking $dst: "
  if test -d "$dst"
  then
    echo OK
  else
    echo -n 'creating: '
    if mkdir -p "$dst"
    then
      echo OK
    else
      echo FAILED
      exit -1
    fi
  fi

  echo -e "\t$cmd\n"

  eval "$cmd"
}

# Процедура - точка входа для выполнения команды unpack. На входе аргументы
# командной строки, на выходе - процесс распаковки от имени пользователя root.
#
# Пример использования: unpack -f archive.tar.xz -d new/chroot/dir

unpack () {
  local SRC_ARCHIVE DST_DIR
  unpack_parse "$@"

  # Информация для пользователя
  echo "source archive: $SRC_ARCHIVE"
  echo "destination directory: $DST_DIR"
  echo

  # Проверка файла с архивом
  if test ! -f "$SRC_ARCHIVE"
  then
    echo "ERROR: no source archive: $SRC_ARCHIVE" >&2
    exit -1
  fi

  # Формирование команды распаковки
  cmd="$(unpack_mkcmd "$DST_DIR" "$SRC_ARCHIVE")"
  
  echo "proceeding as root:"

  # Выполнение распаковки от имени пользователя root. См. pack для пояснение
  # способа запуска процедуры unpack_execute и способа передачи аргументов через
  # канал с дескриптором 5.
  su_run unpack_execute <(echo "$DST_DIR"; echo "$cmd")
}

# Процедура для разбора строки с параметром для команды on. Устроена так же, как
# и прочие процедуры имя-команды_parse. Сначала простой автомат разбирает
# аргументы командной строки, затем для неуказанных значений выбираются значения
# по умолчанию, затем всё это выдаётся в виде команд присваивания значений
# переменным для последующего использования в eval.
#
# Пример использования: on_parse -d chroot/dir -b a b c d -c config.conf

on_parse () {
  # Переменные, которые on_parse дополнит собранной информацией. Даже если
  # никакие точки монтирования не будут указаны в конфигурационном файле или
  # командной строке, кое-что on_parse выдаст для монтирования по-умолчанию.
  local cfg_dir=''
  local wrk_dir=''
  local bindings=()
  local ARGS=("$@")

  local go=''
  local mode=''
  local opt
  while test -z $go
  do
    if getopts :hd:b:c: opt "${ARGS[@]}"
    then
      case "$opt" in
        'h') on_usage "$0" >&2; exit 1;;

        'd')
          mode=''

          if [ -z "$wrk_dir" ]
          then
            wrk_dir="$OPTARG"
          else
            {
              echo "ERROR: work dir already selected: $wrk_dir";
              on_usage "$0";
            } >&2

            exit -1
          fi;;

        'c')
          mode=''

          if [ -z "$cfg_dir" ]
          then
            cfg_dir="$OPTARG"
          else
            {
              echo "ERROR: configuration file already selected: $cfg_dir"
              on_usage "$0";
            } >&2

            exit -1
          fi;;

        'i')
          mode='bind'
          bindings+=("$OPTARG") ;;

        *)
          on_usage "$0" >&2; exit -1;;
      esac
    else
      if test $OPTIND -gt ${#ARGS[@]}
      then
        go='stop'
      else
        case "$mode" in
          'bind')
            bindings+=("${ARGS[$(($OPTIND-1))]}")
            OPTIND=$(($OPTIND+1));;

          *)
            on_usage "$0" >&2; exit -1;;
        esac
      fi
    fi
  done

  # Назначение переменных по-умолчанию при необходимости
  if test -z "$wrk_dir"
  then
    wrk_dir='.'
  fi

  if test -z "$cfg_dir"
  then
    cfg_dir="${wrk_dir}/chroot.d"
  fi

  # Возвращаем результат
  WRK_DIR="$wrk_dir"
  CFG_DIR="$cfg_dir"
  BINDINGS=("${bindings[@]}")
  readonly WRK_DIR CFG_DIR BINDINGS
}

# Процедуры формирования опций для команды mount. Если передано 2 аргумента, то
# первый должен быть R, второй - описание типа монтирования

mount_parse_type () {
  local a b
  read a b <<EOF
    $@
EOF
  readonly a b

  local remount_flag fs_type
  if test -z "$b"
  then
    # Если в b ничего нет, значит, в а просто описание типа монтирования
    remount_flag='M'
    fs_type="$a"
  else
    # Иначе, некий флаг и описание типа монтирования. 
    fs_type="$b"
    case "$a" in
      'R' | 'P') remount_flag="$a" ;;

      *) echo 'FIXME: unknown mount modifier: ' "$a" '; treating as R'
         remount_flag='R'
         ;;
    esac
  fi
  readonly remount_flag fs_type

  # Возвращаем результат через переменные
  case "$fs_type" in
    'bind' | 'bind-ro') FS_FLAG='-B' ;;
    *) FS_FLAG="-t$fs_type"
  esac

  FS_TYPE="$fs_type"
  REMOUNT_FLAG="$remount_flag"

  readonly FS_TYPE FS_FLAG REMOUNT_FLAG
}

mount_options () {
  local fs_type="$1"
  local opt="$2"
  readonly fs_type opt

  echo "$(mount_type "$fs") -o $opt"
}

# Процедура проверки и создания, в случае необходимости, директории
mount_dir_check () {
  local comment="$1"
  local dir="$2"
  readonly comment dir
  
  echo -ne "\t\tchecking $comment $dir: "
  if test -d "$dir"
  then
    echo OK
  else
    echo -n 'creating: '
    if mkdir -p "$dir"
    then
      echo OK
    else
      echo FAILED
      return 1
    fi
  fi
}

# Обёртка вокруг процедуры монтирования, учитывающая, что опции могут быть
# пустыми
do_mount () {
  local fs_flag="$1"
  local opt="$2"
  local rp="$3"
  local tp="$4"
  readonly fs_flag opt rp tp

  if test -z "$opt"
  then
    mount "$fs_flag" "$rp" "$tp"
  else
    mount "$fs_flag" -o "$opt" "$rp" "$tp"
  fi
}

# Установка флагов продвижения
do_propagation () {
  local tp="$1"
  local opt="$2"
  readonly tp opt

  local -a OPT
  IFS=',' read -a OPT <<< "$opt"
  readonly OPT

  if test ${#OPT[@]} -le 0
  then
    # Если так получилось, что никаких флагов нет, можно просто возвращаться,
    # ничего не делая
    true
  else
    # Иначе, к флагам надо добавить приставку --make, и всё это приписать к
    # mount
    local -a FLAGS
    for f in ${OPT[@]}
    do
      FLAGS[${#FLAGS[@]}]="--make-$f"
    done
    readonly FLAGS

    mount ${FLAGS[@]} "$tp"
  fi
}

# Универсальная процедура монтирования
mount_generic () {
  local fs="$1"
  local opt="$2"
  local rp="$3"
  local tp="$4"
  readonly fs opt rp tp

  local FS_TYPE FS_FLAG REMOUNT_FLAG
  mount_parse_type $fs

  local RF
  RF="$REMOUNT_FLAG"
  readonly RF

#   if test "$REMOUNT_FLAG" = 'R'
#   then
#     echo -ne "\n\tre-binding "
#   else
#     echo -ne "\n\tbinding "
#   fi

  case "$REMOUNT_FLAG" in
    'R') echo -ne "\n\tre-binding " ;;
    'M') echo -ne "\n\tbinding " ;;
    'P') echo -ne "\n\tre-propagating " ;;
    *) echo "Unknown binding mode. Giving up" >&2
       return
       ;;
  esac

  if test -z "$opt"
  then
    echo "$FS_FLAG $rp → $tp:"
  else
    echo "$FS_FLAG -o $opt $rp → $tp:"
  fi

  # Если запланировано ремонтирование, осуществляем его. При неудаче прерываем
  # процесс
  if test "$REMOUNT_FLAG" = 'R'
  then
    if ! try_unmount "$tp" 3 '\t\t'
    then
      return
    fi
  fi

  # Если к этой точке монтирования уже что-то подключено, а у нас режим M или R,
  # то делать нечего, возвращаемся. Если режим P, то точка должна существовать,
  # иначе делать нечего, и тоже возвращаемся. Идущий выше case оставляет только
  # три варианта значений для RF
  if test "$RF" = 'P'
  then
    if ! { findmnt -Alo TARGET | grep "$tp" >/dev/null; }
    then
      echo -e "\t\tNOT MOUNTED; skipping"
      return
    fi
  else
    if findmnt -Alo TARGET | grep "$tp" >/dev/null
    then
      echo -e "\t\tMOUNTED; skipping"
      return 
    fi
  fi

  # Проверяем и создаём по необходимости исходную директорию и точку
  # монтирования в chroot. Точка монтирования в chroot проверяется и создаётся
  # во всех случаях. Точка в хост-системе создаётся только при связывании
  # директорий: fs ∈ {bind, bind-ro}. Требуется только в том случае, если
  # RF != P.

  if test "$RF" != 'P'
  then
    case "$FS_TYPE" in
      "bind" | "bind-ro")
        if ! mount_dir_check 'source' "$rp"
        then
          return
        fi
        ;;

      *) true ;;
    esac

    if ! mount_dir_check 'chroot target' "$tp"
    then
      return
    fi
  fi

  if test "$RF" != 'P'
  then
    # Связывание исходной директории с точкой монтирования в chroot. Если не
    # получается, пропускаем. Флаг private из руководства
    # https://blog.dhampir.no/content/duplicate-bind-mounts-with-chroots-on-systemd
    echo -ne "\t\tbinding $rp → $tp: "
    if do_mount "$FS_FLAG" "$opt" "$rp" "$tp"
    then
      echo OK
    else
      echo FAILED
      return
    fi

    # Если запланировано связывание в режиме ro ремонтируем точку
    if test "$FS_TYPE" = 'bind-ro'
    then
      echo -ne "\t\tsetting ro flag $tp: "
      if mount -o remount,ro "$tp"
      then
        echo OK
      else
        echo FAILED
        return
      fi
    fi
  fi

  if test "$RF" = 'P'
  then
    echo -ne "\t\t re-propagating $tp: "
    if do_propagation "$tp" "$opt"
    then
      echo OK
    else
      echo FAILED
      return
    fi
  fi
}

# Процедура выполняющая монтирование необходимых директорий в chroot. Это
# процедура класса имя-команды_execute, поэтому выполняется от имени root и
# получает свои параметры через канал, связанный с дексриптором номер 5. Первый
# параметр - это директория chroot, следующие параметры - это список точек
# монтирования. Процедура выполняет ряд проверок перед тем, как что-то
# смонтировать, стараясь не допустить повторного монтирования и создавая точки
# монтирования в chroot в случае необходимости.

on_execute () {
  # Цикл связывания хост директорий (rp) с точками монтирования в
  # chroot-окружении (tp)
  local fs opt rp tp
  while read fs && read opt && read rp && read tp
  do
    mount_generic "$fs" "$opt" "$rp" "$tp"
  done <&5
}

# Процедура создаёт временный файл и открывает несколько дескрипторов для работы
# с ним: 6 -- для записи; 7 -- первый для чтения; 8 -- для чтения. Несколько
# дескрипторов для чтения нужны, потому что в POSIX 1003.2a shell (к
# совместимости с которой мы стремимся на всякий случай) нет способов перемотки
# файлов

make_tmp_678 () {
  local base_fd
  test -z "$1" && base_fd=6 || base_fd="$1"
  readonly base_fd

  local name=$(mktemp /tmp/chroot-tool-XXXXXXXX)
  readonly name

  # Мы хотим как можно быстрее закрыть файл, чтобы не создавать нежелательных
  # паразитных потоков информации. Поэтому открываем набор дескрипторов,
  # связанных с этим файлом и удаляем его
  # echo "exec $(($base_fd))>'$name' $(($base_fd + 1))<'$name' $((base_fd + 2))<'$name'"
  eval "exec $(($base_fd))>'$name' $(($base_fd + 1))<'$name' $((base_fd + 2))<'$name'"
  unlink "$name"
}

# Процедура вывода информации о точке монтирования
echo_mount () {
  local fs="$1"
  local opt="$2"
  local rp="$3"
  local tp="$4"
  readonly fs opt rp tp

  # Разбираем тип монтирования
  local FS_TYPE REMOUNT_FLAG FS_FLAG
  mount_parse_type $fs

  if test -z "$opt"
  then
    echo -e "\t$FS_TYPE($REMOUNT_FLAG) $FS_FLAG $rp → $tp"
  else
    echo -e "\t$FS_TYPE($REMOUNT_FLAG) $FS_FLAG -o $opt $rp → $tp"
  fi
}

# Процедура - точка входа для команды on. Стандартная точка входа по меркам
# этого скрипта: разбирает командную строку, запускает этап execute от имени
# root. Проверять здесь особенно нечего, все основные проверки в on_execute и в
# on_parse.
#
# Пример запуска: on -c config/file.conf -b a b c d -d path/to/chroot

on () {
  local WRK_DIR CFG_DIR BINDINGS
  on_parse "$@"

  # Развлекаем пользователя, чтобы не скучал и не сомневался в работоспособности
  # программы
  echo -e \
    "work directory: $WRK_DIR" \
    "\nconfiguration: $CFG_DIR"

  # Создаём временные файлы для сохранения списка связок. Это нужно, чтобы: (1)
  # не вызывать {gen, filter}-bindings повторно; (2) автоматически сломать
  # chroot-tool, если {gen, filter}-bindings обнаружат ошибку

  # Для gen-bindings.scm. Стандартная оболочка в текущей версии Debian не
  # работает с перенаправлениями с более, чем однозначными, дескрипторами.
  # Поэтому необходимо для gen-bindings оставить свободным 9-ый дескриптор.
  # Дескриптор 8 сразу не нужен
  make_tmp_678 6
  exec 8<&-

  echo "Reading binding records"
  # Сохраняем список связок и закрываем дескриптор для записи. gen-bindings
  # использует для формирования результата переданный в командной строке
  # дескриптор. filter-bindings читает данные из стандартного потока, выводит
  # данные в стандартный поток
  "$base/gen-bindings.scm" up "$WRK_DIR" 6
  exec 6>&-

  # Для filter-bindings.scm. Дескрипторы 9 10 11
  make_tmp_678 9

  # Указываем filter-bindings, в какой дескриптор следует отправить результат.
  # Оба использованных дескриптора можно закрывать
  "$base/filter-bindings.scm" 9 <&7
  exec 9>&-
  exec 7<&-

  echo  "binding plan:"
  # Выводим информацию для пользователя и закрываем первый дескриптор для чтения
  local FS_TYPE FS_FLAG REMOUNT_FLAG
  local n_mounts=0
  local fs opt rp tp
  while read fs && read opt && read rp && read tp
  do
    echo_mount "$fs" "$opt" "$rp" "$tp"
    n_mounts=$(($n_mounts + 1))
  done <&10
  exec 10<&-

  if test $n_mounts -le 0
  then
    echo -e "\tno mount points. Nothing to do"
  else
    echo -e "\nproceeding as root:"
    # Переключение в режим root, загрузка текущего скрипта и исполнение процедуры
    # on_execute (см. on_pack). Предварительно 5 дескриптор связывается со
    # временным каналом (конструкция <(...)), в который записываются параметры для
    # on_execute: сначала путь до chroot, а затем список точек монтирования
    su_run on_execute <(cat <&11)
  fi

  exec 11<&-
}

# Процедура разбора аргументов для команды off. Тут совсем простенький автомат,
# который определяет только директорию chroot. Протокол для обмена стандартный
# для parse-процедур этого скрипта: возвращается программа для eval, которая
# формирует переменные
#
# Пример использования: off_parse -d some/chroot/dir

off_parse () {
  local wrk_dir=''
  local run_stop='yes'

  local opt
  while getopts :hSd: opt "$@"
  do
    case $opt in
      'h')
        off_usage "$0" >&2; exit 1;;

      'S')
        run_stop='NO';;

      'd')
        if test -z "$wrk_dir"
        then
          wrk_dir="$OPTARG"
        else
          {
            echo "ERROR: working dir already specified: $wrk_dir"
            off_usage "$0"
          } >&2

          exit -1
        fi;;

      *)
        off_usage "$0" >&2; exit -1;;
    esac
  done

  test -z "$wrk_dir" && wrk_dir='.'

#   echo "local WRK_DIR='$wrk_dir'"
#   echo "readonly WRK_DIR"

  WRK_DIR="$wrk_dir"
  RUN_STOP="$run_stop"
  readonly WRK_DIR RUN_STOP
}

# Цикл размонтирования некоторой точки. Вынесен в отдельную процедуру для
# использования в нескольких местах.
#
# Пример использования: try_unmount some/point 3 '\t\t\t'

try_unmount () {
  local n="$2"
  local mp="$1"
  readonly n mp

  local padding='\t'
  test -z "$3" || padding="$3"
  readonly padding

  local i
  for i in $(seq "$n")
  do
    echo -ne "${padding}try $i: $mp: "

    if ! findmnt -Alo TARGET | grep "$mp" >/dev/null
    then
      # Если точка больше не смонтирована, прерываем цикл попыток размонтирования.
      echo UNMOUNTED
      return
    else
      echo -n 'unmounting: '
      if ! umount "$mp"
      then
        # Если размонтирование не удалось по причине ошибки в umount, прекращаем
        # попытки.
        echo FAILED
        return 1
      else
        echo -n 'OK; checking: '
        if ! findmnt -Alo TARGET | grep "$mp" >/dev/null
        then
          # Если очередная попытка размонтирования удалась, прекращаем цикл.
          echo UNMOUNTED
          return
        else
          # Иначе, пробуем снова
          echo 'MOUNTED; repeating'
        fi
      fi
    fi
  done

  # Все наши попытки оказались неудачными
  return 1
}

# Процедура, выполняющая отключение точек монтирования. Протокол стандартный для
# execute-процедур в этом скрипте: параметры передаются через канал, связанный с
# дескриптором 5. Для off_execute первый параметр - директория chroot,
# оставшиеся параметры - список точек монтирования для отключения. Вполне может
# быть так, что unmount не сработает с первого раза (например, может быть, что
# по неким обстоятельствам точка монтирования была смонтирована повторно),
# поэтому off_execute повторяет попытку размонтирования несколько раз (3 в
# данной версии).

off_execute () {
  # Получение и уточнение до глобального пути до chroot
  local wd
  read wd <&5
  local awdp=$(cd "${wd}" && pwd)
  test "${awdp}" = '/' && awdp=''
  readonly wd awdp

  # Цикл тройных попыток размонтирования для каждой точки монтирования p
  local fp
  while read fp
  do
    echo
    # Здесь, в случае ошибки нечего делать, кроме как перейти к следующей точке
    # монтирования
    try_unmount "$fp" 3 || true
  done <&5
}

# Процедура - точка входа для команды off. Структура стандартная для точек входа
# этого скрипта: разбор аргументов, развлечение пользователя, запуск
# execute-процедуры от имени root.
#
# Пример использования: off -d some/path/to/chroot/dir

off () {
#   # Аккуратный разбор параметров. С учётом возможных ошибок.
#   local vars="$(off_parse "$@")"
#   readonly vars
#   test $? -eq 0 || exit $?
# 
#   # Определение переменных, сформированных разбором. Здесь определяется только
#   # рабочая директория.
#   eval "$vars"

  local WRK_DIR RUN_STOP
  off_parse "$@"

  # Развлечение пользователя
  echo "work directory: $WRK_DIR"

  # Развлечение пользователя
  # echo "unmounting plan:"

  # Создаём временный файл со списком точек для размонтирования (см. комментарии
  # к on). Сначала этот файл используем для подсчёта подключённых точек
  # монтирования и определения, нужно ли действительно исполнять процедуру
  # размонтирования. Второй дескриптор для чтения (8) разу не нужен.
  make_tmp_678
  exec 8<&-

  "$base/gen-bindings.scm" down "$WRK_DIR" 6
  exec 6>&-

  local n_mounts=$(wc -l <&7)
  readonly n_mounts
  exec 7<&-

  if test $n_mounts -le 0
  then
    echo "No mount points. Nothing to do"
  else
    # То, что обнаружены точки монтирования считаем признаком «включённости»
    # текущего chroot-окружения. Исходя из этой логики, сначала выполняем, если
    # разрешено, stop перед демонтированием. Параметра CFG_DIR для действия off
    # сейчас не предусмотрено, поэтому будет взят путь по-умолчанию
    # "$WRK_DIR/chdoor.d/stop"

    if test "$RUN_STOP" = 'yes'
    then
      echo -e "\nWARNING: executing stop within chroot $WRK_DIR"

      # Не считаем завершение stop с ошибкой печальным обстоятельством и
      # продолжаем
      "$0" 'stop' -d "$WRK_DIR" || true
    fi

    # После действия stop (которое подразумевает выполнение действие on)
    # необходимо составить новый план размонтирования. И это будет тот план по
    # которому размонтирование выполняется.
    echo "unmounting plan:"

    make_tmp_678
    "$base/gen-bindings.scm" down "$WRK_DIR" 6
    exec 6>&-

    local n_mnts=0
    local m
    while read m
    do
      echo -e "\t$m"
      n_mnts=$(($n_mnts+1))
    done <&7
    exec 7<&-

    if test $n_mnts -le 0
    then
      echo -e "\tno mount points. Nothing to do"
    else
      echo -e "\nproceeding as root:"
      su_run off_execute <(echo "$WRK_DIR"; cat <&8)
      exec 8<&-
    fi
  fi
}

create_parse () {
  # Пользуемся тем, что у create и off одинаковые параметры. Переменную RUN_STOP
  # оставляем в кадре этой (create_parse) функции
  local RUN_STOP
  off_parse "$@"
}

create_execute () {
  local target_dir
  read target_dir <&5

  local atd="$(mkdir -p "${target_dir}" && cd "${target_dir}" && pwd)"
  readonly target_dir atd

  CHROOT_TOOL_DEBIAN_VERSION="${CHROOT_TOOL_DEBIAN_VERSION:-buster}"
  CHROOT_TOOL_DEBIAN_ARCH="${CHROOT_TOOL_DEBIAN_ARCH:-amd64}"
  CHROOT_TOOL_TIME_ZONE="${CHROOT_TOOL_TIME_ZONE:-Asia/Yekaterinburg}"
  # idea - do wee need an --arch at all??

  # todo maybe just cmd line args?
  # debootstrap --arch amd64 stretch "$atd" http://ftp.ru.debian.org/debian

  # debootstrap --extractor ar --arch amd64 $CHROOT_TOOL_DEBIAN_VERSION "$atd" http://ftp.ru.debian.org/debian
  # cmd="debootstrap --arch amd64 $CHROOT_TOOL_DEBOOTSTRAP_OPTIONS $CHROOT_TOOL_DEBIAN_VERSION $atd http://ftp.ru.debian.org/debian"
  # cmd="debootstrap $CHROOT_TOOL_DEBOOTSTRAP_OPTIONS $CHROOT_TOOL_DEBIAN_VERSION '$atd' http://ftp.ru.debian.org/debian"
  # по какой-то причине на stretch (лепшем) экранирование '$atd' в кавычках ' дебутсрап не может проглотить
  cmd="debootstrap $CHROOT_TOOL_DEBOOTSTRAP_OPTIONS $CHROOT_TOOL_DEBIAN_VERSION $atd http://ftp.ru.debian.org/debian"

  echo "Executing cmd: $cmd"
  $cmd
  
  # todo - move locale ru_RU.UTF-8 UTF-8 so on - to params!
  # or just outside from here - to chroota.zdb
  
  echo "Chroot-tool: calling chroot-tool specific setup things inside chroot env in dir $atd"
  
  # for some reason, got error "dash: source not found"
  # internet says that dash does not support source command
  # how is that worked at all?
  chroot "$atd" bash <<EOF
set -ex
source /etc/profile # need this if running in ArchLinux host
apt-get update
apt-get -y install locales
grep -e '^en_US.UTF-8 UTF-8$' /etc/locale.gen || echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen
grep -e '^ru_RU.UTF-8 UTF-8$' /etc/locale.gen || echo 'ru_RU.UTF-8 UTF-8' >> /etc/locale.gen
locale-gen
echo 'export LANG=en_US.UTF-8' > /etc/profile.d/set-locale.sh
ln -sf /usr/share/zoneinfo/$CHROOT_TOOL_TIME_ZONE /etc/localtime
mkdir -p /chroot.d
EOF
}

create () {
#   local vars="$(create_parse "$@")"
#   test $? -eq 0 || exit $?
#   readonly vars
#   eval "$vars"

  local WRK_DIR
  create_parse "$@"

  echo "target directory: $WRK_DIR"

  # Проверка того, что мы в Debian, что установлено необходимое ПО

#  echo -n "Debian checking: " \
#    && source /etc/os-release \
#    && [ "${ID}" = 'debian' ] && echo OK \
#    || { echo FAILED: 'designed to work under Debian'; exit -1; } 

  echo -n "debootstrap checking: " \
    && test -x '/usr/sbin/debootstrap' && echo OK \
    || { echo FAILED: 'please, apt-get install debootstrap'; exit -1; }

  # Понеслась
  echo 'proceeding as root:'
  su_run create_execute <(echo "$WRK_DIR")
  echo 'DONE. Consider using schroot (apt-get install schroot) tool for your convinience'
}

# Запуск скриптов из конфигурационной директории.

function run_parse () {
  local cfg_dir=''
  local wrk_dir=''
  local run_cmd="$1"
  shift

  local opt
  while getopts :hd:c: opt "$@"
  do
    case "$opt" in
      'h') ${run_cmd}_usage "$0" >&2; exit 1;;

      'd')
        if test -z "$wrk_dir"
        then
          wrk_dir="$OPTARG"
        else
          {
            echo "ERROR: work dir already specified: $wrk_dir"
            ${run_cmd}_usage "$0"
          } >&2
          
          exit -1
        fi;;

      'c')
        if test -z "$cfg_dir"
        then
          cfg_dir="$OPTARG"
        else
          {
            echo "ERROR: configuration dir already specified: ${cfg_dir}"
            ${run_cmd}_usage "$0"
          } >&2

          exit -1
        fi;;

      *) ${run_cmd}_usage "$0" >&2; exit -1;;
    esac
  done

  if test -z "$wrk_dir"
  then
    wrk_dir='.'
  fi

  if test -z "$cfg_dir"
  then
    cfg_dir="$wrk_dir/chroot.d"
  fi

  WRK_DIR="$wrk_dir"
  CFG_DIR="$cfg_dir"
  RUN_CMD="$run_cmd"
  readonly WRK_DIR CFG_DIR RUN_CMD
}

run_execute () {
  local wrk_dir cfg_dir cmd_line

  { read wrk_dir;
    read cfg_dir;
    read cmd_line; } <&5

  readonly wrk_dir cfg_dir cmd_line
  exec 5<&-

  if test -x "$base/host-cmd.sh"
  then
    echo "Host cmd detected. Executing"
    "$base/host-cmd.sh" "$cmd_line" "$wrk_dir"
  fi

#  exec chroot "$wrk_dir" $(which bash) -c "cd '$cfg_dir' && exec $cmd_line"
# оказалось что на хосте which bash дает /usr/bin/bash, а этого нет в чруте
  exec chroot "$wrk_dir" /bin/bash -c "cd '$cfg_dir' && exec $cmd_line"
}

# Абсолютный ли путь? Проверка по первому символу в имени

absolute_path () {
#  echo "das-test, firtchar=${1:0:1}"
#  if test ${1:0:1} = '/'; then 
#    echo "test passed"
#  fi
  test ${1:0:1} = '/' # || test ${1:0:1} = '('
}

run () {
  set -e

  local WRK_DIR CFG_DIR RUN_CMD
  run_parse "$@"

  echo -e \
    "work directory: $WRK_DIR" \
    "\ncommand to run: $RUN_CMD" \
    "\nconfiguration dir: $CFG_DIR"

  # -a -- логическое И для test

# оригинальный тест
#  if ! test \
#    -d "$WRK_DIR" \
#    -a -d "$CFG_DIR" \
#    -a -f "$CFG_DIR/$RUN_CMD"  
  
#  if ! test \
#    -d "$WRK_DIR" \
#    -a -d "$CFG_DIR" # -a -f "$CFG_DIR/$RUN_CMD" команды не проверять. Можно конечно таки требовать наличие файла, но тогда надо уметь подавать параметр команду.. Ну либо проверять только первую часть, а не всю строку.. Кстати вариант.
#  then 
#    echo "ERROR(chroot-tool): Run parameters are inconsistent. Please check all things exist: work dir, config dir, file of command to run." >&2
#    exit -1
#  fi
  
  if ! test -d "$WRK_DIR"
  then 
    echo "ERROR(chroot-tool): work directory does not exist!" >&2
    exit -1
  fi
  
  if ! test -d "$CFG_DIR"
  then 
    echo "ERROR(chroot-tool): configuration dir does not exist!" >&2
    exit -1
  fi

  set -e
  # Сперва включаем chroot. О чём сообщаем:

  echo -e "\nWARNING: Setting chroot $WRK_DIR on."
  $(which bash) "$0" on -d "$WRK_DIR" -c "$CFG_DIR"

  echo "Chroot $WRK_DIR is now ON."

  echo -e "\nproceeding as root:"

  # FIXME: Видимо, передача скрипта на вход /bin/bash, который запускается в
  # run_execute сделана, чтобы не размышлять о согласовании путей во вне и
  # внутри chroot-окружения. С этим надо разобраться
  #
  # su_run run_execute <(echo $WRK_DIR; echo 'cd /chroot.d'; cat "$CFG_DIR/$RUN_CMD")

  # Процедура run_execute прочитает со входа рабочую директорию (это путь до
  # chroot), директорию настроек и командную строку для запуска.  Если команда,
  # указанная в параметрах chroot-tool.sh run задана абсолютным путём, то
  # директория настроек - это /, а командная строка - это командная строка без
  # изменений. Иначе, директория настроек -- это /chroot.d, а команда для
  # запуска -- это ./$RUN_CMD

  local cmd_line cfg_dir
  if absolute_path "$RUN_CMD"
  then
    cmd_line="$RUN_CMD"
    cfg_dir='/'
    #cfg_dir='/chroot.d'
#    echo "this is absolute path, $RUN_CMD"
  else
    cmd_line="./$RUN_CMD"
    cfg_dir='/chroot.d'
#    echo "this is NOT absolute path, $RUN_CMD"
  fi
  readonly cmd_line cfg_dir

#  echo "WRK_DIR=$WRK_DIR"; echo "cfg_dir=$cfg_dir"; echo "cmd_line=$cmd_line"

  su_run run_execute <(echo "$WRK_DIR"; echo "$cfg_dir"; echo "$cmd_line")
}

# Точка входа самого скрипта с маршрутизацией дальнейшей работы по командам.
case "$1" in
  'pack')
    shift; pack "$@";;

  'unpack')
    shift; unpack "$@";;

  'on')
    shift; on "$@";;

  'off')
    shift; off "$@";;

  'start' | 'stop' | 'restart' | 'update')
    run "$@";;

  'run')
    shift; run "$@";;

  'create')
    shift; create "$@";;

# Специальный режим для функции source в Bash. Эта функция считывает скрипт и,
# кроме того, что загружает все переменные из него, скрипт этот выполняет. Ну,
# логика такая, что некоторые переменные могут быть записаны только в ходе
# исполнения скрипта. Чтобы случайно не наломать дров, специальная команда
# source-mode для этого скрипта просто ничего не делает и позволяет загрузить
# набор функций (нам важны execute-функции) туда, где вызван source.

  'source-mode') true;;

# Если не сообразили, что от нас хотят, выдаём справку по использованию.
  *)
    usage >&2; exit -1;;
esac

# Спасибо за внимание!
