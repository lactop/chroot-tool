#! /usr/bin/env guile
!#

; Программа строит списки для монтирования (первый аргумент up) и
; размонтирования каталогов (первый аргумент down) в chroot-окружении,
; основываясь: (1) при монтировании на конфигурации из переменных окружения,
; заданных конфигурационными файлами (на текущий момент это chroot.d/conf.sh и
; chroot.d/tool.conf); (2) при размонтировании на списке смонтированных на
; текущий момент в chroot-окружении директорий.
;
; Список в (2) формируется запуском команды 'findmnt -Alo TARGET', фильтрацией и
; сортировкой её результата.
;
; Список монтирования (1) формируется в виде последовательности четвёрок строк:
;   тип -- тип файловой системы (tmpfs, ext4 и т.п.) или специальное указание
;          для chroot-tool (например: bind, bind-ro), требующие специального
;          вызова mount (например: для связывания директорий в режиме только для чтения
;          нужно выполнить: mount -B src tgt && mount -o remount,ro tgt);
;   опции для mount;
;   источник;
;   цель.
; Строка с опциями может быть пустой.
;
; Список размонтирования (2) формируется в виде последовательности строк:
;   цель.
;
; Пример запуска команды: gen-bindings up some/chroot/dir
;
; Пример описания точки монтирования: tgt:/run src:/run fs:bind opt:default.
; Все поля, кроме tgt (целевая директория монтирования) могут быть опущены, их
; значения по умолчанию вычисляются в следующей последовательности:
;
;   fs ← bind, если не указано другое (это указание на то, что следует связать
;   пару директорий в хост системе и chroot-окружении);
;
;   src ← tgt, если fs ∈ {bind, bind-ro},
;   src ← none, если fs = tmpfs,
;   src ← "", в остальных случаях;
;
;   opt ← "noexec,nosuid,nodev,rw", если fs = tmpfs,
;   opt ← "private", если fs ∈ {bind, bin-ro}.
;
; Таким образом, если требуется связать директорию /run с директорией
; some/path/to-chroot/run, достаточно описать эту точку монтирования так:
;
;   tgt:/run

; Начальная конфигурация окружения исполнения программы

(setlocale LC_ALL "") 
(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) "."))) 

(use-modules (ice-9 popen)
             (srfi srfi-1)
             (srfi srfi-41)
             (lact utils)
             (lact fs)
             (lact table)
             (lact microjson)
             (lact error-handling)
             (lact sourcing env)
             (lact sourcing conf)
             (lact mounts))

; Вывод сообщение с информацией об использовании gen-bindings
(define (usage str)
  (define usage-fmt (string-append "~A {up|down} [dir] [n-fd]\n"
                                   "\tup\t- create binding sequence\n"
                                   "\tdown\t- create unbinding sequence\n"
                                   "\tdir\t- chroot directory; DEFAULT: ./\n"
                                   "\tn-fd\t- descriptor to dump result; DEFAULT: 1\n"))
  (dump-error usage-fmt str))

; Разбор параметров командной строки
(define cmd-line (command-line))
(define action (get-param cmd-line 1 "undefined"))
(define chroot-dir (normalize-path (get-param cmd-line 2 ".")))
(define dump-port (fdes->outport (string->number (get-param cmd-line 3 "1"))))

; Верное ли действие затребовано?
(when (not (or (equal? "up" action)
               (equal? "down" action)))
  (usage (first cmd-line))
  (exit -1))

; Верно ли указан путь до chroot-директории
(when (not (file-ok? (join-path chroot-dir "chroot.d") 'directory mode-rx))
  (dump-error "No accessible chroot dir at ~S. Aborting~%" chroot-dir)
  (exit -1))

; Формирование путей до файлов с конфигурацией
(define params-path (join-path chroot-dir "chroot.d" "conf.sh"))
(define conf-path (join-path chroot-dir "chroot.d" "tool.conf"))

; РАЗБОР СТРОКИ С ОПИСАНИЕМ ТОЧЕК МОНТИРОВАНИЯ В Μ-JSON ФОРМАТЕ

; Процедура выбора источника по-умолчанию. Сделана для того, чтобы облегчить
; код в chroot-tool.sh, и не разбирать там различные варианты. Пока всё
; просто: если речь о tmpfs, возвращаем none; если о bind или o bind-ro
; возвращаем саму точку монтирования. В остальных случаях возвращаем пустую
; строку и это должно вызывать ошибку.
(define (default-source fs t)
  (cond ((tmpfs? fs) "none") 
        ((bind? fs) t)
        (else "")))  

; Процедура выбора опций по умолчанию
(define (default-options fs)
  (cond ((tmpfs? fs) "noexec,nosuid,nodev,rw")
        ((bind? fs)
         ; Флаг private по рекомендациям
         ; https://blog.dhampir.no/content/duplicate-bind-mounts-with-chroots-on-systemd
         "private")
        (else ""))) 

; Процедура слияния ключей в записи: пара (ключ значение) со встретившимся
; повторно ключом заменяет предыдущую пару
(define (merge-keys rec) (table->kv-list (table-append table-null rec))) 

; Процедура проверки корректности путей. Если тип файловой системы tmpfs, то
; абсолютным путём должна быть только цель монтирования в chroot-директории.
; В остальных случае и источник и цель должны быть полными путями.
(define (paths-absolute? fs src tgt)
  (cond ((tmpfs? fs) (absolute-file-name? tgt))
        ((bind? fs) (and (absolute-file-name? src)
                         (absolute-file-name? tgt)))
        (else (absolute-file-name? tgt)))) 

; Процедура преобразования μ-json записи в структуру, описывающую точку
; монтирования. Нужна главным образом для того, чтобы корректно заполнить
; неуказанные поля и выполнить проверки на ошибки. 
(define (record->mount-record rec)
  (let* ((mr (merge-keys rec))
         (fs (micro-field mr "fs" "bind")) 
         (t (micro-field mr "tgt" ""))
         (s (micro-field mr "src" (default-source fs t)))
         (o (micro-field mr "opt" (default-options fs))))
    (if (or (string-null? t)
            (string-null? s))
        ; Если один из путей пустой, это ошибка. Возвращаем строку с сообщением
        ; об этом.
        (format #f "Parsing record: ~A. Source or target path is not specified"
                (micro-record->string rec))
        (if (not (paths-absolute? fs s t))
            ; Если пути недостаточно абсолютны, это ошибка. Возвращаем строку с её
            ; описанием
            (format #f "Parsing record: ~A. Source or target path is not absolute"
                    (micro-record->string rec))
            ; Проверки пройдены, формируем структуру
            (make-mount-record fs o (repath s) (repath t))))))

; Процедура разбора строки в μ-json формате в список записей о точках
; монтирования.
;
; Основная проблема - обработка ошибок.
;
; Предлагается следующая логика. Каждая строка содержит набор записей. Поэтому,
; каждая строка должна быть разобрана в список структур Mount-Record.
;
;   1. Если результатом работы процедуры стал список структур Mount-Record, значит,
;   ошибок не было.
;
; Если ошибки были, то функция возвращает список строк, описывающих обнаруженные
; ошибки в двух вариантах.
;
;   2. Если micro-parse возвращает пустой список, значит, строку не получилось
;   разобрать на элементы. В этом случае возвращается список с двумя строками:
;   разбираемая строка и сообщение об ошибке в формате.
;
;   3. Если в полученной от micro-parse структуры нет необходимых полей, или эти
;   поля не проходят проверку, то процедура record->mount-record возвращает
;   строку с описанием ошибки. Эта строка попадает в формируемый список структур
;   монтирования. Список затем фильтруется в поисках строк. Если строки
;   обнаружены, то были ошибки, к их списку приписывается вначало разбираемая
;   строка, и этот список возвращается.
(define (string->mount-record-list str)
  (let ((records (micro-parse #\: str)))
    (if (null? records)
      ; Если разбор не удался, возвращаем список строк, описывающих ошибку
      (list str "Parse error")
      ; Если всё хорошо, формируем список структур по записям, и отфильтровываем
      ; из него строки -- сообщения об ошибках
      (let* ((mnt-records (map record->mount-record records))
             (error-strings (filter string? mnt-records)))
        (if (null? error-strings)
          ; Если сообщений об ошибках нет, возвращаем список записей
          mnt-records
          ; Иначе возвращаем список ошибок, озаглавленный (первый элемент) разбираемой строкой
          (cons str error-strings))))))

; ФОРМИРОВАНИЕ СПИСКА МОНТИРОВАНИЯ ПО РАЗЛИЧНЫМ ФАЙЛАМ КОНФИГУРАЦИИ

; Процедура загрузки строки с записями о точках монтирования из переменной
; BINDINGS и переменных вида BINDINGS_*
(define (strings-from-vars path)
  (if (not (file-ok? path 'regular mode-r))
    ; Если нет окружений, то и список строк пуст 
    '()
    ; Иначе, нам нужны непустые строки...
    (filter (compose not string-null?)
            ; ..., полученные обрезанием пробелов в значениях переменных, ...
            (map (compose string-trim-both val)
                 ; ..., являющихся bindings-переменными, загруженными из сценария
                 ; по пути path.
                 ; 
                 ; Сортировка в порядке order переставит переменные в
                 ; лексикографический порядок для последующей обработки:
                 ; BINDINGS попадёт на первое место.
                 (let ((bindings? (lambda (p)
                                    (or (string=? "BINDINGS" (key p))
                                        (string-prefix? "BINDINGS_" (key p)))))
                       (order (lambda (a b) (string<? (key a) (key b)))))
                   (sort (filter bindings? (source-bash path)) order))))))

; Загрузка строк с записями о точках монтирования из файла с конфигурацией. 
(define (strings-from-conf path)
  (if (not (file-ok? path 'regular mode-r))
    ; Если файла нет, то и список строк пустой
    '()
    ; Если файл есть, то загружаем из него набор строк, строки будут обрезаны,
    ; пустые строки будут отброшены процедурой source-ini
    (strings-by-key "bind" (source-ini path))))

(define tgt car)
(define mnt cdr)

; У /tmp нет флага noexec, потому что это препятствует нормально работе apt-get
; при установке некоторых пакетов (например, openssl). Поэтому явно указаны
; опции, вместо используемых по-умолчанию.
(define system-points `("tgt:/tmp fs:tmpfs opt:nosuid,nodev,rw"
                        "tgt:/run fs:tmpfs"
                        "tgt:/proc"
                        "tgt:/sys"
                        "tgt:/dev src:dev fs:devtmpfs opt:private"

                        ; Правка внесена под особенности файловой системы
                        ; Debian 7. /dev/shm является в нём символической
                        ; ссылкой на /run/shm. Поэтому /dev/shm просто
                        ; разрешается в физический путь, который потом
                        ; клонируется в chroot-окружение. FIXME.1: Это не
                        ; должно считаться нормальным поведением. FIXME.2:
                        ; вычисляется на этапе загрузки/компиляции.
                        ,(string-append "tgt:" (normalize-path "/dev/shm"))

                        "tgt:/dev/pts"
                        "tgt:/lib/init/rw"
                        "tgt:/run/systemd/journal"))

; КАНОНИЗАЦИЯ ПУТЕЙ ДО ЦЕЛЕЙ МОНТИРОВАНИЯ

; Несколько основных тезисов.
;
; 1. Канонизировать необходимо только пути для целей. Потому что именно по ним
; проверяется факт смонтированности некоторой точки: findmnt показывает
; физические (канонические) пути.

(define (canonicalize-targets base points)
  (map (lambda (p) (let ((path (normalize-path (repath base (mount:target p)))))
                     (set-mount:target p path)))
       points))

; (define (canonicalize-targets base points) points)

; СБОР ВСЕГО ВМЕСТЕ В ПРОЦЕДУРУ ГЕНЕРАЦИИ СПИСКА МОНТИРОВАНИЯ

(define (up-execute)
  ; (dump-error "up-execute HERE~%")
  ; Составляем список всех добытых из конфигурации строк, каждую из которых
  ; преобразуем в список записей о точках монтирования. Выделяем из этого списка
  ; списков потенциальные сообщения об ошибках (списки строк).
  (let* ((strings (append system-points
                          (strings-from-conf conf-path)
                          (strings-from-vars params-path)))
         (records (map string->mount-record-list strings))
         (errors (filter (compose string? first) records)))
    ; (dump-error "READ records. Errors: ~a~%" errors)
    (if (not (null? errors))
      ; Если обнаружены ошибки, выбрасываем их в стандартный lact-обработчик
      (throw 'parse-error errors)
      ; Если не обнаружены, загружаем все записи в таблицу по ключам, которые
      ; соответствуют целям монтирования, затем распечатываем эту таблицу
      (let* ((canonical (canonicalize-targets chroot-dir (concatenate records)))
             (keyed-records (map mount-record->kv-pair canonical))
             (t-all (table-append table-null keyed-records))
             ; Порядок монтирования должен определяться целями, чтобы
             ; смонтировать /p/q, сначала нужно смонтировать /p, если нужно
             (order (lambda (a b) (string<? (tgt a) (tgt b))))
             (b-all (sort (table->kv-list t-all) order)))
        (with-output-to-port dump-port
          (lambda ()
            ; Из каждой записи в b-all извлекаем запись о точке монтирования, и
            ; передаём её на вход в функцию (dump-mnt chroot-dir), которая
            ; выводит эти записи в текущий порт вывода, который перенастроен
            ; функцией with-output-to-port
            (for-each (compose (dump-mnt "" DUMP-BOTH) mnt) b-all)))))))

; ГЕНЕРАЦИЯ СПИСКА ДЕМОНТИРОВАНИЯ

(define (down-execute)
  ; (dump-error "down-execute HERE~%")
  (let ((S (findmnt-record-stream chroot-dir)))
    (with-output-to-port dump-port
      (lambda ()
        (for-each (lambda (m) (format #t "~A~%" m))
                  (sort-list (stream->list (stream-map mount:target S))
                             string>?))))))

; (dump-error "WE ARE HERE~%")

; (catch
;   #t
;   (if (string=? "up" action) up-execute down-execute)
;   ; Стандартный обработчик lact-error-handler вернёт значение #f, которое будет
;   ; передано процедуре exit, что приведёт к прекращению процесса с кодом ошибки
;   ; 1
;   (compose exit (lact-error-handler "gen-bindings")))

(let ((handler (compose exit (lact-error-handler "gen-bindings")))
      (process (if (string=? "up" action) up-execute down-execute)))
  (catch 'system-error
         (lambda ()
           (catch 'bad-var-string
                  (lambda ()
                    (catch 'parse-error
                           (lambda ()
                             (catch 'readlink-failed
                                    process
                                    handler))
                           handler))
                  handler))
         handler))
