(define-module (lact fs)
               #:use-module (srfi srfi-1)
               #:use-module (lact utils)
               #:use-module (lact error-handling)
               #:export (ensure-path! read-path!
                         mode-r mode-rx mode-rwx stat-match? file-ok?
                         extension drop-extension
                         join-path split-path rebuild-path repath))

; Более подходящий под нужды Lact аналог mkdir -p
(define (ensure-path! path)
  ; Проход вдоль существующих директорий со сменой текущей директории. Процедура
  ; возвращает список ещё не пройденных компонент пути. В случае непреодолимой
  ; ошибки выкидывается исключение. Логика простая: последовательно вызывается
  ; chdir, пока она не сломается по причине отсутствия соответствующего имени в
  ; текущей директории.
  (define (into-dirs! path)
    ; Генератор обработчика исключения от chdir. Если выбрасывается ошибка с
    ; кодом ENOENT, то возвращается оставшийся участок пути items. В противном
    ; случае переброс ошибки на более высокий уровень
    (define (handler items)
      (lambda err
        (let ((errno (system-error-errno err)))
          (if (eqv? ENOENT errno)
            items
            (apply throw err)))))

    ; Цикл прохода по элементам пути
    (let loop ((items path))
      (if (null? items)
        ; Прошли по всем элементам. Более делать нечего
        '()
        ; chdir в случае успеха ничего не возвращает (возвращает
        ; unspecified-значение), поэтому просто продолжаем цикл; иначе handler
        ; вернул список непройденных элементов пути; этот список и возвращаем
        (let ((v (catch 'system-error (lambda () (chdir (head items))) (handler items))))
          (if (unspecified? v)
            (loop (tail items))
            v)))))

  ; Создание необходимых директорий. Ошибки будут обрабатываться во внешнем по
  ; отношению к ensure-path!
  (define (make-dirs! path) (for-each (lambda (i) (mkdir i mode-rwx) (chdir i)) path))
  
  ; В любом случае возвращаемся в текущую рабочую директорию, даже если
  ; выбрасываем ошибку выше
  (let* ((cwd (getcwd)))
    (catch
      'system-error
      (lambda () (make-dirs! (into-dirs! path)) (chdir cwd))
      (lambda err (chdir cwd) (apply throw err)))))

; Адаптированный под нужды Lact аналог readlink -f. Логика работы процедуры в
; следующем.
;
; Нам доступна процедура Guile getcwd, возвращающая абсолютный физический (по
; директориям в файловой системе, без символических ссылок) путь до текущей
; директории. Поэтому нужно войти в некоторую директорию, заданную путём,
; вызвать в ней getcwd.
;
; Нужно учесть, что некоторый остаток пути может не существовать в файловой
; системе. Поэтому нужно войти на самую большую возможную глубину вдоль этого
; пути, вызвать getcwd и дописать оставшуюся часть пути к результату.
(define (read-path! path)
  ; Список состоит из одного элемента?
  (define (singleton? l) (and (pair? l) (null? (tail l))))
  ; Хвост хвоста списка
  (define tail-tail cddr)
  ; Следующий вариант пути для проверки
  (define (next p)
    (if (singleton? p)
      ; Все пути созданы, возвращаем '() -- знак остановки цикла развёртки
      '()
      ; Иначе дописываем в путь следующий компонент
      (cons (join-path (first p) (second p)) (tail-tail p))))

  ; Пробуем текущую комбинацию для путей. Меняет текущую директорию
  (define (try-path! work-dir p)
    (catch
      'system-error
      (lambda ()
        ; Вызов chdir может не сработать по множеству разных причин. Считаем
        ; любую невозможность пройти дальше индикатором того, что следует
        ; попробовать путь покороче. Чему внешнему циклу сообщит возврат #f из
        ; lact-error-handler
        (chdir (head p))
        ; Получилось сменить директорию, возвращаем полный путь:
        (apply join-path (getcwd) (tail p)))
      (lact-error-handler "try-path!")))

  ; В любом случае стараемся восстановить текущую директорию
  (let ((wd (getcwd)))
    (if (null? path)
      wd
      (catch
        'system-error
        (lambda ()
          ; Особенности unfold-right приведут к порядку путей в seq от
          ; глубочайшего; например: (("a/b/c") ("a/b" "c") ("a" "b" "c")). Это и
          ; требуется.
          (let loop ((seq (unfold-right null? identity next path)))
            (let* ((p (head seq))
                   (r (try-path! wd (head seq))))
              (if (string? r)
                ; Получилось. Переход в исходную директорию и возврат результата
                (begin (chdir wd) r)
                (if (singleton? seq)
                  ; Это был последний элемент в списке. Никуда не удалось пройти,
                  ; возвращаем придуманный путь. Текущая директория, теоретически,
                  ; не менялась
                  (apply join-path wd p)
                  ; Повторяем цикл
                  (loop (tail seq)))))))
        (lambda err (chdir wd) (apply throw err))))))

; Режимы доступа к файлам

(define mode-r #o400) 
(define mode-rx #o500)
(define mode-rwx #o700)  
 
; Проверка структуры stat на соответствие типу и режиму доступа
(define (stat-match? st type mode) (and (eqv? type (stat:type st))
                                        (eqv? mode (logand mode (stat:mode st)))))

; Проверка наличия файла нужного типа и с нужным режимом доступа к нему
(define (file-ok? file type mode)
  (catch 'system-error
         (lambda () (stat-match? (stat file) type mode))
         ; Стандартная обработка ошибок: сообщение и возврат false
         (lact-error-handler "file-ok?")))

; В Scheme имена переменных могут быть произвольными. \ -- вполне допустимо
(define \ file-name-separator-string) 

; Составление пути из отдельных компонент 
(define join-path (lambda args (if (string=? \ (head args))
                                 (string-join (tail args) \ 'prefix)
                                 (string-join args \))))

(define (split-path str)
  (let ((path (filter (compose not string-null?)
                      (string-split str file-name-separator?))))
    (if (not (string-prefix? \ str))
      path
      (cons \ path))))

; Извлечение расширения файла
(define (extension str)
  (let ((dot-pos (string-rindex str #\.)))
    (if (number? dot-pos) (substring/read-only str (+ 1 dot-pos)) "")))

; Отбрасывание расширения у имени файла
(define (drop-extension str)
  (let ((dot-pos (or (string-rindex str #\.) (string-length str))))
    (substring/read-only str 0 dot-pos))) 

; Процедура вычисления абсолютного пути по некоторому пути. Используется
; композиция функций. Сначала исходная строка с путём разбивается на компоненты
; процедурой split-path, потом по этим компонентам формируется абсолютный путь
; процедурой read-path!, которая является более устойчивым к аргументу аналогом
; readlink -f
(define rebuild-path (compose read-path! split-path)) 

; Перевод пути в красивую каноническую форму, чтобы путь можно было использовать
; в качестве ключа 
(define (repath path) (apply join-path (split-path path)))  

