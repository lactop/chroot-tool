(define-module (lact fs)
               #:use-module (srfi srfi-1)
               #:use-module (lact utils)
               #:use-module (lact error-handling)
               #:export (ensure-path! read-path!
                         mode-r mode-rx mode-rwx stat-match? file-ok?
                         extension drop-extension
                         split-path join-path repath normalize-path))

; ПРОВЕРКИ ФАЙЛОВ В ФАЙЛОВОЙ СИСТЕМЕ

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

; УТИЛИТЫ ДЛЯ РАБОТЫ С ИМЕНАМИ ФАЙЛОВ

; Простое разбиение пути на части
(define (path-items path) (filter string-inhabited?
                                  (string-split path file-name-separator?)))

; Разбиение с учётом абсолютности пути
(define (split-path path) (let ((items (path-items path)))
                            ; Если путь абсолютный, добавляем к его кусочкам "/" 
                            (if (absolute-file-name? path)
                                (cons file-name-separator-string items)
                                items)))

; Составление пути из отдельных компонент 
(define (join-path . args) (let ((fns file-name-separator-string))
                             (if (string=? fns (head args))
                                 (string-join (tail args) fns 'prefix)
                                 (string-join args fns))))

; Полезнее оказывается процедура пересборки путей из исходных кусочков
(define (repath path . paths)
  (let ((items (fold (lambda (p P) (append P (path-items p)))
                     (path-items path)
                     paths)))
    (string-join items
                 file-name-separator-string
                 (if (absolute-file-name? path) 'prefix 'infix))))

; Извлечение расширения файла
(define (extension str) (let ((dot-pos (string-rindex str #\.)))
                          (if (number? dot-pos)
                              (substring/read-only str (+ 1 dot-pos))
                              "")))

; Отбрасывание расширения у имени файла
(define (drop-extension str)
  (let ((dot-pos (or (string-rindex str #\.) (string-length str))))
    (substring/read-only str 0 dot-pos)))  

; АНАЛОГ mkdir -p, РАБОТАЮЩИЙ БЕЗ ЗАПУСКА ВНЕШНИХ ПРОЦЕССОВ. Основная функция
; ensure-path!

; Проход вдоль существующих директорий со сменой текущей директории. Процедура
; возвращает список ещё не пройденных компонент пути. В случае непреодолимой
; ошибки выкидывается исключение. Логика простая: последовательно вызывается
; chdir, пока она не сломается по причине отсутствия соответствующего имени в
; текущей директории.
(define (into-dirs! path)
  (let ((handler
          ; Генератор обработчика исключения от chdir. Если выбрасывается ошибка с
          ; кодом ENOENT, то возвращается оставшийся участок пути items. В противном
          ; случае переброс ошибки на более высокий уровень 
          (lambda (items) (lambda err (let ((errno (system-error-errno err)))
                                        (if (= ENOENT errno)
                                            items
                                            (apply throw err)))))))
    ; Цикл прохода по элементам пути 
    (let loop ((items path))
      (if (null? items)
          ; Прошли по всем элементам. Делать нечего
          '()
          ; chdir в случае успеха ничего не возвращает (возвращает
          ; unspecified-значение). В этом случае продолжаем цикл. Иначе handler
          ; вернул список непройденных элементов пути, этот список и будет
          ; результатом работы
          (let ((v (catch 'system-error
                          (lambda () (chdir (head items)))
                          (handler items))))
            (if (unspecified? v)
                (loop (tail items))
                v)))))) 

(define (ensure-path! path)
  (let ((make-dirs!
          ; Создание необходимых директорий. Ошибки будут обрабатываться во
          ; внешнем по отношению к ensure-path! 
          (lambda (path)
            (for-each (lambda (i) (mkdir i mode-rwx) (chdir i))
                      path)))
        (cwd (getcwd)))
    ; В любом случае возвращаемся в текущую рабочую директорию, даже если
    ; выбрасываем ошибку выше 
    (catch 'system-error
           (lambda ()
             (make-dirs! (into-dirs! path))
             (chdir cwd))
           (lambda err
             (chdir cwd)
             (apply throw err)))))

; АНАЛОГ readlink -m, РАБОТАЮЩИЙ БЕЗ ВНЕШНИХ ПРОЦЕССОВ. Интерфейсная процедура
; normalize-path
;
; Основную работу делает процедура read-path! (восклицательный знак, потому что
; меняются состояния). Логика задаваемого ей процесса следующая.
;
; Процедура Guile getcwd возвращает абсолютный физический (по директориям в
; файловой системе, без символических ссылок) путь до текущей директории.
; Поэтому нужно войти в некоторую директорию, заданную путём p, вызвать в ней
; getcwd.
;
; Нужно учесть, что некоторый остаток пути может не существовать в файловой
; системе. Поэтому нужно: (1) войти на самую большую возможную глубину вдоль
; пути p, (2) вызвать getcwd и дописать оставшуюся часть пути к результату.

; Формирование списка путей которые надо проверить на существование. Делается
; это от самого длинного пути к самому короткому. Например, по входному пути (а
; все пути задаются списками) ("a" "b" "c") будет создан набор
;
;   (("a/b/c") ("a/b" "c") ("a" "b" "c"))
;
; в рассчёте на то, что большинство обрабатываемых путей существуют, и наиболее
; вероятно с первой попытки попасть в нужное место и узнать при помощи getcwd
; полный физический путь.
(define (unfold-paths path)
  (let* ((tail-of-tail cddr)
         (next (lambda (p)
                 (if (singleton? p)
                     ; Все пути созданы, возвращаем '() -- знак остановки цикла
                     ; развёртки
                     '()
                     ; Иначе дописываем в путь следующий компонент
                     (cons (join-path (first p) (second p)) (tail-of-tail p))))))
    (unfold-right null? identity next path)))

(define (try-path! p)
  (catch
    'system-error
    (lambda ()
      ; Вызов chdir может не сработать по множеству разных причин. Считаем
      ; любую невозможность пройти дальше индикатором того, что следует
      ; попробовать путь покороче. Чему внешнему циклу сообщит возврат #f из
      ; lact-error-handler.
      (chdir (head p))
      ; Получилось сменить директорию, возвращаем полный путь:
      (apply join-path (getcwd) (tail p)))
    (lact-error-handler "try-path!"))) 

(define (read-path! path)
  ; В любом случае стараемся восстановить текущую директорию
  (let ((wd (getcwd))
        (try-path!
          ; Тест текущей комбинации (путь и остаток пути). Теоретически, в
          ; случае не успеха, текущая директория не меняется. ВНИМАНИЕ: В случае
          ; успеха она меняется и не возвращается обратно. 
          (lambda (p)
            (catch 'system-error
                   ; Вызов chdir может не сработать по множеству разных причин.
                   ; Считаем любую невозможность пройти дальше индикатором того,
                   ; что следует пробовать путь короче. Об этом внешнему циклу
                   ; сообщит возврат #f из lact-error-handler. 
                   (lambda ()
                     (chdir (head p))
                     ; Получилось сменить директорию, возвращаем полный путь:
                     (apply join-path (getcwd) (tail p)))
                   (lact-error-handler "try-path!")))))
    (if (null? path)
      wd
      (catch
        'system-error
        (lambda ()
          (let loop ((seq (unfold-paths path)))
            (let* ((p (head seq))
                   (r (try-path! (head seq))))
              (if (string? r)
                ; Получилось. Переход в исходную директорию и возврат результата
                (begin (chdir wd) r)
                ; ВНИМАНИЕ: Не получилось; текущая директория так и не менялась
                (if (singleton? seq)
                    ; Это был последний элемент в списке. Никуда не удалось
                    ; пройти, возвращаем придуманный путь 
                    (apply join-path wd p)
                    ; Повторяем цикл 
                    (loop (tail seq)))))))
        (lambda err (chdir wd) (apply throw err))))))

; Процедура вычисления абсолютного пути по некоторому пути, заданному строкой.
; Используется композиция процедур. Сначала аргумент разбирается на кусочки в
; split-path, потом считывается полный путь до него
(define normalize-path (compose read-path! split-path)) 
