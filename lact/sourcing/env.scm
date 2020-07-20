; Процедуры загрузки пар (имя-переменной значение), генерируемых различными
; программами. В текущей версии источником служат shell-скрипты, формирующие
; наборы переменных окружения.

(define-module (lact sourcing env)
               #:use-module ((ice-9 popen) #:select (open-input-pipe))
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-41)
               #:use-module (lact utils)
               #:use-module (lact fs)
               #:use-module (lact error-handling)
               #:export (ext:ext ext:mode ext:procedure ext-record source-bash))

; Функция разбивает строку str на две подстроки: имя переменной до символа
; delim и значение переменной после символа delim -- и возвращает
; соответствующую пару
(define (split-var-string delim str)
  (let ((i (string-index str delim)))
    (if (number? i)
      ; Если разделитель найден возвращаем пару
      (cons (substring/read-only str 0 i)
            (substring/read-only str (+ 1 i)))
      ; Если разделитель не найден, кидаем исключение
      (throw 'bad-var-string str)))) 

; Низкоуровневые функции для чтения переменных окружения из разных источников.
; Возвращают списки из пар (имя-переменной значение)

(define (source-ruby file) '())

; Формирование команды для запуска shell-сценария с именем file и вывода всех
; переменных окружения, накопленных во время исполнения этого сценария в поток
; вывода, заданный дескриптором fd. Сделано для того, чтобы из сценариев можно
; было осуществлять привычную печать
(define (cmd-line file fd)
  ; cd в стандартной Shell Debian меняет переменные окружения PWD и
  ; OLDPWD. Чтобы не пачкать ими исходное окружение, где уже могут
  ; быть параметры всего приложения, использован приём с локальными
  ; переменными функции  
  (define cmd-template
    (string-join '("fn () { local PWD OLDPWD; cd '~A'; }"
                   "set -ae"
                   "fn"
                   ". './~A'"
                   "env >&~A")
                 "; "))
  (if (string-null? file)
    (format #f "env >&~A" fd)
    (format #f cmd-template (dirname file) (basename file) fd)))

; Хитрозадая процедура запуска команды с переопределением вывода для env (см.
; cmd-template выше) в дескриптор, связанный с примитивом pipe. Всё, что
; попадает в этот канал, читается, как строки в процессе формирования потока
; (stream) из них. Сделано так, чтобы аккуратно дождаться завершения процесса и
; среагировать на возможные ошибки.
(define (env-stream file)
  (define to-read car)
  (define to-write cdr)
  (define status cdr)

  ; Процедура дочернего процесса
  (define (run-child p)
    ; Закрываем конец канала для чтения
    (close-input-port (to-read p))
    ; Выполняем команду. Первый элемент в списке команд -- это argv[0]
    (let ((cl (cmd-line file (port->fdes (to-write p)))))
      ; (dump-error "command: ~S~%" cl)
      (execl "/bin/sh" "/bin/sh" "-c" cl)))

  ; Процедура родительского процесса
  (define (run-parent p pid)
    ; Закрываем конец канала для записи
    (close-output-port (to-write p))
    ; Формируем поток прочитанных строк
    (let loop ((l (read-line (to-read p))))
      (if (eof-object? l)
        ; Если всё прочитано до конца, нужно убедится, что процесс завершился
        ; корректно, но перед этим в любом случае закрываем канал для ввода
        (begin
          (close-input-port (to-read p))
          (if (zero? (status:exit-val (status (waitpid pid))))
            ; Если всё хорошо, завершаем поток
            stream-null
            ; Иначе, выбрасываем исключение, чтобы сломать весь процесс обработки
            (throw 'process-failed file)))
        ; Если прочитана строка, продолжаем поток
        (stream-cons l (loop (read-line (to-read p))))))) 

  ; Создаём канал и ветвим процесс. Используется let*, чтобы сделать это
  ; последовательно
  (let* ((p (pipe))
         (pid (primitive-fork)))
    (if (zero? pid)
      (run-child p)
      (run-parent p pid))))

; (define (source-bash file)
;   (define (read-vars)
;     (stream->list (stream-map (lambda (s) (split-var-string #\= s))
;                               (port->string-stream (open-input-pipe (cmd-line file))))))
; 
;   ; (catch 'system-error read-loop (lact-error-handler "source-bash")) 
;   ;
;   ; FIXME? Есть мнение великих (Р. Пайк, например), что прокидывать исключения
;   ; через границы модулей - плохая идея. Но, видимо, в духе Scheme (и, видимо, в
;   ; духе RiDE и в духе UNIX) отдавать решение о формировании доменов ошибок
;   ; пользователю (в UNIX для этого используются процессы, а в процессах на
;   ; каждый чих exit(-1)). Это упрощает код. Пока придерживаюсь этой философии
;   (read-vars)) 

(define (source-bash file)
  (stream->list (stream-map (lambda (s) (split-var-string #\= s)) (env-stream file))))

; FIXME: это должно быть на видном месте.
;
; Список ассоциаций расширений файлов с процедурами загрузки файлов. Это
; Специальная структура данных для Scheme -- alist (association list). Если
; расписать каждый элемент alist, он такой: ("ext" . (list mode proc)), что
; совпадает с (list "ext" mode proc). Здесь "ext" -- ключ (list mode proc) --
; значение
(define source-extensions (list (list "sh" mode-r source-bash)))

(define (ext-record str) (assoc str source-extensions))

; Доступ к полям для улучшения читаемости кода
(define ext:ext first)
(define ext:mode second)
(define ext:procedure third) 


