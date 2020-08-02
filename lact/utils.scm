; Различные вспомогательные функции, не требующие взаимодействия с операционной
; системой

(define-module (lact utils)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-41)
               #:use-module ((ice-9 rdelim) #:select (read-line))
               #:use-module (ice-9 popen)
               #:export (head tail
                         port->string-stream pipe->string-stream
                         get-param
                         inhabited?
                         string-inhabited?
                         string-split-ne))

; Различные доступы к элементам структур, состоящих из пар (списки)
(define head car)
(define tail cdr)

; Есть ли в контейнере элементы
(define string-inhabited? (compose not string-null?)) 
(define inhabited? (compose not null?)) 

; Поток чтения строк из порта
(define-stream (port->string-stream p)
  (stream-let loop ((l (read-line p)))
    (if (eof-object? l)
      ; Если дочитали до конца, закрываем канал и завершаем поток
      (begin (close-input-port p) stream-null)
      ; В противном случае пытаемся продолжить поток с новой строкой
      (stream-cons l (loop (read-line p))))))

; Аналогично для каналов
(define-stream (pipe->string-stream p)
  (stream-let loop ((l (read-line p)))
    (if (eof-object? l)
        (begin (close-pipe p) stream-null)
        (stream-cons l (loop (read-line p))))))

; Получение параметра с номером n (счёт от 0) из командной строки cl. Если
; такого параметра нет, то list-ref выкидывает исключение, которое
; обрабатывается в обработчике, возвращающем значение default, которое и будет
; результатом выражения catch
(define (get-param cl n default)
  (catch 'out-of-range
         (lambda () (list-ref cl n))
         (lambda err default))) 

; Разбиение строки на элементы с выбором непустных элементов (non empty)
(define string-split-ne (compose (lambda (l) (filter string-inhabited? l))
                                 string-split)) 
