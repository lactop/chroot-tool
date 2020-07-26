; Модуль с процедурами, необходимыми для обработки ошибок (в виде исключений)

(define-module (lact error-handling)
               #:use-module (lact utils)
               #:use-module (srfi srfi-11)
               #:export (dump-error dump-parse-error lact-error-handler))

; Вывод сообщения в стандартный поток ошибок
; (define (dump-error fmt . args) (apply format (current-error-port) fmt args)) 
(define dump-error
  (let ((p (current-error-port)))
    (lambda (fmt . args) (apply format p fmt args))))

; Вывод сообщения об ошибке разбора. Форма данных об ошибке -- это список строк,
; первой из которых является исходная строка, за которым следует список строк с
; информацией об ошибках
(define (dump-parse-error err)
  (dump-error "Parsing: ~A~%" (head err))
  (for-each (lambda (str) (dump-error "~/~A~%" str)) (tail err)))

(define (dump-links requested given)
  (dump-error "Reading links failed; requested:~%")
  (let ((dump-item (lambda (s) (dump-error "~/~a~%" s))))
    (for-each dump-item requested)
    (dump-error "Given:~%")
    (for-each dump-item given)))

(define (check-type s) (lambda (v) (eq? v s)))
(define system-error? (check-type 'system-error))
(define bad-var-string? (check-type 'bad-var-string))
(define parse-error? (check-type 'parse-error))
(define readlink-failed? (check-type 'readlink-failed))

; Стандартный обработчик ошибок
(define (lact-error-handler clarification)
  (lambda (type . body)
    ; (dump-error "HANDLER. T: ~s. B: ~s~%" type body)
    (cond ((system-error? type)
           (let-values (((fn fmt args code) (apply values body)))
             ; (dump-error "HANDLER. fn: ~s. fmt: ~s. args: ~s~%" fn fmt args)
             (if (string-null? clarification)
                 (apply dump-error (string-append fn ": " fmt "~%") args)
                 (apply dump-error (string-append clarification ": " fn ": " fmt "~%") args))))

          ((bad-var-string? type) (dump-error "Cannot parse var string: ~A~%" body))

          ((parse-error? type) (for-each dump-parse-error (head body))) 

          ((readlink-failed? type) (apply dump-links body))

          (else (apply throw type body)))
    ; Всегда возвращаем false, чтобы иметь возможность работать с ошибками во
    ; внешнем контексте
    #f))
