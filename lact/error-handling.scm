; Модуль с процедурами, необходимыми для обработки ошибок (в виде исключений)

(define-module (lact error-handling)
               #:use-module (lact utils)
               #:use-module (srfi srfi-1) 
               #:export (dump-error dump-parse-error lact-error-handler))

; Вывод сообщения в стандартный поток ошибок
; (define (dump-error fmt . args) (apply format (current-error-port) fmt args)) 
(define dump-error
  (let ((p (current-error-port)))
    (lambda (fmt . args) (apply format p fmt args))))

; Вывод сообщения об ошибке разбора. Форма данных об ошибке -- это список строк,
; первой из которых является исходная строка, за которым следует список строк с
; информацией об ошибках
(define (dump-parse-error error)
  (dump-error "Parsing: ~A~%" (head error))
  (for-each (lambda (str) (dump-error "~/~A~%" str)) (tail error)))

; Стандартный обработчик ошибок
(define (lact-error-handler clarification)
  (lambda err
    (let ((type (head err))
          (body (tail err)))
      (cond ((equal? type 'system-error)
             (let ((fn (first body))
                   (fmt (second body))
                   (args (third body)))
               (if (string-null? clarification)
                 (apply dump-error (string-append fn ": " fmt "~%") args)
                 (apply dump-error (string-append clarification ": " fn ": " fmt "~%") args))))

            ((equal? type 'bad-var-string)
             (dump-error "Cannot parse var string: ~A~%" body))

            ((equal? type 'parse-error) (for-each dump-parse-error (head body))) 

            (else (apply throw err)))
      ; Всегда возвращаем false, чтобы иметь возможность работать с ошибками во
      ; внешнем контексте
      #f)))
