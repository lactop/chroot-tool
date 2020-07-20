; Модуль для работы с таблицами из пар (ключ значение)

(define-module (lact table)
               #:use-module (ice-9 vlist)
               #:use-module (srfi srfi-1)
               #:export (key val 
                         dump-kv dump-kv-list
                         table-null table-append table-clear filter-new
                         table->kv-list kv-list->table
                         table-ref))

; Доступ к элементам пары
(define key car)
(define val cdr)

; Пустая таблица
(define table-null vlist-null)

(define (dump-kv p) (format #t "~a='~a'~%" (key p) (val p)))
(define (dump-kv-list l) (for-each dump-kv l)) 

; Создание новой таблицы переменных из существующей table и списка пар с
; описанием переменных в виде (ключ значение) kv-list. Значения по ключам из
; kv-list заменяют существующие в table.
;
; Структура данных для table -- vhash -- прозрачные (referential transparent)
; словари в Guile Scheme

; Удаление всех значений из table, имеющих ключами ключи из lst (списка kv-пар)
(define (table-clear table kv-list)
  (fold (lambda (p t) (vhash-delete (key p) t))
        table
        kv-list))

; Добавление всех пар (ключ значение) из kv-list в таблицу. Если ключ
; повторяется, то он заменяет предыдущую запись в таблице
(define (table-append table kv-list)
  (fold (lambda (p t) (let ((k (key p))
                            (v (val p)))
                        (vhash-cons k v (vhash-delete k t))))
        table
        kv-list))

; Фильтрация kv-list, отсеивающая уже существующие в table ключи
(define (filter-new table kv-list)
  (define (new? kv) (let ((p (vhash-assoc (key kv) table)))
                       ; Условие читается так: или переменной с таким именем
                       ; не найдено (в случае успеха vhash-assoc возвращает
                       ; пару), или в var-list содержится другое значение для
                       ; переменной
                       (or (not (pair? p))
                           (not (equal? (val p) (val kv))))))

  (filter new? kv-list))

(define table->kv-list vlist->list)

(define (table-ref t k)
  (let ((p (vhash-assoc k t)))
    ; (format #t "table search: ~S ~S~%" k p)
    (and (pair? p)
         (val p))))

; Создание таблицы по списку пар (ключ значение). В Scheme такие списки
; называются alist, но мы тут начинающие программисты на Scheme, поэтому
; используем свою kv-терминологию
(define (kv-list->table l) (table-append table-null l))
