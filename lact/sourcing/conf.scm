; Процедуры загрузки данных из файлов с типичной ini-структурой

(define-module (lact sourcing conf)
               #:use-module (ice-9 vlist)
               #:use-module (srfi srfi-41)
               #:use-module (srfi srfi-9)
               #:use-module (lact utils)
               #:export (source-ini strings-by-key))

(define key car)
(define val cdr)

; FIXME: Можно использовать PEG-парсер 

(define (source-ini file)
  ; Структура в которую будем собирать информацию: key -- текущий ключ раздела,
  ; strings -- текущий список строк в разделе, table -- накопленная
  ; vhash-таблица вида key -> (список строк в разделе key)
  (define-record-type state-record
    (state key strings table)
    state?
    (key state:key)
    (strings state:strings)
    (table state:table))

  ; Процедура запоминающая текущий накопленный ключ в таблице, и устанавливающая
  ; новый накапливаемый ключ
  (define (flip-key new-key st)
    (let* ((k (state:key st))
           (v (reverse (state:strings st)))
           (t (state:table st))
           (p (vhash-assoc k t))
           ; Убираем все упоминания в таблице о записи с ключом k и создаём новый
           ; список, в котором учитываем предыдущие накопленные строки, если они есть 
           (t-clean (vhash-delete k t))
           (v-new (if (pair? p) (append (val p) v) v)))
      (state new-key '() (vhash-cons k v-new t-clean))))

  ; Процедура, запоминающая строку в списке строк для текущего ключа
  (define (add-string s st)
    ; Реализация вручную, без расширений GNU. Структура мелкая
    (state (state:key st)
           (cons s (state:strings st))
           (state:table st)))

  ; Пытаемся интерпретировать строку, как ключ Считаем, что новый ini-ключ --
  ; это [буквочки] окружённые только пробелами. Пробельные префикс и суффикс
  ; убираем со всех строк, поэтому проверяем только сами [буквочки]
  (define (as-key str)
    (let ((l (string-length str)))
      (and (> l 2)
           (eq? (string-ref str 0) #\[)
           (eq? (string-ref str (- l 1)) #\])
           (string-every char-set:letter str 1 (- l 1))
           (substring/read-only str 1 (- l 1)))))

  (define (collect st str)
    (let ((s (string-trim-both str)))
      (if (string-null? s)
        ; Пустая строка, нечего делать
        st
        (let ((k (as-key s)))
          ; Если попался новый ключ, заменяем его. В противном случае добавляем
          ; строку s в список строк для текущего ключа
          (if (string? k)
            (flip-key k st)
            (add-string s st))))))
  
  (state:table (flip-key "" (stream-fold collect
                                         (state "" '() vlist-null)
                                         (port->string-stream (open-input-file file))))))

(define (strings-by-key str table) (let ((p (vhash-assoc str table))) (if (pair? p) (val p) '())))
