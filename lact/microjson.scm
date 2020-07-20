; Модуль для разбора строк в формате, названном microjson (μ-json)

(define-module (lact microjson)
               #:export (micro-parse micro-field micro-record->string seq)
               #:use-module (srfi srfi-1)
               #:use-module (lact utils))

; Классический пропуск пробельных символов с возвратом подстроки, начинающейся с
; не-пробела. Если пропущены все символы до конца строки, возвращается строка
; пустая
(define (skip-space str) (string-trim str char-set:whitespace))

; Получаем следующий токен из исходной строки. Токены бывают такие:
;
;   #:SC -- «;»,
;   #:C  -- «:»,
;   строка -- строчка с ключом или со значением,
;   #:END -- конец строки,
;   #:ERR -- ошибка.
;
; Возвращается пара из токена и неразобранного остатка строки. Символ sep --
; разделитель для пары (ключ значение)
(define (next-token sep input-string)
  ; Сбор значения в стиле shell, то есть ab"c d:; e"f\"g\\ означает следующую
  ; строку.
  ;
  ;   ab c d:; ef"g\
  ;
  ; Реализация циклом хвостовой рекурсии с аккумулятором
  (define (collect-value str acc)
    ; Несколько множеств символов для упрощения ветвлений
    (define char-set:value-end (char-set-adjoin char-set:whitespace #\; sep))
    (define char-set:value-end-or-quote (char-set-adjoin char-set:value-end #\"))

    ; Определение, закончилось ли на строке str собираемое строковое значение
    (define (value-ended? str)
      (or (string-null? str)
          (char-set-contains? char-set:value-end (string-ref str 0))))

    ; Тело процедуры collect-value
    (if (value-ended? str)
      ; Если дошли до конца значения без ошибок, просто возвращаем накопленное
      ; и остаток строки
      (cons acc str)
      ; Иначе, ориентируемся по первому символу строки
      (case (string-ref str 0)
        ; Если это кавычка, необходимо к результату добавить всё до следующей
        ; кавычки в строке, и продолжить со строкой, которая начинается за этой
        ; кавычкой. Может быть так, что закрывающейся кавычки нет до конца
        ; строки, тогда это ошибка
        ((#\")
         (let ((pos (string-index str #\" 1)))
           (if (not (number? pos))
             (cons #:ERR str)
             (collect-value (substring/read-only str (+ 1 pos))
                            (string-append acc (substring/read-only str 1 pos))))))
        ; Если это \-эскейп, то в строке должно быть, как минимум, два символа,
        ; из которых второй -- следующий за \ -- нужно добавить в аккумулятор и
        ; продолжить сборку, со строки, начинающейся за ним. Если строка
        ; недостаточно длинная -- это ошибка
        ((#\\)
         (if (< (string-length str) 2)
           (cons #:ERR str)
           (collect-value (substring/read-only str 1)
                          (string-append acc (substring/read-only str 1 2)))))
        ; В остальных случая нужно читать значение до его конца, или до кавычки.
        (else
         (let ((pos (string-index str char-set:value-end-or-quote)))
           (if (number? pos)
             (collect-value (substring/read-only str pos)
                            (string-append acc (substring/read-only str 0 pos)))
             (cons (string-append acc str) "")))))))

  ; Тело процедуры next-token
  (let ((str (skip-space input-string)))
    (if (string-null? str)
      ; Если в строке ничего не осталось, сигнализируем об её конце
      (cons #:END "")
      ; Иначе ориентируемся по первому символу в строке
      (let ((c (string-ref str 0)))
        (cond
          ; Очевидные варианты
          ((eq? #\; c) (cons #:SC (substring/read-only str 1)))
          ((eq? sep c) (cons #:C (substring/read-only str 1)))
          ; Иначе натолкнулись на значение, которое надо собрать
          (else (collect-value str "")))))))

; Процедура разбора строки по грамматике для выражения seq. В случае той или
; иной неудачи процедура возвращает пустой список. Дурацкий хак: tokenize вернёт
; список токенов в обратном порядке, seq соберёт их тоже в обратном порядке, в
; итоге, исходный порядок сохранится.
(define (micro-parse sep input)
  (define tok car)
  (define str cdr)

  (define (tokenize sep input)
    (let loop ((t (next-token sep input))
               (r '()))
      (case (tok t)
        ((#:ERR) (throw 'parse-error '()))
        ((#:END) r)
        (else (loop (next-token sep (str t)) (cons (tok t) r))))))

  (define (seq tokens result record)
    ; Процедура для откусывания 3-х токенов, описывающих пару
    (define (take-3 lst) (catch 'wrong-type-arg (lambda () (take lst 3)) (lambda err '())))

    ; Процедура для вставки накопленной записи в накопленный же результат.
    ; Пустые записи не добавляются
    (define (append-rec res rec) (if (null? rec) res (cons rec res)))

    ; (format #t "result: ~S~%record: ~S~%" result record)
    (if (null? tokens)
      ; Если исчерпаны все токены, надо возвращать результат вместе с текущей
      ; записью
      (append-rec result record)
      ; Иначе, есть с чем работать
      (let ((tok (head tokens)))
        (cond 
          ; Обнаружен конец записи. Необходимо нужное добавить в результат и
          ; продолжить разбор с новой записью и оставшимися токенами
          ((eq? tok #:SC) (seq (tail tokens) (append-rec result record) '()))
          
          ; Если обнаружена строка, тогда нужно получить следующие три токена и
          ; соорудить из них пару. Список токенов развёрнут, поэтому в tok, не
          ; ключ, а значение
          ((string? tok)
           (let ((p (take-3 tokens)))
             (if (null? p)
               ; В корректной структуре должно быть три токена. Если это не так,
               ; take-3 вернёт пустой список. В этом случае нужно сломаться
               (throw 'parse-error tokens)
               ; Если всё хорошо
               (let ((val tok)
                     (col (second p))
                     (key (third p)))
                 ; (format #t "~S ~S ~S~%" val col key)
                 (if (not (and (string? key)
                               (eq? #:C col)))
                   ; Если структура не в порядке ломаемся
                   (throw 'parse-error tokens)
                   ; Формируем пару и добавляем её в текущую запись, продолжая
                   ; разбор
                   (seq (drop tokens 3) result (cons (cons key val) record)))))))
          
          ; В остальных случаях ошибка разбора
          (else (throw 'parse-error tokens))))))

  (catch 'parse-error
         (lambda () (seq (tokenize sep input) '() '()))
         (lambda err '())))

; Процедура извлечения значения для поля записи rec, заданного ключом k. Если такого
; поля нет, возвращается значение default. Запись представляет собой список из
; пар (ключ значение). Поиск по ключам в таком списке ассоциаций (association
; list) можно осуществлять функцией assoc
(define (micro-field rec k default)
  ; Доступ ко второму элементу пары
  (define value cdr)
  ; Тело процедуры micro-field
  (let ((p (assoc k rec)))
    (if (pair? p)
      ; Пара с ключом k найдена. Возвращаем значение
      (value p)
      ; Иначе, значение по-умолчанию
      default)))

; Преобразование списка пар, составляющих одну запись μ-json, в строку.
(define (micro-record->string rec)
  ; Вспомогательные процедуры для доступа к элементам пары
  (define key car)
  (define val cdr)

  ; Процедура преобразования пары (key value) в строку вида "key:value". По
  ; необходимости ключ или значение обрамляются двойными кавычками
  (define (pair->string p)
    ; Если в строке v есть пробелы, то надо взять её в кавычки
    (define (val->string v)
      (if (number? (string-index v char-set:whitespace)) (string-append "\"" v "\"") v)) 
    (string-append (val->string (key p)) ":" (val->string (val p))))

  ; Преобразуем список пар в список строк и объединяем эти строки через пробел
  (string-join (map pair->string rec) " "))
