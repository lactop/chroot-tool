#! /usr/bin/env guile
!#

; Программа получает на стандартный вход (stdin) описание требуемых точек
; монтирования для включения chroot-окружения в формате, выдаваемом программой
; gen-bindings. На стандартный выход (stdout) она выдаёт список точек
; монтирования, которые либо ещё не смонтированы, либо смонтированы с другими
; параметрами. В последнем случае к типу монтирования добавляется маркер R.
; Список уже смонтированных точек программа получает при помощи утилиты findmnt
;
; Пример запуска программы: filter-bindings.scm

(setlocale LC_ALL "")
(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-41)
             (lact utils)
             (lact table)
             (lact mounts)
             (lact error-handling))

; ПРОСТЫЕ ВСПОМОГАТЕЛЬНЫЕ ПРОЦЕДУРЫ

; Поток вывода результатов
(define dump-port (fdes->outport (string->number (get-param (command-line) 1 "1"))))

; Есть ли в контейнере элементы
(define string-inhabited? (compose not string-null?)) 
(define inhabited? (compose not null?))

; Красивый вывод информации о точке монтирования
(define (pretty-mount r) (format #f "~A -o ~A ~A → ~A"
                                 (mount:type r)
                                 (mount:options r) 
                                 (mount:source r)
                                 (mount:target r)))

; Проверка значения на логическую истинность
(define (true? v) (and (boolean? v) v))

; ЗАГРУЗКА ТАБЛИЦЫ СМОНТИРОВАННЫХ ТОЧЕК МОНТИРОВАНИЯ

; Процедура разделения опций на список. Добавляе ro, если указан
; соответствующий флаг
(define (option-list str ro?)
  (let ((l (filter string-inhabited? (string-split str #\,))))
    (if ro? (cons "ro" l) l)))   

; Преобразование строки с опциями в таблицу. Ссылка на таблицу записывается в
; поле options сформированной структуры
(define (tabulate-options r)
  ; l -- список опций,
  ; m -- список, в котором каждая опция отмечает #t.
  (let* ((l (option-list (mount:options r) #f))
         (m (map (lambda (o) (cons o #t)) l)))
    (set-mount:options r (table-append table-null m))))  

; Обратное преобразование таблицы опций в строку

(define (option-table->string t)
  (string-join (map car (table->kv-list t)) ","))

(define (read-mount-table)
  (kv-list->table
    (stream->list (stream-map (compose mount-record->kv-pair tabulate-options)
                              (findmnt-record-stream "")))))

; ЧТЕНИЕ СПИСКА ЗАДАНИЙ ДЛЯ МОНТИРОВАНИЯ

; Задания поступают в виде списка простых строк. Каждые 4 строки подряд
; описывают одно задание. Последовательно их значения: операция монтирования
; (bind, tmpfs), опции монтирования, исходная директория для монтирования,
; целевая директория для монтирования. Директории указаны своими полными путями.
; Этот неструктурированный поток следующие процедуры превращают в поток структур
; mound-record.

; Превращение порта p в поток структур данных Mount-Record. Каждые 4 подряд
; идущих строки из потока, созданного по порту p, превращаются в структуру
; Mount-Record.
(define (mount-order-stream p)
  (letrec ((chunk (lambda (s)
                    (let ((vals (stream->list (stream-take 4 s))))
                      (cond
                        ; Конец потока строк
                        ((null? vals) stream-null)

                        ; Действительно прочитано 4 следующих элемента
                        ((= 4 (length vals))
                         (stream-cons (apply mount-record vals)
                                      (chunk (stream-drop 4 s))))

                        ; Нечто не так с форматом входных данных
                        (else (throw 'bindings-format vals)))))))
    (chunk (port->string-stream p))))

; СООТНЕСЕНИЕ ЗАПРОШЕННЫХ ОПЕРАЦИЙ МОНТИРОВАНИЯ С УЖЕ СМОНТИРОВАННЫМИ ТОЧКАМИ 

; Чтение inode директорий и их сравнение
(define (inode p) (false-if-exception (stat:ino (stat p))))
(define (inode=? p q) (and (number? p) (number? q) (= p q)))

; Часто встречающиеся проверки


; Все ли ожидаемые опции expected включены в таблицу опций options? Опции заданы
; строками со словами, разделёнными запятыми. Тип монтирования добавляет в
; список опцию "ro"
(define (options-set? options expected mount-type)
  (every (lambda (o) (table-ref options o))
         (option-list expected (bind-ro? mount-type))))

; Подходит ли смонтированная точка монтирования m под ожидаемое целевое описание
; e. Процедура возвращает список проваленных проверок. Если список пустой, то
; всё в порядке.

; FIXME: Небольшая неувязка в этой функции: mount:options для m - таблица для
; более эффективного сравнения, а mount:options для e - строка. Жаба давит
; делать Mount-Record более сложной. ЗАМЕТКА: вообще, конечно, концепция maps в
; Clojure более гибкая.

(define (mount-point-mismatches m e)
  (let* ((et (mount:type e))
         (mo (mount:options m))
         (eo (mount:options e))
         ; Попытка сделать всё «по теории», монады в стиле Клейсли. Чтобы код
         ; был немного проще Два действия: mismatch и ok. mismatch добавит
         ; ошибки к списку ошибок, ok ничего не добавит
         (mismatch (lambda (fmt . args)
                     (let ((err (apply format #f fmt args)))
                       (lambda (error-list) (cons err error-list)))))
         (ok identity)

         (check-sources
           (if (bind? et) 
               ; Если вид монтирования - связывание директорий, то всё
               ; смонтировано ожидаемо, когда inode ожидаемого источника и
               ; inode смонтированной цели совпадают 
               (let ((eno (inode (mount:source e)))
                     (tno (inode (mount:target m))))
                 (if (inode=? eno tno)
                     ok
                     (mismatch "sources mismatch: ~a ≠ ~a" eno tno)))
               ; Иначе, должны совпадать строки описывающие источники
               (let ((es (mount:source e))
                     (ms (mount:source m)))
                 (if (string=? es ms) 
                     ok
                     (mismatch "sources mismatch: ~a ≠ ~a" es ms)))))

         (check-options
           (if (options-set? mo eo et)
               ok
               (mismatch "options mismatch: ¬(~a ⊆ ~a)"
                         (if (bind-ro? et) (string-append "ro," eo) eo)
                         (option-table->string mo)))))
    ((compose check-sources check-options) '())))

; (define (mount-point-proper? m e)
;   (let ((et (mount:type e)))
;     ; Если тип монтирования - связывание директорий, то всё смонтировано
;     ; ожидаемо, если inode исходной директории совпадает с уже смонтированной. 
;     (if (or (string "bind" et) (string "bind-ro" et))
;         (if (ino=? (mount:source e) (mount:target m))
;             
;             )
;         (string=? (mount:source e) (mount:source m))
; 
;       
;       ((or (string? "bind" et)
;            (string? "bind-ro" et))
;        (or (ino=? (mount:source e) (mount:target m)) "Inodes mismatch"))
;       
;       )
;     )
;   (and
;     (let ((mt (mount:type e)))
;       (cond
;         ; Если тип монтирования -- связывание директорий, то монтирование
;         ; кооректное, если inode исходной ожидаемой директории совпадает
;         ; с целевой (пути (mount:target e) (mount:target m) и без того
;         ; совпадают)
;         ((or (string=? "bind" mt)
;              (string=? "bind-ro" mt)) (ino=? (mount:source e) (mount:target m)))
; 
;         ; В противном случае должны совпадать ожидаемые источники, как
;         ; строки
;         (else (string=? (mount:source e) (mount:source m)))))
;     (options-set? (mount:options m) (mount:options e) (mount:type e))))

; Процедура разметки точек монтирования. Каждая точка превращается в дерево
; (отметка . (причина . точка)), где отметка -- одно из следующих значений:
;
;   #:clear -- точка не смонтирована; 
;   #:mounted -- точка смонтирована, необходимо ремонтирование;
;   #:expected -- точка смонтирована так, как ожидается.
;
; Причина - строка, описывающая причину отметки.
(define (mark t-mounts expected)
  (let ((m (table-ref t-mounts (mount:target expected))))
    (if (not (mount-record? m))
      ; Если ожидаемая точка монтирования не найдена среди смонтированных,
      ; отмечаем её для последующего монтирования
      (cons #:clear (cons "not mounted" expected))
      ; Иначе, проверяем, есть ли несовпадения в параметров монтирования точки с
      ; ожидаемыми
      (let ((mismatches (mount-point-mismatches m expected)))
        ; (dump-error "expected options: ~A~%" (mount:options expected))
        ; (dump-error "mounted options: ~S~%" (table->kv-list (mount:options m)))
        (if (null? mismatches)
            ; Если несовпадений нет, то всё, как нужно
            (cons #:expected (cons "mounted as expected" expected))
            ; Иначе, отмечаем её для ремонтирования
            (cons #:mounted (cons (string-join mismatches "; ") expected))))))) 

; Удобства. Вспомогательные процедуры для доступа к полям, возвращаемой mark структуры
(define mark:mark car)
(define mark:reason cadr)
(define mark:record cddr)

; Удобства. Проверка отметок
(define (mark-check m) (lambda (p) (eq? m (mark:mark p))))
(define clean? (mark-check #:clean))
; (define mounted? (mark-check #:mounted))
(define expected? (mark-check #:expected))

; Процедура, которая добавляет маркер R к типу тех записей монтирования, которые
; следует перемонтировать. Маркер нужен, потому что процесс ремонтирования в
; chroot-tool.sh отличается от процесса монтирования.
(define (retype f r)
  (if (eq? #:clear f)
      r
      (set-mount:type r (string-append/shared "R " (mount:type r)))))

(define (process)
  (let* ((pretty-record (compose pretty-mount mark:record))
         (mounts (read-mount-table))
         (bindings (stream-map (lambda (r) (mark mounts r))
                               (mount-order-stream (current-input-port))))
         )
    ; Делим список всех точек на те, которые смонтированы нужным образом, и те,
    ; что будут записаны в план монтирования
    (receive (expected plan) (partition expected? (stream->list bindings))

      ; Развлекаем пользователя информацией об уже смонтированных точках 
      (when (inhabited? expected) 
        (dump-error "mounted as expected:~%")
        (for-each (lambda (p) (dump-error "~/~A~%" (pretty-record p)))
                  expected))

      ; Производственная необходимость информирования о причинах
      ; (ре)монтирования
      (when (not (null? plan))
        (dump-error "~%(re)mounting plan:~%")
        ; Группировка для удобства
        (receive (clean mounted) (partition clean? plan)
          (let ((dump-item (lambda (p)
                             (dump-error "~/~a (reason: ~a)~%"
                                         (pretty-record p) (mark:reason p)))))
            (when (inhabited? clean)
              (for-each dump-item clean)
              (dump-error "~%"))
            
            (for-each dump-item mounted))))

      ; Выводим план монтирования для chroot-tool.sh
      (with-output-to-port dump-port
        (lambda ()
          (for-each (lambda (p)
                      ((dump-mnt "") (retype (mark:mark p) (mark:record p))))
                    plan))))))

(catch #t process (compose exit (lact-error-handler "filter-bindings")))
