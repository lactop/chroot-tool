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
(add-to-load-path
  (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-41)
             (srfi srfi-9 gnu)
             (lact utils)
             (lact table)
             (lact mounts)
             (lact error-handling)
             (lact kit))

; ПРОСТЫЕ ВСПОМОГАТЕЛЬНЫЕ ПРОЦЕДУРЫ

; Поток вывода результатов
(define dump-port
  (fdes->outport (string->number (get-param (command-line) 1 "1"))))

; Красивый вывод информации о точке монтирования
(define (pretty-mount r)
  (format #f "~A -o ~A ~A → ~A"
          (mount:type r)
          (string-join (append (mount:options r) (mount:propagations r)) ",")
          (mount:source r)
          (mount:target r)))

; Проверка значения на логическую истинность
(define (true? v) (and (boolean? v) v))

; ЗАГРУЗКА ТАБЛИЦЫ СМОНТИРОВАННЫХ ТОЧЕК МОНТИРОВАНИЯ

; Преобразование опций в структуре точки монтрования в таблицу. Создаётся новая
; структура с таблицей (это просто набор: опция → Bool) в поле options
; (define (tabulate-options r)
;   (set-mount:options r (list->kit (mount:options r))))

(define (tabulate-options r)
  (set-fields r
    ((mount:options) (list->kit (mount:options r)))
    ((mount:propagations) (list->kit (mount:propagations r)))))

; Обратное преобразование опций в строку
(define (options->string t)
  (cond ((list? t) (string-join t ","))
        ((kit? t) (string-join (kit->list t)))
        (else (error "Wrong options format:" t))))

(define (read-mount-table)
  (kv-list->table
    (stream->list
      (stream-map
        (compose mount-record->kv-pair tabulate-options)
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
                         (stream-cons (apply make-mount-record vals)
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
; (define (options-set? options expected mount-type)
;   (kit-contains? options (option-list expected (bind-ro? mount-type))))

; (define (options-set? options expected) (kit-contains? options expected))

(define options-set? kit-contains?)

; Подходит ли смонтированная точка монтирования m под ожидаемое целевое описание
; e. Процедура возвращает список проваленных проверок. Если список пустой, то
; всё в порядке. Информация о проваленой проверке - это пара: (маркер .
; сообщение об ошибке). Маркер говорит о том, что делать с этой точкой
; монтирования: #:remount означает, что нужно перемонтировать; #:reflag
; означает, что нужно изменить флаги продвижения.

(define (mount-point-mismatches m e)
  (let* ((et (mount:type e))
         (mo (mount:options m))
         (eo (mount:options e))
         (mp (mount:propagations m))
         (ep (mount:propagations e))
         ; Попытка сделать всё «по теории», монады в стиле Клейсли. Чтобы код
         ; был немного проще Два действия: mismatch и ok. mismatch добавит
         ; описание ошибки к текущему описанию, а маркер к текущему набору
         ; маркеров; ok ничего не добавит
         (mismatch (lambda (marker fmt . args)
                     (let ((err (apply format #f fmt args)))
                       (lambda (markers reasons)
                         (values (kit-cons marker markers)
                                 (string-append reasons ";" err))))))
         (ok values)

         (check-sources
           (if (bind? et) 
               ; Если вид монтирования - связывание директорий, то всё
               ; смонтировано ожидаемо, когда inode ожидаемого источника и
               ; inode смонтированной цели совпадают 
               (let ((eno (inode (mount:source e)))
                     (tno (inode (mount:target m))))
                 (if (inode=? eno tno)
                     ok
                     (mismatch #:remount "sources mismatch: ~a ≠ ~a" eno tno)))
               ; Иначе, должны совпадать строки описывающие источники
               (let ((es (mount:source e))
                     (ms (mount:source m)))
                 (if (string=? es ms) 
                     ok
                     (mismatch #:remount "sources mismatch: ~a ≠ ~a" es ms)))))

         (check-options
           (if (options-set? mo eo)
               ok
               (mismatch #:remount
                         "options mismatch: ¬(~a ⊆ ~a)"
                         (options->string eo)
                         (options->string mo))))
         
         (check-flags
           (if (options-set? mp ep)
               ok
               (mismatch #:reflag
                         "propagations mismatch: ¬(~a ⊆ ~a)"
                         (option->string eo)
                         (options->string ep)))))
    ((compose check-sources check-options check-flags) kit-empty "")))

; Процедура разметки точек монтирования. Каждая точка превращается в дерево
; (отметки . (причины . точка)), где отметка -- одно из следующих значений:
;
;   #:clear    - точка не смонтирована, требуется монтирование;
;   #:remount  - точка смонтирована, требуется ремонтирование;
;   #:reflag   - точка смонтирована, требуется изменение флагов продвижения;
;   #:expected - всё смонтировано так, как нужно.
;
; Причины - строка, описывающая причины установки отметок
(define (mark t-mounts expected)
  (let ((m (table-ref t-mounts (mount:target expected))))
    (if (not (mount-record? m))
      ; Если ожидаемая точка монтирования не найдена среди смонтированных,
      ; отмечаем её для последующего монтирования
      (cons (kit #:clean) (cons "not mounted" expected))
      ; Иначе, проверяем, есть ли несовпадения в параметрах монтирования точки с
      ; ожидаемыми
      (receive (mismatches reasons) (mount-point-mismatches m expected)
        (if (kit-empty? mismatches)
            ; Если несовпадений нет, то всё, как нужно
            (cons (kit #:expected) (cons "mounted as expected" expected))
            ; Иначе, возвращаем несоответствия
            (cons mismatches (cons reasons expected))))))) 

; Удобства. Вспомогательные процедуры для доступа к полям, возвращаемой mark структуры
(define mark:marks car)
(define mark:reason cadr)
(define mark:record cddr)

; Удобства. Проверка отметок
(define (mark-check m) (lambda (p) (kit-item? m (mark:marks p))))
(define clean? (mark-check #:clean))
(define expected? (mark-check #:expected))
(define remount? (mark-check #:remount))
(define reflag? (mark-check #:reflag))

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
                               (mount-order-stream (current-input-port)))))
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
      (when (inhabited? plan) 
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
          (let ((dmp (dump-mnt "" DUMP-OPTIONS)))
            (for-each (lambda (p) (dmp (retype (mark:mark p) (mark:record p))))
                      plan)))))))

; (catch #t process (compose exit (lact-error-handler "filter-bindings")))

; FIXME: наверняка, существует более адекватный способ точного перехвата этих
; ошибок. «Лесенка» нужна, чтобы система адекватно указывала места тех
; исключений, которые явно не обрабатываются.

(let ((handler (compose exit (lact-error-handler "filter-bindings"))))
  (catch 'system-error
         (lambda ()
           (catch 'bad-var-string
                  (lambda ()
                    (catch 'parse-error
                           (lambda ()
                             (catch 'readlink-failed
                                    process
                                    handler))
                           handler))
                  handler))
         handler))
