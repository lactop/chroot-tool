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

; Поток вывода результатов
(define dump-port (fdes->outport (string->number (get-param (command-line) 1 "1"))))

(define (process)
  ; Разбиение потока на 4-х элементные списки. Количество элементов должно быть
  ; кратно 4
  (define (partition-4 s)
    (let ((vals (stream->list (stream-take 4 s))))
      (cond
        ; Конец потока
        ((null? vals) stream-null)
        ; Подходящий кусочек из 4-ёх элементов
        ((= 4 (length vals)) (stream-cons vals (partition-4 (stream-drop 4 s))))
        ; Иначе ошибка
        (else (throw 'bindings-format vals)))))

  ; Формирование записи о точке монтирования из списка 4-х значений
  (define (mount-record-from-4 v4)
    (call-with-values (lambda () (apply values v4)) mount-record))

  ; Процедура разделения опций на список. Добавляе ro, если указан
  ; соответствующий флаг
  (define (option-list str ro?)
    (let ((l (filter (compose not string-null?) (string-split str #\,))))
      (if ro? (cons "ro" l) l)))  

  ; Преобразование строки с опциями в таблицу. Ссылка на таблицу записывается в
  ; поле options сформированной структуры
  (define (tabulate-options r)
    ; l -- список опций,
    ; m -- список в котором каждая опция отмечает #t
    (let* ((l (option-list (mount:options r) #f))
           (m (map (lambda (o) (cons o #t)) l)))
      (set-mount:options r (table-append table-null m)))) 

  ; Процедура разметки точек монтирования. Каждая точка превращается в пару
  ; (отметка точка), где отметка -- одно из следующих значений:
  ;
  ;   #:clear -- точка не смонтирована; 
  ;   #:mounted -- точка смонтирована, необходимо ремонтирование;
  ;   #:expected -- точка смонтирована так, как ожидается;
  ;
  (define (mark t-mounts expected)
    ; Подходит ли точка монтирования m под ожидаемое целевое описание e
    (define (mount-point-proper? m e)
      ; Сравнение inode для двух файлов, заданных путями
      (define (ino=? p q)
        ; Извлечение inode
        (define (ino p)
          (catch 'system-error (lambda () (stat:ino (stat p)))
                 (lambda err #f))) 
        ; Тело процедры ino=?
        (let ((np (ino p)) (nq (ino q)))
          (and (number? np) (number? nq) (= np nq))))

      ; Все ли ожидаемые опции e включены в таблицу опций m? Опции заданы
      ; строками со словами, разделёнными запятыми. Нужен ещё и тип
      ; монтирования
      (define (options-set? m e e-fs)
        (every (lambda (o) (table-ref m o))
               (option-list e (string=? "bind-ro" e-fs))))

      ; Тело процедуры mount-point-proper?
      (and
        (let ((fs (mount:type e)))
          (cond
            ; Если тип монтирования -- связывание директорий, то монтирование
            ; кооректное, если inode исходной ожидаемой директории совпадает
            ; с целевой (пути (mount:target e) (mount:target m) и без того
            ; совпадают)
            ((or (string=? "bind" fs)
                 (string=? "bind-ro" fs)) (ino=? (mount:source e) (mount:target m)))

            ; В противном случае должны совпадать ожидаемые источники, как
            ; строки
            (else (string=? (mount:source e) (mount:source m)))))
        (options-set? (mount:options m) (mount:options e) (mount:type e))))

    ; Реализация процедуры mark
    (let ((m (table-ref t-mounts (mount:target expected))))
      (if (not (mount-record? m))
        ; Если ожидаемая точка монтирования не найдена
        (cons #:clear expected)
        ; Иначе, проверяем, верно ли смонтирована точка
        (let ((ok (mount-point-proper? m expected)))
          ; (dump-error "expected options: ~A~%" (mount:options expected))
          ; (dump-error "mounted options: ~S~%" (table->kv-list (mount:options m)))
          (if (not ok)
            ; Если нет, отмечаем её для ремонтирования
            (cons #:mounted expected)
            ; Иначе, всё так, как нужно
            (cons #:expected expected))))))

  (define (pretty-mount r)
    (format #f "~A -o ~A ~A → ~A"
            (mount:type r)
            (mount:options r) 
            (mount:source r)
            (mount:target r)))

  (define (retype f r)
    (if (eq? #:clear f)
      r
      (set-mount:type r (string-append/shared "R " (mount:type r)))))

  ; Реализация процедуры process
  (let* ((mounts (kv-list->table
                   (stream->list (stream-map
                                   (compose mount-record->kv-pair tabulate-options)
                                   (findmnt-record-stream "")))))
         (bindings (stream-map
                     (compose (lambda (r) (mark mounts r)) mount-record-from-4)
                     (partition-4 (port->string-stream (current-input-port))))))
    ; Делим список всех точек на те, которые смонтированы нужным образом, и те,
    ; что будут записаны в план
    (receive (expected plan) (partition (lambda (p) (eq? #:expected (key p)))
                                        (stream->list bindings))
      (when (not (null? expected))
        ; Развлекаем пользователя информацией об уже смонтированных точках
        (dump-error "mounted as expected:~%")
        (for-each (lambda (p) (dump-error "~/~A~%" (pretty-mount (val p)))) expected))


      ; Выводим план монтирования
      (with-output-to-port dump-port
        (lambda ()
          (for-each
            (lambda (p) ((dump-mnt "") (retype (key p) (val p))))
            plan)))))) 

(catch #t process (compose exit (lact-error-handler "filter-bindings")))
