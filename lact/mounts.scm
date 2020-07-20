(define-module (lact mounts)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 iconv)
               #:use-module (ice-9 regex)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9 gnu) 
               #:use-module (srfi srfi-41)
               #:use-module (lact microjson)
               #:use-module (lact utils)
               #:use-module (lact fs)
               #:use-module (lact error-handling)
               #:export (mount-record mount-record?
                         set-mount:type mount:type
                         mount:options set-mount:options
                         mount:source mount:target
                         dump-mnt
                         mount-record->kv-pair
                         findmnt-record-stream
                         unhexify))


; Структура для описания точки монтирования
(define-immutable-record-type mount-record-t
  ; Конструктор
  (mount-record type options source target)
  ; Процедура определения: является ли значение данной структурой
  mount-record?
  ; Набор функций для доступа к полям записи
  (type mount:type set-mount:type)
  (options mount:options set-mount:options)
  (source mount:source)
  (target mount:target)) 

; Процедура формирует процедуру генерации строкового представления точки
; монтирования в текущий порт вывода. Если chroot-dir не пустая строка, то эта
; строка приписывается к пути точки монтирования в chroot-окружении
; (используется в gen-bindings). В противном случае путь до цели берётся таким,
; какой есть (используется в filter-bindings)
(define (dump-mnt chroot-dir)
  (let ((chroot-repath
          (if (null? chroot-dir)
            identity
            (lambda (path) (join-path chroot-dir (string-trim path file-name-separator?))))))
    (lambda (r)
      (format #t "~A~%~A~%~A~%~A~%"
              (mount:type r)
              (mount:options r)
              (mount:source r)
              (chroot-repath (mount:target r))))))

; Преобразование записи о точке монтирования в пару, в которой ключом служит
; цель монтирования. Это необходимо для использования наших таблиц
(define (mount-record->kv-pair mr) (cons (mount:target mr) mr)) 

; findmnt считает разнообразные символы «опасными» и побайтово кодирует их
; \xHH-последовательностями. Приходится переводить так закодированные строки в
; строки обычные вручную 
(define (unhexify str)
  (define re-code (make-regexp "\\\\x[[:xdigit:]]{2}"))
 
  ; Процедура перекодирования разбиения строки на список из подстрок обычных и
  ; подстрок из x-кодов. Результат накапливается в r в виде пары из списка строк
  ; и позиции, с которой начинается следующая за последней цепочкой x-кодов
  ; строка
  (define (chop-string str)
    (define str-list car)
    (define last-offset cdr)
 
    (define (collect m r)
      (let ((f (match:start m))
            (t (match:end m))
            (l (last-offset r)))
        (if (< l f)
          ; Между текущим совпадением и последним есть некоторая подстрока.
          ; Нужно её добавить в список
          (cons (cons (match:substring m)
                      (cons (substring/read-only str l f) (str-list r)))
                t)
          ; Иначе нужно добавить только найденный x-код
          (cons (cons (match:substring m) (str-list r))
                t))))

    ; Финальное извлечение результата свёртки
    (define (extract r)
      (let ((l (last-offset r)))
        (if (= (string-length str) l)
        ; Если последнее совпадение достигнуто на конце строки, делать нечего,
        ; возвращаем список строк
        (str-list r)
        ; Иначе, нужно добавить в список конец строки
        (cons (substring/read-only str l) (str-list r)))))

    ; Признаком x-кода является наличие префикса \x в начале строки 
    (define (x-code? str) (string-prefix? "\\x" str))

    ; Собираем разбитую на x-коды и «безопасные» подстроки строку обратно
    (define (recode lst)
      (define str car)
      (define blk cdr)

      ; Извлечение накопленной строки
      (define (extract p)
        (if (not (null? (blk p)))
          ; Если накопился блок с х-кодами, надо их добавить к копящейся строки.
          ; Нужно учесть, что порядок обратный
          (string-append (bytevector->string (list->u8vector (blk p)) "")
                         (str p))
          ; В противном случае нужно вернуть просто строку
          (str p)))

      (define (collect s r)
        (if (x-code? s)
          ; Если это очередной x-код добавляем его в список
          (cons (str r)
                (cons (string->number (string-replace s "#" 0 1 0 1))
                      (blk r)))
          ; В противном случае, добавляем строку к накопленной строке
          (cons (string-append/shared s (extract r))
                '())))
      (extract (fold collect '("" . ()) lst)))

    (recode (extract (fold-matches re-code str '(() . 0) collect))))
  (chop-string str))

(define (findmnt-record-stream chroot-dir)
  (define (findmnt-string->mount-record s)
    ; Процедура объединения опций монтирования и флагов распространения (что бы
    ; это ни значило) в один список, разделённый запятыми
    (define (join-options r)
      (let* ((o (micro-field r "OPTIONS" ""))
             (p (micro-field r "PROPAGATION" ""))
             (pl (if (string-null? p) '() (cons p '())))
             (ol (if (string-null? o) pl (cons o pl))))
        (string-join ol ",")))

    (let ((rec (first (micro-parse #\= s))))
      ; (dump-error "~S~%~S~%" s rec)
      (mount-record (micro-field rec "FSTYPE" "")
                    (join-options rec)
                    (unhexify (micro-field rec "SOURCE" ""))
                    (unhexify (micro-field rec "TARGET" "")))))

  (let ((p (open-pipe* OPEN_READ "findmnt" "-PAo" "TARGET,SOURCE,FSTYPE,OPTIONS,PROPAGATION")))
    (stream-filter (lambda (r) (string-prefix? chroot-dir (mount:target r)))
                   (stream-map findmnt-string->mount-record (port->string-stream p)))))
