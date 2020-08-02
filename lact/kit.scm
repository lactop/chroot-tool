; Модуль для работы с множествами. Множества названы kit (набор), чтобы не
; конфликтовать по имени с операторами семейства set

(define-module (lact kit)
               #:use-module (ice-9 vlist)
               #:use-module (srfi srfi-1)
               #:export (kit-empty kit-empty? kit-cons kit kit?
                         kit-insert kit-append
                         kit->list list->kit
                         kit-item? kit-contains? kit-items? kits<=?
                         kit-unite kit-intersect))

(define kit-empty vlist-null)

(define kit-empty? vlist-null?) 

(define (kit-item? item K) (pair? (vhash-assoc item K))) 

(define (kit-cons item K) (if (kit-item? item K) K (vhash-cons item #t K)))

(define (kit-append K . lists)
  (fold (lambda (l k) (fold kit-cons k l)) K lists))

(define (kit-instert K . items) (kit-append K items))

(define (list->kit items) (kit-append kit-empty items))

(define (kit->list K) (vlist->list (vlist-map car K)))

(define (kit . items) (list->kit items))

(define (kit? k) (or (vlist-null? k) (vhash? k)))

(define (kit-contains? K items) (every (lambda (i) (kit-item? i K)) items))

(define (kit-items? K . items) (kit-contains? K items))

(define (kit<=? . kits)
  (or (null? kits)
      (null? (cdr kits))
      (let ((k1 (car kits))
            (k2 (cadr kits))
            (rest (cddr kits)))
        (and (kit-contains? k2 (kit->list k1))
             (apply kit<=? k2 rest)))))

(define (kit-unite . kits) (apply kit-append kit-empty (map kit->list kits)))

(define (kit-intersect K . kits)
  (fold (lambda (k R)
          (list->kit (filter (lambda (i) (kit-item? i R)) (kit->list k))))
        K
        kits))
