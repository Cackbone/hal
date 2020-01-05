;; List constructor
(define (list . lsts) lsts)


;; Make a list of size k
(define (make-list k e)
  (if (= k 1)
      (list e)
      (cons e (make-list (- k 1) e))))


;; Merge 2 lists
;; Check if list is empty
(define (null? l) (eq? l'()))

(define (merge-lists  l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2)))
        (#t                        (cons (car l2) (merge-lists  l1 (cdr l2))))))


(define (split-half l l1 l2)
  (cond ((null? l) (cons l1 l2))
        ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2))
        (#t (split-half (cdr (cdr l))(cons (car l) l1)
                        (cons (car (cdr l)) l2)))))


;; Sort a list in ascending order
(define (sort  lst)
  (cond ((null? lst)'())
        ((null? (cdr  lst)) lst)
        (#t (let ((lsts (split-half lst '() '())))
              (merge-lists (sort (car  lsts))
                           (sort (cdr  lsts)))))))


;; Fold right
(define (fold-right fn end lst)
  (if (null? lst)
      end
      (fn (car lst) (fold-right fn end (cdr lst)))))


;; Fold left
(define (fold-left fn acc lst)
  (if (null? lst)
      acc
      (fold-left fn (fn acc (car lst)) (cdr lst))))


;; Get element by index
(define (list-ref lst idx)
  (if (zero? idx)
      (car lst)
      (list-ref (cdr lst) (- idx 1))))


;; Map function over a list
(define (map p list)
  (fold-right (lambda (x r) (cons (p x) r)) '() list))


;; Filter a list
(define (filter p lst)
  (fold-right (lambda (x y)
                (if (p x) (cons x y) y))
              '()
              lst))


;; Check if member and return list from associated pair
(define (member x lst)
   (if (null? lst)
       #f
       (if (eqv? (car lst) x)
           lst
           (member x (cdr lst)))))


;; For each procedure
(define (for-each p lst)
   (cond ((null? (cdr lst))
          (p (car lst)))
         (else
          (p (car lst))
          (for-each p (cdr lst)))))


;; Append
(define (append . lsts)
  (fold-right _append
         '()
         lsts))

;; Append helper
(define (_append lst1 lst2)
  (cond ((null? lst1)
         lst2)
        (else
         (cons (car lst1)
               (_append (cdr lst1) lst2)))))

;; Reverse a list
(define (reverse items)
  (fold-right (lambda (x r) (append r (list x))) '() items))


;; Get list length
(define (length lst)
  (fold-left (lambda (x y) (+ 1 x)) 0 lst))

