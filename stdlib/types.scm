;; A helper function to check if 'x' is of type 'type'
(define (_checkType x type)
  (string=? type (type? x)))


;; Check if 'x' is of type string
(define (string? x) (_checkType x "string"))


;; Check if 'x' is of type number
(define (number? x) (_checkType x "number"))


;; Check if 'x' is of type list
(define (list? x) (_checkType x "list"))


;; Check if 'x' is of type pair
(define (pair? x) (or
                   (_checkType x "pair")
                   (list? x)))


;; Check if 'x' is of type boolean
(define (boolean? x) (_checkType x "boolean"))


;; Check if 'x' is of type procedure
(define (procedure? x) (_checkType x "procedure"))


;; Check if 'x' is of type procedure
(define (symbol? x) (_checkType x "symbol"))


;; Check if 'x' is of type procedure
(define (char? x) (_checkType x "char"))


;; Check if type of 'x' is atomic
(define (atom? x)
  (cond ((list? x) (null? x))
        (#t)))
