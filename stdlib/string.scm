;; Check if string is null
(define (string-null? str) (eq? str ""))


;; Get string length
(define (string-length str)
  (length (string->list str)))


;; Get char in 'str' at position 'i'
(define (string-ref str i)
  (list-ref (string->list str) i))


;; Concat string
(define (string-append . strs)
  (list->string
   (apply append
         (map string->list strs))))


;; Make string of size k fill with char c
(define (make-string k c)
  (list->string
   (make-list k c)))


;; Create a string from chars
(define (string . chars)
  (list->string chars))
