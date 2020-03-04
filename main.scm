(define (make-tuple list-a list-b)
  (iterate-a list-a list-b ()))

(define (iterate-a list-a b tuple-list)
  (cond ((null? list-a) tuple-list)
        ((not (list? list-a)) (iterate-b list-a b tuple-list))
        (else
          (iterate-a (cdr list-a) b (iterate-b (car list-a) b tuple-list)))))

(define (iterate-b element list-b iter-list)
  (cond ((null? list-b) iter-list)
        ((not (list? list-b)) (cons iter-list (list element list-b)))
        (else 
          (if (null? iter-list) 
            (iterate-b element (cdr list-b) (list element (car list-b)))
            (iterate-b element (cdr list-b) (cons iter-list (list element (car list-b))))))))

(display (make-tuple (list 1 2 3) (list 4 5 6)))  (newline) 
(display (make-tuple (list 1 2 3) (list 4 5))) (newline)
(display (make-tuple (list ) (list 4 5))) (newline)
(display (make-tuple (list 1 2) (list ))) (newline)
(display (make-tuple (list 3) (list 4))) (newline)