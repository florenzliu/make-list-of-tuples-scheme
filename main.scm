(define (func list-a list-b)
  (iterate-a list-a list-b ()))

(define (iterate-a list-a b tuple-list)
  (cond ((null? list-a) tuple-list)
        ((not (list? list-a)) (iterate-b list-a b tuple-list))
        (else
          (iterate-a (cdr list-a) b (iterate-b (car list-a) b tuple-list)))))

(define (iterate-b element list-b iter-list)
  (cond ((null? list-b) iter-list)
        ((not (list? list-b)) (append iter-list (list element list-b)))
        (else 
          (if (null? iter-list) 
            (iterate-b element (cdr list-b) (list (list element (car list-b))))
            (iterate-b element (cdr list-b) (append iter-list (list (list element (car list-b)))))))))

(display (func (list 1 2 3) (list 4 5 6)))  (newline) 
;(display (func (list 1 2 3) (list 4 5))) (newline)
;(display (func (list ) (list 4 5))) (newline)
;(display (func (list 1 2) (list ))) (newline)
;(display (func (list 3) (list 4))) (newline)