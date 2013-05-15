(define empty '())
(define isEmpty null?)

;;(define cons cons)
(define head car)
(define tail cdr)

(define (++ xs ys)
  (cond
   ((isEmpty xs) ys)
   (else (cons (car xs)
               (++ (cdr xs)
                   ys)))))

(define (update xs i y)
  (cond
   ((isEmpty xs) (error 'Subscript))
   ((= i 0) (cons y
                  (tail xs)))
   (else (cons (head xs)
               (update (tail xs)
                       (- i 1)
                       y)))))

(define (suffixes xs)
  (cond
   ((isEmpty xs) empty)
   (else (cons xs
               (suffixes (tail xs))))))
