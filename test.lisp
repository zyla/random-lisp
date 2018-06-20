((lambda (f)
   ((lambda (x) (f (lambda (y) ((x x) y))))
    (lambda (x) (f (lambda (y) ((x x) y))))))
 (lambda (fac)
   (lambda (n)
     (if (zero? n)
       1
       (* n (fac (- n 1)))))))
