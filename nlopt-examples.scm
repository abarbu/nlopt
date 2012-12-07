(use lolevel)(use posix)(require-extension nlopt)

(define (myfunc x grad?)
 (vector
  (sqrt (vector-ref x 1))
  (vector 0.0 (/ 0.5 (sqrt (vector-ref x 1))))))

(define (myconstraint x grad? a b)
 (let ((x0 (vector-ref x 0)) (x1 (vector-ref x 1)))
  (vector (- (expt (+ (* a x0) b) 3) x1)
          (vector (* 3 a (expt (+ (* a x0) b) 2)) -1.0))))

(define (minimize-with-nlopt f g i)
 (nlopt:optimize
  ;; nlopt:ld-mma 
  nlopt:ln-sbplx
  i
  (lambda (opt)
   (nlopt:set-min-objective opt (lambda (x g?) (vector (f x) (if g? ((g f) x)))))
   (nlopt:set-lower-bounds opt (vector (- +inf.0) 0))
   (nlopt:add-inequality-constraint opt (lambda (x grad) (myconstraint x grad 2 0)) 1e-8)
   (nlopt:add-inequality-constraint opt (lambda (x grad) (myconstraint x grad -1 1)) 1e-8)
   (nlopt:set-xtol-rel opt 1e-4))))

(minimize-with-nlopt 
 (lambda (x) (sqrt (vector-ref x 1)))
 ;; gradient-F, TODO add this back in
 #f
 '#(1.234 5.678))

;; Should equal: #(#(-0.4756041666667601 0.) #(0.) 4)

(use AD)

(define (minimize-with-nlopt f g i)
 (nlopt:optimize
  nlopt:ld-mma 
  i
  (lambda (opt)
   (nlopt:set-min-objective opt (lambda (x g?) (vector (f x) (if g? ((g f) x)))))
   (nlopt:set-lower-bounds opt (vector (- +inf.0) 0))
   (nlopt:add-inequality-constraint opt (lambda (x grad) (myconstraint x grad 2 0)) 1e-8)
   (nlopt:add-inequality-constraint opt (lambda (x grad) (myconstraint x grad -1 1)) 1e-8)
   (nlopt:set-xtol-rel opt 1e-4))))

(minimize-with-nlopt 
 (lambda (x) (display x)(newline) (sqrt (vector-ref x 1)))
 gradient-F
 '#(1.234 5.678))
