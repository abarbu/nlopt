(module nlopt * 
(import chicken scheme srfi-1 foreign posix lolevel extras)
(use posix lolevel foreigners)
(require-extension define-structure scheme2c-compatibility traversal)

;; TODO use chicken's XXXvector types with sharing so that there's no
;;    overhead copying to C
;; TODO Remove nlopt: prefix
;; TODO use foreign enums

;; API Change: 
;;   nlopt:optimize -> nlopt:optimize-now
;;   optimize-with-nlopt -> nlopt:optimize

(foreign-declare "#include <nlopt.h>")

(define-structure nlopt pointer)
(define-foreign-type nlopt c-pointer nlopt-pointer make-nlopt)

(foreign-declare "
 double nlopt_func_call_gate(unsigned n, const double *x,
                             double *grad, void *data) {
  C_word *p0 = C_alloc(C_SIZEOF_POINTER);
  C_word *p1 = C_alloc(C_SIZEOF_POINTER);
  C_save(C_fix(n)); 
  C_save(C_mpointer(&p0, (double*)x)); 
  C_save(C_mpointer(&p1, grad));
  return C_flonum_magnitude(C_callback(CHICKEN_gc_root_ref((C_word)data), 3));
}
")

(foreign-declare "
 double nlopt_mfunc_call_gate(unsigned m, double *result,
                              unsigned n, const double *x,
                              double *grad, void *data) {
  C_word *p0 = C_alloc(C_SIZEOF_POINTER);
  C_word *p1 = C_alloc(C_SIZEOF_POINTER);
  C_word *p2 = C_alloc(C_SIZEOF_POINTER);
  C_save(C_fix(m)); 
  C_save(C_mpointer(&p0, result)); 
  C_save(n); 
  C_save(C_mpointer(&p1, (double*)x));
  C_save(C_mpointer(&p2, grad));
  return C_flonum_magnitude(C_callback(CHICKEN_gc_root_ref((C_word)data), 5));
}
")

(define nlopt:gn-direct (c-value int "NLOPT_GN_DIRECT"))
(define nlopt:gn-direct-l (c-value int "NLOPT_GN_DIRECT_L"))
(define nlopt:gn-direct-l-rand (c-value int "NLOPT_GN_DIRECT_L_RAND"))
(define nlopt:gn-direct-noscal (c-value int "NLOPT_GN_DIRECT_NOSCAL"))
(define nlopt:gn-direct-l-noscal (c-value int "NLOPT_GN_DIRECT_L_NOSCAL"))
(define nlopt:gn-direct-l-rand-noscal (c-value int "NLOPT_GN_DIRECT_L_RAND_NOSCAL"))
(define nlopt:gn-orig-direct (c-value int "NLOPT_GN_ORIG_DIRECT"))
(define nlopt:gn-orig-direct-l (c-value int "NLOPT_GN_ORIG_DIRECT_L"))
(define nlopt:gd-stogo (c-value int "NLOPT_GD_STOGO"))
(define nlopt:gd-stogo-rand (c-value int "NLOPT_GD_STOGO_RAND"))
(define nlopt:ld-lbfgs-nocedal (c-value int "NLOPT_LD_LBFGS_NOCEDAL"))
(define nlopt:ld-lbfgs (c-value int "NLOPT_LD_LBFGS"))
(define nlopt:ln-praxis (c-value int "NLOPT_LN_PRAXIS"))
(define nlopt:ld-var1 (c-value int "NLOPT_LD_VAR1"))
(define nlopt:ld-var2 (c-value int "NLOPT_LD_VAR2"))
(define nlopt:ld-tnewton (c-value int "NLOPT_LD_TNEWTON"))
(define nlopt:ld-tnewton-restart (c-value int "NLOPT_LD_TNEWTON_RESTART"))
(define nlopt:ld-tnewton-precond (c-value int "NLOPT_LD_TNEWTON_PRECOND"))
(define nlopt:ld-tnewton-precond-restart (c-value int "NLOPT_LD_TNEWTON_PRECOND_RESTART"))
(define nlopt:gn-crs2-lm (c-value int "NLOPT_GN_CRS2_LM"))
(define nlopt:gn-mlsl (c-value int "NLOPT_GN_MLSL"))
(define nlopt:gd-mlsl (c-value int "NLOPT_GD_MLSL"))
(define nlopt:gn-mlsl-lds (c-value int "NLOPT_GN_MLSL_LDS"))
(define nlopt:gd-mlsl-lds (c-value int "NLOPT_GD_MLSL_LDS"))
(define nlopt:ld-mma (c-value int "NLOPT_LD_MMA"))
(define nlopt:ln-cobyla (c-value int "NLOPT_LN_COBYLA"))
(define nlopt:ln-newuoa (c-value int "NLOPT_LN_NEWUOA"))
(define nlopt:ln-newuoa_bound (c-value int "NLOPT_LN_NEWUOA_BOUND"))
(define nlopt:ln-neldermead (c-value int "NLOPT_LN_NELDERMEAD"))
(define nlopt:ln-sbplx (c-value int "NLOPT_LN_SBPLX"))
(define nlopt:ln-auglag (c-value int "NLOPT_LN_AUGLAG"))
(define nlopt:ld-auglag (c-value int "NLOPT_LD_AUGLAG"))
(define nlopt:ln-auglag-eq (c-value int "NLOPT_LN_AUGLAG_EQ"))
(define nlopt:ld-auglag-eq (c-value int "NLOPT_LD_AUGLAG_EQ"))
(define nlopt:ln-bobyqa (c-value int "NLOPT_LN_BOBYQA"))
(define nlopt:gn-isres (c-value int "NLOPT_GN_ISRES"))
(define nlopt:auglag (c-value int "NLOPT_AUGLAG"))
(define nlopt:auglag-eq (c-value int "NLOPT_AUGLAG_EQ"))
(define nlopt:g-mlsl (c-value int "NLOPT_G_MLSL"))
(define nlopt:g-mlsl-lds (c-value int "NLOPT_G_MLSL_LDS"))
(define nlopt:ld-slsqp (c-value int "NLOPT_LD_SLSQP"))

(define nlopt:algorithm-name (c-function c-string ("nlopt_algorithm_name" int)))

(define nlopt:failure (c-value int "NLOPT_FAILURE"))
(define nlopt:invalid-args (c-value int "NLOPT_INVALID_ARGS"))
(define nlopt:out-of-memory (c-value int "NLOPT_OUT_OF_MEMORY"))
(define nlopt:roundoff-limited (c-value int "NLOPT_ROUNDOFF_LIMITED"))
(define nlopt:forced-stop (c-value int "NLOPT_FORCED_STOP"))
(define nlopt:success (c-value int "NLOPT_SUCCESS"))
(define nlopt:stopval-reached (c-value int "NLOPT_STOPVAL_REACHED"))
(define nlopt:ftol-reached (c-value int "NLOPT_FTOL_REACHED"))
(define nlopt:xtol-reached (c-value int "NLOPT_XTOL_REACHED"))
(define nlopt:maxeval-reached (c-value int "NLOPT_MAXEVAL_REACHED"))
(define nlopt:maxtime-reached (c-value int "NLOPT_MAXTIME_REACHED"))

;;(define nlopt:srand (c-function void ("nlopt_srand" long)))
(define nlopt:srand (c-function void ("nlopt_srand" int)))
(define nlopt:srand-time (c-function void ("nlopt_srand_time")))

(define nlopt:create (c-function nlopt ("nlopt_create" int unsigned-int)))
(define nlopt:destroy (c-function void ("nlopt_destroy" nlopt)))
(define nlopt:copy (c-function nlopt ("nlopt_copy" nlopt)))

(define (nlopt:wrap-single f)
 (lambda (n x grad)
  (let ((v (f (c-inexact-array->vector x c-sizeof-double n #t)
              (not (pointer=? (address->pointer 0) grad)))))
   (when (not (pointer=? (address->pointer 0) grad))
    (vector->c-inexact-array grad (vector-ref v 1) c-sizeof-double #t))
   (vector-ref v 0))))

(define (with-nlopt kind arg f)
 (let* ((opt (nlopt:create kind arg))
	(result (f opt)))
  (nlopt:destroy opt)
  result))

(define (nlopt:optimize kind initial f)
 (let ((opt (nlopt:create kind (vector-length initial))))
  (f opt)
  (let ((result (nlopt:optimize-now opt initial)))
   (nlopt:destroy opt)
   result)))

(define (nlopt:optimize-now nlopt initial)
 (with-vector->c-array
  (lambda (initial-array)
   (with-alloc
    c-sizeof-double
    (lambda (objective-value)
     (let ((result ((foreign-safe-lambda int "nlopt_optimize" nlopt c-pointer c-pointer)
		    nlopt initial-array objective-value)))
      (vector
       (c-inexact-array->vector initial-array c-sizeof-double
				(vector-length initial) #t)
       (c-inexact-array->vector objective-value c-sizeof-double 1 #t)
       result)))))
  c-double-set! c-sizeof-double initial))

(define (nlopt:set-min-objective nlopt f)
 (let ((root ((foreign-lambda*
               c-pointer ((nlopt nlopt) (scheme-object proc))
               "void *thunk = CHICKEN_new_gc_root();"
               "CHICKEN_gc_root_set(thunk, proc);"
               "nlopt_set_min_objective(nlopt, nlopt_func_call_gate, thunk);"
               "C_return(thunk);")
              nlopt (nlopt:wrap-single f))))
  (set-finalizer!
   nlopt
   (lambda (nlopt)
    ((foreign-lambda* 
      void ((c-pointer root))
      "CHICKEN_delete_gc_root(root); C_return(0);") root)))
  #f))
(define (nlopt:set-max-objective nlopt f)
 (let ((root ((foreign-lambda*
               c-pointer ((nlopt nlopt) (scheme-object proc))
               "void *thunk = CHICKEN_new_gc_root();"
               "CHICKEN_gc_root_set(thunk, proc);"
               "nlopt_set_max_objective(nlopt, nlopt_func_call_gate, thunk);"
               "C_return(thunk);")
              nlopt (nlopt:wrap-single f))))
  (set-finalizer!
   nlopt
   (lambda (nlopt)
    ((foreign-lambda* 
      void ((c-pointer root))
      "CHICKEN_delete_gc_root(root); C_return(0);") root)))
  #f))

(define nlopt:get-algorithm (c-function int ("nlopt_get_algorithm" nlopt)))
(define nlopt:get-dimension (c-function unsigned-int ("nlopt_get_dimension" nlopt)))

;; TODO Error checking

(define (nlopt:set-lower-bounds nlopt bounds)
 (with-vector->c-array
  (lambda (bounds-array)
   ((c-function int ("nlopt_set_lower_bounds" nlopt c-pointer))
    nlopt bounds-array))
  c-double-set!
  c-sizeof-double
  bounds))
(define nlopt:set-lower-bounds1
 (c-function int ("nlopt_set_lower_bounds1" nlopt double)))
(define (nlopt:get-lower-bounds nlopt)
 (let ((dimension (nlopt:get-dimension nlopt)))
  (with-alloc
   (* c-sizeof-double (nlopt:get-dimension nlopt))
   (lambda (array)
    ((c-function int ("nlopt_get_lower_bounds" nlopt c-pointer))
     nlopt array)
    (c-inexact-array->vector array c-sizeof-double dimension #t)))))

(define (nlopt:set-upper-bounds nlopt bounds)
 (with-vector->c-array
  (lambda (bounds-array)
   ((c-function int ("nlopt_set_upper_bounds" nlopt c-pointer))
    nlopt bounds-array))
  c-double-set!
  c-sizeof-double
  bounds))
(define nlopt:set-upper-bounds1
 (c-function int ("nlopt_set_upper_bounds1" nlopt double)))
(define (nlopt:get-upper-bounds nlopt)
 (let ((dimension (nlopt:get-dimension nlopt)))
  (with-alloc
   (* c-sizeof-double (nlopt:get-dimension nlopt))
   (lambda (array)
    ((c-function int ("nlopt_get_upper_bounds" nlopt c-pointer))
     nlopt array)
    (c-inexact-array->vector array c-sizeof-double dimension #t)))))

(define nlopt:remove-inequality-constraints
 (c-function int ("nlopt_remove_inequality_constraints" nlopt)))
(define (nlopt:add-inequality-constraint nlopt f tol)
 (let ((root ((foreign-lambda*
               c-pointer ((nlopt nlopt) (scheme-object proc) (double tol))
               "void *thunk = CHICKEN_new_gc_root();"
               "CHICKEN_gc_root_set(thunk, proc);"
               "nlopt_add_inequality_constraint(nlopt, nlopt_func_call_gate, thunk, tol);"
               "C_return(thunk);")
              nlopt (nlopt:wrap-single f) tol)))
  (set-finalizer!
   nlopt
   (lambda (nlopt)
    ((foreign-lambda* 
      void ((c-pointer root))
      "CHICKEN_delete_gc_root(root); C_return(0);") root)))
  #f))
;; TODO (define nlopt:add-inequality-mconstraint
;;  (c-function int ("nlopt_add_inequality_mconstraint" nlopt unsigned-int c-pointer c-pointer c-pointer)))

(define nlopt:remove-equality-constraints
 (c-function int ("nlopt_remove_equality_constraints" nlopt)))
(define (nlopt:add-equality-constraint nlopt f tol)
 (let ((root ((foreign-lambda*
               c-pointer ((nlopt nlopt) (scheme-object proc) (double tol))
               "void *thunk = CHICKEN_new_gc_root();"
               "CHICKEN_gc_root_set(thunk, proc);"
               "nlopt_add_equality_constraint(nlopt, nlopt_func_call_gate, thunk, tol);"
               "C_return(thunk);")
              nlopt (nlopt:wrap-single f) tol)))
  (set-finalizer!
   nlopt
   (lambda (nlopt)
    ((foreign-lambda* 
      void ((c-pointer root))
      "CHICKEN_delete_gc_root(root); C_return(0);") root)))
  #f))
;; TODO (define nlopt:add-equality-mconstraint
;;  (c-function int ("nlopt_add_equality_mconstraint" nlopt unsigned-int c-pointer c-pointer c-pointer)))

(define nlopt:set-stopval (c-function int ("nlopt_set_stopval" nlopt double)))
(define nlopt:get-stopval (c-function double ("nlopt_get_stopval" nlopt)))

(define nlopt:set-ftol-rel (c-function int ("nlopt_set_ftol_rel" nlopt double)))
(define nlopt:get-ftol-rel (c-function double ("nlopt_get_ftol_rel" nlopt)))
(define nlopt:set-ftol-abs (c-function int ("nlopt_set_ftol_abs" nlopt double)))
(define nlopt:get-ftol-abs (c-function double ("nlopt_get_ftol_abs" nlopt)))

(define nlopt:set-xtol-rel (c-function int ("nlopt_set_xtol_rel" nlopt double)))
(define nlopt:get-xtol-rel (c-function double ("nlopt_get_xtol_rel" nlopt)))
(define nlopt:set-xtol-abs1 (c-function int ("nlopt_set_xtol_abs1" nlopt double)))
(define nlopt:set-xtol-abs (c-function int ("nlopt_set_xtol_abs" nlopt c-pointer)))
(define nlopt:get-xtol-abs (c-function int ("nlopt_get_xtol_abs" nlopt c-pointer)))

(define nlopt:set-maxeval (c-function int ("nlopt_set_maxeval" nlopt int)))
(define nlopt:get-maxeval (c-function int ("nlopt_get_maxeval" nlopt)))

(define nlopt:set-maxtime (c-function int ("nlopt_set_maxtime" nlopt double)))
(define nlopt:get-maxtime (c-function double ("nlopt_get_maxtime" nlopt)))

(define nlopt:force-stop (c-function int ("nlopt_force_stop" nlopt)))
(define nlopt:set-force-stop (c-function int ("nlopt_set_force_stop" nlopt int)))
(define nlopt:get-force-stop (c-function int ("nlopt_get_force_stop" nlopt)))

(define nlopt:set-local-optimizer
 (c-function int ("nlopt_set_local_optimizer" nlopt c-pointer)))

(define nlopt:set-population (c-function int ("nlopt_set_population" nlopt unsigned-int)))
(define nlopt:get-population (c-function unsigned-int ("nlopt_get_population" nlopt)))

(define nlopt:set-default-initial-step
 (c-function int ("nlopt_set_default_initial_step" nlopt c-pointer)))
(define nlopt:set-initial-step
 (c-function int ("nlopt_set_initial_step" nlopt c-pointer)))
(define nlopt:set-initial-step1
 (c-function int ("nlopt_set_initial_step1" nlopt double)))
(define nlopt:get-initial-step
 (c-function int ("nlopt_get_initial_step" nlopt c-pointer c-pointer)))

)
