(declare (usual-integrations))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc)
    args))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procdeure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	   (procedure-body procedure)
	   (extend-environment
	     (procedure-parameters procedure)
	     arguments
	     (procedure-environment procedure))))
	(else
	  (error "Unknown procedure type: APPLY" procedure))))
