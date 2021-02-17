(declare (usual-integrations))

(load "table.scm")
(load "syntax.scm")
(load "apply.scm")

(define eval-table (make-table))

(define (expression-type exp)
  (car exp))

(define (install-eval-definitions)
  (define (list-of-values exps env)
    (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

  (define (eval-and exps env)
    (cond ((null? exps) #t)
	  ((not (true? (eval (car exps) env)))
	   #f)
	  (else
	    (eval-and (cdr exps) env))))

  (define (eval-or exps env)
    (cond ((null? exps) #f)
	  ((true? (eval (car exps) env))
	   #t)
	  (else
	    (eval-or (cdr exps) env))))

  (define (eval-sequence exps env)
    (cond ((last-exp? exps)
	   (eval (first-exp exps) env))
	  (else
	    (eval (first-exp exps) env)
	    (eval (rest-exps exps) env))))

  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
			 (eval (assignment-value exp) env)
			 env)
    'ok)

  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
		      (eval (definition-value exp) env))
    'ok)

  (define (eval-unbinding exp env)
    (remove-binding! (definition-variable exp)
		     env)
    'ok)


  (define (list-of-values-ltr exps env)
    (if (no-operands? exps)
      '()
      (let (first-val (eval (first-operand exps) env))
	(cons first-val
	      (list-of-values (rest-operands exps) env)))))

  (define (list-of-values-rtl exps env)
    (if (no-operands? exps env)
      '()
      (let (right (list-of-values-rtl (rest-operands exps) env))
	(let (left (eval (first-operand exps) env))
	  (cons left right)))))

  (define (eval-let exp env)
    (eval (let->combination exp) env))

  (insert! 'set! eval-assignment eval-table)
  (insert! 'define eval-definition eval-table)
  (insert! 'if eval-if eval-table)
  (insert! 'lambda (lambda (exp env)
		       (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	   eval-table)
  (insert! 'begin (lambda (exp env)
		      (eval-sequence (begin-actions exp) env))
	   eval-table)
  (insert! 'cond 
	   (lambda (exp env)
	     (eval (cond->if exp) env))
	   eval-table)
  (insert 'and (lambda (exp env)
		(eval-and (and-expressions exp) env))
	  eval-table)
  (insert 'or (lambda (exp env)
		(eval-or (or-expressions exp) env))
	  eval-table)
  (insert 'let eval-let eval-table)
  (insert 'let* (lambda (exp env)
		  (eval (let*->nested-lets exp) env)))
  (insert 'make-unbound! eval-unbinding env)
  'done)

(install-eval-definitions)

(define (eval exp env)
  (let ((fun (lookup (expression-type exp) eval-table)))
    (cond ((self-evaluating? exp) exp)
	  ((variable? exp) (lookup-variable-value exp env))
	  ((quoted? exp) (text-of-quotation exp))
	  (fun (fun exp env))
	  ((application? exp)
	   (apply (eval (operator exp env)
			(list-of-values (operands exp) env))))
	  (else "Unknown operation type EVAL: exp"))))
