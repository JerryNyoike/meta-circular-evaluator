(declare (usual-integrations))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value) exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? exp)
    (caddr exp)
    (make-lambda
      (cdadr exp)	;params
      (cddr exp))))	;body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (cond ((null? (cdddr exp)) false)
	(else (cadddr exp))))
(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))
(define (seq->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (if (cond-else-clause? first)
	(if (null? rest)
	  (seq->exp (cond-actions first))
	  (error "ELSE clause isn't last: COND->IF" clauses))
	(make-if (cond-predicate first)
		 (let ((actions (cond-actions first)))
		   (if (eq? (car actions) '=>)
		     (list ((cadr actions) (cond-predicate first)))
		     (seq->exp (cond-actions first))))
		 (expand-clauses rest))))))

;; louis functions
(define (l-application? exp) (tagged-list? exp 'call))
(define (l-operator exp) (cadr exp))
(define (l-operands exp) (cddr exp))

;; and
(define (and? exp) (tagged-list exp 'and))
(define (expressions exp) (cdr exp))
(define (last-expression? exp) (null? (cdr exp)))
(define (and->if exps) (expand-and-expressions (expressions exp)))
(define (expand-and-expressions exps)
  (cond ((null? exps) 'true)
	((last-expression? exps) (car exps))
	(else
	  (make-if (car exps)
		   (expand-and-expressions (cdr exps))
		   'false))))

;; or
(define (or? exp) (tagged-list exp 'or))
(define (or->if exp) (expand-or-expressions (expressions exp)))
(define (expand-or-expressions exps)
  (cond ((null? exps)
	 'false)
	((last-expression? exps) (car exps))
	(else
	  (make-if (car exps)
		   (expand-or-expressions (cdr exps))
		   'true))))

;; let expressions
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-binding-names bindings)
  (map car bindings))
(define (let-binding-exps bindings)
  (map cdr bindings))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cond ((named-let? exp)
	 (let ((bindings (named-let-bindings exp)))
	   (append
	     (make-lambda (let-binding-names bindings)
			  (named-let-body exp))
	     (let-binding-exps bindings))))
	(else
	  (let ((bindings (let-bindings exp)))
	    (append
	      (make-lambda (let-binding-names bindings) ;params
			   (let-body exp))		;body
	      (let-binding-exps bindings))))))
(define (make-let bindings body)
  (cons 'let (list bindings) body))
(define (last-binding? binding)
  (null? (cdr binding)))

;; let*
(define (let*? exp) (tagged-list? 'let* exp))
(define (let*->nested-lets exp)
  (define (let*->lets bindings body)
    (cond ((last-binding? bindings)
	   (make-let (car bindings)
		     body))
	  (else
	    (make-let (car bindings)
		      (let*->lets (cdr bindings)
				  body)))))
  (let ((bindings (let-bindings exp))
	(body (let-body exp)))
    (let*->lets bindings body)))

;; named let
(define (named-let? exp) (and (let? exp) (not (pair? (cadr exp)))))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))
