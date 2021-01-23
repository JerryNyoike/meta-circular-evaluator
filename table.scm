(declare (usual-integrations))

(define make-table
  (let ((table (list '*eval* '())))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
	(if record
	  (cadr record)
	  false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
	(if record
	  (set-cdr! record value)
	  (set-cdr! table (cons 
			    (list key value)
			    (cdr table)))))
      'ok)
    
    (define (dispatch action)
      (cond ((eq? action 'lookup) lookup)
	    ((eq? action 'insert!) insert!)
	    (else (error 
		    "Unsupported operation MAKE-TABLE:" action))))
    dispatch))

(define (lookup key table)
  ((table 'lookup) key))

(define (insert! key value table)
  ((table 'insert!) key value))
