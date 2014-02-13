#lang racket/base
(require racket/match
	 racket/set
	 compiler/zo-structs)

(provide flow)

(define (foldl. f ys . xs)
  (match ys
    [(list)
     (apply values xs)]
    [(cons y ys)
     (call-with-values
       (lambda () (apply f y xs))
       (lambda xs (apply foldl. f ys xs)))]))

(define (transitive-closure seed relation)
  (let ([empty-set (set-clear seed)])
    (define (inner x seen)
      (if (set-member? seen x)
	seen
	(let ([seen (set-add seen x)])
	  (foldl inner seen (set->list (hash-ref relation x empty-set))))))
    (foldl inner empty-set (set->list seed))))

(define current-toplevel-pos (make-parameter '(root)))

(define (toplevel-dependencies zo uses is-changed-by)
  (define (inner zo seen uses is-changed-by)
    (if (set-member? seen zo)
      (values seen uses is-changed-by)
      (let ([seen (set-add seen zo)])
	(match zo
	  [(or (? boolean?)
	       (? byte-regexp?)
	       (? bytes?)
	       (? char?)
	       (? hash?)
	       (? list?)
	       (? keyword?)
	       (? number?)
	       (? path?)
	       (? regexp?)
	       (? string?)
	       (? symbol?)
	       (? vector?)
	       (? void?))
	   (values seen uses is-changed-by)]
	  [(application rator rands)
	   (let-values ([(seen uses is-changed-by) (inner rator seen uses is-changed-by)])
	     (foldl. inner rands seen uses is-changed-by))]
	  [(apply-values proc args-expr)
	   (let*-values ([(seen uses is-changed-by) (inner proc seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner args-expr seen uses is-changed-by)])
	     (values seen uses is-changed-by))]
	  [(assign (toplevel depth pos const? ready?) rhs undef-ok?)
	   (let ([is-changed-by (foldl (lambda (tl-pos is-changed-by)
					 (hash-update is-changed-by
						      pos
						      (lambda (s) (set-add s tl-pos))
						      (seteqv)))
				       is-changed-by
				       (current-toplevel-pos))])
	     (inner rhs seen uses is-changed-by))]
	  [(beg0 seq)
	   (foldl. inner seq seen uses is-changed-by)]
	  [(boxenv pos body)
	   (inner body seen uses is-changed-by)]
	  [(branch test then else)
	   (let*-values ([(seen uses is-changed-by) (inner test seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner then seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner else seen uses is-changed-by)])
	     (values seen uses is-changed-by))]
	  [(case-lam name clauses)
	   (foldl. inner clauses seen uses is-changed-by)]
	  [(closure code gen-id)
	   (inner code seen uses is-changed-by)]
	  [(def-values ids rhs)
	   (parameterize ([current-toplevel-pos (map toplevel-pos ids)])
	     (inner rhs seen uses is-changed-by))]
	  [(or (install-value _ _ _ rhs body)
	       (let-one rhs body _ _))
	   (let*-values ([(seen uses is-changed-by) (inner rhs seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner body seen uses is-changed-by)])
	     (values seen uses is-changed-by))]
	  [(lam name flags num-params param-types rest? closure-map
		closure-types toplevel-map max-let-depth body)
	   (inner body seen uses is-changed-by)] ; maybe can use toplevel-map
	  [(let-rec procs body)
	   (let-values ([(seen uses is-changed-by) (foldl. inner procs seen uses is-changed-by)])
	     (inner body seen uses is-changed-by))]
	  [(let-void count boxes? body)
	   (inner body seen uses is-changed-by)]
	  [(or (localref _ _ _ _ _)
	       (primval _))
	   (values seen uses is-changed-by)]
	  [(req reqs dummy)
	   (inner dummy seen uses is-changed-by)] ; might not need this...
	  [(seq forms)
	   (foldl. inner forms seen uses is-changed-by)]
	  [(toplevel depth pos const ready?)
	   (let ([uses (foldl (lambda (tl-pos uses)
				(hash-update uses
					     tl-pos
					     (lambda (s) (set-add s pos))
					     (seteqv)))
			      uses
			      (current-toplevel-pos))])
	     (values seen uses is-changed-by))]
	  [(topsyntax depth pos midpt)
	   (values seen uses is-changed-by)]
	  [(varref toplevel dummy)
	   (let*-values ([(seen uses is-changed-by) (inner toplevel seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner dummy seen uses is-changed-by)])
	     (values seen uses is-changed-by))]
	  [(with-cont-mark key val body)
	   (let*-values ([(seen uses is-changed-by) (inner key seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner val seen uses is-changed-by)]
			 [(seen uses is-changed-by) (inner body seen uses is-changed-by)])
	     (values seen uses is-changed-by))]))))
  (let-values ([(seen uses is-changed-by) (inner zo (seteq) uses is-changed-by)])
    (values uses is-changed-by)))


(define (flow zo)
  (match zo
    [(compilation-top mld (prefix nl tls stxs) code)
     (let-values ([(defined-at uses is-changed-by)
		   (for/fold ([defined-at (hasheqv)]
			      [uses (hasheqv)]
			      [is-changed-by (hasheqv)])
		       ([form (in-list (splice-forms code))]
			[i (in-naturals)])
		     (let-values ([(uses is-changed-by) (toplevel-dependencies form uses is-changed-by)])
		       (let ([defined-at (match form
					   [(def-values ids rhs)
					    (foldl (lambda (pos defined-at)
						     (hash-set defined-at pos i))
						   defined-at
						   (map toplevel-pos ids))]
					   [_
					    (hash-update defined-at 'root (lambda (s) (set-add s i)) (seteqv))])])
			 (values defined-at uses is-changed-by))))])
	   (let ([used (transitive-closure (hash-ref uses 'root (seteqv)) uses)]
		 [changers (transitive-closure (hash-ref is-changed-by 'root (seteqv)) is-changed-by)])
	     (if (set-empty? changers)
	       (begin
		 (displayln (for/fold ([toplevel-indices (hash-ref defined-at 'root (seteqv))])
				([used-toplevel (in-set used)])
			      (if (hash-has-key? defined-at used-toplevel)
				(set-add toplevel-indices (hash-ref defined-at used-toplevel))
				toplevel-indices)))
		 zo)
	       (begin
		 (displayln "some toplevel changed; not removing anything")
		 zo))))]))

#;(define (demod-test zo)
  (match zo
    [(compilation-top mld prefix (splice forms))
     (compilation-top mld prefix (splice (cons #s((req form 0 zo 0) #s((stx zo 0) #s((wrapped zo 0) #%network () clean)) #s((toplevel expr 0 form 0 zo 0) 0 0 #f #f)) forms)))]))
