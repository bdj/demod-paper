#lang racket/base
(require racket/match
	 racket/set
	 compiler/zo-structs)

(provide gc)


(define (foldl. f ys . xs)
  (match ys
    [(list)
     (apply values xs)]
    [(cons y ys)
     (call-with-values
       (lambda () (apply f y xs))
       (lambda xs (apply foldl. f ys xs)))]))

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

(define should-kill? #f)

(define (gc zo)
  #;(if should-kill?
      (error 'demod-test "only running once")
      (set! should-kill? #t))
  (match zo
    [(compilation-top mld (prefix nl tls stxs) code)
     (printf "max-let-depth: ~a\n" mld)
     (printf "num-lifts: ~a\n" nl)
     #;(printf "toplevels not define-values\n")
     #;(for ([i (in-naturals)]
	   [tl (in-list tls)])
       (printf "~a\t~a\n" i tl))
     #;(printf "stxs: ~a\n" stxs)
     (let-values ([(uses is-changed-by)
		   (foldl. toplevel-dependencies (splice-forms code) (hasheqv) (hasheqv))])
       (let ([W (lambda (f)
		  (let ([seen (make-hasheqv)])
		    (lambda (v)
		      (unless (hash-has-key? seen v)
			(hash-set! seen v #t)
			(f v)))))])
	 (let ([show (lambda (verb map)
		       (letrec ([show-inner (W (lambda (key)
					       (for ([val (in-set (hash-ref map key (seteqv)))])
						 (printf "~a ~a ~a\n" key verb val)
						 (show-inner val))))])
			 (show-inner 'root)))])
	   (show "uses" uses)
	   (show "is changed by" is-changed-by))))])
  zo)

#;(define (demod-test zo)
  (match zo
    [(compilation-top mld prefix (splice forms))
     (compilation-top mld prefix (splice (cons #s((req form 0 zo 0) #s((stx zo 0) #s((wrapped zo 0) #%network () clean)) #s((toplevel expr 0 form 0 zo 0) 0 0 #f #f)) forms)))]))
