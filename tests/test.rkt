#lang racket/base
(require racket/match
	 racket/set
	 compiler/zo-structs)

(provide demod-test)


(define (foldl. f ys . xs)
  (match ys
    [(list)
     (apply values xs)]
    [(cons y ys)
     (call-with-values
       (lambda () (apply f y xs))
       (lambda xs (apply foldl. f ys xs)))]))

(define current-toplevel-pos (make-parameter '(none)))

(define (toplevel-dependencies zo)
  (define (inner zo seen tls)
    (if (set-member? seen zo)
      (values seen tls)
      (let ([seen (set-add seen zo)])
	(match zo
	  [(or (? boolean?)
	       (? byte-regexp?)
	       (? bytes?)
	       (? char?)
	       (? list?)
	       (? keyword?)
	       (? number?)
	       (? regexp?)
	       (? string?)
	       (? symbol?)
	       (? void?))
	   (values seen tls)]
	  [(application rator rands)
	   (let-values ([(seen tls) (inner rator seen tls)])
	     (foldl. inner seen tls rands))]
	  [(apply-values proc args-expr)
	   (let*-values ([(seen tls) (inner proc seen tls)]
			 [(seen tls) (inner args-expr seen tls)])
	     (values seen tls))]
	  [(assign (toplevel depth pos const? ready?) rhs undef-ok?)
	   (inner rhs seen (foldl (lambda (tl-pos tls) (hash-update tls tl-pos (lambda (s) (set-add s pos)) (seteqv))) tls (current-toplevel-pos)))]
	  [(beg0 seq)
	   (foldl. inner seen tls seq)]
	  [(boxenv pos body)
	   (inner body seen tls)]
	  [(branch test then else)
	   (let*-values ([(seen tls) (inner test seen tls)]
			 [(seen tls) (inner then seen tls)]
			 [(seen tls) (inner else seen tls)])
	     (values seen tls))]
	  [(case-lam name clauses)
	   (foldl. inner seen tls clauses)]
	  [(closure code gen-id)
	   (inner code seen tls)]
	  [(def-values ids rhs)
	   (parameterize ([current-toplevel-pos (map toplevel-pos ids)])
	     (inner rhs seen tls))]
	  [(or (install-value _ _ _ rhs body)
	       (let-one rhs body _ _))
	   (let*-values ([(seen tls) (inner rhs seen tls)]
			 [(seen tls) (inner body seen tls)])
	     (values seen tls))]
	  [(lam name flags num-params param-types rest? closure-map
		closure-types toplevel-map max-let-depth body)
	   (inner body seen tls)] ; maybe can use toplevel-map
	  [(let-rec procs body)
	   (let-values ([(seen tls) (foldl. inner seen tls procs)])
	     (inner body seen tls))]
	  [(let-void count boxes? body)
	   (inner body seen tls)]
	  [(or (localref _ _ _ _ _)
	       (primval _))
	   (values seen tls)]
	  [(req reqs dummy)
	   (inner dummy seen tls)]
	  [(seq forms)
	   (foldl. inner seen tls forms)]
	  [(toplevel depth pos const ready?)
	   (values seen tls)]
	  [(varref toplevel dummy)
	   (let-values ([(seen tls) (inner toplevel seen tls)])
	     (inner dummy seen tls))]
	  [(with-cont-mark key val body)
	   (let*-values ([(seen tls) (inner key seen tls)]
			 [(seen tls) (inner val seen tls)]
			 [(seen tls) (inner body seen tls)])
	     (values seen tls))]))))
  (let-values ([(seen tls) (inner zo (seteq) (hasheqv))])
    tls))

(define should-kill? #f)

(define (demod-test zo)
  #;(if should-kill?
      (error 'demod-test "only running once")
      (set! should-kill? #t))
  (match zo
    [(compilation-top mld (prefix nl tls stxs) code)
     (printf "max-let-depth: ~a\n" mld)
     (printf "num-lifts: ~a\n" nl)
     (printf "toplevels not define-values\n")
     #;(for ([i (in-naturals)]
	   [tl (in-list tls)])
       (printf "~a\t~a\n" i tl))
     #;(printf "stxs: ~a\n" stxs)
     (for ([form (in-list (splice-forms code))])
       (unless (def-values? form)
	 (displayln form))
      #; (displayln (toplevel-dependencies form)))])
  zo)

#;(define (demod-test zo)
  (match zo
    [(compilation-top mld prefix (splice forms))
     (compilation-top mld prefix (splice (cons #s((req form 0 zo 0) #s((stx zo 0) #s((wrapped zo 0) #%network () clean)) #s((toplevel expr 0 form 0 zo 0) 0 0 #f #f)) forms)))]))
