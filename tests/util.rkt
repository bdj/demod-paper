#lang racket/base
(require racket/list
	 racket/match
	 racket/set
	 compiler/zo-structs)

(provide zo-map
	 zo-fold
	 subsequence
	 current-context)

(define current-context (make-parameter '()))

(define (foldl. f xs . accs)
  (match xs
    [(list)
     (apply values accs)]
    [(cons x xs)
     (apply* (lambda accs (apply foldl. f xs accs))
	     (apply f x accs))]))
     
(define-syntax-rule (apply* proc args-expr)
  (call-with-values (lambda () args-expr) proc))

(define-syntax fold/single
  (syntax-rules ()
    [(_ inner seen accs fields ... field)
     (match-let* ([(cons seen accs) (apply* list (apply inner fields seen accs))] ...)
       (apply inner field seen accs))]))


(define (zo-fold f zo . units)
  (define (inner zo seen . accs)
    (if (or (not (zo? zo))
	    (set-member? seen zo))
      (apply values seen accs)
      (let ([seen (set-add seen zo)]
	    [accs (with-handlers ([exn:misc:match? (lambda (e) accs)])
		    (apply* list (apply f zo accs)))])
	(match zo
	  [(application rator rands)
	   (match-let* ([(cons seen accs) (apply* list (apply inner rator seen accs))]
			[(cons seen accs) (apply* list (apply foldl. inner rands seen accs))])
	     (apply values seen accs))]
	  [(apply-values proc args-expr)
	   (fold/single inner seen accs proc args-expr)]
	  [(beg0 seq)
	   (apply foldl. inner seq seen accs)]
	  [(assign id rhs undef-ok?)
	   (fold/single inner seen accs id rhs)]
	  [(boxenv pos body)
	   (apply inner body seen accs)]
	  [(branch test then else)
	   (fold/single inner seen accs test then else)]
	  [(case-lam name clauses)
	   (apply foldl. inner clauses seen accs)]
	  [(closure code gen-id)
	   (apply inner code seen accs)]
	  [(def-values ids rhs)
	   (match-let* ([(cons seen accs) (apply* list (apply foldl. inner ids seen accs))]
		        [(cons seen accs) (apply* list (apply inner rhs seen accs))])
	     (apply values seen accs))]
	  [(install-value count pos boxes? rhs body)
	   (fold/single inner seen accs rhs body)]
	  [(lam name flags num-params param-types rest? closure-map
		closure-types toplevel-map max-let-depth body)
	   (apply inner body seen accs)]
	  [(let-one rhs body type unused?)
	   (fold/single inner seen accs rhs body)]
	  [(let-rec procs body)
	   (match-let* ([(cons seen accs) (apply* list (apply foldl. inner procs seen accs))]
			[(cons seen accs) (apply* list (apply inner body seen accs))])
	     (apply values seen accs))]
	  [(let-void count boxes? body)
	   (apply inner body seen accs)]
	  [(localref unbox? pos clear? other-clears? type)
	   (apply values seen accs)]
	  [(primval id)
	   (apply values seen accs)]
	  [(req reqs dummy)
	   (apply inner dummy seen accs)]
	  [(seq forms)
	   (apply foldl. inner forms seen accs)]
	  [(toplevel depth pos const? ready?)
	   (apply values seen accs)]
	  [(topsyntax depth pos midpt)
	   (apply values seen accs)]
	  [(varref toplevel dummy)
	   (fold/single inner seen accs toplevel dummy)]
	  [(with-cont-mark key val body)
	   (fold/single inner seen accs key val body)]

))))
  (match-let ([(cons seen accs) (apply* list (apply inner zo (seteqv) units))])
    (apply values accs)))
	  

(define (zo-map f zo . args)
  (define ((inner seen) zo . args)
    (if (zo? zo)
      (if (hash-has-key? seen zo)
        (hash-ref seen zo)
        (let ([seen (hash-set seen zo (make-placeholder #f))])
          (let ([zo* (apply (f (inner seen)) zo args)])
            (placeholder-set! (hash-ref seen zo) zo*)
            zo*)))
      zo))
  (make-reader-graph (apply (inner (hasheq)) zo args)))

; selective map: only specify which forms you want to change
; - should preserve cycles and "diamond" sharing
; - pass a procedure to recur?
#|
(zo-map
 (lambda (inner)
   (lambda (zo env)
     (match zo
       [(toplevel depth pos const? ready?)
	(toplevel 0 pos const? ready?)]
       [(localref depth ...)
	(localref (env-lookup env depth) ...)]
       [(application rator rands)
	(let ([env (env-append (map fresh rands) env)])
	  (application (inner rator env)
		       (map (lambda (rand) (inner rand env)) rands)))]
       [(let-one rhs body)
	(let ([env (env-cons (fresh) env)])
	  (let-one (inner rhs env) (inner body env)))]
      |#

(define (subsequence xs is)
  (define (inner xs i j js)
    (if (= i j)
      (match xs
	[(cons x xs)
	 (if (empty? js)
	   (list x)
	   (cons x (inner xs (add1 i) (first js) (rest js))))]
	[(list)
	 (error 'subsequence "overrun")])
      (inner (rest xs) (add1 i) j js)))
  (inner xs 0 (first is) (rest is)))
       
; takes a procedure which matches zo structs of interest
