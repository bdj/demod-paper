#lang racket/base
(require racket/list
	 racket/match
	 compiler/zo-structs
	 "util.rkt")

(provide gc)

(define fresh-variable
  (let ([i 0])
    (lambda ignore
      (begin0 i	(set! i (add1 i))))))

(define (localref-lookup id env)
  (match-let ([(localref unbox? pos clear? other-clears? type) id])
    (localref unbox? (list-ref env pos) clear? other-clears? type)))

(define (toplevel-lookup id env)
  (match-let ([(toplevel depth pos const? ready?) id])
    (toplevel depth (list-ref (list-ref env depth) pos) const? ready?)))

(define (recover-names zo prefix)
  (define ((inner inner) zo env)
    (match zo
      [(or (primval _)
	   (req _ _))
       zo]
      [(application rator rands)
       (let ([env (append (build-list (length rands) fresh-variable) env)])
	 (application (inner rator env)
		      (map (lambda (rand) (inner rand env)) rands)))]
      [(apply-values proc args-expr)
       (apply-values (inner proc env) (inner args-expr env))]
      [(assign id rhs undef-ok?)
       (assign (inner id env) (inner rhs env) undef-ok?)]
      [(beg0 seq)
       (beg0 (map (lambda (seq-element) (inner seq-element env)) seq))]
      [(boxenv pos body)
       (boxenv (list-ref env pos) (inner body env))]
      [(branch test then else)
       (branch (inner test env)
	       (inner then env)
	       (inner else env))]
      [(case-lam name clauses)
       (case-lam name (map (lambda (clause) (inner clause env)) clauses))]
      [(closure code gen-id)
       (closure (inner code env) gen-id)]
      [(def-values ids rhs)
       (def-values (map (lambda (id) (toplevel-lookup id env)) ids) (inner rhs env))]
      [(install-value count pos boxes? rhs body)
       (install-value count pos boxes?
		      (inner rhs env)
		      (inner body env))]
      [(lam name flags num-params param-types rest? closure-map
	    closure-types toplevel-map max-let-depth body)
       (let ([captured (map (lambda (i) (list-ref env i)) (vector->list closure-map))]
	     [params   (build-list (+ num-params (if rest? 1 0)) fresh-variable)])
	 (lam name flags num-params param-types rest? closure-map closure-types
	      toplevel-map max-let-depth (inner body (append captured params env))))]
      [(let-one rhs body type unused?)
       (let ([env (cons (fresh-variable) env)])
	 (let-one (inner rhs env)
		  (inner body env)
		  type
		  unused?))]
      [(let-rec procs body)
       (let-rec (map (lambda (proc) (inner proc env)) procs) (inner body env))]
      [(let-void count boxes? body)
       (let ([env (append (build-list count fresh-variable) env)])
	 (let-void count boxes? (inner body env)))]
      [(? localref? id)
       (localref-lookup id env)]
      [(seq forms)
       (seq (map (lambda (form) (inner form env)) forms))]
      [(? toplevel? id)
       (toplevel-lookup id env)]
      [(topsyntax depth pos midpt)
       (topsyntax depth pos midpt)]
      [(varref toplevel dummy)
       (varref (inner toplevel env)
	       (inner dummy env))]
      [(with-cont-mark key val body)
       (with-cont-mark (inner key env)
		       (inner val env)
		       (inner body env))]))
  (zo-map inner zo (list prefix)))

(struct state (exp tl-env env sto kon) #:transparent)

(define (inject exp prefix)
  (state exp (hasheqv) (hasheqv) (hasheqv) 0))


(define (gc zo)
  (match zo
    [(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms))
     (let ([prefix (append (for/list ([id (in-list toplevels)]) (fresh-variable))
			   stxs
			   (if (empty? stxs) (list) (list 'stxs-bucket))
			   (build-list num-lifts fresh-variable))])
       (inject (map (lambda (form) (recover-names form prefix)) forms) prefix)
       (compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice (map (lambda (form) (recover-names form prefix-map)) forms))))]))
