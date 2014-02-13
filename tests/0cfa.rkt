#lang racket/base
(require racket/list
	 racket/match
	 compiler/zo-structs
	 "zo-util.rkt")

(provide gc)

(define fresh-variable
  (let ([i 0])
    (lambda ignore
      (begin0 i	(set! i (add1 i))))))



(define (recover-names zo)
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
       (def-values ids (inner rhs env))]
      [(install-value count pos boxes? rhs body)
       (install-value count pos boxes?
		      (inner rhs env)
		      (inner body env))]
      [(lam name flags num-params param-types rest? closure-map
	    closure-types toplevel-map max-let-depth body)
       (let ([params (build-list (+ num-params (if rest? 1 0)) fresh-variable)]
	     [captus (build-list (vector-length closure-map) fresh-variable)])
	 (lam name flags num-params param-types rest? closure-map closure-types
	      toplevel-map max-let-depth (inner body (append params captus env))))]
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
      [(localref unbox? pos clear? other-clears? type)
       (localref unbox? (list-ref env pos) clear? other-clears? type)]
      [(seq forms)
       (seq (map (lambda (form) (inner form env)) forms))]
      [(toplevel _ pos const? ready?)
       (toplevel 0 pos const? ready?)]
      [(topsyntax _ pos midpt)
       (topsyntax 0 pos midpt)]
      [(varref toplevel dummy)
       (varref (inner toplevel env)
	       (inner dummy env))]
      [(with-cont-mark key val body)
       (with-cont-mark (inner key env)
		       (inner val env)
		       (inner body env))]))
  (zo-map inner zo empty))

; the pos field of a localref is the name of the variable. of course, it must be an exact-nonnegative-integer? to
; meet the localref struct contract.;
; the toplevel is kept as is and the depth of toplevel references is changed to 0.

(define (gc zo)
  (match zo
    [(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms))
     (compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice (map recover-names forms)))]))
