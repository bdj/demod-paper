#lang racket/base
(require racket/list
	 racket/match
	 racket/set
	 compiler/zo-structs
	 "util.rkt")

(provide gc)

#|
this strategy uses the const? and ready? flags of toplevels to determine
whether a toplevel is needed.

look at all the toplevel expressions. call the toplevels they reference the
root set. recur through all the toplevel references and determine the 
transitive closure of reference. if all /uses?/ of the toplevel are const?
we can safely prune the other expressions (as it is apparent they must not
alter any of them) and toplevels.

if there is a non-const? within the set, fail (keep everything).

----

an expression is side-effecting if it performs a side effect or calls a
function with a side-effecting body.
|#

; gathers toplevels used in expression positions
(define ((gather-toplevels recur) zo tls)
  (match zo
    [(or (localref _ _ _ _ _)
	 (primval _)
	 (req _ _)
	 (topsyntax _ _ _))
     tls]
    [(application rator rands)
     (foldl recur (recur rator tls) rands)]
    [(apply-values proc args-expr)
     (recur args-expr (recur proc tls))]
    [(assign id rhs undef-ok?)
     (recur rhs tls)]
    [(beg0 seq)
     (foldl recur tls seq)]
    [(boxenv pos body)
     (recur body tls)]
    [(branch test then else)
     (recur else (recur then (recur test tls)))]
    [(case-lam name clauses)
     (foldl recur tls clauses)]
    [(closure lam gen-id)
     (recur lam tls)]
    [(def-values ids rhs)
     (recur rhs tls)]
    [(install-value count pos boxes? rhs body)
     (recur body (recur rhs tls))]
    [(lam name flags num-params param-types rest? closure-map
	  closure-types toplevel-map max-let-depth body)
     (recur body tls)]
    [(let-one rhs body type unused?)
     (recur body (recur rhs tls))]
    [(let-rec procs body)
     (recur body (foldl recur tls procs))]
    [(let-void count boxes? body)
     (recur body tls)]
    [(seq forms)
     (foldl recur tls forms)]
    [(? toplevel? tl)
     (set-add tls tl)]
    [(varref toplevel dummy)
     (recur dummy (recur toplevel tls))]
    [(with-cont-mark key val body)
     (recur body (recur val (recur key tls)))]))

(define (gc zo)
  (define ctl ; canonize-toplevel
    (match-lambda
      [(toplevel _ pos const? ready?)
       (toplevel 0 pos const? ready?)]))
  (define (hash-ref-tl hash tl)
    (match-let ([(toplevel 0 pos _ _) tl])
      (hash-ref hash (toplevel 0 pos #t #t)
		(lambda () (hash-ref hash (toplevel 0 pos #t #f)
				     (lambda () (hash-ref hash (toplevel 0 pos #f #t)
							  (lambda () (hash-ref hash (toplevel 0 pos #f #f))))))))))
  (match zo
    [(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms))
     (let-values ([(uses defined-at expr-indices) 
		   (for/fold ([uses (hash)]
			      [defined-at (hash)]
			      [expr-indices empty])
		       ([form (in-list forms)]
			[i (in-naturals)])
		     (match form
		       [(def-values ids rhs)
			(values
			 (for/fold ([uses uses])
			     ([tl (in-set (zo-fold gather-toplevels rhs (set)))])
			   (let ([tl (ctl tl)])
			     (foldl (lambda (id uses) (hash-update uses id (lambda (s) (set-add s tl)) (set))) uses ids)))
			 (foldl (lambda (id d-a) (hash-set d-a (ctl id) i)) defined-at ids)
			 expr-indices)]
		       [form
			(values
			 (for/fold ([uses uses])
			     ([tl (in-set (zo-fold gather-toplevels form (set)))])
			   (let ([tl (ctl tl)])
			     (hash-update uses i (lambda (s) (set-add s tl)) (set))))
			 defined-at
			 (cons i expr-indices))]))])
       (for ([i (in-list (reverse expr-indices))])
	 (let ([tls (transitive-closure (hash-ref uses i (set)) uses)])
	   (if (for/and ([tl (in-set tls)])
		 (and (toplevel-const? tl)
		      #;(toplevel-ready? tl)))
	     (for ([i (in-set (set-add (for/seteqv ([tl (in-set tls)])
		                   (hash-ref-tl defined-at tl))
				 i))])
	       (displayln (list-ref forms i)))
	     (printf "not at ~a\n" i))))
       #;(displayln (hash-ref uses 'root)))])
  zo)
