#lang racket/base
(require racket/match
	 racket/set
	 compiler/zo-structs
	 "util.rkt")

(provide gc)

(define (gc zo)
  (match zo
    [(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms))
     (let-values ([(num-forms) (length forms)]
		  [(simple defs)
		   (for/fold ([simple 0]
			      [defs   0])
		       ([form (in-list forms)])
		     (match form
		       [(def-values (list id) (or (? closure?)
						  (? lam?)
						  (? case-lam?)
						  (? primval?)
						  (? toplevel?)
						  (not (? zo?))))
			(values (add1 simple) defs)]
		       [(def-values ids rhs)
			(let ([rep (format "~a" rhs)])
			  (displayln (substring rep 0 (min 120 (string-length rep)))))
			(values simple (add1 defs))]
		       [_
			(values simple defs)]))])
       (printf "~a/~a (~a) ~a/~a (~a)\n"
	       simple num-forms (exact->inexact (/ simple num-forms))
	       defs   num-forms (exact->inexact (/ defs   num-forms))))])
  zo)
