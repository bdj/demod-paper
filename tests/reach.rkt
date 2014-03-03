#lang racket/base
(require racket/match
	 racket/set
	 compiler/zo-structs
	 "util.rkt")

(provide gc)

(define (gc zo)
  (define (update-map map x . ys)
    (foldl (lambda (y map) (hash-update map x (lambda (s) (set-add s y)) (seteqv))) map ys))
  (match-let ([(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms)) zo])
    (for ([form (in-list forms)]
	  [i (in-naturals)])
      (call-with-values
       (lambda ()
	 (zo-fold
	  (lambda (zo defined-at referenced-at assigned-at ids)
	    (match zo
	      [(def-values ids rhs)
	       (values (apply update-map defined-at i (map toplevel-pos ids))
		       referenced-at
		       assigned-at
		       (list->seteq ids))]
	      [(toplevel depth pos const? ready?)
	       (values defined-at
		       (if (set-member? ids zo) referenced-at (update-map referenced-at i pos))
		       assigned-at
		       ids)]
	      [(assign (toplevel depth pos const? ready?) rhs undef-ok?)
	       (values defined-at
		       referenced-at
		       (update-map assigned-at i pos)
		       ids)]))
	  form (hasheqv) (hasheqv) (hasheqv) (seteq)))
       (lambda accs (for-each void #;displayln accs)))))
  zo)
