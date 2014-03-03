#lang racket/base
(require racket/match
	 racket/set
	 compiler/zo-structs
	 "util.rkt"
	 "primitive-purity.rkt")

(provide gc)

(define (gc zo)
  (match zo
    [(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms))
     ((lambda (id-frequency)
	(let* ([ids (map car
			 (sort (for/list ([(id frequency) (in-hash id-frequency)])
				 (cons id frequency))
			       >
			       #:key cdr))]
	       [ids-length (length ids)])
	  (for ([id (in-list ids)]
		[i (in-naturals)])
	    (printf "~a/~a\n" (add1 i) ids-length)
	    (primitive-pure? id))))
      (for/fold ([id-frequency (hasheqv)])
	  ([form (in-list forms)])
	(zo-fold
	 (lambda (zo id-frequency)
	   (match zo
	     [(primval id)
	      (hash-update id-frequency id add1 0)]))
	 form
	 id-frequency)))])
  zo)
