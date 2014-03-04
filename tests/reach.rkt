#lang racket/base
(require racket/list
	 racket/match
	 racket/set
	 compiler/zo-structs
	 "util.rkt"
	 "primitive-purity.rkt")

(provide gc)

(define (simple? zo)
  (or (not (zo? zo))
      (closure? zo)
      (lam? zo)
      (case-lam? zo)
      (primval? zo)
      (toplevel? zo)
      (match zo
	[(application (primval id) rands)
	 (and (primitive-pure? id)
	      (andmap simple? rands))]
	[(branch test then else)
	 (and (simple? test)
	      (simple? then)
	      (simple? else))]
	[_
	 #f])))

(define (gc zo)
  (define (update-map map x . ys)
    (foldl (lambda (y map) (hash-update map x (lambda (s) (set-add s y)) (seteqv))) map ys))
  (define (gather-info zo i references assigns)
    (zo-fold
     (lambda (zo references assigns)
       (match zo
	 [(toplevel depth pos const? ready?)
	  (values (update-map references i pos)
		  assigns)]
	 [(assign (toplevel depth pos const? ready?) rhs undef-ok?)
	  (values references
		  (update-map assigns i pos))]))
     zo
     references
     assigns))
  (match-let ([(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice forms)) zo])
    (define (->name toplevel) (with-handlers ([exn:fail? (lambda (e) 'lifted-local)]) (list-ref toplevels (toplevel-pos toplevel))))
    (let-values ([(eval-locs defines references assigns)
		  (for/fold ([eval-locs (seteqv)]
			     [defines (hasheqv)]
			     [references (hasheqv)]
			     [assigns (hasheqv)])
		      ([form (in-list forms)]
		       [i (in-naturals)])
		    (match form
		      [(def-values ids (? simple? rhs))
		       #;(displayln i)
		       #;(displayln "def-no-expr")
		       #;(displayln (map ->name ids))
		       (let-values ([(defines) (hash-set defines i (list->seteqv (map toplevel-pos ids)))]
				    [(references assigns) (gather-info rhs i references assigns)])
			 #;(displayln "references")
			 #;(displayln (map ->name (set->list (hash-ref references i (seteqv)))))
			 (values eval-locs defines references assigns))]
		      [(def-values ids rhs)
		       #;(displayln i)
		       #;(displayln "def")
		       #;(displayln (map ->name ids))
		       #;(displayln rhs)
		       (let-values ([(defines) (hash-set defines i (list->seteqv (map toplevel-pos ids)))]
				    [(references assigns) (gather-info rhs i references assigns)])
			 #;(displayln "references")
			 #;(displayln (map ->name (set->list (hash-ref references i (seteqv)))))
			 (values (set-add eval-locs i) defines references assigns))]
		       
		      [form
		       #;(displayln i)
		       #;(displayln "expr")
		       #;(displayln form)
		       (let-values ([(references assigns) (gather-info form i references assigns)])
			 #;(displayln "references")
			 #;(displayln (map ->name (set->list (hash-ref references i (seteqv)))))
			 (values (set-add eval-locs i) defines references assigns))]))])
      (let ([defined-at (for/fold ([defined-at (hasheqv)])
			  ([(i ids) (in-hash defines)])
			  (for/fold ([defined-at defined-at])
			    ([id (in-set ids)])
			    (hash-set defined-at id i)))])
	#;(displayln eval-locs)
	#;(displayln references)
	#;(displayln defined-at)
      (let ([form-indices
	     (let loop ([clos (seteqv)] ; a set of form indices
			[todo eval-locs]) ; a set of form inidices
	       (if (set-empty? todo)
		 clos
		 (let ([i (set-first todo)]
		       [todo (set-rest todo)])
		   (if (set-member? clos i)
		     (begin
		       #;(printf "already handled ~a\n" i)
		       (loop clos todo))
		     (loop (begin
			     #;(printf "handling ~a\n" i)
			     (set-add clos i))
			   (for/fold ([todo todo])
			     ([id (in-set (hash-ref references i (seteqv)))])
			     (if (hash-has-key? defined-at id)
			       (begin
				 #;(printf "adding ~a for ~a\n" (hash-ref defined-at id) i)
				 (set-add todo (hash-ref defined-at id)))
			       todo)))))))])
	#;(displayln (apply set-union (map (lambda (i) (hash-ref defines i (seteqv))) (set->list form-indices))))
	#;(for-each displayln (subsequence forms (sort (set->list form-indices) <)))
	(printf "keeping ~a/~a (~a%) top-level forms\n" (set-count form-indices) (length forms) (exact->inexact (* 100 (/ (set-count form-indices) (length forms)))))
	(compilation-top max-let-depth (prefix num-lifts toplevels stxs) (splice (subsequence forms (sort (set->list form-indices) <)))))))))
