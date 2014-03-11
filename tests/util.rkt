#lang racket/base
(require racket/list
	 racket/match
	 racket/set
	 compiler/zo-structs)

(provide zo-map
	 zo-fold
	 subsequence
	 process)

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
	   (fold/single inner seen accs key val body)]))))
  (match-let ([(cons seen accs) (apply* list (apply inner zo (seteqv) units))])
    (apply values accs)))
	  
(define (default-map recur zo)
  (match zo
    [(application rator rands)
     (application (recur rator) (map recur rands))]
    [(apply-values proc args-expr)
     (apply-values (recur proc) (recur args-expr))]
    [(beg0 seq)
     (beg0 (map recur seq))]
    [(assign id rhs undef-ok?)
     zo]
    [(boxenv pos body)
     zo]
    [(branch test then else)
     (branch (recur test) (recur then) (recur else))]
    [(case-lam name clauses)
     (case-lam name (map recur clauses))]
    [(closure code gen-id)
     (closure (recur code) gen-id)]
    [(def-values ids rhs)
     (def-values (map recur ids) (recur rhs))]
    [(install-value count pos boxes? rhs body)
     (install-value count pos boxes? (recur rhs) (recur body))]
    [(lam name flags num-params param-types rest? closure-map
	  closure-types toplevel-map max-let-depth body)
     (lam name flags num-params param-types rest? closure-map
	  closure-types toplevel-map max-let-depth (recur body))]
    [(let-one rhs body type unused?)
     (let-one (recur rhs) (recur body) type unused?)]
    [(let-rec procs body)
     (let-rec (map recur procs) (recur body))]
    [(let-void count boxes? body)
     (let-void count boxes? (recur body))]
    [(localref unbox? pos clear? other-clears? type)
     zo]
    [(primval id)
     zo]
    [(req reqs dummy)
     (req reqs (recur dummy))]
    [(seq forms)
     (seq (map recur forms))]
    [(toplevel depth pos const? ready?)
     zo]
    [(topsyntax depth pos midpt)
     zo]
    [(varref toplevel dummy)
     (varref (recur toplevel) (recur dummy))]
    [(with-cont-mark key val body)
     (with-cont-mark (recur key) (recur val) (recur body))]))

(define (zo-map f zo)
  (letrec ([inner (let ([seen (make-hasheq)])
		    (lambda (zo)
		      (if (zo? zo)
			(if (hash-has-key? seen zo)
			  (hash-ref seen zo)
			  (begin
			    (hash-set! seen zo (make-placeholder #f))
			    (let ([zo* (with-handlers ([exn:misc:match? (lambda (e) (default-map inner zo))])
					 ((f inner) zo))])
			      (placeholder-set! (hash-ref seen zo) zo*)
			      zo*)))
			zo)))])
    (make-reader-graph (inner zo))))

#;(define (zo-map f zo)
  (define ((inner seen) zo)
    (if (zo? zo)
      (if (hash-has-key? seen zo)
	(hash-ref seen zo)
	(let ([seen (hash-set seen zo (make-placeholder #f))])
	  (let ([zo* (with-handlers ([exn:misc:match? (lambda (e) (default-map (inner seen) zo))])
		       ((f (inner seen)) zo))])
	    (placeholder-set! (hash-ref seen zo) zo*)
	    zo*)))
      zo))
  (make-reader-graph ((inner (hasheq)) zo)))

; given a prefix, a list of top-level forms to include, return a compacted prefix and the list of toplevels
; with rewritten toplevel references
(define (process prefix* forms)
  (match-let ([(prefix num-lifts toplevels stxs) prefix*])
    (let ([num-toplevels (length toplevels)])
      (let*-values ([(tls lls)
		     (for/fold ([tls (seteqv)]
				[lls (seteqv)])
			 ([form (in-list forms)])
		       (zo-fold
			(lambda (zo tls lls)
			  (match zo
			    [(toplevel depth pos const? ready?)
			     (if (< pos num-toplevels)
				 (values (set-add tls pos) lls)
				 (values tls (set-add lls pos)))]
			    #;[(def-values ids rhs)
			     (for/fold ([tls tls]
					[lls lls])
				 ([id (in-list ids)])
			       (let ([pos (toplevel-pos id)])
				 (if (< pos num-toplevels)
				     (values (cons pos tls) lls)
				     (values tls (cons pos lls)))))]))
			form tls lls))]
		    [(tls lls) (values (sort (set->list tls) <) (sort (set->list lls) <))])
	(let*-values ([(tl-map j) (values (hasheqv 0 0) 1)]
		      [(tl-map j) (for/fold ([tl-map tl-map]
					     [j j])
				      ([i (in-list tls)])
				    (values (hash-set tl-map i j)
					    (add1 j)))]
		      [(tl-map j) (values tl-map (if (empty? stxs) j (add1 j)))]
		      [(tl-map j) (for/fold ([tl-map tl-map]
					     [j j])
				      ([i (in-list lls)])
				    (values (hash-set tl-map i j)
					    (add1 j)))])
	  (values prefix* #;(prefix (length lls) (subsequence toplevels tls) stxs)
		  (map
		   (lambda (form)
		     (zo-map
		      (lambda (recur)
			(match-lambda))
		      form))
		   forms)))))))

(define (subsequence xs is)
  (define (inner xs* j is*)
    (match is*
      [(list)
       (list)]
      [(cons i is)
       (match xs*
	 [(list)
	  (error 'subsequence "overrun")]
	 [(cons x xs)
	  (cond
	   [(> j i)
	    (error 'subsequence "not strictly monotonic")]
	   [(= j i)
	    (cons x (inner xs (add1 j) is))]
	   [(< j i)
	    (inner xs (add1 j) is*)])])]))
  (inner xs 0 is))
