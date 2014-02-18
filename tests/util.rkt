#lang racket/base
(require racket/set
	 compiler/zo-structs)

(provide zo-map
	 zo-fold
	 transitive-closure
	 current-context)

(define current-context (make-parameter '()))

(define (zo-map f zo . args)
  (define ((inner seen) zo . args)
    (if (zo? zo)
      (if (hash-has-key? seen zo)
        (hash-ref seen zo)
        (let ([seen (hash-set seen zo (make-placeholder #f))])
          (let ([zo* (parameterize ([current-context (cons zo (current-context))])
		       (apply (f (inner seen)) zo args))])
            (placeholder-set! (hash-ref seen zo) zo*)
            zo*)))
      zo))
  (make-reader-graph (apply (inner (hasheq)) zo args)))

(define (zo-fold f zo . units)
  (define ((inner seen) zo . units)
    (if (or (not (zo? zo))
	    (set-member? seen zo))
      (apply values units)
      (apply (f (inner (set-add seen zo))) zo units)))
  (apply (inner (seteq)) zo units))


(define (transitive-closure seed relation)
  (let ([empty-set (set-clear seed)])
    (define (inner x seen)
      (if (set-member? seen x)
        seen
        (let ([seen (set-add seen x)])
          (foldl inner seen (set->list (hash-ref relation x empty-set))))))
    (foldl inner empty-set (set->list seed))))
