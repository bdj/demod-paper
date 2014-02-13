#lang racket/base
(require racket/set
	 compiler/zo-structs)

(provide zo-map
	 zo-fold)

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

(define (zo-fold f zo . units)
  (define ((inner seen) zo . units)
    (if (zo? zo)
      (if (set-member? seen zo)
        (apply values units)
	(apply (f (inner (set-add seen zo))) zo units))
      zo))
  (apply (f (inner (seteq))) zo units))


#|
(define (zo-fold f zo . units)
  (if (set-member? seen zo)
    unit))


(zo-fold
 (lambda (recur)
   (match-lambda
    [(branch test then else)
     (retur (recur (recur test u) then u) else u)])))

(define (toplevels zo)
  (define (inner zo seen tls)
    (if (set-member? seen zo)
      tls
      (let ([seen (set-add seen zo)])
	(match zo
	  [(branch test then else)
	   (let*-values ([(seen tls) (inner test seen tls)]
			 [(seen tls) (inner then seen tls)]
			 [(seen tls) (inner else seen tls)])
	     (values seen tls))]
	  [(toplevel depth pos const? ready)
	   (values seen (set-add tls zo))]))))
  (inner zo (seteq) (seteq)))

(define gather-toplevels
  (lambda (inner)
    (lambda (zo tls)
      (match zo
	[(branch test then else)
	 (inner else (inner then (inner test tls)))]
	[(? toplevel? tl)
	 (set-add tls tl)]))))
|#

