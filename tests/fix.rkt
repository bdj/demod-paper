#lang racket/base
(require racket/match)

(provide subsequence)

(define (subsequence xs is)
  (define (inner xs i is)
    (match is
      [(list)
       (list)]
      [(cons i* is*)
       (match xs
	 [(list)
	  (error 'subsequence "overrun")]
	 [(cons x* xs)
	  (if (= i i*)
	    (cons x* (inner xs (add1 i) is*))
	    (inner xs (add1 i) is))])]))
  (inner xs 0 is))

; what information do we need to demodularize?
; some of the toplevels will be taken out
; some of the top-level expressions will be taken out
; given a list of top-level expressions, w
