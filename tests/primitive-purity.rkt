#lang racket/base
(require racket/file
	 racket/match
	 "primitive-maps.rkt")

(provide primitive-pure?
	 primitive-impure?
	 primitive-purity-unknown?)

(define (unserialize-map)
  (make-hasheqv (with-handlers ([exn:fail? (lambda (e) '())]) (file->value "purity-map.sexp"))))

(define (serialize-map h)
  (write-to-file
   (for/list ([(id purity) (in-hash h)])
     (cons id purity))
   "purity-map.sexp"
   #:exists 'replace))

(define purity-map (unserialize-map))

(define ((read-primitive-purity id))
  (printf "id: ~a, name: ~a (0 - unknown, 1 - pure, 2 - impure)\n" id (or (primitive-id->name id) "<unknown"))
  (let loop ()
    (display "? ")
    (match (read)
      [0 'unknown]
      [1 'pure]
      [2 'impure]
      [_ (loop)])))

(define ((make-primitive-check purity) id)
  (begin0
    (eq? (hash-ref! purity-map id (read-primitive-purity id)) purity)
    (serialize-map purity-map)))

(define-values (primitive-pure? primitive-impure? primitive-purity-unknown?)
  (apply values (map make-primitive-check '(pure impure unknown))))

