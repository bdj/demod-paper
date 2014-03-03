

; store monad?

type State a b = a -> (b,a)

return x = \s -> (x,s)
m >>= f = \s -> let (x,s') = m s in (f x) s'

get = \s -> (s,s)
put = \_ -> ((),s)
modify f = \s -> ((),f s)


(define (f zo a b c)
  (match zo
    [(compilation-top x y z)
     (compilation-top (f x a b c)
		      (f y a b c)
		      (f z a b c))]))

(define (reconstitute f)
  (let ([seen (make-hasheq)])
    (lambda (zo . args)
      (if (hash-has-key? seen zo)
	(hash-ref seen zo)
	(hash-set seen zo (make-placeholder #f))
	
((f zo a b c)
