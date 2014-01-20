#lang racket/base
(require racket/math)

;; Fermat and Solovay-Strassen primality testing in Scheme.

;; Author: Matthew Might
;; Site:   http://matt.might.net/
;; translated to Racket by Kimball Germane

;; Mathematical support.

; modulo-power: a fast modular exponentiation routine.
; modulo-power(base,exp,n) = base^exp [mod n]
(define (modulo-power base exp n)
  (if (zero? exp)
      1
      (if (odd? exp)
          (modulo (* base (modulo-power base (sub1 exp) n)) n)
          (modulo (sqr (modulo-power base (/ exp 2) n)) n))))


; jacobi: computes the Jacobi symbol, an extension of the Legendre symbol.
(define (jacobi a n)
  (cond
    ((= n 1) 1)
    ((= a 1) 1)
    ((not (= (gcd a n) 1)) 0)
    ((and (= a 2)
          (let ((n-mod-8 (modulo n 8)))
            (cond 
              ((or (= n-mod-8 1) (= n-mod-8 7)) 1)
              ((or (= n-mod-8 3) (= n-mod-8 5)) -1)))))
    ((> a n) (jacobi (modulo a n) n))    
    ((even? a) (* (jacobi (/ a 2) n) (jacobi 2 n)))    
    ((even? n) (* (jacobi a (/ n 2)) (jacobi a 2)))
    (else (* (jacobi n a) (if (even? (/ (* (- a 1) (- n 1)) 4)) 1 -1)))))
                              
    


;; Random number utilities.

;(define (random-char) 
;  (call-with-input-file "/dev/random" 
;    (lambda (port)
;     (read-char port))))

;(define (random-num)
;  (let ((n (char->integer (random-char))))
;    (if (= n 65533)
;        (random-num)
;        n)))

;(define (random-bit) (modulo (random-num) 2))

;(define (random-byte) (+ (modulo (random-num) ;128) (* 128 (random-bit))))

;(define (random bytes)
;  (if (<= bytes 0)
;      0
;      (+ (* 256 (random (- bytes 1))) (random-byte))))




;; Primality tests.

; is-trivial-composite?: divisibility tests with the first few primes.
(define (is-trivial-composite? n)
  (or (zero? (modulo n 2))
      (zero? (modulo n 3))
      (zero? (modulo n 5))
      (zero? (modulo n 7))
      (zero? (modulo n 11))
      (zero? (modulo n 13))
      (zero? (modulo n 17))
      (zero? (modulo n 19))
      (zero? (modulo n 23))))

; is-fermat-prime?:
; Check, for many values of a:
;  a^(n-1) = 1 [mod n] ?  
;   If yes, could be prime.  
;   If no, then composite.
; Warning: Some Carmichael numbers (though rare) defeat this test.
(define (is-fermat-prime? n iterations)
  (or (<= iterations 0)
      (let* ((byte-size (ceiling (/ (log n) (log 2))))
             (a (random byte-size)))
        (and (= (modulo-power a (- n 1) n) 1)
	     (is-fermat-prime? n (- iterations 1))))))


; is-solovay-strassen-prime?: 
; Check for many values of a:
;  jacobi(a,n) = a^((n - 1)/2) [mod n] ?
;  If yes, then prime with probability (at least) 1/2.
;  If no, then composite.
; Probability of false positive is lower than 1/2^iterations.
(define (is-solovay-strassen-prime? n iterations)
  (cond 
    ((<= iterations 0) #t)
    ((and (even? n) (not (= n 2))) #f)
    (else (let* ((byte-size (ceiling (/ (log n) (log 2))))
                 (a (+ 1 (modulo (random byte-size) (- n 1)))))
            (let* ((jacobi-a-n (jacobi a n))
                   (exp (modulo-power a (/ (- n 1) 2) n)))
              (if (or (= jacobi-a-n 0) (not (= (modulo jacobi-a-n n) exp)))
                  #f
                  (is-solovay-strassen-prime? n (- iterations 1))))))))


      
;; Prime generation.

; generate-fermat-prime(byte-size) yields a prime satisfying the Fermat test.
(define (generate-fermat-prime byte-size iterations)
  (let ((n (random byte-size)))
    (if
     (and (not (is-trivial-composite? n)) (is-fermat-prime? n iterations))
     n
     (generate-fermat-prime byte-size iterations))))

; generate-solovay-strassen-prime(byte-size, iterations) 
;  yields a prime of 'byte-size' bytes with a probability of 1-1/2^iterations.
(define (generate-solovay-strassen-prime byte-size iterations)
  (let ((n (generate-fermat-prime byte-size 5)))
    (if
     (is-solovay-strassen-prime? n iterations)
     n
     (generate-solovay-strassen-prime byte-size iterations))))



;; Example

(define iterations 10)
(define byte-size 15)

(display "Generating prime...") 
(newline)
(display (generate-solovay-strassen-prime byte-size iterations)) 
(display " is prime with at least probability 1 - 1/2^")
(display iterations)
(display ".")
(newline)
