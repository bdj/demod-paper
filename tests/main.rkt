#lang racket/base
(require racket/match
	 racket/path
	 compiler/zo-structs
	 compiler/demodularizer/main)

(define (rkt->zo-path path)
  (let ([path-string (path->string path)])
    (string->path
     (string-append
      (substring path-string 0 (- (string-length path-string) 3))
      "zo"))))

(define (rkt-path? path)
  (let ([extension (filename-extension path)])
    (and extension
	 (bytes=? extension #"rkt"))))

(define (call-with-new-directory path f)
  (unless (directory-exists? path)
    (make-directory path))
  (f path))

(define (run-demod method-name gc-toplevel)
  (displayln method-name)
  (call-with-new-directory
   method-name
   (lambda (method-path)
     (for ([suite-name (in-list (directory-list "src"))])
       (call-with-new-directory
	(build-path method-name suite-name)
	(lambda (suite-path)
	  (for ([test-filename (in-list (directory-list (build-path "src" suite-name)))])
	    (when (rkt-path? test-filename)
	      (let ([test-filepath (build-path "src" suite-name test-filename)]
		    [test-dest-filepath (build-path suite-path (rkt->zo-path test-filename))])
		(displayln (path->string test-filename))
		(parameterize ([compile-context-preservation-enabled #t])
		  (demodularize test-filepath test-dest-filepath
				(lambda (zo)
				  (let ([zo (gc-toplevel zo)])
				    (match-let ([(compilation-top max-let-depth prefix (splice forms)) zo])
				      (displayln (length forms)))
				    zo))))
		(displayln (file-size test-dest-filepath)))))))))))

(module+ main
  (require #;(prefix-in test: "test.rkt")
	   #;(prefix-in flow: "flow.rkt")
           (prefix-in reach: "reach.rkt")
           #;(prefix-in pure: "pure.rkt")
	   #;(prefix-in 0cfa: "0cfa.rkt")
	   #;(prefix-in const: "const.rkt"))

  (define none:gc (lambda (zo) zo))


  #;(run-demod "none" none:gc)
  #;(run-demod "test" test:gc)
  #;(run-demod "flow" flow:gc)
  (run-demod "reach" reach:gc))



