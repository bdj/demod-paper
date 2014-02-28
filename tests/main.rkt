#lang racket/base
(require racket/path
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

(define (run-demod method-name gc-toplevel)
  (if (directory-exists? method-name)
    (printf "directory \"~a\" already exists; skipping\n" method-name)
    (begin
      (printf "running strategy \"~a\"...\n" method-name)
      (make-directory method-name)
      (for ([suite-name (in-list (directory-list "src"))])
	(let ([suite-path (build-path method-name suite-name)])
	  (if (directory-exists? suite-path)
	    (printf "directory \"~a\" already exists; skipping\n" suite-path)
	    (begin
	      (make-directory suite-path)
	      (for ([test-filename (in-list (directory-list (build-path "src" suite-name)))])
		(when (rkt-path? test-filename)
		  (printf "processing \"~a\"\n" (build-path "src" suite-name test-filename))
		  (parameterize ([compile-context-preservation-enabled #t])
		    (demodularize (build-path "src" suite-name test-filename)
				  (build-path method-name suite-name (rkt->zo-path test-filename))
				  gc-toplevel)))))))))))

(module+ main
  (require #;(prefix-in test: "test.rkt")
	   #;(prefix-in flow: "flow.rkt")
           (prefix-in reach: "reach.rkt")
           (prefix-in pure: "pure.rkt")
	   (prefix-in 0cfa: "0cfa.rkt")
	   #;(prefix-in const: "const.rkt"))

  (define none:gc (lambda (zo) zo))


  (run-demod "none" none:gc)
  #;(run-demod "test" test:gc)
  #;(run-demod "flow" flow:gc)
  (run-demod "reach" reach:gc)
  (run-demod "pure" pure:gc)
  (run-demod "0cfa" 0cfa:gc)
  #;(run-demod "const" const:gc))


