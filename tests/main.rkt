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

(define (call-with-new-directory path f)
  (if (directory-exists? path)
    (printf "directory \"~a\" already exists; skipping\n" path)
    (begin
      (make-directory path)
      (f path))))

(define (run-demod method-name gc-toplevel)
  (call-with-new-directory
   method-name
   (lambda (method-path)
     (for ([suite-name (in-list (directory-list "src"))])
       (call-with-new-directory
	(build-path method-name suite-name)
	(lambda (suite-path)
	  (for ([test-filename (in-list (directory-list (build-path "src" suite-name)))])
	    (when (rkt-path? test-filename)
	      (let ([test-filepath (build-path "src" suite-name test-filename)])
		(printf "processing \"~a\"\n" test-filepath)
		(parameterize ([compile-context-preservation-enabled #t])
		  (demodularize test-filepath
				(build-path suite-path (rkt->zo-path test-filename))
				gc-toplevel)))))))))))

(module+ main
  (require racket/list
	   racket/match
	   compiler/zo-structs
	   #;(prefix-in test: "test.rkt")
	   #;(prefix-in flow: "flow.rkt")
           (prefix-in reach: "reach.rkt")
           #;(prefix-in pure: "pure.rkt")
	   #;(prefix-in 0cfa: "0cfa.rkt")
	   #;(prefix-in const: "const.rkt"))

  (define (run-demod* name gc)
    (run-demod name gc)
    (displayln name)
    (for ([suite-pc (in-directory name)])
      (displayln (path->string suite-pc))
      (for ([file-path (in-directory (build-path name suite-pc))])
        (let ([file-path (build-path name suite-pc file-pc)])
	  (printf "~a\t~a\t~a\n"
	          (path->string file-pc)
		  (file-size file-path)
		  (match-let ([(compilation-top max-let-depth prefix (splice forms))
		               (with-input-from-file file-path zo-parse)])
                    (length forms)))))))

  (define none:gc (lambda (zo) zo))


  (run-demod* "none" none:gc)
  #;(run-demod* "test" test:gc)
  #;(run-demod* "flow" flow:gc)
  (run-demod* "reach" reach:gc))



