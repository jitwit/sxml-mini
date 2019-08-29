(import (srfi :13))

;; quotator split up string in chunks without given chars, and return list of strings
;; that list can then be printed to a port 
(define make-char-quotator
  (lambda (char-encoding)
    (lambda (S)
      (let ((n (string-length S)))
	(let loop ((b 0) (chunks '()) (a 0))
	  (if (>= b n)
	      (if (zero? a)
		  S
		  (reverse (cons (substring S a b) chunks)))
	      (cond ((assq (string-ref S b) char-encoding)
		     => (lambda (quot)
			  (loop (1+ b)
				(cons* (cdr quot) (substring S a b) chunks)
				(1+ b))))
		    (else (loop (1+ b) chunks a)))))))))

(define (nodeset? x)
  (or (and (pair? x)
	   (not (symbol? (car x))))
      (null? x)))

(define element?
  (lambda (obj)	
    (and (pair? obj)
	 (symbol? (car obj))
	 (not (memq (car obj)
		    '(@ @@ *PI* *COMMENT* *ENTITY*))))))

(define attr-list
  (lambda (obj)
    (if (and (element? obj) 
	     (not (null? (cdr obj)))
	     (pair? (cadr obj)) 
	     (eq? '@ (caadr obj)))
	(cdadr obj)
	'())))

(define name car)

(define ncname
  (lambda (attr)
    (symbol->string (name attr))))

(define content-raw 
  (lambda (obj)
    ((if (and (not (null? (cdr obj)))
	      (pair? (cadr obj)) (eq? (caadr obj) '@))
	 (if (and (not (null? (cddr obj)))
		  (pair? (caddr obj)) (eq? (caaddr obj) '@@))
	     cdddr
	     cddr)
	 cdr)
     obj)))

;; todo content-raw, attr->html

