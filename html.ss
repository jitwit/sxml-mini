(define html-encoding
  '((#\& . "&amp;")
    (#\> . "&gt;")
    (#\< . "&lt;")
    (#\" . "&quot;")))

(define non-terminated-html-tag?
  (lambda (tag)
    (case tag
      ((br) #t)
      (else #f))))

(define string->goodHTML
  (make-char-quotator html-encoding))

(define attr->html 
  (lambda (attr)
    (if (string-null? (cadr attr))
	`(" " ,(symbol->string (name attr)))
	`(" " ,(symbol->string (name attr)) "='" ,(cadr attr) "'"))))

(define sxml->html
  (lambda (tree)
    (cond ((nodeset? tree)
	   (map sxml->html tree))
	  ((pair? tree)
	   (let* ((tag (name tree))
		  (name (symbol->string tag))
		  (content (content-raw tree)))
	     `("<" ,name ,@(map attr->html (attr-list tree))
	       ,@(if (null? content)
		     (if (non-terminated-html-tag? tag)
			 '(">")
			 '("/>"))
		     `(">"
		       ,@(map sxml->html content)
		       "</" ,name ">")))))
	  ((string? tree) (string->goodHTML tree))
	  ((symbol? tree) (string->goodHTML
			   (symbol->string tree)))
	  (else (error 'sxml->html "unexpected node" tree)))))

(define put-html
  (lambda (tree)
    (let ((html (sxml->html tree)))
      (let loop ((html html))
	(unless (null? html)
	  (cond ((pair? html)
		 (loop (car html))
		 (loop (cdr html)))
		(else (display html))))))))

(define eg
  '((head
     (meta (@ (charset "UTF-8")))
     (title jitwit))
    (body
     (h1 (a (@ (href "http://jitwit.github.io"))
	    "bio"))
     (p "3 >= 2 bitches. "
	"brouhaha"
	(br)
	"brou"
	(b "ha")
	"ha"))))

(define latex
  '(document
    (quotation "citation line 1")
    (br)
    "another line"))

(define alga
  '(title alga!))

(define (run)
  (let ((file "test.html"))
    (when (file-exists? file)
      (delete-file file))
    (with-output-to-file
      file
      (lambda ()
	(put-html eg)))))
