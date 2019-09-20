(define send-reply
  (lambda fragments
    (let loop ((fragments fragments) (result #f))
      (cond ((null? fragments) result)
	    ((not (car fragments)) (loop (cdr fragments) result))
	    ((null? (car fragments)) (loop (cdr fragments) result))
	    ((eq? #t (car fragments)) (loop (cdr fragments) #t))
	    ((pair? (car fragments))
	     (loop (cdr fragments) (loop (car fragments) result)))
	    ((procedure? (car fragments))
	     ((car fragments))
	     (loop (cdr fragments) #t))
	    (else
	     (display (car fragments))
	     (loop (cdr fragments) #t))))))

(define (pre-post-order tree bindings)
  (let* ((default-binding (assq '*default* bindings))
	 (text-binding (or (assq '*text* bindings) default-binding))
	 (text-handler			; Cache default and text bindings
	  (and text-binding
	       (if (procedure? (cdr text-binding))
		   (cdr text-binding) (cddr text-binding)))))
    (let loop ((tree tree))
      (cond ((null? tree) '())
	    ((not (pair? tree))
	     (let ((trigger '*text*))
	       (if text-handler
		   (text-handler trigger tree)
		   (error 'pre-post-order
			  (format "Unknown binding for ~a and no default"
				  trigger)))))
	    ;; tree is a nodelist
	    ((not (symbol? (car tree)))
	     (map loop tree))
	    ;; tree is an SXML node
	    (else
	     (let* ((trigger (car tree))
		    (binding (or (assq trigger bindings)
				 default-binding)))
	       (cond ((not binding) 
		      (error 'pre-post-order
			     (format "Unknown binding for ~a and no default"
				     trigger)))
		     ((not (pair? (cdr binding)))  ; must be a procedure: handler
		      (apply (cdr binding) trigger (map loop (cdr tree))))
		     ((eq? '*preorder* (cadr binding))
		      (apply (cddr binding) tree))
		     ((eq? '*macro* (cadr binding))
		      (loop (apply (cddr binding) tree)))
		     (else			    ; (cadr binding) is a local binding
		      (apply (cddr binding) trigger 
			     (pre-post-order (cdr tree)
					     (append (cadr binding)
						     bindings)))))))))))

(define (foldts fdown fup fhere seed tree)
  (cond ((null? tree) seed)
	;; An atom
	((not (pair? tree))
	 (fhere seed tree))
	(else
	 (let loop ((kid-seed (fdown seed tree)) (kids (cdr tree)))
	   (if (null? kids)
	       (fup seed kid-seed tree)
	       (loop (foldts fdown fup fhere kid-seed (car kids))
		     (cdr kids)))))))





