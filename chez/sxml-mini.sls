(library (chez sxml-mini)
  (export send-reply
	  pre-post-order
	  put-html
	  attr->html
	  sxml->html
	  make-char-quotator)
  (import (chezscheme))

  (define string-null?
    (lambda (S)
      (zero? (string-length S))))
  
  (include "sxml.ss")
  (include "sxml-transform.ss")
  (include "html.ss")
  
  )
