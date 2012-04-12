(define (make-wonderful inImage inDrawable blurfactor brightness contrast flatten)
  ;; no undo group or display flush
  (let ((new-layer (car (gimp-layer-copy inDrawable 1))))
    (gimp-image-insert-layer inImage  new-layer 0 0)
    (plug-in-gauss-iir 1 inImage new-layer blurfactor 1 1)
    (gimp-brightness-contrast new-layer brightness contrast)
    (let ((layer-mask (car (gimp-layer-create-mask inDrawable WHITE-MASK))))
      (gimp-layer-add-mask new-layer layer-mask)
      (gimp-edit-copy new-layer)
      (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
      (gimp-layer-set-mode new-layer ADDITION))
    (gimp-item-set-name new-layer "Vedunderleg"))
  (if (= flatten TRUE)
      (gimp-image-flatten inImage)))

(define (layer-cover-image1 image drawable)
  ;; no undo group or display flush
  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image))))
    (gimp-layer-scale
     drawable
     width
     height   
     1))    
  (gimp-layer-set-offsets drawable 
			  0
			  0))

(define (layer-load-add-cover image filename mode-symbol opacity)
  (gimp-message (string-append "Prøver å leggje til " filename
			       " med modus " (symbol->string mode-symbol)
			       " og dekkevne " (number->string opacity)))
  (let* ((newlayer (car (gimp-file-load-layer RUN-NONINTERACTIVE image filename))))
    (gimp-image-insert-layer image newlayer 0 -1)
    (layer-cover-image1 image newlayer)
    (gimp-layer-set-mode newlayer (eval mode-symbol))
    (gimp-layer-set-opacity newlayer opacity)))

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

(define (find-eqv elt list)
  (if (pair? list)
      (if (eqv? elt (car list))
	  0
	  (let ((ret (find-eqv elt (cdr list))))
	    (if (number? ret) (+ 1 ret) '())))))
(define (basename filename)
  (let* ((slash-pos (find-eqv #\/ (reverse (string->list filename))))
	 (last-slash
	  (if (number? slash-pos)
	      (- (string-length filename) slash-pos)
	      0)))
    (substring filename last-slash (string-length filename))))

(define (satsvis-lag-mw fpattern
			blurfactor brightness contrast
			layers)
  "fpattern er ein file-glob, t.d. \"*.jpg\". 
Dei tri neste argumenta går til make-wonderful.
layers er ei liste der kvart element er ei liste 
med (filnamn modus-symbol dekkevne), modus må vere sitert 
sidan me skriv det ut."
  (let* ((filelist (cadr (file-glob fpattern 1))))
    (while (not (null? filelist))
      (let* ((filename (car filelist))
             (image (car (gimp-file-load RUN-NONINTERACTIVE
                                         filename filename)))
             (drawable (car (gimp-image-get-active-layer image))))
	(gimp-message (string-append "Innbilete: " filename))
        (gimp-item-set-name drawable (basename filename)) ; alltid nyttig
	
	(map (lambda (l)
	       (layer-load-add-cover image (first l) (second l) (third l)))
	     layers)
	(gimp-message "Alle lag er lagt til!")
	
	(gimp-message (string-append "Prøver å gjere " filename " vedunderleg..."))
	(make-wonderful image drawable
			blurfactor brightness contrast
			FALSE)		; ikkje noko flatten
        (set! drawable (car (gimp-image-get-active-layer image)))
	(gimp-layer-set-opacity drawable 15) ; vedunderleg-laget til 15%

	(let ((xcfname (string-append (basename filename) ".xcf"))
	      (jpgname (string-append (basename filename) ".xcf.jpg")))
	  (gimp-message (string-append "Lagrar som " xcfname " ..."))
	  (gimp-xcf-save RUN-NONINTERACTIVE
			 image
			 drawable
			 xcfname
			 xcfname)
	  (gimp-image-flatten image)
	  (set! drawable (car (gimp-image-get-active-layer image)))
	  (gimp-message (string-append "Lagrar som " jpgname " ..."))
	  (file-jpeg-save RUN-NONINTERACTIVE
			  image
			  drawable
			  jpgname
			  jpgname
			  1		; kvalitet
			  0		; utjamning
			  1		; optimaliser
			  0		; progressiv
			  "Created with GIMP by ~T~"
			  3		; subsmp, 3 is best quality (?)
			  1		; force baseline
			  0		; restart markers
			  0		; dct slow
			  ))

        (gimp-image-delete image))
      (set! filelist (cdr filelist))))
  
  (gimp-message "Ferdig med alle bilete!"))

