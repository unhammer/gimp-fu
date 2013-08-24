;; -*- mode: Gimp; -*-

;;; Usage:

;; $ gimp -d -f -i -b "(begin $defcurves (satsvis-curves $opacity \"*.[jJ][pP][gG]\"))" -b '(gimp-quit 0)'

;;; where 0<opacity<100, and you've read a gimp curve file into
;;; $defcurves with e.g.

;; $ defcurves=$(< ~/.gimp-2.8/curves/KodakPortra awk  '
;;     BEGIN{
;;         print "(define (my-curves)\n  (quote ("
;;     }
;;     /^[0-9 -]+$/{print "    ("$0")"} # simple curve file format
;;     /^ *\(points [0-9 .-]+\) *$/ {   # GIMP curve settings format
;;         printf "    ("
;;         for(i=3;i<=NF;i++) {
;;             sub(/\)/,"",$i)
;;             if($i ~ /^-/) printf "" # "-1 "
;;             else printf "%d ", $i*255
;;         }
;;         print ")"
;;     }
;;     END{print ")))"}')

;;; This will make "$defcurves" contain something like:
;; (define (my-curves)
;;   '((0 0 128 118 221 215  -1  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 255 255)
;;     (0 0  41  28 183 209  -1  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 255 255)
;;     (0 0  25  21  95 102 181 208 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 255 255)
;;     (0 0  25  21 122 153 165 206 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 255 255)
;;     (0 0 -1   -1  -1  -1  -1  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 255 255))
;;   )

(define (apply-curves drawable curves)
  (map (lambda (channel)
	 (gimp-curves-spline drawable
			     channel
			     (length (nth channel curves))
			     (list->vector (nth channel curves))))
       (if (= 0 (car (gimp-drawable-has-alpha drawable)))
	   (list HISTOGRAM-VALUE HISTOGRAM-RED HISTOGRAM-GREEN HISTOGRAM-BLUE)
	   (list HISTOGRAM-VALUE HISTOGRAM-RED HISTOGRAM-GREEN HISTOGRAM-BLUE HISTOGRAM-ALPHA))))

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

(define (satsvis-curves opacity fpattern)
  "fpattern er ein file-glob, t.d. \"*.jpg\"."
  (let* ((filelist (cadr (file-glob fpattern 1))))
    (while (not (null? filelist))
	   (let* ((filename (car filelist))
		  (image (car (gimp-file-load RUN-NONINTERACTIVE
					      filename filename)))
		  (drawable (car (gimp-image-get-active-layer image)))
		  (viscopy (car (gimp-layer-new-from-visible image image
							     "copy of visible"))))
	     (gimp-message (string-append "Innbilete: " filename))
	     (gimp-item-set-name drawable (basename filename))
	     (gimp-item-set-name viscopy "copy with curve")

	     (gimp-image-insert-layer image viscopy 0 -1)
	     (gimp-layer-set-opacity viscopy opacity)

	     (apply-curves viscopy (my-curves))

	     (let ((jpgname (string-append (basename filename) ".curved.jpg")))
	       (gimp-image-flatten image)
	       (set! drawable (car (gimp-image-get-active-layer image)))
	       (gimp-message (string-append "Lagrar som " jpgname " ..."))
	       (file-jpeg-save RUN-NONINTERACTIVE
			       image
			       drawable
			       jpgname
			       jpgname
			       1 ; kvalitet
			       0 ; utjamning
			       1 ; optimaliser
			       0 ; progressiv
			       "Created with GIMP by ~T~"
			       3 ; subsmp, 3 is best quality (?)
			       1 ; force baseline
			       0 ; restart markers
			       0 ; dct slow
			       ))

	     (gimp-image-delete image))
	   (set! filelist (cdr filelist))))
  (gimp-message "Ferdig med alle bileta!"))
