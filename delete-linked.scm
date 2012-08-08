; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

;; Delete all linked layers.
;; 
;

(define (delete-linked-layers image)
  (gimp-image-undo-group-start image)
  (map (lambda (layer)
	 (if (zero? (car (gimp-drawable-get-linked layer))) ; if linked, this function returns '(0). Dunno why.
	     '()
	     (gimp-image-remove-layer image layer)))
       (vector->list (cadr (gimp-image-get-layers image))))
  (gimp-displays-flush)
  (gimp-image-undo-group-end image))
      
        
(script-fu-register "delete-linked-layers"
  "Delete linked layers"
  "Delete all linked layers (ignoring visibility)"
  "Kevin Brubeck Unhammer"
  "Kevin Brubeck Unhammer"
  "January 2011"
  "*"
  SF-IMAGE    "Image"    0
  )

(script-fu-menu-register "delete-linked-layers"
 "<Image>/Layer"
 )

