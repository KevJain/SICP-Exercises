#lang sicp
(#%require sicp-pict)
(define pic (flip-horiz (bitmap->painter "chef.png")))
(paint (beside pic (flip-horiz (flip-vert pic))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

;2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

;(paint (right-split pic 3))
;(paint (up-split pic 3))
(paint (corner-split pic 5))

(paint (square-limit pic 5))
;2.45
(define (split large-op small-op)
  (define inner-fn (lambda (img n)
                     (if (= n 0)
                         img
                         (let ((smaller (inner-fn img (- n 1))))
                           (large-op img (small-op smaller smaller))))))
  inner-fn)

;(define general-rs (split beside below))
;(paint (right-split pic 3))
;(paint (general-rs pic 3))

;2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))
(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

;(add-vect (make-vect 1 2) (make-vect 4 4))
;(scale-vect 5 (make-vect 3 4))
;(sub-vect (make-vect 1 2) (make-vect 4 4))

;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame) (car frame))
(define (edge1-frame2 frame) (cadr frame))
(define (edge2-frame2 frame) (cddr frame))

(define o (make-vect 1 1))
(define e1 (make-vect -1 2))
(define e2 (make-vect 4 3))

(define f1 (make-frame o e1 e2))
#|
(origin-frame f1)
(edge1-frame f1)
(edge2-frame f1)

(define f2 (make-frame2 o e1 e2))
(origin-frame2 f1)
(edge1-frame2 f1)
(edge2-frame2 f1)
|#

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))


;((frame-coord-map f1) (make-vect 0 0))
;(origin-frame f1)

;2.48
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

;2.49: skipped
#|
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))
|#
;2.50
(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (flip-vert2 painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz2 painter)
  (transform-painter 
   painter
   (make-vect 1.0 0.0)   ; new origin
   (make-vect 0.0 0.0)   ; new end of edge1
   (make-vect 0.0 1.0))) ; new end of edge2

(define (ccw-180 painter)
  (flip-vert2 (flip-horiz2 painter)))

(define (ccw-270 painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 0.0 0.0)   ; new end of edge1
   (make-vect 1.0 1.0))) ; new end of edge2

;2.51
(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0. 0.5)))
    (let ((paint-bottom  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        (make-vect 0.0 1.0)
                        split-point))
          (paint-top (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.5)
                        (make-vect 1.0 0.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (ccw-270 (ccw-270 (ccw-270 (beside (ccw-270 painter1) (ccw-270 painter2))))))

;2.52
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (beside (below painter painter)
              (below painter (corner-split2 painter (- n 1))))))

(paint (corner-split2 pic 3))
(define (square-limit2 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below half (flip-vert half)))))

(paint (square-limit2 pic 5))
  



