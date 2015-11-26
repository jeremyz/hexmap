; hexmap.scm
; by Jérémy Zurcher <jeremy@asynk.ch>
; based on hex_grid by Rob Antonishen

; Version 1.0 (2015)
;

; Description
;
;  build hex map with nice options
;

; Issues
;
;  for
;

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (search l x y)
  (if (null? l)
    #f
    (if (and (= (vector-ref (car l) 0) x)
             (= (vector-ref (car l) 1) y))
      #t
      (search (cdr l) x y)
    )
  )
)

(define (maybe_add l x y)
  (set! x (/ (trunc (* x 1000)) 1000))
  (set! y (/ (trunc (* y 1000)) 1000))
  (if (search l x y)
    l
    (list* (vector x y) l)
  )
)

(define (crawl_hex_edges edges mask pressure)
  (if (not (null? edges))
    (begin
      (gimp-airbrush mask pressure 2 (car edges))
      (crawl_hex_edges (cdr edges) mask pressure)
    )
  )
)

(define (erase_hex_edges edges mask size)
  (gimp-context-set-brush-size (* 1.4 size))
  (gimp-context-set-foreground "black")
  (gimp-context-set-opacity 80)
  (gimp-context-set-brush "2. Hardness 025")
  (crawl_hex_edges edges mask 80)
)

(define (build_border_path border_edges border_path)
  (let*
    (
      (i 0)
      (y 0)
      (s (length border_edges))
      (n (* s 3))
      (v (make-vector n 0))
    )
    (while (< i s)
      (vector-set! v y       (list-ref border_edges i))
      (vector-set! v (+ y 1) (list-ref border_edges (+ i 1)))
      (vector-set! v (+ y 2) (list-ref border_edges i))
      (vector-set! v (+ y 3) (list-ref border_edges (+ i 1)))
      (vector-set! v (+ y 4) (list-ref border_edges i))
      (vector-set! v (+ y 5) (list-ref border_edges (+ i 1)))
      (set! i (+ i 2))
      (set! y (+ y 6))
    )
    (gimp-vectors-stroke-new-from-points border_path 0 n v TRUE)
  )
)

(define (build_h_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom)
  (if (and (>= x 0) (>= y 0) (<= x xBorder) (<= y yBorder))
    ; top ?
    (if (= y 0)
      (if (or (equal? bRight #t) (< x xBorder))
        (set! border_edges (append border_edges
                                   (list
                                     (vector-ref vx 4)
                                     (vector-ref vy 4)
                                     (vector-ref vx 3)
                                     (vector-ref vy 3)
                                     (vector-ref vx 2)
                                     (vector-ref vy 2)
                                     (vector-ref vx 1)
                                     (vector-ref vy 1)
                                     ))
        )
        (set! border_edges (append border_edges
                                   (list
                                     (vector-ref vx 4)
                                     (vector-ref vy 4)
                                     (vector-ref vx 5)
                                     (vector-ref vy 5)
                                     ))
        )
      )
      ; else bottom ?
      (if (= y yBorder)
        (if (= x 0)
          (set! border_edges (list*
                               (vector-ref vx 7)
                               (vector-ref vy 7)
                               (vector-ref vx 6)
                               (vector-ref vy 6)
                               (vector-ref vx 5)
                               (vector-ref vy 5)
                               (vector-ref vx 4)
                               (vector-ref vy 4)
                               (vector-ref vx 3)
                               (vector-ref vy 3)
                               border_edges)
          )
          (if (< x xBorder)
            (set! border_edges (list*
                                 (vector-ref vx 7)
                                 (vector-ref vy 7)
                                 (vector-ref vx 6)
                                 (vector-ref vy 6)
                                 (vector-ref vx 5)
                                 (vector-ref vy 5)
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
            (if (equal? bRight #t)
              (set! border_edges (list*
                                   (vector-ref vx 2)
                                   (vector-ref vy 2)
                                   (vector-ref vx 1)
                                   (vector-ref vy 1)
                                   (vector-ref vx 6)
                                   (vector-ref vy 6)
                                   (vector-ref vx 5)
                                   (vector-ref vy 5)
                                   (vector-ref vx 4)
                                   (vector-ref vy 4)
                                   border_edges)
              )
              (set! border_edges (list*
                                   (vector-ref vx 4)
                                   (vector-ref vy 4)
                                   border_edges)
              )
            )
          )
        )
        ; else left ?
        (if (= x 0)
          (set! border_edges (list*
                               (vector-ref vx 4)
                               (vector-ref vy 4)
                               (vector-ref vx 3)
                               (vector-ref vy 3)
                               border_edges)
          )
          ; else right ?
          (if (= x xBorder)
            (if (equal? bRight #t)
              (set! border_edges (append border_edges
                                         (list
                                           (vector-ref vx 2)
                                           (vector-ref vy 2)
                                           (vector-ref vx 1)
                                           (vector-ref vy 1)
                                           ))
              )
              (set! border_edges (append border_edges
                                         (list
                                           (vector-ref vx 4)
                                           (vector-ref vy 4)
                                           (vector-ref vx 5)
                                           (vector-ref vy 5)
                                           ))
              )
            )
          )
        )
      )
    )
  )
  border_edges
)

(define (build_v_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom)
  (if (and (>= x 0) (>= y 0) (<= x xBorder) (<= y yBorder))
    ; top ?
    (if (= y 0)
      ; top left
      (if (= x 0)
        (set! border_edges (append border_edges
                                   (list
                                     (vector-ref vx 1)
                                     (vector-ref vy 1)
                                     (vector-ref vx 2)
                                     (vector-ref vy 2)
                                     (vector-ref vx 3)
                                     (vector-ref vy 3)
                                     (vector-ref vx 4)
                                     (vector-ref vy 4)
                                     (vector-ref vx 5)
                                     (vector-ref vy 5)
                                     ))
        )
        (if (= x xBorder)
          (set! border_edges (append border_edges
                                     (list
                                       (vector-ref vx 4)
                                       (vector-ref vy 4)
                                       (vector-ref vx 5)
                                       (vector-ref vy 5)
                                       (vector-ref vx 6)
                                       (vector-ref vy 6)
                                       (vector-ref vx 7)
                                       (vector-ref vy 7)
                                       ))
          )
          (set! border_edges (append border_edges
                                     (list
                                       (vector-ref vx 4)
                                       (vector-ref vy 4)
                                       (vector-ref vx 5)
                                       (vector-ref vy 5)
                                       ))
          )
        )
      )
      ; else bottom ?
      (if (= y yBorder)
        (if (equal? bBottom #t)
          (if (= x 0)
            (set! border_edges (list*
                                 (vector-ref vx 6)
                                 (vector-ref vy 6)
                                 (vector-ref vx 1)
                                 (vector-ref vy 1)
                                 (vector-ref vx 2)
                                 (vector-ref vy 2)
                                 (vector-ref vx 3)
                                 (vector-ref vy 3)
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
            (if (< x xBorder)
              (set! border_edges (list*
                                   (vector-ref vx 6)
                                   (vector-ref vy 6)
                                   (vector-ref vx 7)
                                   (vector-ref vy 7)
                                   border_edges)
              )
              (set! border_edges (list*
                                   (vector-ref vx 4)
                                   (vector-ref vy 4)
                                   (vector-ref vx 5)
                                   (vector-ref vy 5)
                                   (vector-ref vx 6)
                                   (vector-ref vy 6)
                                   (vector-ref vx 7)
                                   (vector-ref vy 7)
                                   border_edges)
              )
            )
          )
          (if (< x xBorder)
            (set! border_edges (list*
                                 (vector-ref vx 5)
                                 (vector-ref vy 5)
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
            (set! border_edges (list*
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
          )
        )
        ; else left ?
        (if (= x 0)
          (set! border_edges (list*
                               (vector-ref vx 1)
                               (vector-ref vy 1)
                               (vector-ref vx 2)
                               (vector-ref vy 2)
                               (vector-ref vx 3)
                               (vector-ref vy 3)
                               (vector-ref vx 4)
                               (vector-ref vy 4)
                               border_edges)
          )
          ; else right ?
          (if (= x xBorder)
            (set! border_edges (append border_edges
                                       (list
                                         (vector-ref vx 4)
                                         (vector-ref vy 4)
                                         (vector-ref vx 5)
                                         (vector-ref vy 5)
                                         (vector-ref vx 6)
                                         (vector-ref vy 6)
                                         (vector-ref vx 7)
                                         (vector-ref vy 7)
                                         ))
            )
          )
        )
      )
    )
  )
  border_edges
)

(define (build_grid grid_path border_path width height sideLength orientation xOff yOff)
  (let*
    (
      (w (- width xOff))
      (h (- height yOff))
      (x (if (> xOff 0) -1 0))
      (y (if (> yOff 0) -1 0))
      (vx (make-vector 8 0))
      (vy (make-vector 8 0))
      (hex_edges '())
      (border_edges '())
      (hX 0)
      (hY 0)
      (xAdd 0)
      (yAdd 0)
      (xLast 0)
      (yLast 0)
      (xBorder 0)
      (yBorder 0)
      (bRight #t)
      (bBottom #t)
    )

    (if (= orientation 0)
      ; horizontal
      (begin
        (set! xLast (trunc (/ w (* sideLength 3.0))))
        (set! yLast (trunc (/ h (* sideLength 1.73205))))
        (set! xBorder (trunc (/ (- w xOff) (* sideLength 3.0))))
        (set! yBorder (- yLast 1))
        (set! bRight (if (> (- w xOff (* xBorder sideLength 3.0)) sideLength) #t #f))
        ; (set! bBottom #t)
        (set! hX (vector (* sideLength 3.0) (* sideLength 2.0) (* sideLength 1.5) (* sideLength 0.5) 0 (* sideLength 0.5) (* sideLength 1.5) (* sideLength 2.0)))
        (set! hY (vector (* sideLength 1.73205 0.5) (* sideLength 1.73205 0.5) 0 0 (* sideLength 1.73205 0.5) (* sideLength 1.73205) (* sideLength 1.73205) (* sideLength 1.73205 0.5)))
      )
      (begin
        (set! xLast (trunc (/ w (* sideLength 1.73205))))
        (set! yLast (trunc (/ h (* sideLength 3.0))))
        (set! xBorder (- xLast 1))
        (set! yBorder (trunc (/ (- h yOff) (* sideLength 3.0))))
        ; (set! bRight #t)
        (set! bBottom (if (> (- h yOff (* yBorder sideLength 3.0)) sideLength) #t #f))
        (set! hX (vector (* sideLength 1.73205 0.5) (* sideLength 1.73205 0.5) 0 0 (* sideLength 1.73205 0.5) (* sideLength 1.73205) (* sideLength 1.73205) (* sideLength 1.73205 0.5)))
        (set! hY (vector (* sideLength 3.0) (* sideLength 2.0) (* sideLength 1.5) (* sideLength 0.5) 0 (* sideLength 0.5) (* sideLength 1.5) (* sideLength 2.0)))
      )
    )

    (while (<= y yLast)
      (while (<= x xLast)
        (if (= orientation 0)
          ; horizontal
          (begin
            (set! xAdd (+ (* x sideLength 3.0) xOff))
            (set! yAdd (+ (* y sideLength 1.73205) yOff))
          )
          (begin
            (set! xAdd (+ (* x sideLength 1.73205) xOff))
            (set! yAdd (+ (* y sideLength 3.0) yOff))
          )
        )
        (vector-set! vx 0 (+ (vector-ref hX 0) xAdd))
        (vector-set! vx 1 (+ (vector-ref hX 1) xAdd))
        (vector-set! vx 2 (+ (vector-ref hX 2) xAdd))
        (vector-set! vx 3 (+ (vector-ref hX 3) xAdd))
        (vector-set! vx 4 (+ (vector-ref hX 4) xAdd))
        (vector-set! vx 5 (+ (vector-ref hX 5) xAdd))
        (vector-set! vx 6 (+ (vector-ref hX 6) xAdd))
        (vector-set! vx 7 (+ (vector-ref hX 7) xAdd))
        (vector-set! vy 0 (+ (vector-ref hY 0) yAdd))
        (vector-set! vy 1 (+ (vector-ref hY 1) yAdd))
        (vector-set! vy 2 (+ (vector-ref hY 2) yAdd))
        (vector-set! vy 3 (+ (vector-ref hY 3) yAdd))
        (vector-set! vy 4 (+ (vector-ref hY 4) yAdd))
        (vector-set! vy 5 (+ (vector-ref hY 5) yAdd))
        (vector-set! vy 6 (+ (vector-ref hY 6) yAdd))
        (vector-set! vy 7 (+ (vector-ref hY 7) yAdd))
        ; hex path
        (gimp-vectors-stroke-new-from-points grid_path 0 (* 8 2 3)
                                             (vector
                                               (vector-ref vx 0) (vector-ref vy 0)
                                               (vector-ref vx 0) (vector-ref vy 0)
                                               (vector-ref vx 0) (vector-ref vy 0)
                                               (vector-ref vx 1) (vector-ref vy 1)
                                               (vector-ref vx 1) (vector-ref vy 1)
                                               (vector-ref vx 1) (vector-ref vy 1)
                                               (vector-ref vx 2) (vector-ref vy 2)
                                               (vector-ref vx 2) (vector-ref vy 2)
                                               (vector-ref vx 2) (vector-ref vy 2)
                                               (vector-ref vx 3) (vector-ref vy 3)
                                               (vector-ref vx 3) (vector-ref vy 3)
                                               (vector-ref vx 3) (vector-ref vy 3)
                                               (vector-ref vx 4) (vector-ref vy 4)
                                               (vector-ref vx 4) (vector-ref vy 4)
                                               (vector-ref vx 4) (vector-ref vy 4)
                                               (vector-ref vx 5) (vector-ref vy 5)
                                               (vector-ref vx 5) (vector-ref vy 5)
                                               (vector-ref vx 5) (vector-ref vy 5)
                                               (vector-ref vx 6) (vector-ref vy 6)
                                               (vector-ref vx 6) (vector-ref vy 6)
                                               (vector-ref vx 6) (vector-ref vy 6)
                                               (vector-ref vx 7) (vector-ref vy 7)
                                               (vector-ref vx 7) (vector-ref vy 7)
                                               (vector-ref vx 7) (vector-ref vy 7)
                                               ) FALSE
        )
        ; border
        (if (= orientation 0) ; horizontal
          (set! border_edges (build_h_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom))
          (set! border_edges (build_v_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom))
        )
        ; hex edges
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 0) (vector-ref vy 0)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 1) (vector-ref vy 1)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 2) (vector-ref vy 2)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 3) (vector-ref vy 3)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 4) (vector-ref vy 4)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 5) (vector-ref vy 5)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 6) (vector-ref vy 6)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 7) (vector-ref vy 7)))
        ; next loop values
        (set! x (+ x 1))
      )
      (set! y (+ y 1))
      (set! x (if (> xOff 0) -1 0))
    )
    (display border_edges)
    (build_border_path border_edges border_path)
    hex_edges
  )
)

(define (script-fu-hex_map orientation elm len xN yN xOff yOff erase gStroke gColour bStroke bColour bOpacity)
  (let*
    (
      (img 0)
      (gridLayer 0)
      (borderLayer 0)
      (mask 0)
      (width 0)
      (height 0)
      (grid_path 0)
      (hex_edges 0)
      (border_path 0)
      (border_edges '())
      (sideLength (cond ((equal? elm 0) len) ((equal? elm 1) (/ len 2.0)) ((equal? elm 2) (/ len 1.73205))))
      (brushTemp (car (gimp-brush-new "HexMapBrush")))
    )

    (if (= orientation 0)
      ; horizontal
      (begin
        (set! height (+ (* 2 yOff) (* 1.73205 yN sideLength)))
        (set! width  (+ (* 2 xOff) (* (+ 0.5 (* 1.5 xN)) sideLength)))
      )
      (begin
        (set! width  (+ (* 2 xOff) (* 1.73205 xN sideLength)))
        (set! height (+ (* 2 yOff) (* (+ 0.5 (* 1.5 yN)) sideLength)))
      )
    )

    ; START
    (gimp-context-push)

    (set! img (car (gimp-image-new width height RGB)))
    (gimp-image-undo-group-start img)

    ; set brush
    (gimp-brush-set-angle brushTemp 0)
    (gimp-brush-set-aspect-ratio brushTemp 1)
    (gimp-brush-set-hardness brushTemp 1)
    (gimp-brush-set-shape brushTemp BRUSH-GENERATED-CIRCLE)
    (gimp-brush-set-spacing brushTemp 0)
    (gimp-brush-set-spikes brushTemp 1)
    (gimp-brushes-refresh)

    (gimp-context-set-brush "HexMapBrush")
    (gimp-context-set-opacity 100)
    (gimp-context-set-paint-mode NORMAL-MODE)

    ; paths
    (set! grid_path (car (gimp-vectors-new img "Hex Grid")))
    (gimp-image-add-vectors img grid_path -1)
    (set! border_path (car (gimp-vectors-new img "Map Border")))
    (gimp-image-add-vectors img border_path -1)
    (set! hex_edges (build_grid grid_path border_path width height sideLength orientation xOff yOff))

    ; grid layer
    (set! gridLayer (car (gimp-layer-new img width height RGBA-IMAGE "Grid" 100 NORMAL-MODE)))
    (gimp-image-add-layer img gridLayer -1)
    (gimp-context-set-brush-size gStroke)
    (gimp-context-set-foreground gColour)
    (if (> gStroke 0)
      (begin
        (gimp-edit-stroke-vectors gridLayer grid_path)
        (gimp-path-to-selection img "Map Border" 2 0 0 0 0)
        (gimp-selection-invert img)
        (gimp-edit-clear gridLayer)
      )
    )

    ; border layer
    (set! borderLayer (car (gimp-layer-new img width height RGBA-IMAGE "Border" 100 NORMAL-MODE)))
    (gimp-image-add-layer img borderLayer -1)
    ; transparent border
    (gimp-context-set-foreground "black")
    (gimp-edit-bucket-fill borderLayer 0 0 bOpacity 20 0 0 0)
    ; border stroke
    (if (> bStroke 0)
      (begin
        (gimp-context-set-brush-size bStroke)
        (gimp-context-set-foreground bColour)
        (gimp-edit-stroke-vectors borderLayer border_path)
      )
    )

    (gimp-selection-none img)
    (gimp-brush-delete brushTemp)

    ; grid mask
    (if(not (= erase 0))
      (begin
        (set! mask (car (gimp-layer-create-mask gridLayer ADD-WHITE-MASK)))
        (gimp-layer-add-mask gridLayer mask)
        (erase_hex_edges hex_edges mask sideLength)
        (if(= erase 2)
          (gimp-invert mask)
        )
      )
    )

    (gimp-display-new img)
    (gimp-image-clean-all img)

    ; END
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
  )
)

; (define (script-fu-hex_mapp)
;   (script-fu-hex_map 0 0 100 5 5 140 50 2 "black" 6 "red")
; )

(script-fu-register "script-fu-hex_map"
                    "Hex Map..."
                    "Draw a hex grid on the image."
                    "Jérémy Zurcher"
                    "Copyright 2015, Jérémy Zurcher"
                    "Nov 2015"
                    ""
                    SF-OPTION     "Hex Orientation"         '("Horizontal" "Vertical")
                    SF-OPTION     "Element to Specify"      '("Side" "Point to Point" "Side to Side")
                    SF-ADJUSTMENT "Length of Element"       '(100 2 400 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Horizontal Hex (#)"      '(18 2 500 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Vertical Hex (#)"        '(10 2 500 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Horizontal Offset (px)"  '(50 0 399 0.5 10 1 SF-SPINNER)
                    SF-ADJUSTMENT "Vertical Offset (px)"    '(50 0 399 0.5 10 1 SF-SPINNER)
                    SF-OPTION     "Erase"                   '("None" "Points" "Segments")
                    SF-ADJUSTMENT "Line Width (px)"         '(2 1 20 1 10 0 SF-SPINNER)
                    SF-COLOR      "Line Colour"             '(244 244 244)
                    SF-ADJUSTMENT "Border Width (px)"       '(6 0 20 1 10 0 SF-SPINNER)
                    SF-COLOR      "Border Colour"           '(244 244 244) ;'(69 70 11)
                    SF-ADJUSTMENT "Border Opacity (px)"     '(40 0 100 1 10 0 SF-SPINNER)
)
(script-fu-menu-register "script-fu-hex_map" "<Image>/File/Create")
