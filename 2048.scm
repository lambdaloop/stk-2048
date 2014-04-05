;; scheme implementation of 2048 by Pierre Karashchuk


;; -- Low level graphics and events

(define (canvas-width) 500)
(define (canvas-height) 500)
(define (canvas-position) "+200+200")
(define (canvas-name) "2048")
(define canvas-toplevel-window #f) ; placeholder
(define canvas-widget #f) ; placeholder
(define (delete-canvas!)
  (destroy canvas-toplevel-window)
  (showing-canvas! #f))
(define (erase-canvas!)
  (canvas-widget 'delete 'all))
(define (create-canvas!)
  (set! canvas-toplevel-window (toplevel '.b))
  (set! canvas-widget (canvas '.b.canvas :width (canvas-width)
                              :height (canvas-height)
                              :background (canvas-background-color)))
  (pack canvas-widget)
  (wm 'geometry canvas-toplevel-window (canvas-position))
  (wm 'minsize canvas-toplevel-window (canvas-width) (canvas-height)) 
  (wm 'title canvas-toplevel-window (canvas-name))
  (wm 'iconname canvas-toplevel-window (canvas-name))
  (showing-canvas! #t))
(define *canvas-shown* #f)
(define (showing-canvas! arg)
  (set! *canvas-shown* arg))
(define (canvas-shown?)
  *canvas-shown*)
(define *canvas-foreground-color* "white")
(define (canvas-foreground-color)
  *canvas-foreground-color*)
(define (set-canvas-foreground-color new-color)
  (set! *canvas-foreground-color* new-color))
(define *canvas-background-color* "black")
(define (canvas-background-color)
  *canvas-background-color*)
(define (set-canvas-background-color new-color)
  (set! *canvas-background-color* new-color)
  (if (canvas-shown?)
      (canvas-widget 'configure :background (canvas-background-color))))

(define (clearscreen)
  (if (canvas-shown?) (erase-canvas!) (create-canvas!)))

(define cs clearscreen)


;; found these after a lot of tinkering


(define (bind-keypress f)
  (bind 'all "<KeyPress>"
        (lambda (|K| x y) ;apparently argument names matter here? so weird...
          (f (string->symbol (string-lower |K|))
             x y))))

(define (bind-keyrelease f)
  (bind 'all "<KeyRelease>" (lambda (|K| x y)
                              (f (string->symbol (string-lower |K|))
                                 x y))))

(define (unbind-keypress)
  (bind 'all "<KeyPress>" ""))

(define (unbind-keyrelease)
  (bind 'all "<KeyRelease>" ""))


;; -- Parameters

(define *box-size* 107)
(define *box-padding* 15)

(define *numbox-font* (font 'create
                            :family "DejaVu Sans"
                            :size 28
                            :weight 'bold))



(define *game-over-rw* 300)
(define *game-over-rh* 100)

(define *color-list*
  '((0 cdc0b4)
    (2 eee4da)
    (4 ede0c8)
    (8 f2b179)
    (16 f59563)
    (32 f67c5f)
    (64 f65e3b)
    (128 edcf72)
    (256 edcc61)
    (512 edc850)
    (1024 edc53f)
    (2048 edc22e)))


;; -- Generic procedures
(define (map-count f c L)
  (define (iter L i)
    (if (null? L)
        '()
        (cons (apply f (cons (car L) (c i)))
              (iter (cdr L) (1+ i)))))
  (iter L 0))


(define (list->hash L)
  (let ((h (make-hash-table)))
    (for-each (lambda (x)
                (hash-table-put! h (car x) (cadr x)))
              L)
    h))


(define (id x) x)

(define (ormap f L)
  (cond ((null? L) #f)
        ((f (car L)) #t)
        (else (ormap f (cdr L)))))


(define (map-set! f L)
  (if (null? L) '()
      (begin (set-car! L (f (car L)))
             (map-set! f (cdr L)))))


(define (filter f L)
  (cond ((null? L) '())
        ((f (car L)) (cons (car L) (filter f (cdr L))))
        (else (filter f (cdr L)))))

(define (accumulate comb null L)
  (if (null? L)
      null
      (comb (car L) (accumulate comb null (cdr L)))))


;; -- Tile model

(define tiles '())

(define (reset-tiles!)
  (set! tiles
        (list (list 0 0 0 0)
              (list 0 0 0 0)
              (list 0 0 0 0)
              (list 0 0 0 0))))

(reset-tiles!)



(define (move-row-up! row next)
  (cond ((null? row) '())
        ((or  (and (= (car row) 0) (> (car next) 0))
              (= (car row) (car next)))
         (set-car! row (+ (car next) (car row)))
         (set-car! next 0)
         (move-row-up! (cdr row) (cdr next)))
        (else (move-row-up! (cdr row) (cdr next)))))

(define (move-tiles-up! tiles)
  (if (or  (null? tiles) (null? (cdr tiles)))
      tiles
      (begin
        (move-row-up! (car tiles) (cadr tiles))
        (move-tiles-up! (cdr tiles))
        tiles)))

(define (move-tiles-down! tiles)
  (move-tiles-up! (reverse tiles))
  tiles)


(define (move-row-left row)
  (define (new-row curr rest)
    (cond ((null? rest) (list curr))
          ((or (and (= curr 0) (> (car rest) 0))
               (= curr (car rest)))
           (cons (+ curr (car rest)) (new-row 0 (cdr rest))))
          (else (cons curr (new-row (car rest) (cdr rest))))))
  (new-row (car row) (cdr row)))

(define (move-tiles-left! tiles)
  (map-set! move-row-left tiles)
  tiles)

(define (move-tiles-right! tiles)
  (map-set! (lambda (x) (reverse (move-row-left (reverse x))))
            tiles)
  tiles)



(define (available-cells tiles)
  (filter id
          (accumulate
           append '()
           (map-count (lambda (row r)
                        (map-count
                         (lambda (x c) (if (= x 0) (cons r c) #f))
                         list row))
                      list tiles))))

(define (tiles-contain? tiles num)
  (member num (accumulate append '() tiles)))

(define (transpose tiles)
  (if (or (null? tiles) (null? (car tiles)))
      '()
      (cons (map car tiles) (transpose (map cdr tiles)))))

(define (stuck? tiles)
  (and  (null? (available-cells tiles))
        (not (or (same-close? tiles)
                 (same-close? (transpose tiles))))))

(define (same-close? tiles)
  (define (iter row)
    (cond ((or (null? row) (null? (cdr row))) #f)
          ((= (car row) (cadr row)) #t)
          (else (iter (cdr row)))))
  (ormap iter tiles))

(define (random-pick L)
  (list-ref L (random (length L))))


(define (set-tile! tiles pos num)
  (define (iter row n)
    (if (= n 0)
        (set-car! row num)
        (iter (cdr row) (- n 1))))
  (iter (list-ref tiles (car pos))
        (cdr pos)))

(define (random-tile-value)
  (if (= 0 (random 10)) 4 2))

(define (add-random-tile! tiles)
  (let ((available (available-cells tiles)))
    (if (null? available)
        #f
        (begin
          (set-tile! tiles
                     (random-pick available)
                     (random-tile-value))
          #t))))



;; -- Drawing stuff



(define *color-hash* (list->hash *color-list*))

(define (get-color n)
  (& "#" (hash-table-get *color-hash* n "333333")))

(define (get-text-color n)
  (cond ((= n 0) "#cdc0b4")
        ((<= n 4) "#776e65")
        (else "#f9f6f2")))


(define (draw-tiles tiles)
  (cs)
  (map-count draw-row
             (lambda (i)
               (list (+ *box-padding* (* i (+ *box-size* *box-padding*)))))
             tiles))

(define (draw-row row y)
  (map-count draw-numbox
             (lambda (i)
               (list (+ *box-padding* (* i (+ *box-size* *box-padding*)))
                     y
                     *box-size*
                     *box-size*))
             row))

(define (draw-numbox num x y width height)
  (canvas-widget 'create 'rectangle
                 x y (+ x width) (+ y height)
                 :fill (get-color num)
                 :outline (get-color num))
  (canvas-widget 'create 'text
                 (+ x (/ width 2)) (+ y (/ height 2))
                 :width width
                 :justify 'center
                 :text (format #f "~A" num)
                 :fill (get-text-color num)
                 :font *numbox-font*))

(define (game-over text)
  (let ((x (/ (- (canvas-width) *game-over-rw*) 2))
        (y (/ (- (canvas-height) *game-over-rh*) 2)))
    (canvas-widget 'create 'rectangle
                   x y (+ x *game-over-rw*) (+ y *game-over-rh*)
                   :fill "white"
                   :outline "black"
                   :width 2)
    (canvas-widget 'create 'text
                   (+ x (/ *game-over-rw* 2)) (+ y (/ *game-over-rh* 2))
                   :text text
                   :font *numbox-font*
                   :fill "black")))


;; -- Keyboard bindings

(define (setup-bindings)
  (unbind-keypress)
  (bind-keypress
   (lambda (k x y)
     (when (member k '(escape enter space))
           (reset-tiles!)
           (add-random-tile! tiles)
           (draw-tiles tiles))
     
     (when (member k '(up down right left))
           ( (cond ((eq? k 'up) move-tiles-up!)
                   ((eq? k 'down) move-tiles-down!)
                   ((eq? k 'right) move-tiles-right!)
                   ((eq? k 'left) move-tiles-left!)
                   (else id))
             tiles)

           (add-random-tile! tiles)
           (draw-tiles tiles)
           (cond ((tiles-contain? tiles 2048) (game-over "You win!!"))
                 ((stuck? tiles) (game-over "You lose!")))
           
           ))))

;; -- Game

(define (play)
  (cs)
  (set-canvas-background-color "#776E65")
  (reset-tiles!)
  (add-random-tile! tiles)
  (draw-tiles tiles)
  (setup-bindings))


(play)
