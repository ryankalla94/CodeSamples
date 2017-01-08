;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname SpaceInvaders) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ----------- A Version of Space Invaders ------------- ;;

;; An Invader is a Posn 
;; INTERP: represents the location of the invader

;; A Bullet is a Posn 
;; INTERP: represents the location of a bullet

;; A Location is a Posn 
;; INTERP: represents a location of a spaceship

;; A Direction is one of: 
;; - 'left 
;; - 'right 
;; INTERP: represent the direction of movement for the spaceship

(define-struct ship (dir loc))
;; A Ship is (make-ship Direction Location) 
;; INTERP: represent the spaceship with its current direction 
;;         and movement

;; A List of Invaders (LoI) is one of 
;; - empty 
;; - (cons Invader LoI)

;; A List of Bullets (LoB) is one of 
;; - empty
;; - (cons Bullet LoB)

(define-struct world (ship invaders ship-bullets invader-bullets))
;; A World is (make-world Ship LoI LoB LoB) 
;; INTERP: represent the ship, the current list of invaders, the inflight spaceship bullets
;;         and the inflight invader bullets


(define WIDTH 500) 
(define HEIGHT 500) 

(define MAX-SHIP-BULLETS 3)

(define MAX-INVADER-BULLETS 15)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define SPACESHIP-BULLET-IMAGE (circle 2 'solid 'black))

(define SHIP-WIDTH 25)

(define SHIP-HEIGHT 15)

(define SPACESHIP-IMAGE (rectangle SHIP-WIDTH SHIP-HEIGHT 'solid 'black))

(define INVADER-SIDE 20)

(define INVADER-IMAGE (square INVADER-SIDE 'solid 'red))

(define INVADER-BULLET-IMAGE (circle 2 'solid 'red))

(define SHIP-SPEED 10)

(define BULLET-SPEED 10)

(define SHIP-INIT (make-ship 'left (make-posn 250 480)))

(define INVADERS-INIT 
   (list (make-posn 100 20) (make-posn 140 20) (make-posn 180 20) 
         (make-posn 220 20) (make-posn 260 20) (make-posn 300 20) 
         (make-posn 340 20) (make-posn 380 20) (make-posn 420 20)
         (make-posn 100 50) (make-posn 140 50) (make-posn 180 50) 
         (make-posn 220 50) (make-posn 260 50) (make-posn 300 50) 
         (make-posn 340 50) (make-posn 380 50) (make-posn 420 50)
         (make-posn 100 80) (make-posn 140 80) (make-posn 180 80) 
         (make-posn 220 80) (make-posn 260 80) (make-posn 300 80) 
         (make-posn 340 80) (make-posn 380 80) (make-posn 420 80)
         (make-posn 100 110) (make-posn 140 110) (make-posn 180 110) 
         (make-posn 220 110) (make-posn 260 110) (make-posn 300 110) 
         (make-posn 340 110) (make-posn 380 110) (make-posn 420 110)))

(define WORLD-INIT (make-world SHIP-INIT INVADERS-INIT empty empty))
(define WORLD2 (make-world SHIP-INIT
                           INVADERS-INIT
                           (cons (make-posn 10 10) empty)
                           (cons (make-posn 100 100) empty)))
                           

;; invader-draw : Posn Image -> Image
;; draws an invader on the scene
(check-expect (invader-draw (make-posn 10 10) BACKGROUND)
              (place-image INVADER-IMAGE 10 10 BACKGROUND))
 
(define (invader-draw i image)
  (place-image INVADER-IMAGE
               (posn-x i)
               (posn-y i)
               image))

;; ship-bullet-draw : Posn Image -> Image
;; draws a ship bullet on the scene
(check-expect (ship-bullet-draw (make-posn 10 10) BACKGROUND)
              (place-image SPACESHIP-BULLET-IMAGE 10 10 BACKGROUND))

(define (ship-bullet-draw b image)
  (place-image SPACESHIP-BULLET-IMAGE
               (posn-x b)
               (posn-y b)
               image))

;; invader-bullet-draw : Posn Image -> Image
;; draws an invader bullet on the scene
(check-expect (invader-bullet-draw (make-posn 10 10) BACKGROUND)
              (place-image INVADER-BULLET-IMAGE 10 10 BACKGROUND))

(define (invader-bullet-draw b image)
  (place-image INVADER-BULLET-IMAGE
               (posn-x b)
               (posn-y b)
               image))

;; ship-draw : Posn Image -> Image
;; draws a spaceship on the scene
(check-expect (ship-draw (make-posn 10 10) BACKGROUND)
              (place-image SPACESHIP-IMAGE 10 10 BACKGROUND))

(define (ship-draw s image)
  (place-image SPACESHIP-IMAGE
               (posn-x s)
               (posn-y s)
               image))

(define TESTWORLD1 (make-world SHIP-INIT
                               (list (make-posn 100 100))
                               empty
                               empty))
(define TESTWORLD2 (make-world SHIP-INIT
                               (list (make-posn 100 100))
                               (list (make-posn 10 10))
                               (list (make-posn 200 200))))


;; world-draw : World -> Image
;; draws the world on a background
(check-expect (world-draw TESTWORLD1)
              (place-image INVADER-IMAGE
                           100 100
                           (place-image SPACESHIP-IMAGE
                                        250 480
                                        BACKGROUND)))
(check-expect (world-draw TESTWORLD2)
              (place-image INVADER-IMAGE
                           100 100
                           (place-image SPACESHIP-BULLET-IMAGE
                                        10 10
                                        (place-image INVADER-BULLET-IMAGE
                                                     200 200
                                                     (place-image SPACESHIP-IMAGE
                                                                  250 480
                                                                  BACKGROUND)))))
(define (world-draw w)
  (foldr invader-bullet-draw
       (foldr ship-bullet-draw
              (foldr invader-draw
                     (ship-draw (ship-loc (world-ship w))
                                BACKGROUND)
                     (world-invaders w))
              (world-ship-bullets w))
       (world-invader-bullets w)))

 


;; move-spaceship: Ship -> Ship
;; move the ship in the appropriate direction
(check-expect (move-spaceship (make-ship 'left (make-posn 100 100)))
              (make-ship 'left (make-posn 90 100)))
(check-expect (move-spaceship (make-ship 'right (make-posn 100 100)))
              (make-ship 'right (make-posn 110 100)))
(define (move-spaceship s)
  (cond
    [(or (and (<= (posn-x (ship-loc s)) (/ SHIP-WIDTH 2))
              (symbol=? (ship-dir s) 'left))
         (and (>= (posn-x (ship-loc s)) (- WIDTH (/ SHIP-WIDTH 2)))
              (symbol=? (ship-dir s) 'right))) s]
    [(symbol=? (ship-dir s) 'left)
     (make-ship (ship-dir s)
                (make-posn (- (posn-x (ship-loc s)) SHIP-SPEED)
                           (posn-y (ship-loc s))))]
    [(symbol=? (ship-dir s) 'right)
     (make-ship (ship-dir s)
                (make-posn (+ (posn-x (ship-loc s)) SHIP-SPEED)
                           (posn-y (ship-loc s))))]))

;; move-spaceship-bullets : LoB -> LoB
;; move each spaceship bullet in the list upwards by SPEED units
(check-expect (move-spaceship-bullets empty) empty)
(check-expect (move-spaceship-bullets (list (make-posn 10 0) (make-posn 20 20)))
              (list (make-posn 20 10)))
(define (move-spaceship-bullets alob)
  (local [;; Posn -> Boolean
          ;; Determines if a bullet is out of bounds
          (define (out-of-bounds? b)
            (> (posn-y b) 0))
          ;; Posn -> Posn
          ;; moves a bullet
          (define (move b)
            (make-posn (posn-x b)
                       (- (posn-y b) BULLET-SPEED)))]
    (map move (filter out-of-bounds? alob))))


;; move-invader-bullets : LoB -> LoB
;; move each bullet in the list downwards by SPEED units
(check-expect (move-invader-bullets empty) empty)
(check-expect (move-invader-bullets (list (make-posn 10 HEIGHT) (make-posn 20 20)))
              (list (make-posn 20 30)))
(define (move-invader-bullets alob)
  (local [;; Posn -> Boolean
          ;; Determines if a bullet is out of bounds
          (define (out-of-bounds? b)
            (< (posn-y b) HEIGHT))
          ;; Posn -> Posn
          ;; moves a bullet
          (define (move b)
            (make-posn (posn-x b)
                       (+ (posn-y b) BULLET-SPEED)))]
    (map move (filter out-of-bounds? alob))))



;; list-size : LoB -> Int
;; counts the size of a list of bullets
(check-expect (list-size empty) 0)
(check-expect (list-size (list 1 2 3 4)) 4)
(define (list-size lst)
  (cond
    [(empty? lst) 0]
    [(cons? lst) (+ 1 (list-size (rest lst)))]))


;; choose-invader : LoI Number -> Invader
;; chooses an invader from a list
(check-expect (choose-invader (list (make-posn 1 1)
                                    (make-posn 2 2)
                                    (make-posn 3 3)) 1)
              (make-posn 1 1))
(check-expect (choose-invader (list (make-posn 1 1)
                                    (make-posn 2 2)
                                    (make-posn 3 3)) 0)
              (make-posn 1 1))
(check-expect (choose-invader (list (make-posn 1 1)
                                    (make-posn 2 2)
                                    (make-posn 3 3)) 3)
              (make-posn 3 3))
(define (choose-invader aloi n)
  (cond
    [(or (= n 0) (= n 1)) (first aloi)]
    [(> n 1) (choose-invader (rest aloi) (- n 1))]))

;; add-new-bullet : LoB LoI -> LoB
;; adds a bullet to the list of bullets
(check-random (add-new-bullet empty (list (make-posn 1 1)))
              (list (make-posn 1 1)))
(define (add-new-bullet alob aloi)
  (cons (choose-invader aloi (random (list-size aloi))) alob))


;; invaders-fire : LoB LoI -> LoB
;; fires from a random invader if possible
(check-expect (invaders-fire empty (list (make-posn 1 1)))
              (list (make-posn 1 1)))
(check-expect (invaders-fire (list (make-posn 1 1)
                                   (make-posn 2 2)
                                   (make-posn 3 3)
                                   (make-posn 4 4)
                                   (make-posn 5 5)
                                   (make-posn 6 6)
                                   (make-posn 7 7)
                                   (make-posn 8 8)
                                   (make-posn 9 9)
                                   (make-posn 10 10))
                             (list (make-posn 1 1)))
              (list (make-posn 1 1)
                    (make-posn 2 2)
                    (make-posn 3 3)
                    (make-posn 4 4)
                    (make-posn 5 5)
                    (make-posn 6 6)
                    (make-posn 7 7)
                    (make-posn 8 8)
                    (make-posn 9 9)
                    (make-posn 10 10)))
(define (invaders-fire alob aloi)
  (if (< (list-size alob) 10) (add-new-bullet alob aloi)  alob))


;; bullet-hit : Posn LoI -> Boolean
;; determines whether a bullet hit an invader
(check-expect (bullet-hit (make-posn 5 5) empty) #false)
(check-expect (bullet-hit (make-posn 5 5) (list (make-posn 100 100))) #false)
(check-expect (bullet-hit (make-posn 5 5) (list (make-posn 10 10))) #true)
(define (bullet-hit b aloi)
  (cond
    [(empty? aloi) #false]
    [(cons? aloi) (or (and (<= (abs (- (posn-x b) (posn-x (first aloi))))
                              (/ INVADER-SIDE 2))
                           (<= (abs (- (posn-y b) (posn-y (first aloi))))
                              (/ INVADER-SIDE 2)))
                      (bullet-hit b (rest aloi)))]))


;; remove-invader-helper : Posn LoI -> LoI
;; removes an invader from the scene that has been hit by a bullet
(check-expect (remove-invader-helper (make-posn 5 5) empty) empty)
(check-expect (remove-invader-helper (make-posn 5 5)
                                     (list (make-posn 100 100)))
              (list (make-posn 100 100)))
(check-expect (remove-invader-helper (make-posn 5 5)
                                     (list (make-posn 6 6)
                                           (make-posn 100 100)))
              (list (make-posn 100 100)))
(define (remove-invader-helper b aloi)
  (local [;; Posn -> Boolean
          ;; determines if an invader has been hit by a bullet
          (define (hit i)
            (not (and (<= (abs (- (posn-x b) (posn-x i)))
                          (/ INVADER-SIDE 2))
                      (<= (abs (- (posn-y b) (posn-y i)))
                          (/ INVADER-SIDE 2)))))]
    (filter hit aloi)))



;; remove-invaders : LoB LoI -> LoI
;; removes invaders that have been hit by a bullet
(check-expect (remove-invaders empty (list (make-posn 5 5))) (list (make-posn 5 5)))
(check-expect (remove-invaders (list (make-posn 100 100)) (list (make-posn 5 5)))
              (list (make-posn 5 5)))
(check-expect (remove-invaders (list (make-posn 5 5)) (list (make-posn 5 5)))
              empty)
(define (remove-invaders alob aloi)
  (cond
    [(empty? alob) aloi]
    [(cons? alob) (if (bullet-hit (first alob) aloi)
                      (remove-invaders (rest alob)
                                       (remove-invader-helper (first alob) aloi))
                      (remove-invaders (rest alob) aloi))]))

;; remove-ship-bullets : LoB LoI -> LoB
;; removes ship bullets that have hit an invader
(check-expect (remove-ship-bullets empty (list (make-posn 5 5))) empty)
(check-expect (remove-ship-bullets (list (make-posn 100 100)) (list (make-posn 5 5)))
              (list (make-posn 100 100)))
(check-expect (remove-ship-bullets (list (make-posn 5 5)) (list (make-posn 5 5)))
              empty)
(define (remove-ship-bullets alob aloi)
  (cond
    [(empty? alob) empty]
    [(cons? alob) (if (bullet-hit (first alob) aloi)
                      (remove-ship-bullets (rest alob) aloi)
                      (cons (first alob) (remove-ship-bullets (rest alob) aloi)))]))


(define TESTWORLD3 (make-world SHIP-INIT
                               (list (make-posn 100 100))
                               (list (make-posn 100 100))
                               empty))  

;; remove-items: World -> World 
;; remove any invaders that have been hit by a spaceship bullet.
;; Remove any bullets that are out of bounds
(check-expect (remove-items TESTWORLD1) TESTWORLD1)
(check-expect (remove-items TESTWORLD3)
              (make-world SHIP-INIT
                          empty
                          empty
                          empty))
(define (remove-items w)
  (cond
    [(empty? (world-ship-bullets w)) w]
    [(cons? (world-ship-bullets w))
        (make-world (world-ship w)
                    (remove-invaders (world-ship-bullets w)
                                     (world-invaders w))
                    (remove-ship-bullets (world-ship-bullets w)
                                         (world-invaders w))
                    (world-invader-bullets w))]))
        

;; ship-hit : Ship LoB -> Boolean 
;; true if a bullet hit the ship, false otherwise
(check-expect (ship-hit (make-ship 'left (make-posn 100 100)) empty) #false)
(check-expect (ship-hit (make-ship 'left (make-posn 100 100))
                        (list (make-posn 50 50) (make-posn 100 100))) #true)
(define (ship-hit s alob)
  (cond
    [(empty? alob) #false]
    [(cons? alob) (or (and (< (abs (- (posn-x (ship-loc s))
                                      (posn-x (first alob))))
                              (/ SHIP-WIDTH 2))
                           (< (abs (- (posn-y (ship-loc s))
                                      (posn-y (first alob))))
                              (/ SHIP-HEIGHT 2)))
                      (ship-hit s (rest alob)))]))

;; game-over : World -> Boolean
;; determines if the player won or lost the game
(check-expect (game-over (make-world SHIP-INIT empty empty empty)) #true)
(check-expect (game-over TESTWORLD2) #false)
(define (game-over w)
  (or (empty? (world-invaders w)) (ship-hit (world-ship w)
                                            (world-invader-bullets w))))


;; ship-change : World Key-Event -> World
;; changes the ship's direction or fires a bullet from the ship
(check-expect (ship-change (make-world (make-ship 'left (make-posn 100 100))
                                       (list (make-posn 10 10))
                                       empty empty) " ")
              (make-world (make-ship 'left (make-posn 100 100))
                          (list (make-posn 10 10))
                          (list (make-posn 100 100))
                          empty))
(check-expect (ship-change (make-world (make-ship 'left (make-posn 100 100))
                                       (list (make-posn 10 10))
                                       (list (make-posn 1 1)
                                             (make-posn 2 2)
                                             (make-posn 3 3))
                                       empty) " ")
              (make-world (make-ship 'left (make-posn 100 100))
                          (list (make-posn 10 10))
                          (list(make-posn 1 1)
                               (make-posn 2 2)
                               (make-posn 3 3))
                          empty))
(check-expect (ship-change (make-world (make-ship 'right (make-posn 100 100))
                                       (list (make-posn 10 10))
                                       empty empty) "left")
              (make-world (make-ship 'left (make-posn 100 100))
                          (list (make-posn 10 10))
                          empty
                          empty))
(check-expect (ship-change (make-world (make-ship 'left (make-posn 100 100))
                                       (list (make-posn 10 10))
                                       empty empty) "right")
              (make-world (make-ship 'right (make-posn 100 100))
                          (list (make-posn 10 10))
                          empty
                          empty))
(check-expect (ship-change (make-world (make-ship 'left (make-posn 100 100))
                                       (list (make-posn 10 10))
                                       empty empty) "r")
              (make-world (make-ship 'left (make-posn 100 100))
                          (list (make-posn 10 10))
                          empty
                          empty))
(define (ship-change w a-key)
  (cond
    [(key=? a-key " ")
     (if (< (list-size (world-ship-bullets w)) 3)
         (make-world (world-ship w)
                     (world-invaders w)
                     (cons (make-posn (posn-x (ship-loc (world-ship w)))
                                      (posn-y (ship-loc (world-ship w))))
                           (world-ship-bullets w))
                     (world-invader-bullets w))
         w)]
    [(key=? a-key "left")
     (make-world (make-ship 'left (ship-loc (world-ship w)))
                 (world-invaders w)
                 (world-ship-bullets w)
                 (world-invader-bullets w))]
    [(key=? a-key "right")
     (make-world (make-ship 'right (ship-loc (world-ship w)))
                 (world-invaders w)
                 (world-ship-bullets w)
                 (world-invader-bullets w))]
    [else w]))

;; new-world : World -> World
;; makes a new world given an existing world
;; moves ship and bullets, removes invaders and bullets where necessary
(check-expect (new-world TESTWORLD1)
              (make-world
               (make-ship 'left (make-posn 240 480))
               (list (make-posn 100 100))
               empty
               (list (make-posn 100 110))))
(define (new-world w)
  (remove-items (make-world (move-spaceship (world-ship w))
              (world-invaders w)
              (move-spaceship-bullets (world-ship-bullets w))
              (move-invader-bullets
               (invaders-fire (world-invader-bullets w)
                              (world-invaders w))))))


;; Uncomment to play game
(big-bang WORLD-INIT
          (on-tick new-world 0.1)
          (to-draw world-draw)
          (on-key ship-change)
          (stop-when game-over))

















