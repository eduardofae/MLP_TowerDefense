#lang racket
(require racket/gui/base)

(define SIZE 100)
(define GRID_SIZE 10)
(define SCENE_SIZE (* GRID_SIZE SIZE))

(define SLIME_SIZE 40)
(define SLIME_OFFSET (/ (- SIZE SLIME_SIZE) 2))

(define TOWER_SIZE 100)
(define TOWER_OFFSET (/ (- SIZE TOWER_SIZE) 2))

(define DIFFICULTY_SCALE 0.001)

(define (scene dc) (send dc set-brush (make-color  0   0   0  0) 'solid) (send dc draw-rectangle 0 0 SCENE_SIZE SCENE_SIZE))
(define (bg    dc) (send dc set-brush (make-color 166 176 079 1) 'solid) (send dc draw-rectangle 0 0 SCENE_SIZE SCENE_SIZE))

(define (path        dc x y) (send dc set-brush (make-color 235 150 097) 'solid) (send dc draw-rectangle x y    SIZE       SIZE   ))
(define (slime       dc x y) (send dc set-brush (make-color 093 129 068) 'solid) (send dc draw-ellipse   x y SLIME_SIZE SLIME_SIZE))
(define (tower       dc x y) (send dc set-brush (make-color 188 173 159) 'solid) (send dc draw-ellipse   x y TOWER_SIZE TOWER_SIZE))
(define (tower_space dc x y) (send dc set-brush (make-color 130 149 177) 'solid) (send dc draw-ellipse   x y TOWER_SIZE TOWER_SIZE))

(define (print_paths list_paths dc)
  (if (>= (length list_paths) 1)
      (begin (path dc (* (caar list_paths) SIZE) (* (cadar list_paths) SIZE))
       (print_paths (list-tail list_paths 1) dc))
      (scene dc)
))

(define paths (list (list 9 1) (list 8 1) (list 7 1) (list 6 2) (list 5 3) (list 4 3) (list 3 3) (list 2 3) (list 1 3) (list 1 4)
                    (list 1 5) (list 1 6) (list 1 7) (list 1 8) (list 2 8) (list 3 8) (list 4 8) (list 5 8) (list 6 7) (list 6 6)
                    (list 6 5) (list 7 4) (list 8 5) (list 8 6) (list 8 7) (list 8 8) (list 8 9)))
(define (world_map dc) (bg dc) (print_paths paths dc))

(define (get_enemy_pos step idx)
  (+ (* (list-ref (list-ref paths step) idx) SIZE) SLIME_OFFSET)
)

(define (print_enemies list_enemies dc)
  (if (>= (length list_enemies) 1)
      (begin (slime dc (get_enemy_pos (cadar list_enemies) 0) (get_enemy_pos (cadar list_enemies) 1))
       (print_enemies (list-tail list_enemies 1) dc))
      (scene dc)
))

(define (print_towers list_towers dc)
  (if (>= (length list_towers) 1)
      (if (caar list_towers)
          (begin (tower       dc (+ (* (cadar list_towers) SIZE) TOWER_OFFSET) (+ (* (caddar list_towers) SIZE) TOWER_OFFSET))
           (print_towers (list-tail list_towers 1) dc))
          (begin (tower_space dc (+ (* (cadar list_towers) SIZE) TOWER_OFFSET) (+ (* (caddar list_towers) SIZE) TOWER_OFFSET))
           (print_towers (list-tail list_towers 1) dc)))
      (scene dc)
))

(define (print_game canva)
  (send canva on-paint)
  (send canva refresh-now)
)

(define (get_towers x y towers)
  (length (filter (lambda (tower) (and (car tower) (<= (abs (- x (cadr tower))) 2) (<= (abs (- y (caddr tower))) 2))) towers))
)

(define (damage_calc health pos towers)
  (set! health (- health (get_towers(car pos) (cadr pos) towers)))
  (if (negative? health)
      0
      health
  )
)


(define (new_enemy secs)
  (list (list (random (+ 1 (exact-floor (* DIFFICULTY_SCALE secs))) (+ 5 (exact-floor (* DIFFICULTY_SCALE secs)))) 0))
)

(define last_spawn 10)
        
(define (update_enemies list_enemies towers secs)
  (set! list_enemies (map (lambda (enemy)
         (set! enemy (list-set enemy 0 (damage_calc (car enemy) (list-ref paths (cadr enemy)) towers)))
         (list-set enemy 1 (+ (cadr enemy) 1))
  ) list_enemies))
  (if (or (= last_spawn 10) (zero? (random (- 10 last_spawn))))
    (begin (set! last_spawn (min (+ 0 (exact-floor (* DIFFICULTY_SCALE secs))) 8))
      (append list_enemies (new_enemy secs)))
    (begin (set! last_spawn (+ last_spawn 1))
      list_enemies)
  )
)

(define (remove_enemies enemies)
  (filter (lambda (enemy) (not (or (zero? (car enemy)) (equal? (cadr enemy) 27)))) enemies)
)

(define (check_damage enemies hp)
  (- hp (length (filter (lambda (enemy) (= (cadr enemy) 27)) enemies)))
)

(define (check_kill enemies hp)
  (+ coins (* (length (filter (lambda (enemy) (= (car enemy) 0)) enemies)) 3))
)

(define (game_end hp)
  (not (positive? hp))
)

(define (print_hp hp dc)
  (send (send canva get-dc) set-font (make-font #:size 35 #:face "Fira Code"))
  (send dc set-text-foreground (make-color 240 79 79))
  (send (send canva get-dc) draw-text (format "~v" hp) 15 15)
)

(define (print_coins coins dc)
  (send (send canva get-dc) set-font (make-font #:size 35 #:face "Fira Code"))
  (send dc set-text-foreground (make-color 240 208 79))
  (let-values ([(w h w1 h1) (send (send canva get-dc) get-text-extent (format "~v" coins))])
  (send (send canva get-dc) draw-text (format "~v" coins) 15 (+ 15 h)))
)

(define (show_defeat canva)
  (send (send canva get-dc) set-font (make-font #:size 42 #:face "Fira Code"))
  (send (send canva get-dc) set-text-foreground "white")
  (let-values ([(w h w1 h1) (send (send canva get-dc) get-text-extent "YOU LOST")])
  (send (send canva get-dc) draw-text "YOU LOST" (- (/ SCENE_SIZE 2) (/ w 2.0)) (- (/ SCENE_SIZE 2) (/ h 2.0))))
)

(define (find_tower towers x y)
  (index-of towers (list #f (exact-floor(/ (- x TOWER_OFFSET) SIZE)) (exact-floor(/ (- y TOWER_OFFSET) SIZE))))
)

(define enemies (list ))
(define towers  (list (list #f 4 1) (list #f 8 2) (list #f 2 4) (list #f 4 5) (list #f 4 7) (list #f 3 9) (list #f 7 6) (list #f 9 6)))
(define hp    10)
(define coins 50)

(define frame (new frame%
                   [label "MLP_TowerDefense"]
                   [width  (+ SCENE_SIZE 16)]
                   [height (+ SCENE_SIZE 39)]))

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (let-values ([(pt state) (get-current-mouse-state)])
      (let-values ([(xw yw) (send frame client->screen 0 0)])
      (if (memq 'left state)
          (buy_tower (- (send pt get-x) xw) (- (send pt get-y) yw))
          (void)
      ))))
    (super-new)))


(define canva (new my-canvas% [parent frame]
                   [paint-callback
                    (lambda (canvas dc)
                      (send dc set-pen (make-color 0 0 0 0) 0 'transparent)
                      (world_map dc)
                      (print_enemies enemies dc)
                      (print_towers  towers  dc)
                      (print_hp hp dc)
                      (print_coins coins dc)
                      )]))

(define (buy_tower x y)
  (let ([tower_idx (find_tower towers x y)])
     (if (and (>= coins 50) (number? tower_idx) (not (car (list-ref towers tower_idx))))
         (begin (set! coins (- coins 50))
                (set! towers (list-set towers tower_idx (list #t (cadr (list-ref towers tower_idx)) (caddr (list-ref towers tower_idx))))))
         (void)
   ))
)

(define (update_game secs)
  (set! enemies (update_enemies enemies towers secs))
  (set! hp    (check_damage enemies hp ))
  (set! coins (check_kill enemies coins))
  (set! enemies (remove_enemies enemies))
  (print_game canva)
)


(send frame show #t)
(define i 0)
(define (game_loop)
  (update_game i)
  (sleep/yield 0.8)
  (set! i (+ i 1))
  (if (not (game_end hp))
      (game_loop)
      (show_defeat canva)
  )
)
(game_loop)