#lang racket
(require racket/gui/base)

;; Configs
(define BLOCK_SIZE  100)
(define GRID_WIDTH  10)
(define GRID_HEIGHT 10)
(define SCENE_WIDTH  (* GRID_WIDTH  BLOCK_SIZE))
(define SCENE_HEIGHT (* GRID_HEIGHT BLOCK_SIZE))

(define SLIME_SIZE   (* 0.4 BLOCK_SIZE))
(define SLIME_OFFSET (/ (- BLOCK_SIZE SLIME_SIZE) 2))

(define TOWER_SIZE   (* 1 BLOCK_SIZE))
(define TOWER_OFFSET (/ (- BLOCK_SIZE TOWER_SIZE) 2))

(define FONT_SIZE   36)
(define FONT_OFFSET 15)

(define paths (list (list 9 1) (list 8 1) (list 7 1) (list 6 2) (list 5 3) (list 4 3) (list 3 3) (list 2 3) (list 1 3) (list 1 4)
                    (list 1 5) (list 1 6) (list 1 7) (list 1 8) (list 2 8) (list 3 8) (list 4 8) (list 5 8) (list 6 7) (list 6 6)
                    (list 6 5) (list 7 4) (list 8 5) (list 8 6) (list 8 7) (list 8 8) (list 8 9) ))

;; Draw Functions
(define (scene dc) (send dc set-brush (make-color  0   0   0  0) 'solid) (send dc draw-rectangle 0 0 SCENE_WIDTH SCENE_HEIGHT))
(define (bg    dc) (send dc set-brush (make-color 166 176 079 1) 'solid) (send dc draw-rectangle 0 0 SCENE_WIDTH SCENE_HEIGHT))

(define (path        dc x y) (send dc set-brush (make-color 235 150 097) 'solid) (send dc draw-rectangle x y BLOCK_SIZE BLOCK_SIZE))
(define (slime       dc x y) (send dc set-brush (make-color 093 129 068) 'solid) (send dc draw-ellipse   x y SLIME_SIZE SLIME_SIZE))
(define (tower       dc x y) (send dc set-brush (make-color 188 173 159) 'solid) (send dc draw-ellipse   x y TOWER_SIZE TOWER_SIZE))
(define (tower_space dc x y) (send dc set-brush (make-color 130 149 177) 'solid) (send dc draw-ellipse   x y TOWER_SIZE TOWER_SIZE))

(define (draw_paths list_paths dc)
  (if (>= (length list_paths) 1)
      (begin (path dc (* (caar list_paths) BLOCK_SIZE) (* (cadar list_paths) BLOCK_SIZE))
             (draw_paths (list-tail list_paths 1) dc))
      (scene dc)
))

(define (world_map dc) (bg dc) (draw_paths paths dc))

(define (get_enemy_sprite_pos step idx)
  (+ (* (list-ref (list-ref paths step) idx) BLOCK_SIZE) SLIME_OFFSET)
)

(define (draw_enemies list_enemies dc)
  (if (>= (length list_enemies) 1)
      (begin (slime dc (get_enemy_sprite_pos (cadar list_enemies) 0) (get_enemy_sprite_pos (cadar list_enemies) 1))
       (draw_enemies (list-tail list_enemies 1) dc))
      (scene dc)
))

(define (draw_towers list_towers dc)
  (if (>= (length list_towers) 1)
      (if (caar list_towers)
          (begin (tower       dc (+ (* (cadar list_towers) BLOCK_SIZE) TOWER_OFFSET) (+ (* (caddar list_towers) BLOCK_SIZE) TOWER_OFFSET))
           (draw_towers (list-tail list_towers 1) dc))
          (begin (tower_space dc (+ (* (cadar list_towers) BLOCK_SIZE) TOWER_OFFSET) (+ (* (caddar list_towers) BLOCK_SIZE) TOWER_OFFSET))
           (draw_towers (list-tail list_towers 1) dc)))
      (scene dc)
))

(define (draw_game canva enemies towers hp coins)
  (send canva on-paint)
  (send canva refresh-now)
  (let ([dc (send canva get-dc)])
    (world_map dc)
    (draw_enemies enemies dc)
    (draw_towers  towers  dc)
    (draw_hp hp dc)
    (draw_coins coins dc)
))

(define (draw_hp hp dc)
  (send dc set-font (make-font #:size FONT_SIZE #:face "Fira Code"))
  (send dc set-text-foreground (make-color 240 79 79))
  (send dc draw-text (format "~v" hp) FONT_OFFSET FONT_OFFSET)
)

(define (draw_coins coins dc)
  (send dc set-font (make-font #:size FONT_SIZE #:face "Fira Code"))
  (send dc set-text-foreground (make-color 240 208 79))
  (let-values ([(w h w1 h1) (send dc get-text-extent (format "~v" coins))])
  (send dc draw-text (format "~v" coins) FONT_OFFSET (+ FONT_OFFSET h)))
)

(define (show_defeat dc)
  (send dc set-font (make-font #:size FONT_SIZE #:face "Fira Code"))
  (send dc set-text-foreground "white")
  (let-values ([(w h w1 h1) (send dc get-text-extent "YOU LOST")])
  (send dc draw-text "YOU LOST" (- (/ SCENE_WIDTH 2) (/ w 2.0)) (- (/ SCENE_HEIGHT 2) (/ h 2.0))))
)

(provide (all-defined-out))