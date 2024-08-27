#lang racket
(require racket/gui/base)
(require "Drawing.rkt")
(require "Enemies.rkt")
(require "Towers.rkt")

;; Configs
(define enemies (list ))
(define towers  (list (list #f 4 1) (list #f 8 2) (list #f 2 4) (list #f 4 5) (list #f 4 7) (list #f 3 9) (list #f 7 6) (list #f 9 6)))
(define hp    10)
(define coins 50)

;; GUI
(define frame (new frame%
                   [label "MLP_TowerDefense"]
                   [width  (+ SCENE_WIDTH  16)]
                   [height (+ SCENE_HEIGHT 39)]))

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
                      (draw_enemies enemies dc)
                      (draw_towers  towers  dc)
                      (draw_hp hp dc)
                      (draw_coins coins dc)
 )]))


;; BUY_TOWER CALLBACK
(define (buy_tower x y)
  (let ([tower_idx (find_tower towers x y)])
     (if (and (>= coins 50) (number? tower_idx) (not (car (list-ref towers tower_idx))))
         (begin (set! coins (- coins 50))
                (set! towers (list-set towers tower_idx (list #t (cadr (list-ref towers tower_idx)) (caddr (list-ref towers tower_idx))))))
         (void)
)))

;; Game Loop
(define (game_end? hp)
  (not (positive? hp))
)

(define (update_game secs)
  (set! enemies (update_enemies enemies towers secs))
  (set! hp      (check_damage enemies hp ))
  (set! coins   (check_kill enemies coins))
  (set! enemies (remove_enemies enemies))
  (draw_game canva)
)

(define (game_loop secs)
  (update_game secs)
  (sleep/yield 0.5)
  (if (not (game_end? hp))
      (game_loop (+ secs 1))
      (show_defeat (send canva get-dc))
))

(send frame show #t)
(game_loop 0)