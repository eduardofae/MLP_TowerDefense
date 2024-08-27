#lang racket
(require racket/gui/base)
(require "Drawing.rkt")
(require "Enemies.rkt")
(require "Towers.rkt")

;; Configs
(define enemies (list ))
(define towers  (list (list #f 4 1) (list #f 8 2) (list #f 2 4) (list #f 4 5) (list #f 4 7) (list #f 3 9) (list #f 7 6) (list #f 9 6)))
(define hp    10)
(define coins 30)
(define buy_try (list #f 0 0))

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
          (set! buy_try (list #t (- (send pt get-x) xw) (- (send pt get-y) yw)))
          (void)
      ))))
    (super-new)))


(define canva (new my-canvas% [parent frame]
                   [style (list 'no-autoclear)]
                   [paint-callback
                   (lambda (canvas dc)
                     (send dc set-pen (make-color 0 0 0 0) 0 'transparent))]
))

;; Game Loop
(define (game_end? hp)
  (not (positive? hp))
)

(define (update_game enemies towers hp coins secs)
  (let*-values ([(enemies) (update_enemies enemies towers secs)]
                [(hp) (check_damage enemies hp)]
                [(coins) (check_kill enemies coins)]
                [(enemies) (remove_dead_enemies enemies)]
                [(towers coins) (buy_tower (car buy_try) (cadr buy_try) (caddr buy_try) towers coins)])
    (set! buy_try (list #f 0 0))
    (draw_game canva enemies towers hp coins)
    (values enemies towers hp coins)
))

(define (game_loop enemies towers hp coins secs)
  (let-values ([(enemies towers hp coins) (update_game enemies towers hp coins secs)])
  (sleep/yield 0.5)
  (if (not (game_end? hp))
      (game_loop enemies towers hp coins (+ secs 1))
      (show_defeat (send canva get-dc))
)))

(send frame show #t)
(game_loop enemies towers hp coins 0)