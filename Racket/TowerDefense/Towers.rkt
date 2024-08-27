#lang racket
(require "Drawing.rkt")

(define TOWER_RADIUS 2)
(define TOWER_PRICE 30)

(define (get_towers_on_range x y towers)
  (length (filter (lambda (tower) (and (car tower) (<= (abs (- x (cadr tower))) TOWER_RADIUS) (<= (abs (- y (caddr tower))) TOWER_RADIUS))) towers))
)

(define (find_tower towers x y)
  (index-of towers (list #f (exact-floor(/ (- x TOWER_OFFSET) BLOCK_SIZE)) (exact-floor(/ (- y TOWER_OFFSET) BLOCK_SIZE))))
)

(define (buy_tower try_buy? x y towers coins)
  (if (and try_buy? (>= coins TOWER_PRICE))
      (let ([tower_idx (find_tower towers x y)])
        (if (and (number? tower_idx) (not (car (list-ref towers tower_idx))))
            (begin (let* ([coins (- coins TOWER_PRICE)]
                          [towers (list-set towers tower_idx (list #t (cadr (list-ref towers tower_idx)) (caddr (list-ref towers tower_idx))))])
                     (values towers coins)))
            (values towers coins)))
      (values towers coins)          
))

(provide (all-defined-out))