#lang racket
(require "Drawing.rkt")

(define TOWER_RADIUS 2)

(define (get_towers_on_range x y towers)
  (length (filter (lambda (tower) (and (car tower) (<= (abs (- x (cadr tower))) TOWER_RADIUS) (<= (abs (- y (caddr tower))) TOWER_RADIUS))) towers))
)

(define (find_tower towers x y)
  (index-of towers (list #f (exact-floor(/ (- x TOWER_OFFSET) BLOCK_SIZE)) (exact-floor(/ (- y TOWER_OFFSET) BLOCK_SIZE))))
)

(provide (all-defined-out))