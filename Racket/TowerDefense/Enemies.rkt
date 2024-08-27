#lang racket
(require "Towers.rkt")
(require "Drawing.rkt")

(define DIFFICULTY_SCALE 0.05)

(define (new_enemy secs)
  (list (list (random (+ 1 (exact-floor (* DIFFICULTY_SCALE secs))) (+ 5 (exact-floor (* DIFFICULTY_SCALE secs)))) 0))
)
        
(define (update_enemies list_enemies towers secs)
  (let ([new_list_enemies
         (map (lambda (enemy)
                (let ([new_enemy
                  (list-set enemy 0 (damage_calc (car enemy) (list-ref paths (cadr enemy)) towers))])
                  (list-set new_enemy 1 (+ (cadr new_enemy) 1))
         )) list_enemies)])
  (if (zero? (remainder secs (max (- 8 (exact-floor (* (* DIFFICULTY_SCALE 0.1) secs))) 1)))
      (append new_list_enemies (new_enemy secs))
      new_list_enemies
)))

(define (damage_calc health pos towers)
  (let ([new_health (- health (get_towers_on_range(car pos) (cadr pos) towers))]) 
    (if (negative? new_health)
        0
        new_health
)))

(define (remove_dead_enemies enemies)
  (filter (lambda (enemy) (not (or (zero? (car enemy)) (equal? (cadr enemy) 27)))) enemies)
)

(define (check_damage enemies hp)
  (- hp (length (filter (lambda (enemy) (= (cadr enemy) 27)) enemies)))
)

(define (check_kill enemies coins)
  (+ coins (* (length (filter (lambda (enemy) (= (car enemy) 0)) enemies)) 3))
)

(provide (all-defined-out))