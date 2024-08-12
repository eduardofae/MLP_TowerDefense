#lang racket
(require 2htdp/image)


(define SIZE 40)
(define GRID_SIZE 10)
(define SCENE_SIZE (* GRID_SIZE SIZE))

(define SLIME_SIZE 10)
(define SLIME_OFFSET (* SLIME_SIZE (/ (/ SIZE SLIME_SIZE) 2)))

(define TOWER_SIZE 20)
(define TOWER_OFFSET (* TOWER_SIZE (/ (/ SIZE TOWER_SIZE) 2)))

(define scene (empty-scene SCENE_SIZE SCENE_SIZE (make-color  0   0   0   0 )))
(define bg    (empty-scene SCENE_SIZE SCENE_SIZE (make-color 166 176 079 255)))

(define path        (square SIZE       "solid" (make-color 235 150 097)))
(define slime       (circle SLIME_SIZE "solid" (make-color 093 129 068)))
(define tower       (circle TOWER_SIZE "solid" (make-color 130 149 177)))
(define tower_space (square SIZE       "solid" (make-color 188 173 159)))

(define (print_paths list_paths)
  (if (>= (length list_paths) 1)
      (place-image/align path (* (caar list_paths) SIZE) (* (cadar list_paths) SIZE) "left" "top" (print_paths (list-tail list_paths 1)))
       scene
))

(define paths (list (list 9 1) (list 8 1) (list 7 1) (list 6 2) (list 5 3) (list 4 3) (list 3 3) (list 2 3) (list 1 3) (list 1 4)
                    (list 1 5) (list 1 6) (list 1 7) (list 1 8) (list 2 8) (list 3 8) (list 4 8) (list 5 8) (list 6 7) (list 6 6)
                    (list 6 5) (list 7 4) (list 8 5) (list 8 6) (list 8 7) (list 8 8) (list 8 9)))
(define world_map (place-image/align (print_paths paths) 0 0 "left" "top" bg))

(define (get_enemy_pos step idx)
  (+ (* (list-ref (list-ref paths step) idx) SIZE) SLIME_OFFSET)
)

(define (print_enemies list_enemies)
  (if (>= (length list_enemies) 1)
      (place-image slime (get_enemy_pos (cadar list_enemies) 0) (get_enemy_pos (cadar list_enemies) 1) (print_enemies (list-tail list_enemies 1)))
       scene
))

(define (print_towers list_towers)
  (if (>= (length list_towers) 1)
      (if (caar list_towers)
          (place-image       tower       (+ (* (cadar list_towers) SIZE) TOWER_OFFSET) (+ (* (caddar list_towers) SIZE) TOWER_OFFSET) (print_towers (list-tail list_towers 1)))
          (place-image/align tower_space    (* (cadar list_towers) SIZE)                  (* (caddar list_towers) SIZE) "left" "top"  (print_towers (list-tail list_towers 1)))
       )
       scene
))

(define (print_game list_enemies list_towers)
  (place-image/align (print_towers  list_towers ) 0 0 "left" "top"
  (place-image/align (print_enemies list_enemies) 0 0 "left" "top"
   world_map
 ))
)

(define (damage_calc health)
  (set! health (- health 1))
  (if (negative? health)
      0
      health
  )
)


(define (new_enemy)
  (list (list 27 0))
)

(define (update_enemies list_enemies secs)
  (set! list_enemies (map (lambda (enemy)
         (set! enemy (list-set enemy 0 (damage_calc (car enemy))))
         (list-set enemy 1 (+ (cadr enemy) 1))
  ) list_enemies))
  (set! list_enemies (filter (lambda (enemy) (not (or (zero? (car enemy)) (equal? (cadr enemy) 27)))) list_enemies))
  (if (zero? (remainder secs 5))
    (append list_enemies (new_enemy))
     list_enemies
  )
)

(define enemies (list ))
(define towers  (list (list #t 4 1) (list #t 8 2) (list #f 2 4) (list #t 4 5) (list #t 4 7) (list #f 3 9) (list #f 7 6) (list #t 9 6)))

(define (update_game secs)
  (set! enemies (update_enemies enemies secs))
  (print_game enemies towers)
)

(for ([i 52])
  (display "\n")
  (display (update_game i))
  (sleep 0.5)
)