(in-package :cl-user)

(load "/Users/abesto/Workspace/lisp-koans/greed/lisp-unit.lisp")
(load "/Users/abesto/Workspace/lisp-koans/greed/common.lsp")

(defpackage :greed-test
  (:use :common-lisp)
  (:use :lisp-unit)
  (:use :greed))

(in-package :greed-test)

;;; Dice tests
(define-test test-create-dice-set
;; tests making an instance of the dice-set
    (let ((dice (make-instance 'dice-set)))
      (assert-true dice)))

(define-test test-error-if-trying-to-roll-more-than-5-dice
    (let ((dice (make-instance 'dice-set)))
      (assert-true (roll '(0 1 2 3 4) dice))
      (assert-error 'there-are-not-enough-dice (roll '(1 2 3 4 5 6) dice))))


(define-test test-rolling-the-dice-returns-a-set-of-integers-between-1-and-6
;; tests rolling the dice
    (let ((dice (make-instance 'dice-set)))
      (roll '(0 1 2 3 4) dice)
      (assert-equal 5 (length (get-dice-set-rolls dice)))
      (loop for x across (get-dice-set-rolls dice) do
        (assert-true (and (>= x 1)
                          (<= x 6)
                          (typep x 'integer))))))


(defun dice-equal-p (one two &optional (indices '(0 1 2 3 4)))
  (loop for index in indices do
       (assert-equal
        (aref one index)
        (aref two index))))

(define-test test-dice-values-do-not-change-unless-explicitly-rolled
;; tests that dice don't change just by looking at them
    (let ((dice (make-instance 'dice-set)))
      (roll '(0 2 4) dice)
      (let ((first-time (get-dice-set-rolls dice))
            (second-time (get-dice-set-rolls dice)))
        (dice-equal-p first-time second-time))))


(define-test test-dice-values-should-change-between-rolls
;; tests that rolling the dice DOES change the values.
    (let ((dice (make-instance 'dice-set))
          (first-time nil)
          (second-time nil))
      (roll '(0 1 2 3 4) dice)
      (setf first-time (get-dice-set-rolls dice))
      (roll '(0 1 2 3 4) dice)
      (setf second-time (get-dice-set-rolls dice))
      (assert-false (dice-equal-p first-time second-time))))

(define-test test-score-of-an-empty-list-is-zero
    (assert-equal 0 (score nil)))

(define-test test-score-of-a-single-roll-of-5-is-50
    (assert-equal 50 (score '(5))))


(define-test test-score-of-a-single-roll-of-1-is-100
    (assert-equal 100 (score '(1))))

(define-test test-score-of-multiple-1s-and-5s-is-the-sum-of-individual-scores
    (assert-equal 300 (score '(1 5 5 1))))

(define-test test-score-of-single-2s-3s-4s-and-6s-are-zero
    (assert-equal 0 (score '(2 3 4 6))))


(define-test test-score-of-a-triple-1-is-1000
    (assert-equal 1000  (score '(1 1 1))))

(define-test test-score-of-other-triples-is-100x
    (assert-equal 200  (score '(2 2 2)))
    (assert-equal 300  (score '(3 3 3)))
    (assert-equal 400  (score '(4 4 4)))
    (assert-equal 500  (score '(5 5 5)))
    (assert-equal 600  (score '(6 6 6))))

(define-test test-score-of-mixed-is-sum
    (assert-equal 250  (score '(2 5 2 2 3)))
    (assert-equal 550  (score '(5 5 5 5))))

(define-test test-player-name
    (let ((player (make-player "player1")))
      (assert-equal "player1" (get-name player))))

(defmacro with-game-and-players (game-sym player-syms &body body)
  `(let ((,game-sym (make-game))
         ,@(map 'list
               #'(lambda (player-sym) (list player-sym `(make-player ,(string player-sym))))
               player-syms
               ))
     ,@body))

(defmacro with-game-and-players-in-game (game-sym player-syms &body body)
  `(let ((,game-sym (make-game))
         ,@(map 'list
               #'(lambda (player-sym) (list player-sym `(make-player ,(string player-sym))))
               player-syms
               ))
     ,@(map 'list
            #'(lambda (player-sym) `(join ,game-sym ,player-sym))
            player-syms)
     ,@body))

(define-test test-join
  (with-game-and-players-in-game game (player1 player2)
      (assert-equal player1 (first (get-players game)))
      (assert-equal player2 (second (get-players game)))))

(define-test test-cannot-start-with-less-than-two-players
    (let ((game (make-game)))
      (assert-error 'not-enough-players (start game))
      (join game (make-player "x"))
      (assert-error 'not-enough-players (start game))
      (join game (make-player "y"))))

(define-test test-next-player
  (with-game-and-players-in-game game (p1 p2 p3)
    (set-current-player-index 0 game)
    (loop for p in (list p1 p2 p3 p1 p2) do
         (assert-equal p (get-current-player game))
         (next-player game))))

(define-test test-gets-into-game-by-throwing-high
  (with-game-and-players-in-game game (p1)
    (assert-false (in-game-p p1 game))
    (let ((dice (make-dice-set (make-array 5 :initial-element 1))))
      (account-dice game dice)
      (assert-true (in-game-p p1 game)))))

(define-test test-scores-start-at-0-and-are-updated
  (with-game-and-players-in-game game (p1)
    (let ((dice (make-dice-set (make-array 5 :initial-element 1))))
      (assert-equal 0 (get-score p1 game))
      (account-dice game dice)
      (assert-equal 1200 (get-score p1 game))
      (account-dice game dice)
      (assert-equal 2400 (get-score p1 game)))))

(setf *print-failures* t)
(setf *print-errors* t)
(run-tests)
