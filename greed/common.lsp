;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.

(in-package :cl-user)

(defpackage :greed
  (:use :common-lisp)
  (:export :dice-set
           :make-dice-set
           :get-dice-set-rolls
           :roll
           :there-are-not-enough-dice
           :score)
  (:export :player
           :make-player
           :get-name)
  (:export :game
           :make-game
           :get-players
           :join
           :set-current-player-index
           :get-current-player
           :in-game-p
           :next-player
           :account-dice
           :get-score
           :start
           :not-enough-players))

(in-package :greed)

(define-condition there-are-not-enough-dice (error)
  ((have :initarg have-dice)
   (tried-to-roll :initarg tried-to-roll)))

(define-condition not-enough-players (error) ())
(define-condition player-index-out-of-bounds (error) ())

(defclass dice-set ()
  ((rolls :reader get-dice-set-rolls :initform (make-array 5))))

(defun make-dice-set (&optional (rolls nil))
  (let ((dice (make-instance 'dice-set)))
    (if rolls (setf (slot-value dice 'rolls) rolls))
    dice))

(defmethod roll (reroll-dice (object dice-set))
  (with-slots (rolls) object
    (if (> (length reroll-dice) (length rolls))
        (error (make-condition 'there-are-not-enough-dice
                               :have (length rolls)
                               :tried-to-roll (length reroll-dice))))
    (dolist (index reroll-dice)
      (setf (aref rolls index) (+ 1 (random 6))))
    rolls))

(defun count-rolls (dice)
  (let ((counts (make-hash-table)))
    (loop for roll from 1 to 6 do (setf (gethash roll counts) 0))
    (dolist (roll dice)
      (incf (gethash roll counts)))
    counts))

(defmethod score ((dice list))
  (let ((score 0))
    (loop for roll being the hash-keys in (count-rolls dice)
         using (hash-value count) sum
         (multiple-value-bind (triplets rest) (floor count 3)
           (+
            (* triplets roll (cond
                               ((= 1 roll) 1000)
                               (t 100)))
            (* rest (cond
                      ((= 5 roll) 50)
                      ((= 1 roll) 100)
                      (t 0))))))))

(defmethod score ((dice dice-set))
  (score (coerce (get-dice-set-rolls dice) 'list)))

(defclass player ()
  ((name :reader get-name :initarg :name)))

(defun make-player (name)
  (make-instance 'player :name name))

(defmethod decide ((player player) (dice dice-set)) nil)

(defclass game ()
  ((players :reader get-players :initform '())
   (current-player-index :initform 0)
   (in-game-p-hash :initform (make-hash-table))
   (score-hash :initform (make-hash-table))))

(defun make-game () (make-instance 'game))

(defmethod get-current-player ((game game))
  (nth (slot-value game 'current-player-index) (get-players game)))

(defmethod set-current-player-index (index (game game))
  (with-slots (current-player-index players) game
    (if (>= index (length players)) (error (make-condition 'player-index-out-of-bounds)))
    (setf current-player-index index)))

(defmethod join ((game game) (player player))
  (with-slots (players in-game-p-hash score-hash) game
    (setf players (append players (list player)))
    (setf (gethash player in-game-p-hash) nil)
    (setf (gethash player score-hash) 0)))

(defmethod start ((game game))
  (with-slots (players) game
    (if (< (length players) 2) (error (make-condition 'not-enough-players)))
    (set-current-player-index 0 game)
    (loop while (not (eq :end-game (tick game))))))

(defmethod tick ((game game))
  (let ((dice (make-dice-set))
        (current-player (get-current-player game)))
    (roll '(0 1 2 3 4) dice)
    (format t "Player ~a rolled ~a. Score ~a~%"
            (get-name current-player)
            (get-dice-set-rolls dice)
            (score dice))
    (roll (decide current-player dice) dice)
    (account-dice game dice)
    (if (end-game-p game) :end-game (next-player game))))

(defmethod in-game-p ((player player) (game game))
  (with-slots (in-game-p-hash) game
    (gethash player in-game-p-hash)))

(defmethod account-dice ((game game) (dice dice-set))
  (with-slots (in-game-p-hash score-hash) game
    (let ((player (get-current-player game))
          (score (score dice)))
      (if (and (not (in-game-p player game)) (>= score 300))
               (progn
                 (setf (gethash player in-game-p-hash) t)
                 (format t "Player ~a is now in the game~%" (get-name player))))
      (if (in-game-p player game)
          (progn (incf (gethash player score-hash) score)
                 (format t "Player ~a now has ~a points~%"
                         (get-name player)
                         (get-score player game)))))))

(defmethod next-player ((game game))
  (with-slots (current-player-index players) game
    (incf current-player-index)
    (if (= (length players) current-player-index) (setf current-player-index 0))))

(defmethod get-score ((player player) (game game))
  (with-slots (score-hash) game (gethash player score-hash)))

(defmethod end-game-p ((game game))
  (<= 3000 (get-score (get-current-player game) game)))
