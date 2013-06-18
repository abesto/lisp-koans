(in-package :cl-user)

(load "/Users/abesto/Workspace/lisp-koans/greed/common.lsp")

(defpackage :greed-simulation
  (:use :common-lisp)
  (:use :greed))

(in-package :greed-simulation)

(let ((game (make-game))
      (p1 (make-player "p1"))
      (p2 (make-player "p2"))
      (p3 (make-player "p3")))

  (join game p1)
  (join game p2)
  (join game p3)

  (start game))
