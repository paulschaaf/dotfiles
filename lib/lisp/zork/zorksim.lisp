(load "meta.lisp")
(load "classes.lisp")
(load "printing.lisp")
(load "parser.lisp")
(load "all.lisp")
(load "making.lisp")

; --------------------- Make game and player

(defparameter *avatar* (make-player "Avatar"))

(defun this-room ()
  (location *avatar*))

(defparameter *game* (make-instance 
   'IsGame
   :label '|ZorkSim|
   :max-score 200
   :description "Welcome to ZorkSim, the land of adventure and low cunning."))

(def-method-alias toggle-debug-mode g)

'(defun toggle-debug ()
  (toggle-debug-mode *avatar*))

; -------------------- Make dungeon and rooms

(defparameter *dungeon* 
  (make-dungeon
   "Zork 1"
   "You awaken, unsure whether your dream has ended..."))

(load "dungeon1.lisp")
(setf (entrance *game*) *dungeon*)

; -------------------- Begin the game!

(enter *avatar* *game*)
