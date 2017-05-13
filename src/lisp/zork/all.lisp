(defmethod nosuch ((player IsPlayer) name)
  (player-format player "You don't see any ~A here.~&" (string-downcase name)))

(defmethod nosuch ((player IsPlayer) (obj (eql 'it)))
  (player-format player "You'll have to be more specific.~&"))

; ----------------------------------------------------------------------
; -------------------- Exiting

;(defmethod north ((player IsPlayer))
;  (let ((destination (traverse (location player) 'north)))
;    (if destination
;	(setf (location player) destination)
;      (player-format player "You can't go that way."))))

(defmacro defdirection (direction)
  (let ((dest (gensym)))
    `(defun ,direction (player)
       (let ((,dest (traverse (location player) ',direction)))
	 (if ,dest
	     (setf (location player) ,dest)
	   (player-format player "You can't go that way."))))))

(defdirection up)
(defdirection down)

(defdirection in)
(defdirection out)

(defdirection north)
(defdirection east)
(defdirection south)
(defdirection west)

(defdirection northeast)
(defdirection southeast)
(defdirection northwest)
(defdirection southwest)

(def-method-alias north n)
(def-method-alias south s)
(def-method-alias east e)
(def-method-alias west w)
(def-method-alias northeast ne)
(def-method-alias northwest nw)
(def-method-alias southeast se)
(def-method-alias southwest sw)
(def-method-alias up u)
(def-method-alias down d)

; ----------------------------------------------------------------------
; -------------------- Dropping

(defmethod drop ((player IsPlayer) (objs (eql 'all)))
  (let ((all (contents player)))
    (if all
	(drop player all)
      (player-format player "You have nothing to drop!"))))

(defmethod drop ((player IsPlayer) (objs cons))
  (if (second objs)
      (dolist (each objs nil)
	(player-format player "~&~A: " each)
	(drop player each))
    (drop player (first objs))))

(defmethod drop (player (obj HasLocation))
  (move-obj player obj (location obj) (location player)))

(defmethod move-obj ((player IsPlayer) (obj HasLocation) (from IsAnimal) (to IsRoom))
  (if (eql player from)
      (progn
	(setf (location obj) (location player))
	(player-format player "Dropped."))
    (player-format player "~&Just how do you propose to do that?")))

(defmethod move-obj ((player IsPlayer) (obj HasLocation) (from HasContents) (to IsRoom))
  (if (is-carrying player from)
      (player-format player "You must first remove it from the ~A."
	      (name from))
    (player-format player "You are not carrying it.")))

; ----------------------------------------------------------------------
; -------------------- Taking

(defmethod take ((player IsPlayer) (objs (eql 'all)))
  (let ((all (remove player (contents (location player)))))
    (if all
	(take player all)
      (player-format player "There is nothing to take!"))))

(defmethod take ((player IsPlayer) (objs cons))
  (if (second objs)
      (dolist (each objs nil)
	(player-format player "~&~A: " each)
	(take player each))
    (take player (first objs))))

(defmethod take (player (obj HasLocation))
  (move-obj player obj (location obj) player))

(defmethod move-obj ((player IsPlayer) (obj IsImmobile) from (to IsPlayer))
  (player-format player (immobile-because obj)))

(defmethod move-obj (player obj from to)
  (player-format player "You can't move that!~&"))

(defmethod move-obj ((player IsPlayer) (obj IsImmobile) (from IsPlayer) to)
  (player-format player (immobile-because obj)))

(defmethod move-obj ((player IsPlayer) (obj HasLocation) (from IsAnimal) (to IsPlayer))
  (if (eql from to)
      (player-format player "You are already carrying it.")
    (player-format player "If you want to fight the ~A, just say so." (name from))))

(defmethod move-obj ((player IsPlayer) (obj HasLocation) (from HasContents) (to IsPlayer))
  (setf (location obj) player)
  (if (is-carrying player from)
      (progn
	(player-format player "You remove the ~A from the ~A." 
		       (name obj) 
		       (name from)))
    (player-format player "Taken." (name obj))))

; ----------------------------------------------------------------------
; -------------------- Reachability

(defmethod can-reach ((animal IsAnimal) obj)
  (can-reach-in animal obj (location obj)))

(defmethod can-reach ((animal IsAnimal) (obj isGame))
  (eql (game animal) obj))

(defmethod can-reach-in ((animal IsAnimal) obj (container HasContents))
  (can-reach-in animal container (location container)))

(defmethod can-reach-in :around ((animal IsAnimal) obj (container IsCloseable))
  (and (is-open container)
       (call-next-method animal obj container)))

(defmethod can-reach-in ((animal IsAnimal) obj (container IsAnimal))
  (eql container animal))

(defmethod can-reach-in ((animal IsAnimal) obj (container IsRoom))
  (eql container (room-with animal)))

(defmethod can-reach-in ((animal IsAnimal) obj (container IsDungeon))
  (eql container (dungeon-with animal)))

(defmethod can-reach-in ((animal IsAnimal) obj (container IsGame))
  (eql container (game animal)))

; ----------------------------------------------------------------------
; -------------------- Opening and Closing

(defmethod open-container :around ((player IsPlayer) (obj IsPiece))
  (if (can-reach player obj)
      (call-next-method player obj)
    (player-format player "You aren't carrying the ~A." (name obj))))

(defmethod open-container ((player IsPlayer) (obj IsPiece))
  (player-format player "You can't open ~A!" (a-or-an-then obj)))

(defmethod close-container ((player IsPlayer) (obj IsPiece))
  (player-format player "You can't close ~A!" (a-or-an-then obj)))

(defreduction open-container (animal (objs cons)) objs)

(defreduction close-container (animal (objs cons)) objs)

(defmethod open-container ((player IsPlayer) (container IsCloseable))
  ; must be reachable
  (if-open container
      (player-format player "It is already open!")
    (progn
      (setf (is-open container) T)
      (if (contents container)
	  (progn 
	    (player-format player "Opening the ~A reveals " container)
	    (print-object-list (contents container) t)
	    
	    ; If the list has more than one item, then "it" is ambiguous
	    ; "it" is unambiguous for one item or empty container
	    (if (second (contents container))
		(setf (it player) nil)
	      (setf (it player) (first (contents container)))))
	(player-format player "Opened.")))))

(defmethod close-container ((player IsPlayer) (container IsCloseable))
  ; must be reachable
  (if-open container
      (progn
	(setf (is-open container) nil)
	(player-format player "Closed."))
    (player-format player "It is already closed!")))

; ----------------------------------------------------------------------
; -------------------- Structure

(defmethod game ((obj HasLocation))
  (game (location obj)))

(defmethod game ((obj IsGame))
  obj)

(defmethod dungeon-with ((obj HasLocation))
  (dungeon-with (location obj)))

(defmethod dungeon-with ((dungeon IsDungeon))
  dungeon)

(defmethod dungeon-in ((game IsGame) label)
  (find-in-shallow game label))

(defmethod room-in ((game IsGame) label)
  (room-in (contents game) label))

(defmethod room-in ((dungeon IsDungeon) label)
  (find-in-shallow dungeon label))

(defmethod room-in ((dungeon IsDungeon) (room IsRoom))
  (if (eq dungeon (dungeon room))
      room
    nil))

(defmethod room-in ((places cons) label)
  (or (room-in (car places) label)
      (room-in (cdr places) label)))

(defmethod room-with ((obj HasLocation))
  (room-with (location obj)))

(defmethod room-with ((room IsRoom))
  room)

; ----------------------------------------------------------------------
; -------------------- Containership

;(defmethod has-in-hand ((place HasContents) obj)
;  (eql place (location obj)))

(defmethod is-carrying ((animal IsAnimal) obj)
  (is-in obj animal))

(defmethod is-in (obj (cont cons))
  (or (is-in obj (car cont))
      (is-in obj (cdr cont))))

(defmethod is-in (obj1 obj2)
  nil)

(defmethod is-in ((obj HasLocation) (cont HasContents))
  (let ((obj-loc (location obj)))
    (and obj-loc                      ; if obj is contained, and
	 (or (eql cont obj-loc)       ; either it is directly in cont
	     (is-in obj-loc cont))))) ; or its container is

(defmethod is-in (obj (cont HasContents))
  (or (first (member obj (contents cont)))
      (is-in obj (contents cont))))

; ----------------------------------------------------------------------
; -------------------- Finding

;(defmethod find-for ((player IsPlayer) (obj (eql 'all)))
;  'all)

(defmethod find-for ((player IsPlayer) (obj (eql 'it)))
  (let ((real-obj (it player)))
    (if real-obj (player-format player "[The ~A] " (name real-obj)))
    (find-for player real-obj)))

(defmethod find-for ((animal IsAnimal) obj)
  (or (find-in animal obj)
      (find-in (location animal) obj)))

(defmethod find-in ((places cons) obj)
  (or (find-in (car places) obj)
      (find-in (cdr places) obj)))

(defmethod find-in :around ((place IsCloseable) obj)
  (if-open place
	   (call-next-method place obj)
	   nil))

(defmethod find-in ((place HasContents) obj)
  (or (find-in-shallow place obj)
      (find-in (contents place) obj)))

(defmethod find-in (place label)
  nil)

(defmethod find-in-shallow ((places cons) obj)
  (or (find-in-shallow (car places) obj)
      (find-in-shallow (cdr places) obj)))

(defmethod find-in-shallow ((place HasContents) (obj IsPiece))
  (first (member obj (contents place))))

(defmethod find-in-shallow ((place HasContents) (label symbol))
  (find-in-shallow place (symbol-name label)))

(defmethod find-in-shallow ((place HasContents) (name string))
  (debug-do place 
	    (progn (format t "Searching ~A.~&" (name place))
		   (dolist (each (contents place) nil)
		     (format t "    ~A~&" (label each)))
		   (format t "~2&")))
  (find-if #'(lambda (x)
	       (string-equal name (name x)))
	   (contents place)))

(defmethod find-in-shallow ((place (eql nil)) label)
  nil)

(defmethod find-in-shallow (place label)
  nil)

(defmethod find-in-shallow (place (name string))
  (find-in-shallow place (intern name)))

; ----------------------------------------------------------------------
; -------------------- User Commands

(defmethod state ((game IsGame))
  "")

(defmethod user-quitp ((player IsPlayer))
  (if (debug-on player)
      (toggle-debug-mode player)
    (progn
      (score player)
      (if (eq 'y 'y);(prompt-player game "Really user-quitp?"))
	  (exit)))))

(def-method-alias user-quitp q)

(defmethod enter :around ((player IsPlayer) (game IsGame))
  (call-next-method player game)
  (do ((player-input (next-command player) (next-command player)))
      (())
    (eval-input player player-input)))

(defmethod score ((player IsPlayer))
    (player-format player
	    "Your score is ~A out of a possible ~A.~&This gives you the rank of novice."
	    (current-score player)
	    (max-score (game player))))

; ----------------------------------------------------------------------
; --------------------

;(defmethod command-separator ((str string))
;  (or (eq str ".") (eq str ";") (eq str "then ")))

;(defun replace-string (string target replacement &optional &key (start1 0) (start2 0) (start3 0))
;  (replace target replacement
;	   :start1 (search target string
;			   :start1 start2
;			   :start2 start1)
;	   :start2 start3))
