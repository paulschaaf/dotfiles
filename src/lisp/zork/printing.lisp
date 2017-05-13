; ----------------------------------------------------------------------
; -------------------- Looking, examining, describing

(defmethod player-format ((player IsPlayer) &rest rest)
  (apply 'format (display player) rest))

(defmethod look ((animal IsAnimal) &optional (thing (location animal)))
  (look-at animal thing))

(def-method-alias look l)

(defmethod look-at :around ((player IsPlayer) (object HasLabel))
  (if (can-reach player object)
      (call-next-method player object)
    (player-format "You can't see ~A here." (a-or-an-then object))))

(defmethod look-at ((player IsPlayer) (object HasDescription))
  (player-format player "~A~&" (description object)))

(defreduction look-at ((animal IsAnimal) (objs cons)) objs)

(defmethod look-at :after ((player IsPlayer) (obj IsBag))
  (if-open obj
	   (print-contents obj (display player))
	   (player-format player "~&The ~A is closed." obj)))

(defmethod look-at :after ((player IsPlayer) (obj IsContainer))
  (if-open obj
	   (print-contents obj (display player))
	   (player-format player "~&The ~A is closed." obj)))

(defmethod look-at ((player IsPlayer) (room IsRoom))
  (print-object room (display player))
  (player-format player "~&~A" (description room))
  (let ((exits nil))
    (maphash (lambda (key value)
	       (unless (member value exits)
		 (player-format player " ~A" (description value))
		 (push value exits)))
	     (arcs room))
    '(print-object-list exits (display player)))
  (dolist (item (contents room) nil)
    (unless (eql item player)
      (player-format player "~&There is ~A~A here.~&" (a-or-an item) item)
      (print-contents item t))))

(defmethod look-at :before ((player IsPlayer) (game IsGame))
  (clear-display player))

(defmethod look-at :after ((player IsPlayer) (game IsGame))
  (player-format player "~3&"))

(defmethod look-at :after ((player IsPlayer) (dungeon IsDungeon))
  (player-format player "~3&"))

(defmethod inventory ((player IsPlayer))
  (print-contents player (display player)))

(def-method-alias inventory i)

; ----------------------------------------------------------------------
; -------------------- Printing

(defmethod print-object ((thing HasName) str)
  (debug-do thing 
	    (print-attributes thing str)
	    (format str (name thing))))

(defmethod print-contents (item str) nil)

(defmethod print-contents ((item HasContents) str)
  (unless (or (is-empty item)
	      (is-closed item))
    (format str "~&The ~A contains:~&" (name item))
    (dolist (each (contents item) nil)
      (format str "  ~A~A~&" (capital-a-or-an each) each)
      (print-contents each str))))

(defmethod print-contents ((player IsPlayer) str)
  (if (is-empty player)
      (player-format player "~&You are empty-handed.~&")
    (progn
      (player-format player "~&You are carrying:~&")
      (dolist (each (contents player) nil)
	(player-format player "~2T~A~A~&" (capital-a-or-an each) each)
	(print-contents each str)))))

(defmethod print-object-list ((items cons) str)
    (apply 'format str
	   "~#[none~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^, ~}~]."
	   (mapcar (lambda (each)
		     (a-or-an-then each))
		   items)))

; ----------------------------------------------------------------------
; -------------------- "A <object>" or "An <object>"?

(defmethod capital-a-or-an ((obj HasName))
  (capital-a-or-an (name obj)))

(defmethod capital-a-or-an (string)
  (if (begins-with-vowel-sound string)
      "An "
    "A "))

(defmethod a-or-an ((obj HasName))
  (a-or-an (name obj)))

(defmethod a-or-an-then ((obj HasName))
  (concatenate 'string (a-or-an (name obj)) (name obj)))

(defmethod a-or-an-then ((obj arc))
  (setf desc (description obj))
  (if desc
      (concatenate 'string (a-or-an desc) desc)
    "an exit"))

(defmethod a-or-an (string)
  (if (begins-with-vowel-sound string)
      "an "
    "a "))

(defun begins-with-vowel-sound (str)
  (member (aref str 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)))

; ----------------------------------------------------------------------
; -------------------- Printing Attributes

(defun print-attribute (attribute obj str)
  (debug-do obj
	(progn
	  (prin1 attribute)
	  (prin1 (funcall attribute obj)))))

(defmethod print-attributes (thing str) nil)

(defmethod print-attributes :after ((thing HasLabel) str)
  (prin1 (label thing) str))

(defmethod print-attributes :after ((thing HasName) str)
  (prin1 (name thing) str))

; ----------------------------------------------------------------------
; -------------------- Display

(defmethod clear-display ((display (eql t)))
  (format t "~3&"))

(defmethod clear-display ((player IsPlayer))
  (clear-display (display player)))

; ----------------------------------------------------------------------
; -------------------- 

#|
'(defparameter text "Folding and splicing is the work of an editor, not a mere collection of silicon and mobile electrons!")

'(defun wrap (str max-col)
  (let* ((words (string-tokenize str))
         (all nil)
         (first (car words))
         (col (string-length first))
         (line (list first)))
    (mapcar
	(lambda (x)
	  (let* ((len (string-length x))
		 (new-col (+ col len 1)))
	    (cond ((> new-col max-col)
		   (setf all (cons (string-join (reverse! line) " ") all))
		   (setf line (list x))
		   (setf col len))
		  (else
		   (setf line (cons x line))
		   (setf col new-col)))))
      (cdr words))
    (setf all (cons (string-join (reverse line) " ") all))
    (string-join (reverse all) "\n")))

'(display (wrap text 20))
|#
