(load "directedgraph.lisp")

; -----------------------------------------------------------------------
; -------------------- Primitive Properties

(defclass HasLabel ()
  ((debug-on      :allocation :class
		  :accessor debug-on     :initarg :debug-on
		  :initform nil)
   (label         :accessor label        :initarg :label
		  :initform 'nolabel)))

(defclass HasLocation ()
  ((location      :accessor location     :initarg :location
		  :initform nil)))

(defclass HasContents ()
  ((contents      :accessor contents
		  :initform nil)))

(defclass IsCloseable ()
  ((is-open       :accessor is-open
		  :initform nil)))

(defclass HasDescription ()
  ((description   :accessor description  :initarg :description)))

(defclass HasName (HasLabel)
  ())

(defmethod name ((obj HasName))
  (symbol-name (label obj)))

(defmethod (setf name) (string (obj HasName))
  (setf (label obj) (intern (copy-seq string))))

(defclass IsMobile ()
  (()))

(defclass IsImmobile ()
  ((immobile-because :accessor immobile-because :initarg :immobile-because
		     :initform "remains fixed to the floor.")))

; ----------------------------------------------------------------------
; -------------------- Object Types

(defclass IsExaminable (HasName HasDescription HasLocation) ())
(defclass IsPlace      (HasContents IsExaminable) ())
(defclass ExitPath     (HasDescription arc) ())

(defclass IsPiece      (IsMobile IsExaminable) ())
(defclass IsBag        (IsCloseable HasContents IsPiece) ())

(defclass IsFixture    (IsImmobile IsExaminable) ())
(defclass IsContainer  (IsCloseable HasContents IsFixture) ())

(defclass IsDungeon    (IsPlace)
   ((entrance          :accessor entrance)))

(defclass IsAnimal     (IsPlace) ())
(defclass IsPlayer     (IsAnimal)
  ((game               :accessor game        :initarg :game)
   (it                 :accessor it
		       :initform nil)
   (prompt             :accessor prompt      :initarg :prompt
		       :initform ">")
   (current-score      :accessor current-score
		       :initform 0)
   (display            :accessor display     :initarg :display
		       :initform t)
   (verbose            :accessor verbose     :initarg :verbose
		       :initform t)))

(defmethod (setf it) :after (obj (player IsAnimal))
  (debug-format player "~&'it set to ~A~&" obj))

(defclass IsRoom       (IsPlace node) ())

(defclass IsGame       (HasContents HasName HasDescription)
  ((max-score          :accessor max-score   :initarg :max-score
		       :initform 100)
   (entrance           :accessor entrance)))

(defmethod (setf entrance) ((name symbol) (dungeon IsDungeon))
  (setf (entrance dungeon) (room-in dungeon name)))

(defmethod (setf entrance) ((name symbol) (game IsGame))
  (setf (entrance game) (dungeon-in game name)))

; ----------------------------------------------------------------------
; -------------------- Opening / Closing

(defmethod is-open (obj)
  T)

(defmethod is-closed (obj)
  (not (is-open obj)))

(defmethod (setf is-closed) (value (obj IsCloseable))
  (princ value)
  (setf (is-open obj) (not value)))

(defmacro if-closed (obj then &optional else)
  `(if-open ,obj ,else ,then))

(defmacro if-open (obj then &optional else)
  `(if (is-open ,obj)
       ,then
     ,else))

(defmethod is-empty ((container HasContents))
  (endp (contents container)))

; ----------------------------------------------------------------------
; -------------------- Location

(defmethod put-in (in obj)
  (format t "~&put-in ~A ~A" in obj)
  (put-in (find-obj in) obj))

(defmethod put-in ((in HasContents) obj)
  (put-in in (find-obj obj)))

(defmethod put-in ((in HasContents) (obj HasLocation))
  (setf (location obj) in))

(defmethod find-obj (obj)
  (find-in *dungeon* obj))

(defmethod location (obj)
  (location (find-obj obj)))

(defmethod (setf location) (value obj)
  (setf (location (find-obj obj)) value))

(defmethod (setf location) :around ((dest HasContents) (obj IsExaminable))
  "Set the bi-directional pointers between an object and its container."
  ;(format t "~&put ~A into ~A" obj dest)
  (let* ((src (location obj))
	(old-contents (if src (contents src))))
    (if old-contents                ; remove the link from old parent
	(setf (contents src) (remove obj old-contents))))
  (push obj (contents dest))        ; establish link from new parent
  (call-next-method dest obj))

(defmethod (setf location) :after ((dest IsRoom) (player IsPlayer))
  (look-at player dest))

(defmethod enter ((player IsPlayer) (location IsRoom))
  (setf (location player) location))

;(defmethod enter ((player IsPlayer) (location IsDungeon))
;  (enter player (entrance location)))

(defmethod enter :before ((player IsPlayer) (location IsGame))
  (setf (game player) location))

(defmethod enter ((player IsPlayer) location)
  (look-at player location)
  (enter player (entrance location)))

; ----------------------------------------------------------------------
; -------------------- Debugging

;(defmethod (setf debug-on) :after (value (obj HasLabel))
;  (if value
;      (format t "Debugging is now on.")
;    (format t "Debugging is now off.")))

(defmethod prompt :around ((player IsPlayer))
  (let ((superprompt (call-next-method player)))
    (if (debug-on player)
	(concatenate 'string (state (game player)) "debug " superprompt)
      superprompt)))

(defmacro debug-do (labeled function &optional else)
  `(if (debug-on ,labeled)
       ,function
     ,else))

(defmethod debug-format ((player IsPlayer) &rest rest)
  (debug-do player (apply 'player-format player rest)))

(defun toggle-debug-mode (obj)
  (setf (debug-on obj) (not (debug-on obj))))
