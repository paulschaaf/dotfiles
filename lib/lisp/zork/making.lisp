; ----------------------------------------------------------------------
; -------------------- Making

(defmacro make-obj-of-class-in (class name location &rest rest)
  (let ((obj (gensym)))
    `(let ((,obj (make-instance ,class ,@rest)))
       (if ,name
	   (setf (name ,obj) ,name))
       (if ,location 
	   (setf (location ,obj) ,location))
       ,obj)))

(defmacro make-object-in (location name description &rest rest)
  `(make-obj-of-class-in 'IsPiece
			 ,name
			 ,location
			 :description ,description
			 ,@rest))

(defmacro make-object (name description &rest rest)
  `(make-obj-of-class-in 'IsPiece
			 ,name
			 *dungeon*
			 :description ,description
			 ,@rest))

(defmacro make-player (name &rest rest)
  `(make-obj-of-class-in 'IsPlayer
			 ,name
			 nil
			 ,@rest))

(defmethod make-bag-in (location name description &optional contents)
  (make-obj-of-class-in 'IsBag
			name
			location
			:description description))

(defmethod make-container (name description)
  (make-obj-of-class-in 'IsContainer
			name
			*dungeon*
			:description description))

(defmethod make-room (name description)
  (make-obj-of-class-in 'IsRoom
			name
			*dungeon*
			:description description))

(defmethod make-dungeon (name description)
  (make-obj-of-class-in 'IsDungeon
			name
			*game*
			:description description))

; ----------------------------------------------------------------------
; -------------------- Directions

(defmethod make-path (&key dungeon type from going to desc)
  (let ((fromR (room-in (or dungeon *dungeon*) from))
	(toR (room-in (or dungeon *dungeon*) to)))
    (make-one-path 
     :type  type
     :from  fromR
     :going going
     :to    toR
     :desc  desc)))

(defmethod make-paths-from (from paths)
  (make-paths-from (room-in *dungeon* from) paths))

(defmethod make-paths-from ((from IsRoom) paths)
  (dolist (path paths nil)
    (dolist (going (car path) nil)
      (make-one-path
       :type  type
       :from  from
       :going going
       :to    (second path)
       :desc  (or (third path) "")))))

(defmethod make-one-path (&key type from going to desc)
  (let ((exits (arcs from))
	(new-arc (make-instance 'ExitPath
				:type type
				:to to
				:description desc)))
    (dolist (heading going nil)
      (setf (gethash heading exits) new-arc))))
