(defclass node ()
  ((name        :accessor name         :initarg :name
		:initform nil)
   (arcs        :accessor arcs
	        :initform (make-hash-table))))

; arcs are unidirectional
(defclass arc ()
  ((destination :accessor destination  :initarg :to
		:initform nil)))

; arcs are labeled uniquely for each node 
(defmethod make-arc (&key labeled (from node) (to node))
  (setf (gethash labeled (arcs from)) 
	(make-instance 'arc :to to)))

(defmethod is-adjacent ((a node) (b node))
  (let ((answer nil))
    (maphash #'(lambda (key value)
		 (if (eq b (destination value))
		     (setf answer key)))
	     (arcs a))
    answer))

(defmethod traverse ((from node) direction)
  (let ((anArc (gethash direction (arcs from))))
    (if anArc
	(destination anArc))))

(defmethod print-object ((aNode node) str)
  (format str "<~A>" (name aNode)))

'(defun test ()
  (setf father (make-instance 'node :name  'Paul)
	mother (make-instance 'node :name  'Pat)
	paul   (make-instance 'node :name  'Paul)
	chris  (make-instance 'node :name  'Chris))
  (make-arc :labeled 'child   :from father :to chris)
  (make-arc :labeled 'child   :from father :to paul)
  (make-arc :labeled 'child   :from mother :to chris)
  (make-arc :labeled 'child   :from mother :to paul)
  (make-arc :labeled 'parent  :from chris  :to father)
  (make-arc :labeled 'parent  :from chris  :to mother)
  (make-arc :labeled 'parent  :from paul   :to father)
  (make-arc :labeled 'parent  :from paul   :to mother)
    
  (format t "~A~&" (is-adjacent father paul))
  (format t "~A~&" (is-adjacent mother paul))
  (format t "~A~&" (is-adjacent paul mother))
  (format t "~A~&" (is-adjacent paul father))

  (format t "~A~&" (is-adjacent father chris))
  (format t "~A~&" (is-adjacent mother chris))
  (format t "~A~&" (is-adjacent chris mother))
  (format t "~A~&" (is-adjacent chris father)))
