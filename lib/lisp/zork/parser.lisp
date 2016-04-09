; ----------------------------------------------------------------------
; -------------------- Tokenization and parsing

(defmethod eval-input ((player IsPlayer) input)
  (setf all 'all
	it (it player)
	self player)
  ;(ignore-errors 
  (eval input))

(defmethod next-command ((player IsPlayer))
  (let ((answer (compile-commands player (prompt-player player))))
    (debug-format player "~&~A~2&" answer)
    (cons (car answer)
	  (cons player (cdr answer)))))

(defmacro direct-object (list)
  `(car (last ,list)))   ;naive, but works for now

(defmethod compile-commands ((player IsPlayer) (input sequence))
  (compile-verbs player input)
  (if (second input)
      (let* ((real-obj (find-for player (direct-object input))))
	(setf (it player) real-obj)
	(if real-obj
	    (progn
	      (debug-format player "~&compiled in object ref to: ~A" real-obj)
	      (setf (direct-object input) real-obj)
	      input)
	  (if (eql (direct-object input) 'all)
	      input  ;(setf (direct-object input) nil))))
	    `(nosuch (direct-object ',input)))))
    input))

;`(or (it ,player)
;    (nosuch nil))

(defmethod compile-verbs ((player IsPlayer) (input sequence))
  (setf (first input) 
	(case (first input)
	  ((open)    'open-container)
	  ((close)   'close-container)
	  (t         (first input)))))

(defmethod prompt-player ((player IsPlayer) &optional (aPrompt (prompt player)))
  (player-format player "~2&~A " aPrompt)
  (readlist))

(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))

