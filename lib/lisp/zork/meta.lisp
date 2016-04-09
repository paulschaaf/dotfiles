(defmacro defreduction (name param-list reduce-name)
  (let ((each (gensym)))
    (let ((reduction-funcall (mapcar (lambda (x)
				       (if (atom x)
					   x
					 (if (eql (first x) `,reduce-name)
					     `,each
					   (first x))))
				     `,param-list)))
      (push `,name reduction-funcall)
      ;(prin1 reduction-funcall)
      `(defmethod ,name ,param-list
	 (dolist (,each ,reduce-name nil)
	   ,reduction-funcall)))))


(set-macro-character #\] (get-macro-character #\)))

'(set-dispatch-macro-character #\# #\[
  #'(lambda (stream char1 char2)
      (let* ((name-list    (read-delimited-list #\] stream t))
	     (name-str-spc (format nil "~{~A ~}" name-list))
	     (name-str     (string-right-trim " " name-str-spc)))
	(multiple-value-list (intern name-str)))))

(set-dispatch-macro-character #\# #\[
  #'(lambda (stream char1 char2)
      (intern 
       (string-right-trim
	" " 
	(format 
	 nil
	 "~{~A ~}" 
	 (read-delimited-list #\] stream t))))))

#|(defmacro defreduction (name param-list reduce-name)
  (let ((each (gensym))
	(reduction-funcall (gensym)))
    `(let ((,reduction-funcall (first-ply ',param-list)))
      (push ,name ,reduction-funcall)
      (format t "reduction-funcall: ~A" ,reduction-funcall)
      (defmethod ,name ,param-list
		(dolist (,each ,reduce-name nil)
		  ,reduction-funcall)))))
|#

(defmacro first-ply (args)
  `(mapcar #'(lambda (x)
	       (if (atom x)
		   x
		 (first x)))
	   ,args))

(defmacro def-method-alias (old &rest new)
  (dolist (each new nil)
    (setf (symbol-function each) (symbol-function old))))

(def-method-alias exit x)

;(defun def-method-alias (old newlist &rest rest)
;  (dolist (each newlist nil)
;    (setf (symbol-function each) (symbol-function old)))
;  (if rest
;      (def-method-alias (car rest) (cadr rest) (cddr rest))))

;(def-method-alias
;  'exit '(x xit)
;  'princ '(foo-princ bar-princ))

;;;;
;;;; Macros from ANSI Common Lisp
;;;;

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

#|
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))
|#

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
