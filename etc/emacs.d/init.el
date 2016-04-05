;; $Source: e:/MyDocuments/cvsroot/etc/xemacs/init.el,v $
;; $Revision: 1.61 $
;; $Date: 2004/03/10 21:04:44 $
;; $Author: pschaaf $
;; $State: Exp $
;; $Name:  $
;; (byte-compile-file buffer-file-name)

; (defun if-macro (&rest rest)
;  (do ((name (pop rest) (pop rest))
;       (test (pop rest) (pop rest)))
;      ((null rest) t)
;    (defmacro name (then &optional else)
;      "If running under cygwin answer 'then', else answer 'else'."
;      (format name test)
;      (if test
;         `,then
;       `,else))))

; (defun append-loadpath (&rest dirs)
;  (setq load-path (append
;                   (mapcar #'expand-file-name dirs)
;                   load-path)))

; (append-loadpath "~/etc/xemacs/"
;                  "~/etc/xemacs/site-lisp/")

(if (featurep 'xemacs)
    (gnuserv-start)
  (progn
    ; (custom-set-variables '(custom-file (expand-file-name "~/etc/xemacs/custom.el")))
    (setq custom-file (expand-file-name "~/.xemacs/custom.el"))
    (load custom-file t)))
