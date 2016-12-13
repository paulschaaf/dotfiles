;; $Source: e:/MyDocuments/cvsroot/etc/xemacs/custom.el,v $
;; $Revision: 1.61 $
;; $Date: 2004/03/10 21:04:44 $
;; $Author: pschaaf $
;; $State: Exp $
;; $Name:  $


; ==================================================
; === Comments

;; To automatically set variables when a particular file is loaded, put the
;; following as the first line, or the second if it is executable (i.e. the
;; first line begins with "#!/"). The values are used literally: THEY ARE NOT
;; EVALUATED.
;; -*- mode: Lisp; fill-column: 75; comment-column: 50; outline-minor-mode: t -*-
;;     comment-start: "\""; comment-continue: " "; comment-end: "\"" -*-

;; This will prompt the user if enable-local-eval is t.
;; -*- eval: (...) -*-

;; Because the values are used literally the second ("eval"d) form is often better.
;; -*- compile-command: "scp " (file-name-nondirectory buffer-file-name) " gw-admin@ftp:/ftphome/config") -*-
;; -*- eval: (setq compile-command (concat "cp " buffer-file-name " ../../../../../webapps/cc/resources/javascript/heatmap"))
;; -*- after-save-hook: recompile -*- ;; automatically recompile the buffer each time it is saved
;; -*- after-save-hook: (lambda () (copy-file buffer-file-name "../../../../../webapps/cc/resources/javascript/heatmap" t)) -*- -->

;; To use a \n in a search/replace string type C-Q C-J.

;; http://www.gnu.org/software/emacs/windows/ntemacs.html


;; ==================================================
;; === Useful Tricks
;; M-xalign-regexp  aligns selected lines based upon characters matching regexp
;; C-u 0 M-x byte-recompile-directory  recursively compile all .el files
;;      The C-u 0 makes it not ask about every .el file that does not have an .elc

;; To print evaluation in buffer use C-u C-x C-e


; ==================================================
; === Global Constants and Variables

(defconst in-xemacs   (featurep        'xemacs))
(defconst not-xemacs  (not              in-xemacs))
(defconst in-x        (eq 'x            window-system))
(defconst in-windows  (eq 'w32          window-system))
(defconst i-am-root   (=  0            (user-uid)))
(defconst in-cygwin   (or (eq system-type 'cygwin)
                          (eq system-type 'windows-nt)))
(defconst in-linux    (eq system-type  'linux))

; separate the domain-name from the system-name
(defconst system-name (car (split-string (system-name) "\\.")))
(defconst domain-name (cdr (split-string (system-name) (concat system-name "\\."))))

; #'(lambda (x) x) fullname ".")))

; make system-name (i.e. hostname) a symbol whose value is t
 (eval (list 'defvar (read system-name) t))

(cd "~")

;; http://theory.uwinnipeg.ca/gnu/elib/elib_toc.html
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/elib" t)

;; (add-to-list 'load-path "~/.emacs.d" t)t
(add-to-list 'load-path "~/.emacs.d/site-lisp" t)
(add-to-list 'load-path "~/.emacs.d/elpha/clojure-mode-1.7.1" t)
(add-to-list 'load-path "~/.emacs.d/elpha/javascript-2.2.1" t)
(add-to-list 'load-path "~/.emacs.d/elpha/slime-repl-20100404" t)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime-2.0" t)


; ==================================================
; === Key Bindings

; (iswitchb-default-keybindings)

(global-set-key [(control \()] 'paren-backward-sexp)
(global-set-key [(control \))] 'paren-forward-sexp)

(let ((cut       (if in-xemacs
                     'kill-primary-selection
                   'kill-region))
      (copy      (if in-xemacs
                     'copy-primary-selection
                   'kill-ring-save))
      (paste     (if in-xemacs
                     'yank-clipboard-selection
                   'yank))
      (show-menu (if in-x
                     (if in-xemacs
                         'accelerate-menu
                       'tmm-menubar)))
      (start-server (if in-xemacs
                        'gnuserv-start
                      'server-start)))
  ;; Function Keys
  ;; (global-set-key [f1]  (lambda ()
  ;;                         (interactive)
  ;;                         ;;(cond
  ;;                          ;;(static-man-topic  (man static-man-topic))
  ;;                          ((current-word)    (man (current-word)))));;)

  (global-set-key [(meta f6)]             'compile)
  (global-set-key [f6]                     cut)
  (global-set-key [f7]                     copy)
  (global-set-key [f8]                     paste)
  (global-set-key [f9]                    'list-matching-lines) ;; find
  (global-set-key [f10]                   'query-replace-regexp) ;; replace
  (global-set-key [f11]                   'find-file)        ;; new
  (global-set-key [f12]                   'kill-this-buffer) ;; close
  ;; (global-set-key [f12]                   'kill-windows-then-buffers-no-query)  ;; close

  (global-set-key [(meta f8)]             'yank-pop)

  ;; Editing keys
  (global-set-key [(control insert)]       copy)
  (global-set-key [(shift   delete)]       cut)
  (global-set-key [(shift   insert)]       paste)
  (global-set-key [(meta    backspace)]   'undo)
  (global-set-key [(control z)]           'undo)
  (global-set-key [(control backspace)]   'backward-kill-word)
  (global-set-key [(control delete)]      'kill-word)

  (unless (or in-x
              (equal system-name "stone"))
    (global-set-key [(control x) ?h]      'help-command) ;; overrides mark-whole-buffer
    (global-set-key [(control h)]         'delete-backward-char))

  ;; Other keys
  (global-set-key [(control \;)]          'comment-region)
  (global-set-key [(control x) (control o)] 'sort-lines)
; (global-set-key [(control x) (r) (p)]   'comment-rectangle)

  (when not-xemacs
    (global-set-key [(meta g)]            'goto-line)
    (global-set-key [down-mouse-3]        'mouse-popup-menubar-stuff)
    (global-set-key [vertical-scroll-bar   down-mouse-1] 'scroll-bar-drag))

  (if show-menu
      (if in-xemacs
          (global-set-key [(menu)]         show-menu)
        (global-set-key [(menu)]        'tmm-menubar)))


  (global-set-key [(control meta c)]      'indent-buffer)
  (global-set-key [(control f)]           'isearch-forward-regexp)
  (global-set-key [(control s)]           'save-buffer)
  (global-set-key [(meta r)]              'revert-buffer)
  (global-set-key [(meta s)]               start-server)
  (global-set-key [(meta \")]             'insert-quotes)
  (global-set-key [(control z)]           'undo))


; ================================================
; === Establish Major and Minor Modes

;; (pending-delete-mode t)

(if (not in-xemacs)
  (progn
    (recentf-mode)
    ;; highlight-regexp
    (if (functionp 'global-hi-lock-mode)
        (global-hi-lock-mode 1)
      (hi-lock-mode 1))
    (auto-compression-mode)))

;; (display-time)
(fset 'yes-or-no-p 'y-or-n-p)


; ================================================
; === Worth investigation

;;; delim-col.el --- prettify all columns in a region or rectangle
;;; hilit-chg.el --- minor mode displaying buffer changes with special face
;;; icomplete.el --- minibuffer completion incremental feedback
;;; tmm.el       --- text mode access to menu-bar


; ================================================
; === Libraries

(load-library "package")

(when (load-library "linum")
  (setq linum-format "%d ")
  (linum-mode))

(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
; execute (package-list-packages) to install new packages

; (autoload 'mwheel-install "mwheel" "Enable mouse wheel support.")
; (mwheel-install)

; (load-library "Malyon")

;; Enable mouse wheel support
(when (and in-x
           (load "mwheel" t))
  (mwheel-install))

;; enable version control
(if (not in-xemacs)
    (load-library "vc"))

; eval (toggle-emacs-lock) to lock/unlock a buffer
(when not-xemacs
  (load-library "emacs-lock"))

;(when in-cygwin
;  (if in-xemacs
;      (load "msw-init" t)
;    (require 'w32-symlinks)))

;; Edit gpg encrypted files
(when (load "crypt++" t)
  (setq crypt-confirm-password t
        crypt-bind-insert-file nil
        crypt-encryption-type 'gpg
        crypt-encryption-file-extension "\\(\\.gpg\\)$"
        crypt-freeze-vs-fortran nil)

; don't monkey with the encrypted bytes
  (let* ((binary-extensions '("bz" "bz2" "gpg" "gz" "Z" "zip"))
         (no-conversion-fn  (if in-xemacs
                                'no-conversion-unix
                              'no-conversion))
         (preserve-bytes    (lambda (extn)
                              (modify-coding-system-alist 'file
                                                          (concat "\\." extn "\\'")
                                                          no-conversion-fn))))
    (mapc preserve-bytes binary-extensions)))

(load "rotate-buffers" t)

; (crypt-rebuild-tables))
; (require 'crypt++)

'(when (require 'slime "slime" t)
   (setq inferior-lisp-program "/usr/bin/clisp")
   (slime-setup))

; ==================================================
; === Settings

(make-local-variable 'static-man-topic)

; set the global default
(setq comment-start "# ")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-coding-alist (quote (("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\|tar\\|tgz\\|war\\|ear\\|xpi\\)\\'" . no-conversion) ("\\.\\(gz\\|Z\\|bz\\|bz2\\|gpg\\)\\'" . no-conversion))))
 '(auto-compression-mode t nil (jka-compr))
 '(bar-cursor 2)
 '(browse-url-generic-program "firefox")
 '(buffers-menu-sort-function (quote sort-buffers-menu-by-mode-then-alphabetically))
 '(buffers-tab-filter-functions (quote ((lambda (buf1 buf2) t))))
 '(case-fold-search t)
 '(colon-double-space t)
 '(column-number-mode t)
 '(comint-completion-autolist t)
 '(comment-style (quote multi-line))
 '(current-language-environment "English")
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(default-gutter-position (quote top))
 '(default-major-mode (quote text-mode) t)
 '(delete-selection-mode t nil (delsel))
 '(display-time-24hr-format t)
 '(display-time-mode t nil (time))
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(font-use-system-font t)
 '(frame-title-format (list "Emacs: %f") t)
 '(icon-title-format "%b" t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(jde-compile-option-classpath (quote (".")))
 '(jde-compile-option-debug (quote ("all" (t nil nil))))
 '(jde-compiler (quote ("javac server" "/usr/java/current/bin/javacc")))
 '(jde-jdk-registry (quote (("1.4.2_02" . "/usr/java/j2sdk1.4.2_02"))))
 '(jde-sourcepath (quote ("/home/pschaaf")))
 '(jde-vm-path "/usr/java/current/bin/java")
 '(js2-allow-keywords-as-property-names nil)
 '(js2-enter-indents-newline nil)
 '(js2-highlight-level 3)
 '(js2-missing-semi-one-line-override t)
 '(kept-old-versions 2)
 '(kill-whole-line t)
 '(lazy-highlight-initial-delay 0.5)
 '(minibuffer-max-depth nil)
 '(mode-for-help (quote hyper-apropos-help-mode))
 '(nxml-attribute-indent 5)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 3)
 '(nxml-slash-auto-complete-flag t)
 '(ps-print-color-p (quote black-white))
 '(revert-without-query (quote ("^")))
 '(safe-local-variable-values (quote ((comment-start . "#") (comment-start . "! ") (mode . shell-script) (compile-command concat "crontab " buffer-file-name) (compile-command concat "xrdb -override ~/.Xresources " buffer-file-name) (compile-command byte-compile-file buffer-file-name) (compile-command concat "zcompile '" buffer-file-name "'"))))
 '(save-place t nil (saveplace))
 '(scroll-step 9999)
 '(sentence-end-double-space nil)
 '(shell-completion-fignore (quote ("~" "#" "%")))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(sort-fold-case t t)
 '(speedbar-use-images nil)
 '(tab-width 3)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(user-mail-address "paul.schaaf@gmail.com")
 '(visible-bell (quote top-bottom)))

;; '(sentence-end "[.?!][]\"')}]*\\($\\| $\\|	\\| \\)[ 	\n]*")

(if in-xemacs
    (progn
      ;; (set-specifier scrollbar-on-left-p t)
      ;;(balloon-help-mode t)
      (defconst package-get-remote '("ftp.xemacs.org" "pub/xemacs/packages"))
      (custom-set-variables
       '(font-lock-mode t nil (font-lock))
       '(gnuserv-frame (quote gnuserv-main-frame-function))
       '(gnuserv-kill-quietly t)
       '(gnuserv-program (concat exec-directory "gnuserv"))
       '(pending-delete-mode t nil (pending-del))
       '(toolbar-visible-p nil)))
  (progn
    (if in-cygwin (server-start))
    (global-font-lock-mode 1)
    (tool-bar-mode -1)
    (custom-set-variables
     '(delete-selection-mode t nil (delsel)))
    (add-to-list 'load-path
                 (let ((dir "/usr/local/share/emacs/site-lisp"))
                   (if in-windows
                       (concat "C:/cygwin" dir)
                     dir))
                 t)))

;; To edit remote files with Tramp:
;;   (find-file "/pschaaf@localhost:/etc/passwd")
;;   (find-file "/ausable:/ccenvts/CNA/200412178312/config-env/config/elements/lv_activity_ext.xml")
;;   (find-file "/ausable:.zshrc")

;; (if (equal system-name "SED")
;;     (require 'tramp))

;; Work-around for "Can't check signature [...]" bug
(defconst package-get-require-signed-base-updates nil)

(setq efs-generate-anonymous-password user-mail-address)

;(if (equal "pschaaf" (user-login-name))
;    (read-abbrev-file))

; ==================================================
; === Scratch buffer

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (make-scratch-buffer-resilient)

  ;; Since we killed it, don't let caller do that.
  nil)

(defun make-scratch-buffer-resilient ()
  (set-buffer (get-buffer-create "*scratch*"))
  (if initial-scratch-message
      (insert initial-scratch-message))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(save-excursion
  (make-scratch-buffer-resilient))

; ==================================================
; === Functions

; (defun upcase-region-or-word (arg)
;   "Upcase the selected region or the following word (or ARG words)."
;   (interactive "p")
;   (if (region-active-p)
;       (upcase-region (region-beginning) (region-end))
;     (upcase-word arg)))

(defun byte-compile-this-file ()
  (interactive)
  (byte-compile-file buffer-file-name))

(defun d2u ()
  "Convert dos line-endings to unix line-endings."
  (set-buffer-file-coding-system 'raw-text-unix))

(defun u2d ()
  "Convert unix line-endings to dos line-endings."
  (set-buffer-file-coding-system 'raw-text-dos))

(defun fix-dos-outline ()
  (replace-all-regex "^[A-Za-z]"     "\n\\&"  )
  (replace-all-regex "^ "            "\n"     )
  (replace-all-regex "^[0-9][0-9]?/" "\n\\&"  )
  (replace-all-regex "^\*"           "   \*"  )
  (replace-all-regex "^-"            "      -"))

(defun fix-resume ()
  (d2u)
  (fix-dos-outline)
  (replace-all-regex (format " +%c +" 249) "\n")
  (replace-all-regex "^PAUL G. SCHAAF, JR." "Paul G. Schaaf, Jr." t)

; "<h1>Paul G. Schaaf, Jr.</h1>
; 42629 Saratoga Park Street<br>
; Fremont, CA 94538<br><br>
; 408.644.4762 (cell)<br>
; paul.schaaf@gmail.com<br>
; <br>"

  (replace-all-regex "Street, Fremont, CA 94538" "Street\nFremont, CA 94538\n"))

(defun comment-rectangle (start end &optional arg)
  "Comment each line in the region at the start column."
  (interactive "rM")
  (if (or arg (equal comment-end ""))
      (string-insert-rectangle start end (or arg comment-start))
    (comment-region start end arg)))


(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))


;; insert date into buffer at point
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))

(defun insert-quotes (start end &optional arg)
  "Insert or delete two quotation marks around selection, or around point if none."
  (interactive "rp")
  (if arg
      (save-excursion
        ;; delete quotes instead (todo: doesn't work)
        (when (eq (char-before start) ?\")
          (goto-char start)
          (delete-char -1))
        (when (eq (char-after end) ?\")
          (goto-char end)
          (delete-char 1)))
    (progn
      (save-excursion
        (if mark-active (exchange-point-and-mark))
        (insert-char ?\" 1))
      (insert-char ?\" 1))))


;;
;; Code for Guidewire
;; (defun format-exam-results ()
;;   (interactive)

;;   (goto-char (point-min))

;;   ; some questions are formatted differently than others
;;   (replace-all-regex "(wrong) "  "(wrong)\n")

;;   ; remove trailing spaces
;;   (replace-all-regex " +$"  "")

;;   (xml-unescape)

;;   ; encode the double-quote so Excel will keep it in the same cell
;;   (replace-all-regex "\""  "\"\"")

;;   ; ensure each answer is on a separate line
;;   (replace-all-regex " \\([ABCDE]\\. \\)"  "\n\\1")

;;   ; Split question number from category and open quote the question
;;   (replace-all-regex
;;    "\n*Question ID \\([0-9]+\\) : \\(.*\\) (.*\n"
;;    "\n\\2\t\\1\t\"")

;;   ; remove useless lines
;;   (replace-all-regex
;;    "^\\(Administrator Notification\\|Course\\|Exam Date\\|Result\\|[0-9]+ correct out of\\|[-*]+\\|N/A (only available on passing\\|http://\\).*\n+"
;;    "")

;;   ; close quote after last answer, strip out boilerplate
;;   (replace-all-regex "\n+>> Correct.*: " "\"\t")
;;   (replace-all-regex "\n>> \\(Student\\|YOUR ANSWER\\).*: \\(.*\\)\n+"   "\t\\2\t")

;;   (replace-all-regex "^Exam Title: *" "")

;;   ; Add column headers
;;   ;; (replace-all-regex "\nStudent ID: *\\(.*\\)\nStudent email: *\\(.*\\)\nStudent company: *\\(.*\\)"
;;   ;;                    "=hyperlink(\"mailto:\\2\",\"\\1\")\t\t\\3")
;;   ;; (replace-all-regex "\n\n\\(Score:.*\\)" "\t\t\\1\n\nCategory\t#\tQuestion\tRight\tStudent\tSource")

;;   (replace-all-regex " *Student \\(ID\\|name\\|email\\|company\\): "
;;                      "\n\\1\t")

;;   ; insert abbreviations to shorten the text
;;   (replace-all-regex "^BillingCenter" "BC")
;;   (replace-all-regex "^ClaimCenter"   "CC")
;;   (replace-all-regex "^PolicyCenter"  "PC")

;;   (goto-char (point-min)))
;; (global-set-key (kbd "C-c q") 'format-exam-results)
;; End Code for Guidewire
;;


(defun kill-buffers-then-emacs-no-query ()
  "Kills the current buffer. If no buffers remain, exits Emacs."
  (interactive)
  (if (one-buffer-p)(defun fix-dos-outline ()
                      (replace-all-regex "^[A-Za-z]"     "\n\\&"  )
                      (replace-all-regex "^ "            "\n"     )
                      (replace-all-regex "^[0-9][0-9]?/" "\n\\&"  )
                      (replace-all-regex "^\*"           "   \*"  )
                      (replace-all-regex "^-"            "      -"))


    ;; save it and exit
    ;; (save-buffers-kill-emacs)
    t
    ;; kill this buffer, display the next one
    (kill-this-buffer)))

(defun kill-windows-then-buffers-no-query ()
  "Kills the current window unless there is only one, in which case it kills the current buffer. If no buffers remain, exits Emacs."
  (interactive)
  (if (one-window-p)
      (kill-buffers-then-emacs-no-query)
    ;; close the selected window, buffer (and other windows) unaffected
    (delete-window)))

(defun man (title)
  (manual-entry title))

(if not-xemacs
    (setq server-start #'gnuserv-start)
  (defun package-provide (&rest args)))

(defun one-buffer-p ()
  "Answers whether there is only one buffer."
  (not (yic-next (buffer-list))))

(defun map-all-regex (regex map)
; (point-to-register)
  (goto-char (point-min))
  (while (re-search-forward regex nil t)
    map))

(defun replace-all-regex (from to &optional fixedcase literal)
  "Replaces all matches of regex 'from' with string 'to'."
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to fixedcase literal))
; the following code appears unecessary
;(map-all-regex from #'(lambda ()
;                        (replace-match to nil nil)))
  )

(defun url-unescape ()
  (interactive)
  (replace-all-regex "%3A" ":"  )
  (replace-all-regex "%2F" "/"   ))

(defun xml-escape ()
  (interactive)
  ;; (replace-all-regex "\""               "&quot;" ))
  ;; (replace-all-regex "'"                "&apos;" )
  (replace-all-regex "&"                "&amp;"  )
  (replace-all-regex ">"                "&gt;"   )
  (replace-all-regex "<"                "&lt;"   ))
;; (replace-all-regex "&gt;\n[ \t]*&lt;" "&gt;<br/>&lt;"))

(defun xml-unescape ()
  (interactive)
  (replace-all-regex "&amp;"  "&"  )
  (replace-all-regex "&apos;" "'"  )
  (replace-all-regex "&gt;"   ">"  )
  (replace-all-regex "&lt;"   "<"  )
  (replace-all-regex "&quot;" "\"" )
  (replace-all-regex "&#39;"   "'" ))




; ==================================================
; === Define Modes


; ==================
; === Multi Mode

(autoload 'multi-mode
  "multi-mode"
  "Allowing multiple major modes in a buffer."
  t)


; ==================
; === Clojure Mode

; (require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj&" . clojure-mode))


; ==================
; === Emacs Lisp Mode

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (eldoc-mode t)
             (outline-minor-mode t)
             (setq comment-start  "; "
                   outline-regexp ";\\|(\\|[ \t]+.")
             (setq compile-command (concat "make -k -f " buffer-file-name))
             (local-set-key [f1] '(lambda ()
                                    (interactive)
                                    (hyper-apropos (current-word) nil)))
             (local-set-key [(meta f6)] 'byte-compile-this-file))
          nil
          t)


; ==================
; === Folding Mode

(add-hook 'folding-mode-hook
          '(lambda ()
             (define-key folding-mode-map [(meta divide)]   'folding-whole-buffer)
             (define-key folding-mode-map [(meta multiply)] 'folding-show-all)
             (define-key folding-mode-map [(meta subtract)] 'folding-hide-current-subtree)
             (define-key folding-mode-map [(meta add)]      'folding-show-current-subtree)))


; ==================
; === HTML Mode

(add-to-list 'auto-mode-alist '("\\.vm\\|.rhtml\\'" . html-mode))


; ==================
; === Generic Modes

;(defun define-generic-mode (&rest args))

(if not-xemacs
    (when (require 'generic-x nil t)
      (add-hook 'javascript-generic-mode
                '(lambda ()
                   (set comment-start  "//"
                        outline-regexp "")
                   )
                nil
                t)
      (setq generic-extras-enable-list
            (append generic-extras-enable-list
                    '(
                      alias-generic-mode
                      apache-conf-generic-mode
                      apache-log-generic-mode
                      bat-generic-mode
                      etc-fstab-generic-mode
                      etc-passwd-generic-mode
                      etc-services-generic-mode
                      fvwm-generic-mode
                      hosts-generic-mode
                      inetd-conf-generic-mode
                      io-mode
                      inf-generic-mode
                      ini-generic-mode
                      java-manifest-generic-mode
                      java-properties-generic-mode
                      mailagent-rules-generic-mode
                      mailrc-generic-mode
                      named-boot-generic-mode
                      named-database-generic-mode
                      rc-generic-mode
                      reg-generic-mode
                      resolve-conf-generic-mode
                      samba-generic-mode
                      show-tabs-generic-mode
                      x-resource-generic-mode
                      )))
      (load "generic-x" t)))


;; (define-generic-mode 'inf-generic-mode
;;    (list ?\;)                                          ; comment-list
;;    nil                                                 ; keyword-list
;;    '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)) ; font-lock-list
;;    (list "\\.[iI][nN][fF]\\'")                         ; auto-mode-list
;;    (list 'generic-bracket-support)                     ; function-list
;;    "Generic mode for MS-Windows INF files."))          ; description (opt)


; ==================
; === Io Mode

(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))
(autoload 'io-mode "io-mode" nil t)


; ==================
; === Java Mode


; ==================
; === JavaScript Mode

(when not-xemacs
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(js\\)$" . js2-mode)))

(add-to-list 'auto-mode-alist '("\\.\\(gs\\|gsx\\|gst\\)$" . javascript-mode))

; ==================
; === JKA Compression Mode

(add-to-list 'auto-mode-alist '("\\.\\(ear\\|war\\|xpi\\)'" . archive-mode))

;; (auto-mode-alist(("\\.\\(ba\\|tc\\|[ackz]\\)sh\\'" . sh-mode)
;;                  ("\\.rb\\'" . ruby-mode)
;;                  ("\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\'"  nil jka-compr)
;;                  ("\\.bz2\\'" nil jka-compr)
;;                  ("\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'" nil jka-compr)
;;                  ("\\.te?xt\\'" . text-mode)
;;                  ("\\.c\\'" . c-mode)
;;                  ("\\.h\\'" . c-mode)
;;                  ("\\.tex\\'" . tex-mode)
;;                  ("\\.ltx\\'" . latex-mode)
;;                  ("\\.el\\'" . emacs-lisp-mode)
;;                  ("\\.scm\\'" . scheme-mode) ...)


; ==================
; === JSP Mode

(defun jsp-mode ()
  (interactive)
  (multi-mode 1 'html-mode
              '("<%--" indented-text-mode)
              '("<%@" indented-text-mode)
              '("<%=" html-mode)
              '("<%" java-mode)
              '("%>" html-mode)))

(add-to-list 'auto-mode-alist '("\\.jsp$" . jsp-mode))


; ==================
; === Makefile Mode

; code taken from xemacs 'compile-command' doc
;(add-hook 'c-mode-common-hook
;          (lambda ()
;             (or (file-exists-p "makefile") (file-exists-p "Makefile")
;                 (set (make-local-variable 'compile-command)
;                      (concat "make -k " buffer-file-name))))

(add-hook 'makefile-mode-hook
          '(lambda ()
             (set compile-command (concat "make -k -f " buffer-file-name)))
          nil
          t)

(add-to-list 'auto-mode-alist '("\\.make$" . makefile-mode))


; ==================
; === Malyon Mode

;(autoload 'malyon-mode "z5" nil t)
;(add-to-list 'auto-mode-alist '("\\.z5$" . malyon-mode))


; ==================
; === Outline Mode

; The FEWER chars this matches, the HIGHER its priority.
; (Q: what about empty lines, or no matches?)
; outline-regexp                                  "[*]+" is default

; outline-heading-end-regexp                      "[\n\r]" is default
; (string-to-list outline-heading-end-regexp)     (?\[ ?\n ?\r ?\]) is default

(when not-xemacs
  (defun pgs-set-outline-map (map)
    (define-key map [(meta divide)]   'hide-sublevels)
    (define-key map [(meta multiply)] 'show-all)
    (define-key map [(meta subtract)] 'hide-subtree)
    (define-key map [(meta add)]      'show-subtree)
    (set 'outline-regexp
         (concat "[*]+\\|[ \t]+...\\|"
                 (or comment-start
                     "#")
                 " +[/*=-]")))

  (add-hook 'outline-mode-hook
            '(lambda ()
               (pgs-set-outline-map outline-mode-map)
               (pgs-set-outline-map outline-minor-mode-map))
            nil
            t)

  (add-hook 'outline-minor-mode-hook
            '(lambda ()
               (pgs-set-outline-map  outline-minor-mode-map))
            nil
            t))


; ==================
; === .properties Mode

(define-generic-mode 'properties-generic-mode
  (list ?#)
  nil
  '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)
    ("^\\([^=\n\r]*\\)=\\([^\n\r]*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face)))
  (list "\\.[Pp]roperties\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
            '((nil "^\\[\\(.*\\)\\]" 1)
              ("*Variables*" "^\\s-*\\([^=]+\\)\\s-*=" 1)))
      )))
  "Generic mode for .properties files.")


; ==================
; === Ruby Mode

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/ruby-mode" t)

(when not-xemacs
  (autoload 'ruby-mode     "ruby-mode" "Mode for editing ruby source files" t)
  (autoload 'run-ruby      "inf-ruby"  "Run an inferior Ruby process" t)
  (autoload 'inf-ruby-keys "inf-ruby"  "Set local key defs for inf-ruby in ruby-mode")
  (add-to-list 'auto-mode-alist '("\\.rb\\|irbrc\\'" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (custom-set-faces
              '(default ((t (:inherit default :family "helv")))))
             (inf-ruby-keys)
             (outline-minor-mode t)

             ;; todo: is outline-regexp really a list of strings?
             (set outline-regexp
                  "\\|req\\|cla\\|def\\>\\|mod\\|=beg\\|=end\\|\\w\\w\\w\\w\\[ \t]+\\(end\\>\\|#  *\\|\\w\\w\\)")
; (define-key ruby-mode-map [f1]
;   '(lambda ()
;      (interactive)
;      (shell-command (format "bin/ri %s" (current-word)))))
             )
          nil
          t)


; ==================
; === SGML Mode

(add-hook 'sgml-mode-hook
          '(lambda ()
             (setq sgml-indent-step 3
                   sgml-indent-data t)))


; ==================
; === sh Mode

(add-hook 'sh-mode-hook
          '(lambda ()
             (outline-minor-mode t)
             (setq outline-regexp
                   (concat "\\|[ \t]+\\w\\w\\w\\|\\w\\w\\w" outline-regexp)))
          nil
          t)

(if not-xemacs
    (add-to-list 'auto-mode-alist '("\\.\\(ba\\|tc\\|[ackz]\\)sh\\'" . sh-mode)))


; ==================
; === ssh Mode

(if not-xemacs
    (define-generic-mode 'ssh-config-generic-mode
      (list ?#)
      (list "Host")
      '(("^Host[ \t]+\\(.*\\)"
         (1 'header-line))
        ("^[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)"
         (1 'font-lock-variable-name-face)
         (2 'default-face)))
      (list "config\\'")
      nil
      "Generic mode for ssh config file."))


(define-generic-mode 'ssh-authorized-keys-generic-mode
  (list ?#)
  (list "ssh-dss" "ssh-rsa")
  '(("\\([^ \t]+@[^ \t]+\\)"
     (1 'font-lock-variable-name-face)))
  (list "authorized_keys\\'")
  nil
  "Generic mode for ssh authorized_keys file.")


; ==================
; === Tabbar Mode

(when (and not-xemacs
           (load "tabbar" t)
           (require 'tabbar nil t))
  (tabbar-mode t)
  (global-set-key [(control c) (T)]     'tabbar-backward)
  (global-set-key [(control shift tab)] 'tabbar-backward)
  (global-set-key [(control c) t]       'tabbar-forward)
  (global-set-key [(control tab)]       'tabbar-forward)

  (defun pgs-tabbar-buffer-list ()
    "Return the list of buffers to show in tabs."
    (delq t
          (mapcar #'(lambda (b)
                      (let ((name (buffer-name b)))
                        (cond ; Only display buffers whose name matches the default
                         ((equal name "*Messages*"))
                         ((equal name "*Compile-Log*"))
                         ((equal name "*compilation*"))
                         ((equal name ".recentf"))
                         (b))))
                  (tabbar-buffer-list))))
  (setq tabbar-buffer-list-function 'pgs-tabbar-buffer-list))


; ==================
; === Text Mode

(add-hook 'text-mode-hook
          '(lambda ()
             (outline-minor-mode t))
          nil
          t)


; ==================
; === VisualBasic Mode

;; (define-generic-mode 'vb-generic-mode
;;    (list ?')
;;    (list 'Sub 'Call 'GoTo 'End 'Error 'On)
;;    '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)
;;      ("^\\([^=\n\r]*\\)=\\([^\n\r]*\\)$"
;;       (1 font-lock-function-name-face)
;;       (2 font-lock-variable-name-face)))
;;    (list "\\.[Vv][Bb][Aa]?\\'")
;;    (list
;;     (function
;;      (lambda ()
;;        (setq imenu-generic-expression
;; 	     '((nil "^\\[\\(.*\\)\\]" 1)
;; 	       ("*Variables*" "^\\s-*\\([^=]+\\)\\s-*=" 1)))
;;        )))
;;     "Generic mode for Visual Basic files.")

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist
      (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . visual-basic-mode))
              auto-mode-alist))


; ==================
; === X Resource Mode

(add-hook 'x-resource-generic-mode-hook
          '(lambda ()
             (set comment-start '! '
                  compile-command
                  (concat "xrdb -override '"
                          buffer-file-name "'")))
          nil
          t)


; ==================
; === XML Mode

(if in-xemacs
    (add-hook 'xrdb-mode-hook
              (lambda ()
                (local-set-key [(meta f6)] 'xrdb-database-merge-buffer-or-region)))
  (when (and (add-to-list 'load-path "/usr/share/emacs/site-lisp/nxml-mode" t)
             (add-to-list 'load-path "c:/cygwin/usr/share/emacs/site-lisp/nxml-mode-20041004" t)
             (load "rng-auto" t))
    (add-hook 'nxml-mode-hook
              '(lambda ()
                 (set 'comment-continue "....")
                 (define-key nxml-mode-map [(control meta left)]  'nxml-backward-element)
                 (define-key nxml-mode-map [(control meta right)] 'nxml-forward-element)))))

(add-to-list 'auto-mode-alist
             (cons "\\.\\(dti\\|eix\\|eti\\|etx\\|pcf\\|rng\\|xs[dl]\\|xml\\|xhtml\\|xul\\)\\'"
                   (if in-xemacs
                       'xml-mode
                     'nxml-mode)))


; ==================
; === Keyboard Macro

;; (fset 'formatQuestion
;;    [C-home down return
;;     M-> ?\C-u ?9 return
;;     ;; ?\M-g ?1 ?2 return
;;     ;; C-S-end backspace
;;     ;; C-S-home
;;     ])

;; (defun formatXMLQuestion ()
;;   (interactive)
;;   (xml-escape)      ;; e.g. (replace-all-regex "&" "&amp;")

;;   (goto-char (point-max))
;;   (insert "\n\n\n\n\n\n\n\n\n\n")
;;   (apply-macro-to-region-lines (point-min) (point-max) 'formatQuestion))

;; (global-set-key (kbd "C-c q") 'formatXMLQuestion)


; ==================================================
; === Fonts, Faces and Frames

; To see the font names in Windows, put this in the scratch buffer and press C-j
; (w32-select-font nil t)

(defun hash-table-to-list (table &optional keep-empty)
  (let ((lst))
    (maphash (lambda (key value)
               (when (or value keep-empty)
                 (push value lst)
                 (push key lst)))
             table)
    lst))

;   todo: frame-height and frame-width should care about whether $DISPLAY is local

;; *****check this

;; (if (or in-x in-windows)
;;     (let ((properties  (make-hash-table))
;;           (font-family (cond
;;                         (in-cygwin  "Lucida Console")
;;                         (in-windows "outline-lucida console")
;;                         (t          "fixed")))
;;           (height 100)
;;           (font-size   (if in-xemacs
;;                            "10pt"
;;                          100))
;;           ;; (frame-w     (cond
;;           ;;               (in-cygwin 92)
;;           ;;               (in-windows 94)))
;;           (frame-h     (cond
;;                         (i-am-root 40)
;;                         (in-cygwin 102)
;;                         (in-windows 88))))
;;       (if (and font-family font-size)
;;           (custom-set-faces `(default ((t (:size ,font-size :family ,font-family :height ,height))) t)))

;;       ;; (if frame-w
;;       ;;     (set-frame-width (selected-frame) frame-w))
;;       (if frame-h
;;           (set-frame-height (selected-frame) frame-h))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 71 :width normal))))
 '(cursor ((t (:background "white" :inverse-video nil))))
 '(italic ((t (:slant italic :family "b&h luxi mono"))))
 '(mode-line-buffer-id ((t (:foreground "steelblue1"))))
 '(mwe:nesting-face-0 ((((class color)) (:background "#90b0f0"))) t)
 '(mwe:nesting-face-1 ((((class color)) (:background "#b090f0"))) t)
 '(mwe:nesting-face-2 ((((class color)) (:background "#f0b090"))) t)
 '(mwe:nesting-face-3 ((((class color)) (:background "#90b0f0"))) t)
 '(mwe:nesting-face-4 ((((class color)) (:background "#90f0b0"))) t)
 '(mwe:nesting-face-5 ((((class color)) (:background "#b0f090"))) t)
 '(mwe:nesting-face-6 ((((class color)) (:background "#b090f0"))) t)
 '(mwe:nesting-face-7 ((((class color)) (:background "#90b0f0"))) t)
 '(mwe:nesting-face-8 ((((class color)) (:background "#b0f090"))) t)
 '(nxml-attribute-local-name-face ((t (:foreground "deep sky blue"))) t)
 '(nxml-comment-content-face ((t (:inherit nxml-comment-delimiter-face))) t)
 '(nxml-comment-delimiter-face ((t (:foreground "chocolate1"))) t)
 '(nxml-element-colon-face ((t (:inherit nxml-element-prefix-face))) t)
 '(nxml-element-prefix-face ((t (:inherit nxml-name-face :foreground "cyan"))) t)
 '(nxml-markup-declaration-delimiter-face ((t (:foreground "magenta"))) t)
 '(nxml-namespace-attribute-colon-face ((t (:inherit nxml-namespace-attribute-prefix-face))) t)
 '(nxml-namespace-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "cyan"))) t)
 '(nxml-namespace-attribute-value-delimiter-face ((t (:inherit nxml-namespace-attribute-prefix-face))) t)
 '(nxml-namespace-attribute-value-face ((t (:inherit nxml-namespace-attribute-prefix-face :weight bold :height 1.2))) t)
 '(nxml-namespace-attribute-xmlns-face ((t (:inherit nxml-namespace-attribute-prefix-face))) t)
 '(nxml-processing-instruction-content-face ((t (:inherit nxml-processing-instruction-delimiter-face))) t)
 '(nxml-processing-instruction-delimiter-face ((t (:foreground "lightgoldenrod" :height 1.2))) t)
 '(nxml-processing-instruction-target-face ((t (:inherit nxml-processing-instruction-delimiter-face :foreground "lightgoldenrod"))) t)
 '(nxml-prolog-keyword-face ((t (:inherit nxml-name-face :foreground "magenta"))) t)
 '(tabbar-default-face ((t (:inherit variable-pitch :background "gray72" :foreground "black")))))

; modeline has red background for root user
(if (eq (user-uid) 0)
    (let ((modeln '(mode-line ((t (:background "red2" :foreground "white" :box (:line-width -1 :style released-button))))))
          (menu '(menu ((((type x-toolkit)) (:background "red2"))))))
      (custom-set-faces menu modeln)))


; ==================
; === Recently Opened Files

; Re-open most recently visited file
(recentf-mode t)
(if (file-readable-p recentf-save-file)
    (if (> (length recentf-list) 0)
        (mapc 'find-file (recentf-elements 10))))
