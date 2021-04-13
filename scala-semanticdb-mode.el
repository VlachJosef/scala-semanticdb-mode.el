;;; scala-semanticdb-mode.el --- Major mode for viewing scala.semanticdb files -*- lexical-binding: t; -*-
;;; Copyright (c) 2021 Josef Vlach

(defconst scala-semanticdb-font-lock-keywords
  (eval-when-compile
    `(
      ("Summary:$" . font-lock-keyword-face)
      ("Symbols:$" . font-lock-keyword-face)
      ("Occurrences:$" . font-lock-keyword-face)
      ("Diagnostics:$" . font-lock-keyword-face)
      ("\\[[[:digit:]]*:[[:digit:]]*..[[:digit:]]*:[[:digit:]]*) \\(=>\\) .*$" (1 font-lock-keyword-face)) ;; role: DEFINITION
      ("\\[[[:digit:]]*:[[:digit:]]*..[[:digit:]]*:[[:digit:]]*) \\(<=\\) .*$" (1 font-lock-type-face))    ;; role: REFERENCE
      ))
  "Default expressions to highlight in scala-semanticdb derived modes.")

(defun scala-semanticdb-parent-mode ()
  "Generic mode to derive all other semanticdb buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (add-hook
   'kill-buffer-hook
   (function
    (lambda ()
      ))
   nil t))

(defcustom scala-semanticdb-metap "metap"
  "Program metap executable name."
  :type 'string
  :group 'scala-semanticdb)

(defvar-local semanticdb-source-file nil)

;;from gdb-make-header-line-mouse-map
(defun scala-semanticdb-make-header-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the header line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    (define-key map (vector 'header-line 'down-mouse-1) 'ignore)
    map))

(defmacro scala-semanticdb-propertize-header (name fn help-echo mouse-face face)
  `(propertize ,name
	       'help-echo ,help-echo
	       'mouse-face ',mouse-face
	       'face ',face
	       'local-map
	       (scala-semanticdb-make-header-line-mouse-map
	        'mouse-1
	        (lambda (event) (interactive "e")
                  (when ,fn (funcall ,fn))))))

(defvar scala-semanticdb-compact-header
  (list
   (scala-semanticdb-propertize-header "Compact" nil nil nil mode-line)
   " "
   (scala-semanticdb-propertize-header "Detailed" #'scala-semanticdb-detailed-output "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (scala-semanticdb-propertize-header "Proto" #'scala-semanticdb-proto-output "mouse-1: select" mode-line-highlight mode-line-inactive)))

(defvar scala-semanticdb-detailed-header
  (list
   (scala-semanticdb-propertize-header "Compact" #'scala-semanticdb-compact-output "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (scala-semanticdb-propertize-header "Detailed" nil nil nil mode-line)
   " "
   (scala-semanticdb-propertize-header "Proto" #'scala-semanticdb-proto-output "mouse-1: select" mode-line-highlight mode-line-inactive)))

(defvar scala-semanticdb-proto-header
  (list
   (scala-semanticdb-propertize-header "Compact" #'scala-semanticdb-compact-output "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (scala-semanticdb-propertize-header "Detailed" #'scala-semanticdb-detailed-output "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (scala-semanticdb-propertize-header "Proto" nil nil nil mode-line)))

(defun scala-semanticdb-run-compact-metap ()
  (interactive)
  (scala-semanticdb--compact-run nil (buffer-file-name)))

(defun scala-semanticdb-run-detailed-metap ()
  (interactive)
  (scala-semanticdb--detailed-run nil (buffer-file-name)))

(defun scala-semanticdb-run-proto-metap ()
  (interactive)
  (scala-semanticdb--proto-run nil (buffer-file-name)))

(defun scala-semanticdb-compact-output ()
  (interactive)
  (scala-semanticdb--compact-run))

(defun scala-semanticdb-detailed-output ()
  (interactive)
  (scala-semanticdb--detailed-run))

(defun scala-semanticdb-proto-output ()
  (interactive)
  (scala-semanticdb--proto-run))

(defun scala-semanticdb-refresh ()
  (interactive)
  (message "Refreshing: %s" (buffer-name))
  (cond
   ((derived-mode-p 'scala-semanticdb-compact-mode) (scala-semanticdb--compact-run t))
   ((derived-mode-p 'scala-semanticdb-detailed-mode) (scala-semanticdb--detailed-run t))
   ((derived-mode-p 'scala-semanticdb-proto-mode) (scala-semanticdb--proto-run t))))

(defun scala-semanticdb--compact-run(&optional refresh file)
  (scala-semanticdb-run-metap 'scala-semanticdb-compact-mode "compact" (or file semanticdb-source-file) scala-semanticdb-compact-header refresh))

(defun scala-semanticdb--detailed-run(&optional refresh file)
  (scala-semanticdb-run-metap 'scala-semanticdb-detailed-mode "detailed" (or file semanticdb-source-file) scala-semanticdb-detailed-header refresh))

(defun scala-semanticdb--proto-run(&optional refresh file)
  (scala-semanticdb-run-metap 'scala-semanticdb-proto-mode "proto" (or file semanticdb-source-file) scala-semanticdb-proto-header refresh))

(defun scala-semanticdb-run-metap (minor-mode arg semanticdb-file header-line &optional refresh)
  (unless (executable-find scala-semanticdb-metap)
    (error "Could not find %s on PATH. Please install %s program or customize the scala-semanticdb-metap variable."
           scala-semanticdb-metap scala-semanticdb-metap))

  (let ((metap-buffer (format "*metap %s %s*" arg (file-name-nondirectory semanticdb-file))))

    ;; Switch to existing .metap buffer if one exists
    (if (get-buffer metap-buffer)
        (if (not refresh)
            (switch-to-buffer metap-buffer)
          (kill-buffer metap-buffer)
          (scala-semanticdb-call-process minor-mode metap-buffer arg semanticdb-file header-line))
      (scala-semanticdb-call-process minor-mode metap-buffer arg semanticdb-file header-line))))

(defun scala-semanticdb-call-process (minor-mode metap-buffer arg semanticdb-file header-line)
  (call-process scala-semanticdb-metap nil metap-buffer t (concat "-" arg) semanticdb-file)
  (with-current-buffer metap-buffer
    (goto-char (point-min))
    (funcall minor-mode)
    (setq header-line-format header-line)
    (setq-local semanticdb-source-file semanticdb-file)
    (switch-to-buffer (current-buffer))
    (current-buffer)))

(defvar scala-semanticdb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'scala-semanticdb-run-compact-metap)
    (define-key map (kbd "C-c C-d") 'scala-semanticdb-run-detailed-metap)
    (define-key map (kbd "C-c C-p") 'scala-semanticdb-run-proto-metap)
    map)
  "Local key map used for semanticdb mode")

(defvar scala-semanticdb-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'scala-semanticdb-compact-output)
    (define-key map (kbd "d") 'scala-semanticdb-detailed-output)
    (define-key map (kbd "p") 'scala-semanticdb-proto-output)
    (define-key map (kbd "g") 'scala-semanticdb-refresh)
    map)
  "Local key map used for semanticdb mode")

;;;###autoload
(define-derived-mode scala-semanticdb-mode scala-semanticdb-parent-mode "Semanticdb"
  "Major mode for viewing scala.semanticdb files.

When started, runs `scala-semanticdb-mode-hook'."

  (use-local-map scala-semanticdb-mode-map))

;;;###autoload
(define-derived-mode scala-semanticdb-compact-mode scala-semanticdb-parent-mode "Compact"
  "Major mode for viewing scala.semanticdb files.

When started, runs `scala-semanticdb-compact-mode-hook'."

  (use-local-map scala-semanticdb-map)
  (set (make-local-variable 'font-lock-defaults) '(scala-semanticdb-font-lock-keywords)))

;;;###autoload
(define-derived-mode scala-semanticdb-detailed-mode scala-semanticdb-parent-mode "Detailed"
  "Major mode for viewing scala.semanticdb files.

When started, runs `scala-semanticdb-compact-mode-hook'."

  (use-local-map scala-semanticdb-map)
  (set (make-local-variable 'font-lock-defaults) '(scala-semanticdb-font-lock-keywords)))

;;;###autoload
(define-derived-mode scala-semanticdb-proto-mode scala-semanticdb-parent-mode "Proto"
  "Major mode for viewing scala.semanticdb files.

When started, runs `scala-semanticdb-compact-mode-hook'."

  (use-local-map scala-semanticdb-map)
  (set (make-local-variable 'font-lock-defaults) '(scala-semanticdb-font-lock-keywords)))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.semanticdb\\'" . scala-semanticdb-mode)))

(add-hook 'scala-semanticdb-mode-hook
          (lambda (&rest args)
            (when (string= ".semanticdb" (substring (buffer-file-name) -11 nil))
              (switch-to-buffer (scala-semanticdb-run-compact-metap)))))

(provide 'scala-semanticdb-mode)
;;; scala-semanticdb-mode.el ends here
