;;; scala-semanticdb-mode.el --- Major mode for viewing scala.semanticdb files
;;; Copyright (c) 2021 Josef Vlach

(defcustom scala-semanticdb-metap "metap"
  "Program metap executable name."
  :type 'string
  :group 'scala-semanticdb)

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
  (cond
   ((bound-and-true-p scala-semanticdb-compact-mode) (scala-semanticdb--compact-run t))
   ((bound-and-true-p scala-semanticdb-detailed-mode) (scala-semanticdb--detailed-run t))
   ((bound-and-true-p scala-semanticdb-proto-mode) (scala-semanticdb--proto-run t))))

(defun scala-semanticdb--compact-run(&optional refresh file)
  (scala-semanticdb-run-metap 'scala-semanticdb-compact-mode "compact" (or file semanticdb-source-file) refresh))

(defun scala-semanticdb--detailed-run(&optional refresh file)
  (scala-semanticdb-run-metap 'scala-semanticdb-detailed-mode "detailed" (or file semanticdb-source-file) refresh))

(defun scala-semanticdb--proto-run(&optional refresh file)
  (scala-semanticdb-run-metap 'scala-semanticdb-proto-mode "proto" (or file semanticdb-source-file) refresh))

(defun scala-semanticdb-run-metap (minor-mode arg semanticdb-file &optional refresh)
  (unless (executable-find scala-semanticdb-metap)
    (error "Could not find %s on PATH. Please install %s program or customize the scala-semanticdb-metap variable."
           scala-semanticdb-metap scala-semanticdb-metap))

  (let* ((metap-file-name (format "%s.%s.metap" semanticdb-file arg))
         (metap-buffer (file-name-nondirectory metap-file-name)))

    ;; Switch to existing .metap buffer if one exists
    (if (get-buffer metap-buffer)
        (if (not refresh)
            (switch-to-buffer metap-buffer)
          (kill-buffer metap-buffer)
          (scala-semanticdb-call-process metap-buffer arg semanticdb-file))
      (scala-semanticdb-call-process metap-buffer arg semanticdb-file))))

(defun scala-semanticdb-call-process (metap-buffer arg semanticdb-file)
  (call-process scala-semanticdb-metap nil metap-buffer t (concat "-" arg) semanticdb-file)
  (with-current-buffer metap-buffer
    (setq buffer-read-only t)
    (setq-local semanticdb-source-file semanticdb-file)
    (funcall minor-mode)
    (switch-to-buffer (current-buffer))))

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
(define-derived-mode scala-semanticdb-mode special-mode "Semanticdb"
  "Major mode for viewing scala.semanticdb files.

When started, runs `scala-semanticdb-mode-hook'.

\\{semanticdb-mode-map}"

  (use-local-map scala-semanticdb-mode-map))

(define-minor-mode scala-semanticdb-compact-mode
  "Show detailed output from metap."
  :lighter " compact"
  :keymap scala-semanticdb-map)

(define-minor-mode scala-semanticdb-detailed-mode
  "Show detailed output from metap."
  :lighter " detailed"
  :keymap scala-semanticdb-map)

(define-minor-mode scala-semanticdb-proto-mode
  "Show detailed output from metap."
  :lighter " proto"
  :keymap scala-semanticdb-map)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.semanticdb\\'" . scala-semanticdb-mode)))

(provide 'scala-semanticdb-mode)
;;; scala-semanticdb-mode.el ends here
