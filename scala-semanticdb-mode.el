;;; scala-semanticdb-mode.el --- Major mode for viewing scala.semanticdb files

(defcustom scala-semanticdb-metap "metap"
  "Program metap executable name."
  :type 'string
  :group 'scala-semanticdb)

(defun scala-semanticdb-run-compact-metap ()
  (interactive)
  (scala-semanticdb-run-metap 'scala-semanticdb-compact-mode "compact" (buffer-file-name)))

(defun scala-semanticdb-run-detailed-metap ()
  (interactive)
  (scala-semanticdb-run-metap 'scala-semanticdb-detailed-mode "detailed" (buffer-file-name)))

(defun scala-semanticdb-run-proto-metap ()
  (interactive)
  (scala-semanticdb-run-metap 'scala-semanticdb-proto-mode "proto" (buffer-file-name)))

(defun scala-semanticdb-run-metap (minor-mode arg semanticdb-file)
  (unless (executable-find scala-semanticdb-metap)
    (error "Could not find %s on PATH. Please install %s program or customize the scala-semanticdb-metap variable."
           scala-semanticdb-metap scala-semanticdb-metap))

  (let* ((metap-file-name (format "%s.%s.metap" semanticdb-file arg))
         (metap-buffer (file-name-nondirectory metap-file-name)))

    ;; Switch to existing .metap buffer if one exist
    (if (get-buffer metap-buffer)
        (switch-to-buffer metap-buffer)
      (call-process scala-semanticdb-metap nil metap-buffer t (concat "-" arg) semanticdb-file)
      (with-current-buffer metap-buffer
        (setq buffer-read-only t)
        (setq-local semanticdb-source-file semanticdb-file)
        (funcall minor-mode)
        (switch-to-buffer (current-buffer))))))

(defvar scala-semanticdb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'scala-semanticdb-run-compact-metap)
    (define-key map (kbd "C-c C-d") 'scala-semanticdb-run-detailed-metap)
    (define-key map (kbd "C-c C-p") 'scala-semanticdb-run-proto-metap)
    map)
  "Local key map used for semanticdb mode")

;;;###autoload
(define-derived-mode scala-semanticdb-mode special-mode "Semanticdb"
  "Major mode for viewing scala.semanticdb files.

When started, runs `scala-semanticdb-mode-hook'.

\\{semanticdb-mode-map}"

  (use-local-map scala-semanticdb-mode-map))

(defun scala-semanticdb-compact-output ()
  (interactive)
  (scala-semanticdb-run-metap 'scala-semanticdb-compact-mode "compact" semanticdb-source-file))

(defun scala-semanticdb-detailed-output ()
  (interactive)
  (scala-semanticdb-run-metap 'scala-semanticdb-detailed-mode "detailed" semanticdb-source-file))

(defun scala-semanticdb-proto-output ()
  (interactive)
  (scala-semanticdb-run-metap 'scala-semanticdb-proto-mode "proto" semanticdb-source-file))

(defvar scala-semanticdb-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'scala-semanticdb-compact-output)
    (define-key map (kbd "d") 'scala-semanticdb-detailed-output)
    (define-key map (kbd "p") 'scala-semanticdb-proto-output)
    map)
  "Local key map used for semanticdb mode")

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
