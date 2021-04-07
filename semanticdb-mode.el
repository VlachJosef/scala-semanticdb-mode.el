;;; semanticdb-mode.el --- Major mode for viewing scala.semanticdb files

(defcustom semanticdb-metap-program-name "metap"
  "Program metap executable name."
  :type 'string
  :group 'semanticdb)

(defvar-local semanticdb-source-file)

(defun semanticdb-run-metap ()
  (interactive)

  (unless (executable-find semanticdb-metap-program-name)
    (error "Could not find %s on PATH. Please install %s program or customize the semanticdb-metap-program-name variable."
           semanticdb-metap-program-name semanticdb-metap-program-name))

  (let* ((semanticdb-file (or semanticdb-source-file (buffer-file-name)))
         (metap-file-name (format "%s.metap" semanticdb-file))
         (metap-buffer (file-name-nondirectory metap-file-name)))

    ;; Kill existing .metap buffer if one exist
    (when (get-buffer metap-buffer)
      (kill-buffer metap-buffer))

    (call-process semanticdb-metap-program-name nil metap-buffer t semanticdb-file)
    (with-current-buffer metap-buffer
      (setq buffer-read-only t
            semanticdb-source-file semanticdb-file)
      (semanticdb-mode)
      (switch-to-buffer (current-buffer)))))

(defvar semanticdb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'semanticdb-run-metap)
    map)
  "Local key map used for semanticdb mode")

;;;###autoload
(define-derived-mode semanticdb-mode special-mode "Semanticdb"
  "Major mode for viewing scala.semanticdb files.

When started, runs `semanticdb-mode-hook'.

\\{semanticdb-mode-map}"

  (use-local-map semanticdb-mode-map))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.semanticdb\\'" . semanticdb-mode)))

(provide 'semanticdb-mode)
;;; semanticdb-mode.el ends here
