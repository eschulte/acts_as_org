(let ((this-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir))
(require 'htmlize)
(require 'org)
(require 'org-exp-blocks)

(setq font-lock-mode t)

(defmacro with-temp-filebuffer (file &rest body)
  "Open FILE into a temporary buffer execute BODY there like
`progn', then kill the FILE buffer returning the result of
evaluating BODY."
  (let ((temp-result (make-symbol "temp-result"))
	(temp-file (make-symbol "temp-file")))
    `(let (,temp-result ,temp-file)
       (find-file ,file)
       (setf ,temp-file (current-buffer))
       (setf ,temp-result (progn ,@body))
       (kill-buffer ,temp-file)
       ,temp-result)))

(defvar org-interaction-prefix ".exported_")

(defun org-file-to-html (file-path)
  "Open up an org file, publish it to html, and then return the
html as a string."
  (let* ((file-name (file-name-nondirectory file-path))
	 (file-dir (file-name-directory file-path))
	 (html-path (expand-file-name (concat org-interaction-prefix file-name) file-dir)))
    (if (and (file-exists-p html-path)
	     (< 0 (time-to-seconds
		   (time-subtract
		    (nth 5 (file-attributes html-path))
		    (nth 5 (file-attributes file-path))))))
	html-path
      (with-temp-filebuffer
       file-path
       (org-mode)
       (save-window-excursion
	 (org-export-as-html-to-buffer nil)
	 (write-file html-path)
	 (kill-buffer)))))
  (save-buffers-kill-emacs t))

(defun org-file-to-latex (file-path)
  "Open up an org file, publish it to latex, and then return the
latex as a string."
  (let* ((file-name (file-name-nondirectory file-path))
	 (file-dir (file-name-directory file-path))
	 (latex-path (expand-file-name (concat org-interaction-prefix file-name ".tex") file-dir)))
    (if (and (file-exists-p latex-path)
	     (< 0 (time-to-seconds
		   (time-subtract
		    (nth 5 (file-attributes latex-path))
		    (nth 5 (file-attributes file-path))))))
	latex-path
      (with-temp-filebuffer
       file-path
       (org-mode)
       (save-window-excursion
	 (org-export-as-latex-to-buffer nil)
	 (write-file latex-path)
	 (kill-buffer)))))
  (save-buffers-kill-emacs t))

;; customization
(setq org-export-blocks-witheld '(hidden comment))
