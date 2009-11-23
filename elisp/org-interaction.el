(require 'org)

(setq font-lock-mode t)

(defun refresh-then-find-file (file)
  "Find file ensuring that the latest changes on disk are
represented in the file."
  (let (file-buf)
    (while (setq file-buf (get-file-buffer file))
      (kill-buffer file-buf))
    (find-file file)))

(defmacro with-temp-filebuffer (file &rest body)
  "Open FILE into a temporary buffer execute BODY there like
`progn', then kill the FILE buffer returning the result of
evaluating BODY."
  (let ((temp-result (make-symbol "temp-result"))
	(temp-file (make-symbol "temp-file")))
    `(let (,temp-result ,temp-file)
       (refresh-then-find-file ,file)
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
	 (kill-buffer (current-buffer)))))))

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
	 (kill-buffer (current-buffer)))))))

(defun org-file-to-pdf (file-path)
  "Open up an org file and export it as pdf."
  (let* ((file-name (file-name-nondirectory file-path))
        (file-dir (file-name-directory file-path))
        (org-tmp-path (make-temp-file "org-file-to-pdf-"))
        (pdf-tmp-path (concat org-tmp-path ".pdf"))
        (tex-tmp-path (concat org-tmp-path ".tex"))
        (pdf-path (expand-file-name (concat org-interaction-prefix file-name ".pdf") file-dir)))
    (if (and (file-exists-p pdf-path)
            (< 0 (time-to-seconds
                  (time-subtract
                   (nth 5 (file-attributes pdf-path))
                   (nth 5 (file-attributes file-path))))))
       pdf-path
      (with-temp-filebuffer
       file-path
       (write-file org-tmp-path)
       (org-mode)
       (save-window-excursion
        (org-export-as-pdf nil)
        (rename-file pdf-tmp-path pdf-path t)
        (delete-file org-tmp-path)
        (delete-file tex-tmp-path)
        (kill-buffer (current-buffer)))))))

;; customization
(setq org-export-blocks-witheld '(hidden comment))

;; Start the server
(server-start)

;; save the emacsclient server socket location
(with-temp-file "/tmp/emacsclient-socket-dir"
  (insert server-socket-dir))