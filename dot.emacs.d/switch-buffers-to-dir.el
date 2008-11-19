(require 'collection-functions)

(defun get-open-file-names ()
   "Returns a list of all file names open in buffers
(returns nil for buffers not attached to a file)"
  (mapcar (lambda (f) (buffer-file-name f)) (buffer-list)))

(defun string-includes-p (whole part)
  "Determines whether or not part is a substring of whole"
  (and whole (string-match (regexp-quote part) whole)))

(defun switch-buffers-to-dir ()
  "Changes a set of buffers to point at a different path
For example, if you had buffers pointed at dir1/foo and
dir1/bar, running switch-buffers-to-dir asks for the
prefix (dir1) and the replacement (dir2), and then points
existing buffers at the new prefix - dir2/foo and dir2/bar"

  (interactive)
  (switch-buffers-from-to (ask-for-path-prefix)
			  (ask-for-path-replacement)))

(defun opened-files-matching (string)
  "Returns a list of opened filenames matching string"
  (select (get-open-file-names)
	  (lambda (s) (string-includes-p s string))))

(defun switch-buffers-from-to (prefix replacement)
  "Changes a set of buffers to point at a different path
For example, if you had buffers pointed at dir1/foo and
dir1/bar, running
\(switch-buffers-from-to \"dir1\" \"dir2\"\)
would point the buffers at dir2/foo and dir2/bar"

  (let ((current-buffer-name (buffer-name (current-buffer))))
    (each (opened-files-matching prefix)
	  (lambda (f)
	    (let ((new-file-name
		   (replace-regexp-in-string
		  (regexp-quote prefix) replacement f)))
	      (switch-buffer-containing-file-to-dir
	       f new-file-name))))
    (switch-to-buffer current-buffer-name)))

  
(defun find-buffer-for (filename)
  "Finds the open buffer pointed at filename"
  (detect (buffer-list)
	  (lambda (b)
	    (string= filename (buffer-file-name b)))))

(defun switch-buffer-containing-file-to-dir (orig new)
  (if (not (file-exists-p new))
      (print (concat new " does not exist!  Can't switch"))
    (kill-buffer (find-buffer-for orig))
    (find-file-noselect new)))

(defun ask-for-path-prefix ()
  (interactive)
  (prompt-for-dir "Path to replace: "))

(defun ask-for-path-replacement ()
  (interactive)
  (prompt-for-dir "Path replacement: "))

(defun prompt-for-dir (prompt)
  (expand-file-name (read-file-name prompt default-directory default-directory)))

(provide 'switch-buffers-to-dir)