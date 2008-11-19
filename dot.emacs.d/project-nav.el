;; Copyright (C) 2008 Pat Maddox

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

(defun pnav-generate-tags-for-project ()
  (interactive)
  (let ((prev-dir default-directory))
    (cd (pnav-project-root))
    (setq files-list
          (pnav-do-filtering
           (pnav-file-filters)
           (pnav-files-with-relative-paths
            (pnav-project-root)
            (pnav-recursive-directory-files (pnav-project-root)))))
    (cd prev-dir)
    (pnav-build-tags-for-files files-list)
    t))

(defun pnav-project-root ()
  (let ((project-file (plv-find-project-file default-directory "")))
    (unless project-file (error "No project root found"))
    (file-name-directory project-file)))

(defun pnav-choose-file-from-project (fun)
  (save-excursion
    (let ((tags-file (concat (pnav-project-root) "TAGS")))
      (unless (file-exists-p tags-file) (error "No TAGS file found"))
      (visit-tags-table-buffer tags-file)
      (let ((chosen-file (pnav-prompt-for-file)))
        (funcall fun chosen-file)))))

(defun pnav-prompt-for-file ()
  (unwind-protect
      (progn
        (ad-activate 'ido-set-matches-1)
        (setq chosen-file (ido-completing-read "Project file: "
                                               (tags-table-files)
                                               nil t))
        chosen-file)
    (ad-deactivate 'ido-set-matches-1)))

(defun pnav-find-files-in-project ()
  (interactive)
  (pnav-choose-file-from-project (lambda (file) (find-file file))))

(defun pnav-recursive-directory-files (dir)
  (let ((dir (file-name-as-directory dir)))
    (apply 'append
           (mapcar
            (lambda (f)
              (let ((f (concat dir f)))
                (if (file-directory-p f)
                    (cons f (pnav-recursive-directory-files f))
                  (list f))))
            (remove ".."
                    (remove "."
                            (condition-case nil
                                (directory-files dir)
                              (error nil))))))))


(defun pnav-build-tags-for-files (files)
  (shell-command (concat "cd " (pnav-project-root) " && etags -l none "
                         (mapconcat 'identity
                                    (mapcar (lambda (s) (concat "'" s "'")) files)
                                    " "))))

(defun pnav-known-file-type-p (file)
  (detect
   (mapcar
    (lambda (regex) (string-match regex file))
    (list "\.rb$"
          "^Rakefile$"
          "\.el$"
          ))
   (lambda (matches) matches)))

(defun pnav-file-filters ()
  (list
   (lambda (s) (file-directory-p s))
   (lambda (s) (string-match "\.git\/" s))
   (lambda (s) (string-match "\.svn\/" s))
   (lambda (s) (string-match "^vendor\/" s))
   (lambda (s) (string-match "^tmp\/" s))
   (lambda (s) (string-match "~$" s))
   (lambda (s) (string-match "#" s))

   (lambda (s)
     (and (not (pnav-known-file-type-p s))
          (let* ((result
                  (shell-command-to-string (concat "file '" s "' | awk '{$1=\"\"; print}'")))
                 (file-type (replace-regexp-in-string "^[ ]*\\(.*\\)[ \n]*$" "\\1" result)))
            (not (or (string= "" file-type) (string-match "text" file-type)))))
     )
   ))

(defun pnav-apply-filter (f list)
  (remove-if (lambda (s) (funcall f s)) list))

(defun pnav-files-with-relative-paths (base list)
  (mapcar
   (lambda (s)
     (replace-regexp-in-string (regexp-quote base) "" s))
   list))

(defun pnav-do-filtering (filters-list list)
  (let ((filtered-list (pnav-apply-filter (car filters-list) list)))
        (if (cdr filters-list)
            (pnav-do-filtering (cdr filters-list) filtered-list)
          filtered-list)))

  
(global-set-key (kbd "M-t") 'pnav-find-files-in-project)

(defadvice ido-name (after only-return-file-name)
  (setq ad-return-value (file-name-nondirectory ad-return-value)))

(defadvice ido-set-matches-1 (around match-against-filename-only)
  (ad-activate 'ido-name)
  ad-do-it
  (ad-deactivate 'ido-name))

(provide 'project-nav)