(add-to-list 'load-path' "~/.emacs.d")
(add-to-list 'load-path' "~/.emacs.private.d/")
(add-to-list 'load-path' "~/.emacs.d/site-lisp")
(add-to-list 'load-path' "~/.emacs.d/site-lisp/w3m")
(add-to-list 'load-path' "~/.emacs.d/site-lisp/rhtml")
(add-to-list 'load-path' "~/.emacs.d/site-lisp/bbdb")

;

(server-start)

;; Don't use linum because it fails with emacs 22.2 + outline-mode
;; where the outline has newlines in it
(require 'linum)
(column-number-mode 1)

(abbrev-mode t)

(require 'my-ruby-mode)
(require 'rspec-mode)
(require 'javascript-mode)
(require 'my-emacs-lisp-mode)
(load "my-textmate")

(require 'rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

(require 'w3m-load)
(require 'gnus)
;(require 'bbdb)
;(bbdb-initialize 'gnus 'message)
;(setq bbdb-north-american-phone-numbers-p nil)
;(require 'vcard)
;(require 'bbdb-vcard-import)

;;; Loading modes
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(global-auto-revert-mode t)

(require 'my-scala-mode)

(require 'autotest)
(show-paren-mode t)

; Pimp my interface
(add-to-list 'default-frame-alist '(font . "-apple-monaco-medium-r-normal--0-0-0-0-m-0-iso8859-1"))
(transient-mark-mode t)
(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
  numbers with the C-x C-j prefix.  Another mode,
  `window-number-meta-mode' enables the use of the M- prefix."
  t)
(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)
(window-number-mode 1)
(window-number-meta-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-position (selected-frame) 0 0)

(require 'my-yasnippet)
(require 'sweet-editing)
(require 'project-nav)
(require 'switch-buffers-to-dir)

;;; rinari
(add-to-list 'load-path' "~/.emacs.d/rinari")
(require 'rinari)

;;; SLIME & Lisp
(let ((slime-dir "~/.emacs.d/site-lisp/slime"))
  (if (and (file-directory-p slime-dir)
           (member "slime.el" (directory-files slime-dir)))
      (progn
        (add-to-list 'load-path slime-dir)
        (setq inferior-lisp-program "/opt/local/bin/sbcl")
        (require 'slime)
        (slime-setup))))

;; Keybindings ftw
(global-set-key (kbd "M-l") 'goto-line)
(global-set-key (kbd "C-S-j") 'join-line)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 72 :width normal :foundry "apple" :family "Monaco")))))

(put 'erase-buffer 'disabled nil)
