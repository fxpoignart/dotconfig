(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "<return>") 'newline-and-indent)))

(add-to-list 'auto-mode-alist '("dot.emacs" . emacs-lisp-mode))

(provide 'my-emacs-lisp-mode)