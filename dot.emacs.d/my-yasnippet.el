(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

(add-hook 'yas/after-exit-snippet-hook
          (lambda ()
            (save-excursion
              (indent-region yas/snippet-beg yas/snippet-end))
            (indent-according-to-mode)))
(provide 'my-yasnippet)