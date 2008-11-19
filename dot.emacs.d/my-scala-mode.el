(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "<return>") 'newline-and-indent)))
(provide 'my-scala-mode)