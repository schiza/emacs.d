(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode (quote lines))
(custom-set-faces
 '(flycheck-error ((t (:background "firebrick4")))))

(provide 'my-flycheck)
