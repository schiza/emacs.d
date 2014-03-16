(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)
    (set-face-attribute 'auto-dim-other-buffers-face nil :background "#243242")
 )))

(provide 'my-auto-dim-other-buffers)