(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'my-exec-path-from-shell)
