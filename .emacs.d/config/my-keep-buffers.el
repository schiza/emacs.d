(require 'keep-buffers)

(keep-buffers-mode 1)
(push '("\\`*multi-scratch") keep-buffers-protected-alist)

(provide 'my-keep-buffers)