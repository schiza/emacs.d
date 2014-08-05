(require 'whitespace)

(setq whitespace-display-mappings
  '(
    (space-mark 32 [183] [46])
    (tab-mark 9 [187 9] [92 9])
    ))

(defun my-whitespace-mode ()
  (interactive)
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
  (set-face-attribute 'whitespace-space nil
		      :background nil :foreground "gray30")
  (set-face-attribute 'whitespace-tab nil
		      :background nil :foreground "gray30")
  (whitespace-mode))

(add-hook 'prog-mode-hook 'my-whitespace-mode)

(provide 'my-whitespace)
