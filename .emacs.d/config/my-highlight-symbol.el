;; Automatically highlight symbol on point
(require 'highlight-symbol)
(setq hihlight-symbol-idle-delay 0.5)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))

;; Simplest regexp for getting current variable at point
(defun variable-at-point ()
  (let (p1 p2)
    (let ((wordcharset "[:alnum:]_"))
      (skip-chars-backward wordcharset)
      (setq p1 (point))
      (skip-chars-forward wordcharset)
      (setq p2 (point)))

    (buffer-substring-no-properties p1 p2)))

;; Simple overrides for variable and function, to make it work properly
(setq highlight-symbol-border-pattern '("" . ""))
(defun highlight-symbol-get-symbol ()
  "Return a regular expression identifying the symbol at point."
  (let ((symbol (variable-at-point)))
    (when symbol (concat (car highlight-symbol-border-pattern)
                         (regexp-quote symbol)
                         (cdr highlight-symbol-border-pattern)))))

;; Search forward and backward for highlighted symbol
(global-set-key (kbd "C-2") 'highlight-symbol-prev)
(global-set-key (kbd "C-3") 'highlight-symbol-next)

(provide 'my-highlight-symbol)