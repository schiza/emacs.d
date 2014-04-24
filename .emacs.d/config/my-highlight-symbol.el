;; Automatically highlight symbol on point
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))

;; Override some functions, to ignore the ':' at the beginning
;; Useful for ruby-mode, cause it did differentiate symbols from variables
(defun highlight-symbol-get-symbol ()
  "Return a regular expression identifying the symbol at point."
  (let ((symbol
	 (if (string= ":" (substring (thing-at-point 'symbol) 0 1))
	     (substring (thing-at-point 'symbol) 1)
	   (thing-at-point 'symbol))))
    (when symbol (concat (car highlight-symbol-border-pattern)
			 ":?"
                         (regexp-quote symbol)
                         (cdr highlight-symbol-border-pattern)))))

;; Search forward and backward for highlighted symbol
(global-set-key (kbd "C-2") 'highlight-symbol-prev)
(global-set-key (kbd "C-3") 'highlight-symbol-next)

(provide 'my-highlight-symbol)