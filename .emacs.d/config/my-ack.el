(require 'ack-and-a-half)

;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Configuration
(setq ack-and-a-half-regexp-search nil)
(setq ack-and-a-half-use-ido t)
(setq ack-and-a-half-prompt-for-directory 'unless-guessed)
(setq ack-and-a-half-buffer-name "*ack*")

;; Disable --no-group
;; Maybe patch a pull request? ;)
(defun ack-and-a-half-arguments-from-options (regexp)
  (let ((arguments (list "--nocolor" "--no-column"
                         (ack-and-a-half-option "smart-case" (eq ack-and-a-half-ignore-case 'smart))
                         (ack-and-a-half-option "env" ack-and-a-half-use-environment)
                         )))
    (unless ack-and-a-half-ignore-case
      (push "-i" arguments))
    (unless regexp
      (push "--literal" arguments))
    arguments))

(global-set-key (kbd "C-c f") 'ack)

(provide 'my-ack)
