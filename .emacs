;;; Emacs distributed initialization, for keeping .emacs clean

;; Enable a backtrace when problems occur
;(setq debug-on-error t)

;; Require Common Lisp for some functions enabled
(require 'cl)

;; Load more specific configurations
(defvar schiza/config-dir (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path schiza/config-dir)

;; Load files from vendor directory
(defvar schiza/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path schiza/vendor-dir)

(dolist (project (directory-files schiza/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Easier way to manage specific configurations
(defvar schiza/configs '(my-packages
			 my-global
			 my-theme
			 my-ido-smex
			 my-recentf
			 my-term
			 my-coffee
			 my-yaml
			 my-ruby
			 my-exec-path-from-shell
			 my-ack
			 my-deft
			 my-direx
			 my-projectile
			 my-flycheck))

(dolist (config schiza/configs)
  (require config))

;; Start Emacs server automatically
(server-start)

;; Split window and open Deft buffer in it
(split-window-horizontally)
(windmove-right)
(deft)
(windmove-left)
