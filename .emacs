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
			 my-ag
			 my-deft
			 my-direx
			 my-projectile
			 my-flycheck
			 my-whitespace
			 my-auto-dim-other-buffers
			 my-github-browse-file
			 my-powerline
			 my-highlight-symbol))

(dolist (config schiza/configs)
  (require config))

;; Start Emacs server automatically
(if (window-system)
    (server-start))

;;; Some autostart thingies

;; Set text-mode for scratch
(text-mode)

;; Split window and open Deft buffer in it and after that, open TODO on top of it
(split-window-horizontally)
(windmove-right)
(deft)
(find-file (concat (file-name-as-directory deft-directory) "TODO.org"))
(windmove-left)