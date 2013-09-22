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
(require 'my-packages)
(require 'my-global)
(require 'my-theme)
(require 'my-ido-smex)
(require 'my-recentf)
(require 'my-term)
(require 'my-coffee)
(require 'my-yaml)
(require 'my-ruby)
(require 'my-ack)
(require 'my-deft)
(require 'my-direx)

;; Start Emacs server automatically
(server-start)

;; Split window and open Deft buffer in it
(split-window-horizontally)
(windmove-right)
(deft)
(windmove-left)
