;;; Emacs distributed initialization, for keeping .emacs clean

;; Enable a backtrace when problems occur
;(setq debug-on-error t)

;; Require Common Lisp for some functions enabled

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(split-window-vertically)
(deft)
(windmove-left)
(deft-find-file (concat (file-name-as-directory deft-directory) "TODO.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(dimmer-fraction 0.5)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (yaml-mode smex shell-pop sass-mode ruby-block restclient rainbow-mode rainbow-delimiters projectile powerline marmalade highlight-symbol handlebars-mode go-mode github-browse-file flycheck flx-ido fill-column-indicator exec-path-from-shell direx deft color-theme coffee-mode auto-dim-other-buffers ag)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell schiza/term-cmd)
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "firebrick4"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#46ff00"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#1e9800"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#1e7cff"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#3369c5"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ffff0c"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#e59109"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#da09e5"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#93039b"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#aa5533"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red")))))
