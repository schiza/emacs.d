;;; emacs initialization, for keeping .emacs clean

;; Enable a backtrace when problems occur
;(setq debug-on-error t)

;; Require Common Lisp for some functions enabled
(require 'cl)

;; Package management
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Default packages list
(defvar schiza/packages '(coffee-mode
                          go-mode
                          marmalade
                          org
                          restclient
                          smex
                          yaml-mode)
  "Default packages")

;; Install default packages
(defun schiza/packages-installed-p ()
  (loop for pkg in schiza/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (schiza/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg schiza/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Remove tool-bar and menu-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Make highlighting in common mode (shift works, overwrite selected region)
;; and make clipboards (Emacs and system) work together
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Show full-path in window titlebar
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Indicate empty lines on the end of file
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indent proper way
(setq tab-width 2
      indent-tabs-mode nil)

;; Turn off backups
(setq make-backup-files nil)

;; Disable temporary files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Boolean shorthands
(defalias 'yes-or-no-p 'y-or-n-p)

;; Some generic keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Don't use dialog boxes and don't beep..
;; Also, always show matching parentheses
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Color theme
(load-theme 'wombat t)

;; Show column number
(setq column-number-mode t)
