;; Package management
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Default packages list
(defvar schiza/packages '(coffee-mode
			  handlebars-mode
			  sass-mode
                          go-mode
                          marmalade
                          org
                          restclient
                          smex
			  fill-column-indicator
			  handlebars-mode
			  rainbow-delimiters
			  shell-pop
			  ruby-electric
			  ruby-block
			  highlight-symbol
			  exec-path-from-shell
			  ack-and-a-half
			  deft
			  color-theme
			  direx
			  projectile
			  flx-ido
			  flycheck
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

(provide 'my-packages)
