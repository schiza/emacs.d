;; Use color delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(custom-set-faces
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

;; Set font
(set-face-attribute 'default nil
		    :family "inconsolata"
		    :height 140)

(set-default-font "-apple-Inconsolata-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Current line highlighting
(global-hl-line-mode 1)
(if window-system
	(set-face-background 'hl-line "gray15")
	(set-face-background 'hl-line "gray20")
)

;; Color-theme
;; http://alexpogosyan.com/color-theme-creator/
(require 'color-theme)
(setq color-theme-is-global t)

(defun faces_x ()
  (interactive)
  (color-theme-install
   '(faces_x
      ((background-color . "#101e2e")
      (background-mode . light)
      (border-color . "#1a1a1a")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "black"))
     (fringe ((t (:background "#1a1a1a"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#800006"))))
     (region ((t (:background "#0d4519"))))
     (font-lock-builtin-face ((t (:foreground "#729fcf"))))
     (font-lock-comment-face ((t (:foreground "#888a85" :italic t))))
     (font-lock-function-name-face ((t (:foreground "#edd400"))))
     (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
     (font-lock-string-face ((t (:foreground "#ad7fa8"))))
     (font-lock-type-face ((t (:foreground"#8ae234" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "#ff9224"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     ))
)

(defun faces_terminal ()
  (interactive)
  (color-theme-initialize)
  (color-theme-midnight)
)

(if (window-system)
      (faces_x)
      (faces_terminal))

;; Org mode priorities colors
(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))

(provide 'my-theme)
