;; Color theme
(load-theme 'wombat t)

;; Use color delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
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


(provide 'my-theme)
