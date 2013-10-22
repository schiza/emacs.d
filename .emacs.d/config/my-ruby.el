(require 'ruby-mode)
(require 'ruby-block)

(setq ruby-insert-encoding-magic-comment t)

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (ruby-block-mode t)
	    (setq ruby-block-highlight-toggle t)
	    )
	  )

(setq show-trailing-whitespace nil)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))


(provide 'my-ruby)
