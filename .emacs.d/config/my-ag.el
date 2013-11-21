(global-set-key (kbd "C-c f") 'ag-project)

;; Highlight search phrase
(setq ag-highlight-search t)

;; Override default arguments, to get grouping
(setq ag-arguments (list "--smart-case" "--nogroup" "--column" "--"))

(provide 'my-ag)
