;; Splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Always confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remove tool-bar and menu-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Date-time display
(setq display-time-24hr-format t)
(display-time)

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

;; Always end a file with a newline
(setq require-final-newline t)

;; Don't automatically add new lines when scrolling down at the bottom
;; of a buffer
(setq next-line-add-newlines nil)

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
(global-set-key (kbd "M-g") 'goto-line)

;; Don't use dialog boxes and don't beep..
;; Also, always show matching parentheses
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Show line and column number
(setq line-number-mode t)
(setq column-number-mode t)

;; Force proper encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;; This from a japanese individual. I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq coding-system-for-write 'utf-8)

;; Right alt meta unbind (to type polish signs)
(setq mac-right-option-modifier nil)

;; Buffer's with same name now forwarded with directory, not <2>, <3>, ...
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; zap-to-char modification
;; doesn't zap the last char
;; ex. "text)" and zap-to-char ')' leaves ")"
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))

;; Automatically indenting yanked text if in chosen programming-modes
(defvar yank-indent-modes '(ruby-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defadvice yank (after indent-region activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (member major-mode yank-indent-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (member major-mode yank-indent-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


;; Copy current line
(defun copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-end-position))
  (message "Line copied"))

(global-set-key (kbd "M-d") 'copy-line)

;; newline-withoug-break-of-line
(defun newline-without-break-of-line ()
  "1. remove to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Set default fill column
(set-default 'fill-column 80)

;; Indicate the fill column
(require 'fill-column-indicator)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'coffee-mode-hook 'fci-mode)

;; Windmove bindings
(global-set-key (kbd "<M-s-up>") 'windmove-up)
(global-set-key (kbd "<M-s-down>") 'windmove-down)
(global-set-key (kbd "<M-s-right>") 'windmove-right)
(global-set-key (kbd "<M-s-left>") 'windmove-left)

;; Window resize bindings
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; Enable lines/region moving
;; moving lines/regions
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
   arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
   arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)

;; Automatically highlight symbol on point
(require 'highlight-symbol)
(setq hihlight-symbol-idle-delay 0.5)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))

(provide 'my-global)
