;; Splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Always confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Remove trailing whitespace on save (only in prog-modes)
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

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

;; Always end a file with a newline, but only in prog-mode
(setq require-final-newline nil)
(setq mode-require-final-newline nil)
(add-hook 'prog-mode-hook (lambda () (setq mode-require-final-newline)))

;; Don't automatically add new lines when scrolling down at the bottom
;; of a buffer
(setq next-line-add-newlines nil)

;; Indent proper way
(setq tab-width 2
      indent-tabs-mode nil)

;; Turn autosave on
(auto-save-mode t)

;; Store temporary files in tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; Use buffer-menu instead of list-buffers for  C-x C-b
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "C-c k") 'kill-other-buffers)

;; Some file related utilities
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Highlight comment annotations
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; Join next indented line
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-^") 'top-join-line)

;; Make C-a double functional
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


;; Multi-scratch new with prefix
(defun multi-scratch-new-text  ()
  (interactive)
  (let ((current-prefix-arg t))
    (multi-scratch-new)))

(global-set-key (kbd "C-x n") 'multi-scratch-new-text)

(defun mark-current-word (&optional arg allow-extend)
    "Put point at beginning of current word, set mark at end."
    (interactive "p\np")
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((wbounds (bounds-of-thing-at-point 'word)))
        (unless (consp wbounds)
          (error "No word at point"))
        (if (>= arg 0)
            (goto-char (car wbounds))
          (goto-char (cdr wbounds)))
        (push-mark (save-excursion
                     (forward-word arg)
                     (point)))
        (activate-mark))))

(global-set-key (kbd "C-M-x") 'mark-current-word)

;; Prevent from accidental buffer killing
(defun ask-before-killing-buffer ()
  (let ((buffer (current-buffer)))
    (cond
     ((equal (buffer-name) "*scratch*")
      ;; Never kill *scratch*
      nil)
     ((and buffer-file-name (buffer-modified-p))
      ;; If there's a file associated with the buffer,
      ;; make sure it's saved
      t)
     ((get-buffer-process buffer)
      ;; If there's a process associated with the buffer,
      ;; make sure it's dead
      (y-or-n-p (format "Process %s active; kill anyway? "
                    (process-name (get-buffer-process buffer)))))
     ((string-match "*multi-scratch" (buffer-name))
        (y-or-n-p (format "Do you really want to kill %s? " (buffer-name))))
     (t t))))

(add-to-list 'kill-buffer-query-functions
             'ask-before-killing-buffer)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'my-global)
