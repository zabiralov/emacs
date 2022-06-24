;;; keys.el --- global Emacs keybindings rewrite
;;;
;;; Time-stamp: <2022-06-24 13:41:51 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; global hotkeys

(bind-key "<f1>" 'switch-to-buffer)
(bind-key "M-<f1>" 'kill-this-buffer)

(bind-key "<f2>" 'save-buffer)
(bind-key "M-<f2>" 'my-rename-file-and-buffer)

(bind-key "<f3>" 'replace-string)
(bind-key "M-<f3>" 'my-replace-under-point)

(bind-key "<f5>" 'keyboard-quit)

(bind-key "<f6>" 'split-window-horizontally)
(bind-key "S-<f6>" 'split-window-vertically)

(bind-key "<f7>" 'delete-window)

(bind-key "C-<up>" 'windmove-up)
(bind-key "C-<down>" 'windmove-down)
(bind-key "C-<right>" 'windmove-right)
(bind-key "C-<left>" 'windmove-left)


(bind-key "s-<SPC>" 'my-switch-language)
(bind-key "C-z" 'undo)
(bind-key "C-=" 'my-whack-whitespace)

(bind-key "<mouse-8>" 'previous-buffer)
(bind-key "<mouse-9>" 'next-buffer)

(bind-key "<left-fringe> <mouse-4>" 'previous-error)
(bind-key "<left-margin> <mouse-4>" 'previous-error)
(bind-key "<left-fringe> <mouse-5>" 'next-error)
(bind-key "<left-margin> <mouse-5>" 'next-error)

(bind-key "<vertical-line> <mouse-4>" 'window-swap-states)
(bind-key "<vertical-line> <mouse-5>" 'window-swap-states)

(bind-key "C-x 1" 'delete-window)

(bind-key "C-<backspace>" 'my-backward-delete-line)
(bind-key "C-<delete>" 'my-forward-delete-line)
(bind-key "C-k" 'kill-line)
(bind-key "M-k" 'kill-whole-line)

(bind-key "M-<backspace>" 'my-backward-delete-word)
(bind-key "M-<delete>" 'my-forward-delete-word)
(bind-key "M-d" 'my-forward-delete-word)


;;
;;; keys.el ends here
