;;; 04macrofunc.el --- custom functions, marcoses and keybindings -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2026-01-08 18:46:57 azabiralov>
;;;
;;; Commentary:

;;; Code:


(add-to-list 'image-load-path "~/emacs/etc/images/")


(defun my/switch-to-scratch ()
  "Switch to scratch buffer, if it exists, if not - create new and switch."
  (interactive)
  (let ((scratch-buffer (get-buffer-create "*scratch*")))
    (with-current-buffer scratch-buffer
      (when (eq major-mode 'fundamental-mode)
        (lisp-interaction-mode)))
    (switch-to-buffer scratch-buffer)))

(defun my/quit-tool-bar ()
  "Universal exit - simulate `q` press for exit."
  (interactive)
  (let ((binding (key-binding (kbd "q"))))
    (if (and binding (not (numberp binding)))
        (call-interactively binding)
      (quit-window))))

(defvar my/tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "icons/new" 'find-file 'new map
			 :label " New" :help "New...")
    (tool-bar-local-item "icons/open" 'find-file 'open map
			 :label " Open" :help "Open...")
    (tool-bar-local-item "icons/diropen" 'dired 'dired map
			 :label " Open Dir" :help "Open directory in Dired...")

    (tool-bar-local-item "icons/save" 'save-buffer 'save map
			 :label " Save" :help "Save...")
    (tool-bar-local-item "icons/saveas" 'write-file 'saveas map
			 :label " Save As" :help "Save with new name")
    
    (tool-bar-local-item "icons/undo" 'undo 'undo map
			 :label " Undo" :help "Undo last operation")
    (tool-bar-local-item "icons/redo" 'undo-redo 'redo map
			 :label " Redo" :help "Redo canceled operation")
    
    (tool-bar-local-item "icons/refresh" 'revert-buffer 'revert map
			 :label " Reload" :help "Reload buffer from file...")

    (tool-bar-local-item "icons/prev-node" 'previous-buffer 'prev map
			 :label " Prev Buf" :help "Switch to previous buffer")
    (tool-bar-local-item "icons/next-node" 'next-buffer 'next map
			 :label " Next Buf" :help "Switch to next buffer")

    (tool-bar-local-item "icons/up-node" 'list-buffers 'list map
			 :label " List Buf" :help "List current buffers")

    (tool-bar-local-item "icons/home" 'my/switch-to-scratch 'home map
			 :label " Home" :help "Switch to *scratch* buffer")

    (tool-bar-local-item "icons/zoom-in" 'text-scale-increase 'zoomin map
			 :label " Zoom In" :help "Zoom buffer (increase font size)")
    (tool-bar-local-item "icons/zoom-out" 'text-scale-decrease 'zoomout map
			 :label " Zoom Out" :help "Zoom buffer (decrease font size)")
    (tool-bar-local-item "icons/exit" 'my/quit-tool-bar 'quit map
			 :label " Quit" :help "Try to quit from special mode")
    map))

(defun my/set-tool-bar ()
  "Setup my custom tool bar."
  (interactive)
  (setq tool-bar-map my/tool-bar-map))

(add-hook 'after-change-major-mode-hook #'my/set-tool-bar)


(defun my/keyboard-quit ()
  "Quit minibuffer if active, otherwise `keyboard-quit`."
  (interactive)
  (if (minibufferp (current-buffer))
      (abort-recursive-edit)
    (if (active-minibuffer-window)
	(with-selected-window (active-minibuffer-window)
	  (abort-recursive-edit))
      (keyboard-quit))))

(bind-key "C-g" #'my/keyboard-quit)

(defun my/buffer-enabled-text-modes ()
  "List of buffer-local modes enabled by default for text modes."
  (interactive)
  (auto-insert-mode t)
  (goto-address-mode t)
  (rainbow-delimiters-mode t)
  (prettify-symbols-mode t)
  (highlight-symbol-mode t))

(defun my/buffer-enabled-prog-modes ()
  "List of buffer-local modes enabled by default for text modes."
  (interactive)
  (auto-insert-mode t)
  (goto-address-mode t)
  (rainbow-delimiters-mode t)
  (prettify-symbols-mode t)
  (highlight-symbol-mode t)
  (indent-tabs-mode t)
  (aggressive-indent-mode t))

(defun my/switch-language ()
  "Print input info and switch Ispell language."
  (interactive)
  (my/cycle-ispell-languages)
  (message "%s" "Input language was switched ...")
  (sit-for 2)
  (message nil))

(defun my/replace-under-point (arg)
  "Replace word under point with query ARG."
  (interactive "Replace word under point with: ")
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (replace-string (current-kill 0) arg)))

(defun my/fill-paragraph ()
  "Fill paragraph by width."
  (interactive)
  (fill-paragraph 1))

(defun my/delete-whole-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(bind-key "C-k" #'kill-whole-line)

(bind-key "H-k" #'kill-buffer-and-window)
(bind-key "H-a" #'beginning-of-buffer)
(bind-key "H-e" #'end-of-buffer)
(bind-key "H-w" #'other-window)
(bind-key "H-h" #'mark-whole-buffer)
(bind-key "H-v" #'yank)
(bind-key "H-d" #'duplicate-line)
(bind-key "H-c" #'keyboard-quit)
(bind-key "H-c" #'abort-recursive-edit minibuffer-local-map)
(bind-key "H-f" #'find-file)
(bind-key "H-s" #'save-buffer)
(bind-key "H-x" #'eval-last-sexp)

(bind-key "<f1>" #'kill-current-buffer)
(bind-key "<f2>" #'save-buffer)
(bind-key "<f3>" #'replace-string)
(bind-key "M-<f3>" #'my/replace-under-point)
(bind-key "<f4>" #'my/delete-whole-line)
(bind-key "<f5>" #'indent-tabs-mode)
(bind-key "<f6>" #'aggressive-indent-mode)
(bind-key "<f7>" #'vc-dir)

;; (bind-key "<f8>" ')
;; (bind-key "<f9>" ')
;; (bind-key "<f10>" ')
;; (bind-key "<f11>" ') ; Toggle fullscreen

(bind-key "<f12>" #'kmacro-end-or-call-macro)
(bind-key "M-<f12>" #'kmacro-start-macro-or-insert-counter)

(bind-key "s-<SPC>" #'my/switch-language)
(bind-key "C-z" #'undo)


(bind-key "C-x <left>" #'previous-buffer)
(bind-key "C-<left>" #'previous-buffer)
(bind-key "<mouse-8>" #'previous-buffer)

(bind-key "C-x <right>" #'next-buffer)
(bind-key "C-<right>" #'next-buffer)
(bind-key "<mouse-9>" #'next-buffer)


;; Global hooks
;;
(add-hook 'text-mode-hook #'my/buffer-enabled-text-modes)
(add-hook 'prog-mode-hook #'my/buffer-enabled-prog-modes)

(add-hook 'minibuffer-setup-hook #'(lambda () (highlight-symbol-mode -1)))
(add-hook 'before-save-hook 'time-stamp)






;;; 
;;; 04macrofunc.el ends here
