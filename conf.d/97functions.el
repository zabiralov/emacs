;;; 97functions.el --- custom ELisp functions
;;;
;;; Time-stamp: <2022-08-14 13:19:08 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; custom-functions

;; Recrate destroyed scratch buffer
;;
(defun my-recreate-scratch nil
  "Recreate a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))


;; Insert 'version' date
;;
(defun my-insert-version ()
	"Insert date as version at point."
  (interactive)
  (insert (shell-command-to-string "date +%y.%j | tr -d \\n")))


;; Switch input language
;;
(defun my-switch-language ()
  "Print input info and switch Ispell language."
  (interactive)
  (my-cycle-ispell-languages)
  (message "%s" "Input language was switched ...")
  (sit-for 2)
  (message nil))


;; Replace word under point
;;
(defun my-replace-under-point (arg)
  "Replace word under point with query ARG."
  (interactive "sReplace word under point with: ")
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (replace-string (current-kill 0) arg)))


(defun my-fill-paragraph ()
  "Fill paragraph by width."
  (interactive)
  (fill-paragraph 1))


(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer-other-window (other-buffer (current-buffer) 1)))


(defun my-forward-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))


(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))


(defun my-forward-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))


(defun my-backward-delete-line ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))


(defun my-whack-whitespace (arg)
  "Delete all white space from point to the next word.
With prefix ARG delete across newlines as well."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))


(defun my-rename-file-and-buffer (arg)
  "Renames both current buffer and file it's visiting to ARG."
  (interactive "sNew file and buffer name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer arg)
          (message "A buffer named '%s' already exists!" arg)
        (progn
          (rename-file filename arg 1)
          (rename-buffer arg)
          (set-visited-file-name arg)
          (set-buffer-modified-p nil))))))


(defun my-switch-to-scratch-and-kill-all-buffers ()
  "Kill all buffers and delete all other windows."
  (interactive)
  (if (yes-or-no-p "Switch to *scratch* and kill all other buffers and windows? ")
      ((other-window)
       (switch-to-buffer "*scratch*")
       (mapcar 'kill-buffer (buffer-list))
       (delete-other-windows))
    (message "Abort.")))

(global-set-key (kbd "C-x K") 'my-switch-to-scratch-and-kill-all-buffers)


(defun my-emacs-startup ()
  "My custom Emacs Startup commands."
  (interactive)

  ;; Start Emacs as daemon, if already not running
  (require 'server)
  (unless (server-running-p)
    (server-start))

  (treemacs)
  (treemacs-RET-action))

;;; 
;;; 97functions.el ends here
