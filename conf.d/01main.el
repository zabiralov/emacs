;;; 01global.el --- global Emacs configuration -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2026-03-10 19:30:52 azabiralov>
;;;
;;; Commentary:
;;
;;; Code:
;;

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Advice Dog
;; 
;; (advice-add 'split-window-below :after (lambda (&rest _args) (other-window 1)))
;; (advice-add 'split-window-right :after (lambda (&rest _args) (other-window 1)))


(setq default-input-method 'russian-computer
      auth-sources '("~/.netrc")
      default-justification 'full
      frame-title-format "GNU Emacs"
      initial-scratch-message nil
      kill-ring-max 16
      kill-whole-line t
      make-backup-files nil
      max-mini-window-height 0.5
      ring-bell-function 'ignore
      split-height-threshold nil
      split-width-threshold nil
      use-file-dialog nil
      visible-bell nil
      x-stretch-cursor t
      inhibit-splash-screen t
      help-window-select nil
      use-system-tooltips t
      use-dialog-box nil
      use-short-answers t
      select-enable-clipboard nil
      select-enable-primary nil
      save-interprogram-paste-before-kill nil
      history-length 128
      history-delete-duplicates t
      auto-save-list-file-prefix "/home/azabiralov/emacs/var/auto-save/save-")


(dolist (my-init-frame-options '((fullscreen . maximized)))
  (add-to-list 'initial-frame-alist my-init-frame-options))


(dolist (my-def-frame-options '((fullscreen . maximized)))
  (add-to-list 'default-frame-alist my-def-frame-options))


;; Disable some enabled by default mode
;;
(column-number-mode -1)
(line-number-mode -1)
(desktop-save-mode -1)

;; Enable modes which have not configuration opntions
;; 
(goto-address-mode t)

;; Configuration for built-in packages
;;
(use-package pixel-scroll
  :ensure nil
  :config
  (pixel-scroll-precision-mode t))

(use-package frame
  :ensure nil
  :custom
  (blink-cursor-delay 0.5)
  (blink-cursor-interval 0.5)
  (blink-cursor-blinks 0)
  :config
  (blink-cursor-mode t))

(use-package delsel
  :ensure nil
  :custom
  (delete-selection-temporary-region nil)
  :config
  (delete-selection-mode t))

(use-package scroll-bar
  :ensure nil
  :custom
  (scroll-bar-mode 'right)
  :config
  (scroll-bar-mode t))

(use-package winner
  :ensure nil
  :custom
  (winner-ring-size 16)
  :config
  (winner-mode t))

(use-package menu-bar
  :ensure nil
  :custom
  (yank-menu-length 16)
  (yank-menu-max-items 16)
  :config
  (setq buffers-menu-max-size 16
	buffers-menu-buffer-name-length 16
	buffers-menu-show-status t)
  (menu-bar-mode t))

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-buffer t))

(use-package tool-bar
  :ensure nil
  :custom
  (tool-bar-position 'top)
  (tool-bar-style 'both-horiz)
  :config
  (tool-bar-mode t))

(use-package help-mode
  :ensure nil
  :custom
  (help-clean-buttons t)
  :hook
  ((help-mode . (lambda () (tab-line-mode -1)))))

(use-package tab-line
  :ensure nil
  :custom
  (tab-line-new-button-show nil)
  (tab-line-tab-name-truncated-max 32)
  (tab-line-exclude-modes '(help-mode magit-mode vterm-mode))
  :config
  (global-tab-line-mode -1))

(use-package mouse
  :ensure nil
  :demand t
  :custom
  (mouse-autoselect-window nil)
  (mouse-yank-at-point nil)
  (mouse-buffer-menu-maxlen 16)
  :config
  (context-menu-mode t))

;; Disable F10 completelly for menu bar-mode
(global-unset-key (kbd "<f10>"))


;; default new buffer settings
;;
(setq-default cursor-type 'bar
	      message-log-max nil
              tab-width 8
              fill-column 80
	      default-justification 'full
	      major-mode 'text-mode
              mode-line-format
              (list "%+ " "%4l %4c " "%6p " "%b " "%f " "%e" mode-line-modes))


;; Special settings per buffer name
;; 
(add-to-list 'display-buffer-alist
             '("\\*Buffer List\\*"
               (display-buffer-same-window)
               (inhibit-same-window . nil)))



;;; 01main.el ends here
