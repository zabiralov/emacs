;;; 01global.el --- global Emacs configuration -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2026-01-08 19:36:14 azabiralov>
;;;
;;; Commentary:
;;
;;; Code:
;;

(setq auth-sources '("~/.netrc")
      default-justification 'full
      frame-title-format "GNU Emacs"
      initial-scratch-message nil
      kill-ring-max 3
      kill-whole-line t
      make-backup-files nil
      max-mini-window-height 0.5
      mouse-autoselect-window nil
      mouse-yank-at-point nil
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
      use-short-answers t)


(dolist (my-init-frame-options '((fullscreen . maximized)))
  (add-to-list 'initial-frame-alist my-init-frame-options))


(dolist (my-def-frame-options '((fullscreen . maximized)))
  (add-to-list 'default-frame-alist my-def-frame-options))


;; enable/disable basic minor modes on startup
;;
(column-number-mode -1)
(desktop-save-mode -1)
(line-number-mode -1)
(scroll-bar-mode t)
(blink-cursor-mode t)
(delete-selection-mode t)
(goto-address-mode t)
(pixel-scroll-precision-mode t)
(winner-mode t)
(context-menu-mode t)

;; menu-bar-mode :: menu bar
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Menu-Bar.html#Menu-Bar
;;
(setq yank-menu-length 20
      buffers-menu-max-size 20
      buffers-menu-buffer-name-length 20
      buffers-menu-show-status t)
(menu-bar-mode t)

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


;; Global hooks
;;
(add-hook 'minibuffer-setup-hook #'(lambda () (highlight-symbol-mode -1)))

;; Update timestamp with file saving
(add-hook 'before-save-hook 'time-stamp)


;; human languages support
;;
(set-language-environment 'utf-8)
(setq default-input-method 'russian-computer)


;;; 01main.el ends here
