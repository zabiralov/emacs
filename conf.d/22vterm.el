;;; vterm.el --- configuration for vterm and related packages
;;;
;;; Time-stamp: <2022-06-24 13:25:15 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; vterm :: libvterm-based terminal full-fetured terminal
;; https://github.com/akermu/emacs-libvterm
;;
(use-package vterm
  :demand t
  :config
  (setq vterm-shell "/usr/bin/fish"
        vterm-max-scrollback 100000
        vterm-kill-buffer-on-exit t
        vterm-always-compile-module t
        vterm-min-window-width 80
        vterm-term-environment-variable "xterm-256color"))


;; vterm-toggle :: toggle vterm buffer
;; https://github.com/jixiuf/vterm-toggle
;;
;; (use-package vterm-toggle
;;   :demand t
;;   :config
;;   (setq vterm-toggle-fullscreen-p nil
;;         vterm-toggle-project-root t
;;         vterm-toggle-cd-auto-create-buffer t
;;         vterm-toggle-reset-window-configration-after-exit t)

;;   :bind
;;   ("<f11>" . vterm-toggle))


(use-package multi-vterm
  :ensure t
  :config
  (setq multi-vterm-dedicated-window-height 20
        multi-vterm-buffer-name "*mvterm*")
  :bind
  ("<f11>" . multi-vterm-dedicated-toggle))



;;; vterm.el ends here
