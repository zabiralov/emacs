;;; zfaces.el --- Emacs faces settings
;;;
;;; Time-stamp: <2022-06-24 17:22:23 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; fonts and faces

(when (display-graphic-p)

  (set-face-attribute 'show-paren-match nil :foreground "#3cb371")
  (set-face-attribute 'minibuffer-prompt nil :weight 'normal :font "Source Code Pro-14")
  (set-face-attribute 'default nil :weight 'normal :font "Source Code Pro-14")
  (set-face-attribute 'region nil :background "#3cb371" :underline t)

  (set-face-attribute 'cursor nil :background "#A40000")
  (set-face-attribute 'fixed-pitch nil :weight 'normal :font "Source Code Pro-14" )

  (set-face-attribute 'mode-line nil :weight 'normal :font "Source Code Pro-10")
  ;; (set-face-attribute 'mode-line-active nil :weight 'normal :font "Source Code Pro-10")
  (set-face-attribute 'mode-line-inactive nil :weight 'normal :font "Source Code Pro-10")
  (set-face-attribute 'mode-line-buffer-id nil :weight 'normal :font "Source Code Pro-10")
  (set-face-attribute 'mode-line-emphasis nil :weight 'normal :font "Source Code Pro-10")
  (set-face-attribute 'mode-line-highlight nil :weight 'normal :font "Source Code Pro-11")

  (set-face-attribute 'header-line nil :weight 'normal :font "Cantarell-10")
  (set-face-attribute 'header-line-highlight nil :weight 'normal :font "Cantarell-10")

  ;; (set-face-attribute 'outline-1 nil :weight 'bold :underline t :font "Source Code Pro-17")
  ;; (set-face-attribute 'outline-2 nil :weight 'bold :font "Source Code Pro-17")
  ;; (set-face-attribute 'outline-3 nil :weight 'bold :underline t :font "Source Code Pro-16")
  ;; (set-face-attribute 'outline-4 nil :weight 'bold :font "Source Code Pro-16")
  ;; (set-face-attribute 'outline-5 nil :weight 'bold :underline t :font "Source Code Pro-15")
  ;; (set-face-attribute 'outline-6 nil :weight 'bold :font "Source Code Pro-15")
  ;; (set-face-attribute 'outline-7 nil :weight 'bold :underline t :font "Source Code Pro-14")
  ;; (set-face-attribute 'outline-8 nil :weight 'bold :font "Source Code Pro-14")

  ;; (set-face-attribute 'package-name nil :font "Source Code Pro-13")
  ;; (set-face-attribute 'package-description nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-built-in nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-external nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-available nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-new nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-held nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-disabled nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-installed nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-dependency nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-unsigned nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-incompat nil :font "Source Code Pro-10")
  ;; (set-face-attribute 'package-status-avail-obso nil :font "Source Code Pro-10")

  )


;;; yfaces.el ends here
