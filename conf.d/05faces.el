;;; 05faces.el --- Emacs faces settings -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2025-12-30 11:01:36 azabiralov>
;;;
;;; Commentary:

;;; Code:


(defun my/set-faces nil
  "Set custom faces."
  (interactive)

  (set-face-attribute 'default nil :font "Source Code Pro 14")
  (set-face-attribute 'show-paren-match nil :foreground "red")
  (set-face-attribute 'minibuffer-prompt nil :font "Source Code Pro 14")
  (set-face-attribute 'cursor nil :background "red")
  (set-face-attribute 'fixed-pitch nil :font "Source Code Pro 14")

  ;; (set-face-attribute 'tab-line-tab-special nil :font "Source Code Pro 12" :underline t)
  ;; (set-face-attribute 'tab-line-tab-current nil :font "Source Code Pro 12" :weight 'extra-bold)
  ;; (set-face-attribute 'tab-line-tab-modified nil :font "Source Code Pro 12" :slant 'italic)
  ;; (set-face-attribute 'tab-line-tab-inactive nil :font "Source Code Pro 12")

  (set-face-attribute 'mode-line nil :font "Source Code Pro 10")
  (set-face-attribute 'mode-line-inactive nil	:font "Source Code Pro 10")
  (set-face-attribute 'mode-line-buffer-id nil :font "Source Code Pro 10")
  (set-face-attribute 'mode-line-emphasis nil	:font "Source Code Pro 10")
  (set-face-attribute 'mode-line-highlight nil :font "Source Code Pro 11")

  (set-face-attribute 'header-line nil :font "Cantarell 10")
  (set-face-attribute 'header-line-highlight nil :font "Cantarell 10")

  (set-face-attribute 'package-name nil :font "Source Code Pro 10")
  (set-face-attribute 'package-description nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-built-in nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-external nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-available nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-new nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-held nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-disabled nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-installed nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-dependency nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-unsigned nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-incompat nil :font "Source Code Pro 10")
  (set-face-attribute 'package-status-avail-obso nil :font "Source Code Pro 10"))

(my/set-faces)

(use-package doom-themes
  :ensure t)

;; https://github.com/LionyxML/auto-dark-emacs

;; (use-package auto-dark
;;   :ensure t
;;   :config
;;   (setq auto-dark-dark-theme 'doom-tomorrow-night
;; 	auto-dark-light-theme 'doom-tomorrow-day
;; 	auto-dark-polling-interval-seconds 5
;; 	auto-dark-allow-osascript nil
;; 	auto-dark-allow-powershell nil)

;;   (add-hook 'auto-dark-dark-mode-hook 'my-set-faces)
;;   (add-hook 'auto-dark-light-mode-hook 'my-set-faces)

;;   (auto-dark-mode t))

(load-theme 'doom-tomorrow-night)

;;; 05faces.el ends here
