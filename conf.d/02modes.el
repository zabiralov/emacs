;;; 02modes.el --- common Emacs modes configuration -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2026-01-08 18:04:21 azabiralov>
;;;
;;; Commentary:
;;
;;; Code:

(use-package dedicated
  :ensure t)

(use-package move-dup
  :ensure t
  :demand t
  :bind (("M-p"   . move-dup-move-lines-up)
	 ("C-M-p" . move-dup-duplicate-up)
	 ("M-n"   . move-dup-move-lines-down)
	 ("C-M-n" . move-dup-duplicate-down)))

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/insert-numbers-default 1
        mc/match-cursor-style nil))

(use-package aggressive-indent
  :ensure t
  :diminish " A"
  :config
  (setq aggressive-indent-comments-too t
	aggressive-indent-sit-for-time 0.01))

(use-package smartparens
  :ensure t
  :diminish
  :config
  (setq sp-show-pair-delay 0.1
        sp-undo-pairs-separately t
        sp-autoinsert-pair t
        sp-autodelete-pair nil)
  
  (smartparens-global-mode t))

(use-package git-gutter
  :ensure t
  :diminish
  :config
  (setq git-gutter:update-interval 1
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:modified-sign "*")
  (global-git-gutter-mode t))

(use-package grugru
  :ensure t
  :config
  (grugru-define-global 'symbol '("yes" "no"))
  (grugru-define-global 'symbol '("Yes" "No"))
  (grugru-define-global 'symbol '("YES" "NO"))
  (grugru-define-global 'symbol '("y" "n"))
  (grugru-define-global 'symbol '("Y" "N"))
  (grugru-define-global 'symbol '("t" "nil"))
  (grugru-define-global 'symbol '("true" "false"))
  (grugru-define-global 'symbol '("True" "False"))
  (grugru-define-global 'symbol '("on" "off"))
  (grugru-define-global 'symbol '("On" "Off"))
  (grugru-define-global 'symbol '("ON" "OFF"))
  (grugru-define-global 'symbol '("1" "0"))
  :bind
  ("H-<SPC>" . grugru))

(use-package ws-butler
  :ensure t
  :diminish
  :config
  (setq ws-butler-keep-whitespace-before-point t
        ws-butler-convert-leading-tabs-or-spaces t
        ws-butler-global-exempt-modes '(makefile-mode))
  :hook ((prog-mode . ws-butler-mode)
         (yaml-mode . ws-butler-mode)))

(use-package paren
  :ensure t
  :diminish
  :config
  (setq show-paren-style 'parenthehis
        show-paren-delay 0)
  (show-paren-mode t))

(use-package highlight-symbol
  :ensure t
  :diminish
  :config
  (setq highlight-symbol-idle-delay 0
        highlight-symbol-highlight-single-occurrence nil
        highlight-symbol-colors '("HotPink1"))
  :bind
  ("M-h" . highlight-symbol-next)
  ("C-M-h" . highlight-symbol-prev))

(use-package company
  :ensure t
  :diminish
  :config
  (setq company-backends
	'(company-dabbrev
	  company-dabbrev-code
	  company-semantic
	  company-capf
	  company-keywords
	  company-yasnippet)
	company-tooltip-limit 15
	company-idle-delay 0
	company-minimum-prefix-length 2)
  (global-company-mode t))

(use-package super-save
  :ensure t
  :demand t
  :diminish
  :config
  (setq super-save-auto-save-when-idle t
	super-save-idle-duration 10
	auto-save-default nil)
  (super-save-mode t))

(use-package flycheck
  :ensure t
  :diminish
  :config
  (setq flycheck-display-errors-delay 0.5
	flycheck-indication-mode 'left-fringe
	flycheck-gcc-language-standard "c17"
	flycheck-gcc-pedantic t
	flycheck-cppcheck-checks '("warning" "style" "information"))
  (add-to-list 'flycheck-gcc-include-path "~/src/include/")
  (global-flycheck-mode t))

(use-package flycheck-mmark
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish
  :config
  (setq yas-snippet-dirs '("~/emacs/snippets")
	yas-indent-line "fixed"
	yas-trigger-symbol ">>>"
	yas-also-indent-empty-lines t
	yas-choose-keys-first t
	yas-wrap-around-region t)
  ;;  (yas-reload-all)
  :bind
  ("<f4>" . yas-expand)
  ("M-<f4>" . yas-insert-snippet)
  :hook ((prog-mode . yas-minor-mode)
	 (yaml-mode . yas-minor-mode)))

(use-package diminish
  :ensure t
  :demand t
  :config
  (diminish #'auto-revert-mode)
  (diminish #'yas-minor-mode))

(use-package transpose-frame
  :ensure t
  :bind
  ("H-<up>" . rotate-frame)
  ("H-<down>" . transpose-frame))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-collapse-dirs 0
	treemacs-deferred-git-apply-delay 0.5
	treemacs-display-in-side-window t
	treemacs-file-event-delay 2000
	treemacs-file-follow-delay 0.1
	treemacs-follow-after-init nil
	treemacs-expand-after-init t
	treemacs-hide-dot-git-directory t
	treemacs-indentation '(10 px)
	treemacs-indentation-string " "
	treemacs-is-never-other-window nil
	treemacs-missing-project-action 'remove
	treemacs-move-files-by-mouse-dragging t
	treemacs-move-forward-on-expand nil
	treemacs-no-png-images t
	treemacs-no-delete-other-windows t
	treemacs-project-follow-cleanup nil
	treemacs-persist-file (expand-file-name "var/treemacs/persist" user-emacs-directory)
	treemacs-position 'left
	treemacs-read-string-input 'from-child-frame
	treemacs-litter-directories nil
	treemacs-project-follow-into-home t
	treemacs-show-cursor nil
	treemacs-show-hidden-files nil
	treemacs-silent-filewatch t
	treemacs-silent-refresh t
	treemacs-sorting 'alphabetic-numeric-asc
	treemacs-select-when-already-in-treemacs 'move-back
	treemacs-space-between-root-nodes nil
	treemacs-tag-follow-cleanup t
	treemacs-tag-follow-delay 1.5
	treemacs-wide-toggle-width 50
	treemacs-width 30
	treemacs-width-increment 1
	treemacs-width-is-initially-locked t
	treemacs-workspace-switch-cleanup nil)
  :bind
  ("H-y" . treemacs)
  :custom-face
  (treemacs-directory-face ((t (:font "Cantarell 11"))))
  (treemacs-directory-collapsed-face ((t ( :font "Cantarell 11"))))
  (treemacs-window-background-face ((t ( :font "Cantarell 11"))))
  (treemacs-hl-line-face ((t ( :font "Cantarell 11"))))
  (treemacs-file-face ((t ( :font "Cantarell 11"))))
  (treemacs-root-face ((t ( :font "Cantarell 11"))))
  (treemacs-root-unreadable-face ((t ( :font "Cantarell 11"))))
  (treemacs-root-remote-face ((t ( :font "Cantarell 11"))))
  (treemacs-root-remote-unreadable-face ((t ( :font "Cantarell 11"))))
  (treemacs-root-remote-disconnected-face ((t ( :font "Cantarell 11"))))
  (treemacs-term-node-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-unmodified-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-modified-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-renamed-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-ignored-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-untracked-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-added-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-conflict-face ((t ( :font "Cantarell 11"))))
  (treemacs-tags-face ((t ( :font "Cantarell 11"))))
  (treemacs-help-title-face ((t ( :font "Cantarell 11"))))
  (treemacs-help-column-face ((t ( :font "Cantarell 11"))))
  (treemacs-on-failure-pulse-face ((t ( :font "Cantarell 11"))))
  (treemacs-on-success-pulse-face ((t ( :font "Cantarell 11"))))
  (treemacs-fringe-indicator-face ((t ( :font "Cantarell 11"))))
  (treemacs-header-button-face ((t ( :font "Cantarell 11"))))
  (treemacs-peek-mode-indicator-face ((t ( :font "Cantarell 11"))))
  (treemacs-marked-file-face ((t ( :font "Cantarell 11"))))
  (treemacs-git-commit-diff-face ((t ( :font "Cantarell 11"))))
  (treemacs-async-loading-face ((t ( :font "Cantarell 11"))))
  :hook
  (after-init . treemacs))

(use-package vterm
  :ensure t
  :config
  (setq vterm-always-compile-module t
	vterm-max-scrollback 100000
	vterm-min-window-width 40
	vterm-kill-buffer-on-exit t)
  (add-to-list 'vterm-keymap-exceptions "<f10>")
  :hook
  (vterm-mode . dedicated-mode))

(use-package vterm-toggle
  :ensure t
  :config
  (setq vterm-toggle-scope 'dedicated
	vterm-toggle-hide-method 'delete-window
	vterm-toggle-reset-window-configration-after-exit t)
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "<f10>") #'vterm-toggle))
  :bind
  ("<f10>" . vterm-toggle)
  ("H-t" . vterm-toggle)
  :hook
  (vterm-toggle-show . goto-address-mode))

(use-package bufferfile
  :ensure t
  :defer t
  :config
  (setq bufferfile-use-vc t)
  :bind
  ("H-r" . bufferfile-rename)
  ("H-<delete>" . bufferfile-delete))

(use-package buffer-flip
  :ensure t
  :defer t
  :config
  (setq buffer-flip-skip-patterns '("^\\*.+\\*\\b"))
  :bind
  (("M-<tab>" . buffer-flip)
   :map buffer-flip-map
   ( "M-<tab>" .   buffer-flip-forward)
   ( "M-S-<tab>" . buffer-flip-backward)
   ( "M-ESC" .     buffer-flip-abort)))

(use-package buffer-move
  :ensure t
  :defer t
  :config
  (setq buffer-move-stay-after-swap t)
  :bind
  (("H-<up>" . buf-move-up)
   ("H-<down>" . buf-move-down)
   ("H-<left>" . buf-move-left)
   ("H-<right>" . buf-move-right)))

(use-package org
  :config
  (setq org-table-default-size "4x4"
	org-table-header-line-p t
	org-table-automatic-realign t
	org-table-auto-blank-field t))

(use-package rainbow-delimiters
  :ensure t)

(use-package lsp-mode
  :ensure t
  :defer t
  :config
  (setq lsp-client-packages '(lsp-go)
	lsp-tcp-connection-timeout 5))

(use-package vertico
  :ensure t
  :config
  (setq vertico-count 8
	vertico-preselect 'first
	vertico-sort-function 'vertico-sort-alpha))


;;; 02modes.el ends here
