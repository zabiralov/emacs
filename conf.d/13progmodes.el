;;; progmodes.el --- customization for progmodes
;;;
;;; Time-stamp: <2022-05-22 15:27:55 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; Load auto-complete for access to ac-modes var:
(require 'auto-complete)
(require 'flycheck)


;; fish-mode :: edit Fish shell scripts
;; https://github.com/wwwjfy/emacs-fish
;;
(use-package fish-mode
	:config
	(setq fish-indent-offset 4)
  (add-to-list 'ac-modes 'fish-mode)
	:hook
	(python-mode-hook . my-default-modes))


;; python-mode :: edit Python sources
;; https://github.com/emacsmirror/python-mode
;;
(use-package python
  :config
  (setq python-indent-offset 2
        python-shell-interpreter "ipython3"
        python-shell-prompt-regexp "python3> ")
  (add-to-list 'ac-modes 'python-mode)

  :hook
  (python-mode-hook . my-default-modes))


;; make-mode :: edit Makefiles
;; https://www.emacswiki.org/emacs/MakefileMode
;;
(use-package make-mode
  :config
  (setq makefile-tab-after-target-colon t
        makefile-browser-selected-mark "+   "
        makefile-browser-unselected-mark "   ")
  (add-to-list 'ac-modes 'make-mode)

  :hook
  (make-mode-hook . my-default-modes))


;; sql-mode :: edit SQL sources
;; https://www.emacswiki.org/emacs/SqlMode
;;
(use-package sql
  :mode "\\.sql\\'"
  :config
  (add-to-list 'ac-modes 'sql-mode)
  :hook
  (sql-mode-hook . my-default-modes)
  (sql-mode-hook . sqlup-mode))


;; lisp-mode :: edit Lisp sources
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/External-Lisp.html
;;
(use-package lisp-mode
  :mode "\\.lisp\\'"
  :config
  (add-to-list 'ac-modes 'lisp-mode)
  :hook
  (lisp-mode-hook . my-default-modes))


;; emacs-lisp-mode :: edit this sources
;; https://www.emacswiki.org/emacs/EmacsLispMode
;;
(use-package emacs-lisp-mode
  :config
  (add-to-list 'ac-modes 'emacs-lisp-mode)
  :hook
	(emacs-lisp-mode-hook . my-default-modes))


;; lua-mode :: edit Lua sources
;; http://immerrr.github.io/lua-mode/
;;
(use-package lua-mode
  :config
  (add-to-list 'ac-modes 'lua-mode)
  :hook
  (lua-mode-hook . my-default-modes))


;; go-mode :: edit Golang sources
;; https://github.com/dominikh/go-mode.el
;;
(use-package go-mode
  :config
  (setq flycheck-go-vet-executable "go vet"
        flycheck-go-build-executable "go build")
  (add-to-list 'ac-modes 'go-mode)

  :hook
  (go-mode-hook . my-default-modes))


;; sh-mode :: edit sh/bash shell scripts
;; https://www.emacswiki.org/emacs/ShMode
;;
(use-package shell
  :config
  (add-to-list 'ac-modes 'sh-mode)
  :hook
  (sh-mode-hook . my-default-modes))


;; cperl-mode :: advanced mode for edit Perl5 sources
;; https://www.emacswiki.org/emacs/CPerlMode
;;
(use-package cperl-mode
  :config
  (setq cperl-hairy nil
        cperl-indent-level 4
        cperl-auto-newline t
        cperl-electric-parens nil
        cperl-electric-lbrace-space nil)
  (add-to-list 'ac-modes 'perl-mode)

  :hook
  (perl-mode-hook . my-default-modes))


;; vala-mode :: edit Vala sources
;; https://github.com/emacsorphanage/vala-mode
;;
(use-package vala-mode
  :config
  (add-to-list 'ac-modes 'vala-mode)
  :hook
  (vala-mode-hook . my-default-modes))


;; cc-mode :: edit souces for c-style languages
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;;
(use-package cc-mode
  :config
  (setq c-basic-offset 2
        c-default-style "gnu"
        c-report-syntactic-errors t
        flycheck-gcc-language-standard "c11")
  (add-to-list 'ac-modes 'c-mode)
  (add-to-list 'flycheck-gcc-include-path "~/Code/c/include/")

  :hook
  (c-mode-hook . my-default-modes))


;; sed-mode :: edit sed scripts
;; https://elpa.gnu.org/packages/sed-mode.html
;;
(use-package sed-mode
  :hook
  (sed-mode-hook . my-default-modes))


;; cql-mode :: edit Cassandra CQL sources
;; https://github.com/Yuki-Inoue/cql-mode
;;
(use-package cql-mode
  :hook
  (cql-mode-hook . my-default-modes))


;;; progmodes.el ends here
