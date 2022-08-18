;;; 13progmodes.el --- customization for progmodes
;;;
;;; Time-stamp: <2022-08-18 14:17:42 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; Load auto-complete for access to ac-modes var:
(require 'auto-complete)
(require 'flycheck)


;; fish-mode :: edit Fish shell scripts
;; https://github.com/wwwjfy/emacs-fish
;;
;; (use-package fish-mode
;; 	:config
;; 	(setq fish-indent-offset 4)
;;   (add-to-list 'ac-modes 'fish-mode))


;; python-mode :: edit Python sources
;; https://github.com/emacsmirror/python-mode
;;
(use-package python
  :config
  (setq python-indent-offset 2
        python-shell-interpreter "ipython3"
        python-shell-prompt-regexp "python3> ")
  (add-to-list 'ac-modes 'python-mode))


;; make-mode :: edit Makefiles
;; https://www.emacswiki.org/emacs/MakefileMode
;;
(use-package make-mode
  :config
  (setq makefile-tab-after-target-colon t
        makefile-browser-selected-mark "+   "
        makefile-browser-unselected-mark "   ")
  (add-to-list 'ac-modes 'make-mode))


;; sql-mode :: edit SQL sources
;; https://www.emacswiki.org/emacs/SqlMode
;;
(use-package sql
  :mode "\\.sql\\'"
  :config
  (add-to-list 'ac-modes 'sql-mode)
  :hook
  (sql-mode-hook . sqlup-mode))


;; lisp-mode :: edit Lisp sources
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/External-Lisp.html
;;
(use-package lisp-mode
  :mode "\\.lisp\\'"
  :config
  (add-to-list 'ac-modes 'lisp-mode))


;; emacs-lisp-mode :: edit this sources
;; https://www.emacswiki.org/emacs/EmacsLispMode
;;
;; (use-package emacs-lisp-mode
;;   :config
;;   (add-to-list 'ac-modes 'emacs-lisp-mode))


;; lua-mode :: edit Lua sources
;; http://immerrr.github.io/lua-mode/
;;
;; (use-package lua-mode
;;   :config
;;   (add-to-list 'ac-modes 'lua-mode))


;; go-mode :: edit Golang sources
;; https://github.com/dominikh/go-mode.el
;;
;; (use-package go-mode
;;   :config
;;   (setq flycheck-go-vet-executable "go vet"
;;         flycheck-go-build-executable "go build")
;;   (add-to-list 'ac-modes 'go-mode))


;; sh-mode :: edit sh/bash shell scripts
;; https://www.emacswiki.org/emacs/ShMode
;;
(use-package shell
  :config
  (add-to-list 'ac-modes 'sh-mode))


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
  (add-to-list 'ac-modes 'perl-mode))


;; vala-mode :: edit Vala sources
;; https://github.com/emacsorphanage/vala-mode
;;
;; (use-package vala-mode
;;   :config
;;   (add-to-list 'ac-modes 'vala-mode))


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
  (add-to-list 'flycheck-gcc-include-path "~/Code/c/include/"))


;; sed-mode :: edit sed scripts
;; https://elpa.gnu.org/packages/sed-mode.html
;;
(use-package sed-mode
  :mode "\\.sed\\'")


;; cql-mode :: edit Cassandra CQL sources
;; https://github.com/Yuki-Inoue/cql-mode
;;
(use-package cql-mode
  :mode "\\.cql\\'")


;;; 13progmodes.el ends here
