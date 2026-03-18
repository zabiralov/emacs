;;; 03perfile.el --- customizations for various modes -*- lexical-binding: t -*-
;;;
;;; Commentary:

;;; Code:

(use-package make-mode
  :ensure t
  :defer t
  :custom
  (makefile-tab-after-target-colon t)
  (makefile-browser-selected-mark "+   ")
  (makefile-browser-unselected-mark "   "))

(use-package sql
  :ensure t
  :defer t
  :mode "\\.sql\\'")

(use-package sql-clickhouse
  :ensure t
  :defer t)

(use-package shell
  :ensure t
  :defer t)

(use-package cperl-mode
  :ensure t
  :defer t
  :custom
  (cperl-hairy nil)
  (cperl-indent-level 4)
  (cperl-auto-newline t)
  (cperl-electric-parens nil)
  (cperl-electric-lbrace-space nil))

(use-package cc-mode
  :ensure t
  :defer t
  :custom
  (c-report-syntactic-errors t))

(use-package toml-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t
  :hook
  (yaml-mode-hook . yas-minor-mode))

(use-package jinja2-mode
  :ensure t
  :defer t
  :mode "\\.j2\\'")

(use-package terraform-mode
  :ensure t
  :defer t
  :mode "\\.tf\\'"
  :custom
  (terraform-indent-level 2))

(use-package conf-mode
  :ensure t
  :defer t
  :mode "\\.conf\\'"
  :mode "\\.cnf\\'"
  :mode "\\.cf\\'")

(use-package dns-mode
  :ensure t
  :defer t
  :mode "\\.dns\\'"
  :mode "\\.bind\\'"
  :mode "\\.zone\\'"
  :custom
  (dns-mode-soa-auto-increment-serial t))

(use-package hcl-mode
  :ensure t
  :defer t
  :mode "\\.tpl\\'"
  :mode "\\.hcl\\'"
  :mode "\\.vars\\'"
  :mode "\\.nomad\\'"
  :custom
  (hcl-indent-level 2))

(use-package nginx-mode
  :ensure t
  :defer t
  :custom
  (nginx-indent-level 4)
  (nginx-indent-tabs-mode nil)
  :mode "nginx\\.conf\\'")

(use-package go-mode
  :ensure t
  :defer t
  :custom
  (go-fontify-function-calls t)
  (go-fontify-variables t)
  :hook
	((before-save-hook . lsp-format-buffer)
		(before-save-hook . lsp-organize-imports)))

(use-package nasm-mode
  :ensure t
  :defer t
  :custom
  (nasm-basic-offset 4))

(use-package git-modes
  :ensure t
  :defer t)


;;; 03perfile.el ends here
