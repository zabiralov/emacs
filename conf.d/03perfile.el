;;; 10editmodes.el --- customizations for various modes
;;;
;;; Time-stamp: <2025-12-04 10:30:43 azabiralov>
;;;
;;; Commentary:

;;; Code:


(use-package make-mode
  :ensure t
  :defer t
  :config
  (setq makefile-tab-after-target-colon t
	makefile-browser-selected-mark "+   "
	makefile-browser-unselected-mark "   "))

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
  :config
  (setq cperl-hairy nil
	cperl-indent-level 4
	cperl-auto-newline t
	cperl-electric-parens nil
	cperl-electric-lbrace-space nil))

(use-package cc-mode
  :ensure t
  :defer t
  :config
  (setq c-basic-offset 4
	c-default-style "bsd"
	c-report-syntactic-errors t))

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
  :config
  (setq terraform-indent-level 2))

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
  :config
  (setq dns-mode-soa-auto-increment-serial t))

(use-package hcl-mode
  :ensure t
  :defer t
  :mode "\\.tpl\\'"
  :mode "\\.hcl\\'"
  :mode "\\.vars\\'"
  :mode "\\.nomad\\'"
  :config
  (setq hcl-indent-level 2))

(use-package nginx-mode
  :ensure t
  :defer t
  :config
  (setq nginx-indent-level 2
	nginx-indent-tabs-mode nil))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (setq go-fontify-function-calls t
	go-fontify-variables t))

(use-package nasm-mode
  :ensure t
  :defer t
  :config
  (setq nasm-basic-offset 2))


;;; 03perfile.el ends here
