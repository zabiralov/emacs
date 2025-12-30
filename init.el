;;; init.el -- emacs main configuration file -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2025-12-30 12:03:29 azabiralov>
;;;
;;; Author: Alexander E. Zabiralov
;;
;;; Commentary:
;;;
;;; My GNU Emacs configuration
;;
;; Byte-compiling Emacs config:
;; (byte-compile-file "init.el")'
;;
;;
;;; Code:



;; ELPA
;;
(let ((default-directory  "~/emacs/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))


;; Additional lisp code, not in packages
;;
(add-to-list 'load-path "~/emacs/site-lisp/")



;; Set repositories
;;
(require 'package)
(setq-default load-prefer-newer t
	      package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; Custom file
;;
(setq custom-file "~/emacs/custom.el")
(load custom-file)



;; Additional parts of configuration:
;;
(defun my-load-el-from-directory (dir)
  "Load all .el files from specified DIR directory."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(my-load-el-from-directory "~/emacs/conf.d")

;;
;;; init.el ends here
