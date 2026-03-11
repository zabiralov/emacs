;;; init.el -- emacs main configuration file -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2026-03-11 14:10:22 azabiralov>
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


;; Native complilation settings
;;
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-speed 2
      native-comp-debug 0
      native-comp-verbose 0
      native-comp-async-jobs-number 8)


;; Store ELN cache in emacs.d/var
;; 
(when (boundp 'native-comp-eln-load-path)
  (let ((eln-dir "/home/azabiralov/var/eln-cache/"))
    (unless (file-exists-p eln-dir)
      (make-directory eln-dir t))
    (setq native-comp-eln-load-path (list eln-dir))))


;; ELPA
;;
(let ((default-directory  "/home/azabiralov/emacs/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))


;; Additional lisp code, not in packages
;;
(add-to-list 'load-path "/home/azabiralov/emacs/site-lisp/")



;; Set repositories
;;
(require 'package)
(setq-default load-prefer-newer t
	      package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; Custom file
;;
(setq custom-file "/home/azabiralov/emacs/custom.el")
(load custom-file)



;; Additional parts of configuration:
;;
(defun my/load-el-from-directory (dir)
  "Load all .el files from specified DIR directory."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(my/load-el-from-directory "/home/azabiralov/emacs/conf.d")

;;
;;; init.el ends here
