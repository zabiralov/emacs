;;; init.el -- emacs main configuration file
;;;
;;; Time-stamp: <2022-05-22 15:28:44 azabiralov>
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



;; Additional lisp code, not in packages
;; 
(let ((default-directory  "~/emacs/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))



;; Set repositories
;;
(require 'package)
(setq-default
 load-prefer-newer t
 package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)



;; Configure use-package macro
;;
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/emacs/use-package")
  (require 'use-package))



;; Custom file
;;
(setq custom-file "~/emacs/custom.el")
(load custom-file)



;; Additional parts of configuration:
;;
(defun my-load-el-from-directory (dir)
  "Load all .el files from specified DIR directory."
  (let
      ((load-it (lambda (f)
		              (load-file (concat (file-name-as-directory dir) f)))))
	  (mapc load-it (directory-files dir nil "\\.el$"))))

(my-load-el-from-directory "~/emacs/conf.d")



;; Set initial window layout
;; 
(add-hook 'after-init-hook 'my-emacs-startup)
;;
;;; init.el ends here
