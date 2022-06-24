;;; libs.el --- various Emacs libs
;;;
;;; Time-stamp: <2022-06-24 23:59:11 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; transient :: transient commands library
;; https://github.com/magit/transient
;;
(use-package transient
  :config
  (setq transient-levels-file "~/emacs/var/transient/levels.el"
        transient-values-file "~/emacs/var/transient/values.el"
        transient-history-file "~/emacs/var/transient/history.el"))



;;; 05libs.el ends here
