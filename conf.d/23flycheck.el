;;; flycheck.el --- configuration for FlyCheck
;;;
;;; Time-stamp: <2022-06-24 18:12:36 azabiralov>
;;;
;;; Commentary:
;;
;;; Code:


;; flycheck-mode :: check sources on-the-fly
;; https://github.com/flycheck/flycheck
;; 
(use-package flycheck
  :diminish
  :config
  (setq flycheck-display-errors-delay 0.5
        flycheck-indication-mode 'left-fringe)
  
  (global-flycheck-mode t))





;;
;;; flycheck.el ends here
