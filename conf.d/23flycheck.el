;;; flycheck.el --- configuration for FlyCheck
;;;
;;; Time-stamp: <2022-07-16 14:18:53 azabiralov>
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


;; markdown support for flycheck via mmark
;; https://github.com/mmark-md/flycheck-mmark
;;
(use-package flycheck-mmark)


;;
;;; 23flycheck.el ends here
