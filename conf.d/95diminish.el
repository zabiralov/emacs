;;; 95diminish.el --- diminish for modes, which not dimished by use-package
;;;
;;; Time-stamp: <2022-07-16 15:16:45 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; diminish :: hide minor modes from mode-line
;; https://github.com/myrjola/diminish.el
;;
(use-package diminish
  :demand t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'yas-minor-mode))


;;; 95diminish.el ends here
