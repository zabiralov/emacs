;;; 11confmodes.el --- customizations for configurations modes
;;;
;;; Time-stamp: <2022-08-18 14:18:19 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; Load auto-complete for access to ac-modes var:
(require 'auto-complete)


;; conf-mode :: edit various configuration files
;; http://doc.endlessparentheses.com/Fun/conf-mode.html
;; 
(use-package conf-mode
  :mode "\\.conf\\'"
  :mode "\\.cnf\\'"
  :mode "\\.cf\\'"
  :config
  (add-to-list 'ac-modes 'conf-mode))



;; dns-mode :: emacs mode for edit BIND DNS zones
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/dns-mode.el
;; 
(use-package dns-mode
  :mode "\\.dns\\'"
  :mode "\\.bind\\'"
  :mode "\\.zone\\'"
  :config
  (setq dns-mode-soa-auto-increment-serial t))


;;; 11confmodes.el ends here
