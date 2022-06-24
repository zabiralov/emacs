;;; confmodes.el --- customizations for configurations modes
;;;
;;; Time-stamp: <2022-06-08 18:25:09 azabiralov>
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
  (add-to-list 'ac-modes 'conf-mode)
  :hook
  (conf-mode-hook . my-default-modes))



;; dns-mode :: emacs mode for edit BIND DNS zones
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/dns-mode.el
;; 
(use-package dns-mode
  :mode "\\.dns\\'"
  :mode "\\.bind\\'"
  :mode "\\.zone\\'"
  :config
  (setq dns-mode-soa-auto-increment-serial t)
  :hook
  (dns-mode-hook . my-default-modes))



;; apache-mode :: edit Apache HTTPD server configs
;; https://github.com/emacs-php/apache-mode
;; 
(use-package apache-mode
  :hook
  (apache-mode-hook . my-default-modes))



;; nginx-mode :: edit Nginx web server configs
;; https://github.com/ajc/nginx-mode
;; 
(use-package nginx-mode
  :hook
  (nginx-mode-hook . my-default-modes))


;;; confmodes.el ends here
