;;; 04buffers.el --- customization for Emacs buffers
;;;
;;; Time-stamp: <2022-07-16 15:14:25 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; super-save :: automatic save for buffers
;; https://github.com/bbatsov/super-save
;; 
(use-package super-save
  :demand t
  :diminish
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 10
        auto-save-default nil)
  (super-save-mode t))



;; uniquify :: uniq buffer names
;; https://www.emacswiki.org/emacs/uniquify
;; http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;;
(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-strip-common-suffix t
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


;;; 04buffers.el ends here
