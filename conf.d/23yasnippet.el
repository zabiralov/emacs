;;; yasnippet.el --- yasnippet settings
;;;
;;; Time-stamp: <2022-06-24 19:06:26 azabiralov>
;;;
;;; Commentary:

;;; Code:



;; yasnippet-mode :: snippets for Emacs
;; https://github.com/joaotavora/yasnippet
;; 
(use-package yasnippet
  :diminish
  :config
  (setq yas-snippet-dirs '("~/emacs/snippets")
        yas-indent-line "fixed"
        yas-trigger-symbol ">>>"
        yas-also-indent-empty-lines t
        yas-choose-keys-first t
        yas-wrap-around-region t)
  :bind
  ("<f4>" . yas-expand)
  ("M-<f4>" . yas-insert-snippet))

(add-hook 'prog-mode-hook 'yas-minor-mode)


;;; 23yasnippet.el ends here
