;;; 23yasnippet.el --- yasnippet settings
;;;
;;; Time-stamp: <2022-08-18 17:15:59 azabiralov>
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
  (yas-reload-all)
  :bind
  ("<f4>" . yas-expand)
  ("M-<f4>" . yas-insert-snippet)

  :hook ((prog-mode . yas-minor-mode)
         (yaml-mode . yas-minor-mode)))


;;; 23yasnippet.el ends here
