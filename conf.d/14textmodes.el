;;; textmodes.el --- customizations for text modes
;;;
;;; Time-stamp: <2022-05-22 15:28:18 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; Load auto-complete for access to ac-modes var:
(require 'auto-complete)


;; text-mode :: edit plain text files
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Mode.html
;;
(use-package text-mode
  :mode "\\.t\\'")


;; latex-mode :: edit LaTeX documents
;; https://www.emacswiki.org/emacs/LaTeX
;;
(use-package reftex
  :config
  (setq latex-run-command "pdflatex")

  :hook
  (latex-mode-hook . turn-on-reftex)
  (latex-mode-hook . my-default-modes))


;;; textmodes.el ends here
