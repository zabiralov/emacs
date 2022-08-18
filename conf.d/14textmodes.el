;;; 14textmodes.el --- customizations for text modes
;;;
;;; Time-stamp: <2022-08-14 17:05:17 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; Load auto-complete for access to ac-modes var:
(require 'auto-complete)


;; text-mode :: edit plain text files
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Mode.html
;;
(use-package text-mode
  :mode "\\.t\\'"
  :hook
  (text-mode-hook . aggressive-fill-paragraph-mode))


;; markdown-mode :: edit md files with preview
;; https://jblevins.org/projects/markdown-mode/
;;
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-list-indent-width t
        markdown-enable-wiki-links t
        markdown-coding-system "utf-8")
  :hook
  (markdown-mode-hook . aggressive-fill-paragraph-mode))


;; latex-mode :: edit LaTeX documents
;; https://www.emacswiki.org/emacs/LaTeX
;;
(use-package reftex
  :config
  (setq latex-run-command "pdflatex")
  :hook
  (latex-mode-hook . turn-on-reftex))


;;; 14textmodes.el ends here
