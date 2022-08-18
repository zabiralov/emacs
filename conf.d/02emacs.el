;;; 02emacs.el --- modes for replacing Emacs main parts
;;;
;;; Time-stamp: <2022-07-16 15:14:50 azabiralov>
;;;
;;; Commentary:

;;; Code:



;; yascroll :: yet another scroll mode
;; https://github.com/emacsorphanage/yascroll
;;
(use-package yascroll
  :demand t
  :config
  (setq yascroll:delay-to-hide nil
        yascroll:scroll-bar 'right-fringe)
  (global-yascroll-bar-mode 1))



;; column-enforce-mode :: highlight text that extends beyond a certain column
;; https://github.com/jordonbiondo/column-enforce-mode
;;
(use-package column-enforce-mode
  :diminish
  :config
  (setq column-enforce-column 80
        column-enforce-comments t)
  (column-enforce-mode t))



;; indent-guide :: show vertical lines to guide indentation
;; https://github.com/zk-phi/indent-guide
;;
(use-package indent-guide
  :diminish
  :demand t
  :config
  (setq indent-guide-char ">"
        indent-guide-delay 0.5
        indent-guide-threshold 1
        indent-guide-recursive t)

  (indent-guide-global-mode))



;; auto-revert-mode :: automatically revert buffers
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
;;
(use-package autorevert
  :diminish
  :config
  (setq auto-revert-interval 10
        auto-revert-stop-on-user-input t
        auto-revert-mode t))



;; eldoc-mode :: show elisp help in echo area
;; https://www.emacswiki.org/emacs/ElDoc
;;
(use-package eldoc
  :config
  (setq eldoc-idle-delay 0.6
        eldoc-print-after-edit nil
        eldoc-minor-mode-string nil))



;; savehist :: save minibuffer history
;; https://www.emacswiki.org/emacs/SaveHist
;;
(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/var/savehist/minibuffer")
  (savehist-mode t))



;; diminish :: hide minor modes from mode-line
;; https://github.com/myrjola/diminish.el
;;
(use-package diminish
  :demand t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'yas-minor-mode))


;;; 02emacs.el ends here
