;;; editing.el --- customization for Emacs edit expirience
;;;
;;; Time-stamp: <2022-06-24 18:17:03 azabiralov>
;;;
;;; Commentary:
;;
;;; Code:


;; move-dup :: Eclipse-like moving and duplicating lines/regions/rectangles
;; https://github.com/wyuenho/move-dup
;;
(use-package move-dup
  :demand t
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))


;; multiple-cursors :: edit text with multiple cursrors
;; https://github.com/magnars/multiple-cursors.el
;; 
(use-package multiple-cursors
  :config
  (setq mc/insert-numbers-default 1
        mc/match-cursor-style nil)
  :bind
  ("<f8>" . mc/edit-lines)
  ("M-<f8>" . mc/edit-ends-of-lines))



;; easy-pg :: Emacs frontend for GnuPG
;; https://www.gnu.org/software/emacs/manual/html_mono/epa.html
;; 
(use-package epa-file
  :config
  (setq epa-file-select-keys "none"
        epa-file-cache-passphrase-for-symmetric-encryption t
        epg-pinentry-mode 'loopback
        epa-popup-info-window nil))



;; TRAMP :: transparent remote access
;; https://www.emacswiki.org/emacs/TrampMode
;; 
(use-package tramp
  :config
  (setq tramp-default-method "scp"
        tramp-histfile-override "~/.emacs.d/var/tramp/history"))



;; aggressive-indent-mode :: aggressive automatic code indent
;; https://github.com/Malabarba/aggressive-indent-mode
;; 
(use-package aggressive-indent
  :diminish
  :config

  (setq aggressive-indent-comments-too t
        aggressive-indent-sit-for-time 0.01)

  (global-aggressive-indent-mode t))



;; smartparens :: auto-complete for parens
;; https://github.com/Fuco1/smartparens
;; 
(use-package smartparens
  :diminish
  :config
  (setq sp-show-pair-delay 0.1
        sp-undo-pairs-separately t
        sp-autoinsert-pair t
        sp-autodelete-pair nil)
  
  (smartparens-global-mode t))



;; git-gutter :: show diffs between git commits
;; https://github.com/syohex/emacs-git-gutter
;; 
(use-package git-gutter
  :diminish
  :config
  (setq git-gutter:update-interval 1
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:modified-sign "*")
  (global-git-gutter-mode t))



;; grugru :: rotate text at point
;; https://github.com/ROCKTAKEY/grugru
;;
(use-package grugru
  :config
  (grugru-define-global 'symbol '("yes" "no"))
  (grugru-define-global 'symbol '("Yes" "No"))
  (grugru-define-global 'symbol '("YES" "NO"))
  (grugru-define-global 'symbol '("y" "n"))
  (grugru-define-global 'symbol '("Y" "N"))
  (grugru-define-global 'symbol '("t" "nil"))
  (grugru-define-global 'symbol '("true" "false"))
  (grugru-define-global 'symbol '("True" "False"))
  (grugru-define-global 'symbol '("on" "off"))
  (grugru-define-global 'symbol '("On" "Off"))
  (grugru-define-global 'symbol '("ON" "OFF"))
  (grugru-define-global 'symbol '("1" "0"))
  :bind
  ("S-<SPC>" . grugru))



;; ws-butter :: trim spaces from end of line
;; https://github.com/lewang/ws-butler
;; 
(use-package ws-butler
  :diminish
  :config
  (setq ws-butler-keep-whitespace-before-point t
        ws-butler-convert-leading-tabs-or-spaces t
        ws-butler-global-exempt-modes '(makefile-mode)))


;; show-paren-mode :: show paren characters
;; https://www.emacswiki.org/emacs/ShowParenMode
;; 
(use-package paren
  :diminish
  :config
  (setq show-paren-style 'parenthehis
        show-paren-delay 0)
  (show-paren-mode t))


;; autocomplete-mode :: automatic completion
;; https://github.com/auto-complete/auto-complete
;; 
(use-package auto-complete
  :diminish
  :config
  (setq ac-delay 0.5
        ac-auto-show-menu 1.0
        ac-expand-on-auto-complete t
        ac-stop-flymake-on-completing t
        ac-flycheck-poll-completion-end-interval 1.0
        ac-use-quick-help nil
        ac-menu-height 5
        ac-ignore-case t
        ac-max-width nil
        ac-comphist-file "~/.emacs.d/var/ac/history")
  
  (ac-config-default)
  (global-auto-complete-mode t)
  ;;
  :custom-face
  (ac-candidate-face ((t (:weight normal :font "Source Code Pro-10"))))
  (ac-selection-face ((t (:weight normal :font "Source Code Pro-10")))))


;;
;;; editing.el ends here
