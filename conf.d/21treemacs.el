;;; treemacs.el --- settings for TreeMacs file browser
;;;
;;; Time-stamp: <2022-06-22 22:16:06 azabiralov>
;;;
;;; Commentary:

;;; Code:

;; treemacs :: tree browser mode
;; https://github.com/Alexander-Miller/treemacs
;;
(use-package treemacs
	:demand t

	:config
	(setq treemacs-indentation 1
				treemacs-indentation-string " "
				treemacs-show-hidden-files nil
				treemacs-file-event-delay 2000
				treemacs-file-follow-delay 0.0
				treemacs-width 30
				treemacs-move-forward-on-expand t
				treemacs-is-never-other-window t
				treemacs-no-delete-other-windows t
				treemacs-recenter-after-file-follow nil
				treemacs-wrap-around nil
				treemacs-space-between-root-nodes nil
				treemacs-silent-refresh t
				treemacs-persist-file "~/.emacs.d/var/treemacs/persist"
				treemacs-follow-after-init nil
				treemacs-indentation '(10 px)
				treemacs-no-png-images t)


	:custom-face
	(treemacs-directory-face ((t (:weight normal :font "Cantarell-11"))))
	(treemacs-root-face ((t (:weight normal :font "Cantarell-11"))))
	(treemacs-file-face ((t (:weight normal :font "Cantarell-11"))))
	(treemacs-git-unmodified-face ((t (:weight normal :font "Cantarell-11"))))
	(treemacs-git-modified-face ((t (:weight bold :underline t :font "Cantarell-11" :foreground "#859900"))))
	(treemacs-git-renamed-face ((t (:weight bold  :underline t :font "Cantarell-11" :foreground "#859900"))))
	(treemacs-git-ignored-face ((t (:weight normal :font "Cantarell-11"))))
	(treemacs-git-untracked-face ((t (:weight normal :font "Cantarell-11" :foreground "#dc322f"))))
	(treemacs-git-added-face ((t (:weight bold :font "Cantarell-11" :foreground "#b58900"))))
	(treemacs-git-conflict-face ((t (:weight bold :font "Cantarell-11")))))


;;; treemacs.el ends here
