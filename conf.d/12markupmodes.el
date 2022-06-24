;;; markupmodes.el --- customizations for markup modes
;;;
;;; Time-stamp: <2022-05-22 15:26:35 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; Load auto-complete for access to ac-modes var:
(require 'auto-complete)
;;
;;


;; toml-mode :: edit TOML files
;; https://github.com/dryman/toml-mode.el
;; 
(use-package toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))
(add-hook 'toml-mode-hook 'my-default-modes)
(add-to-list 'ac-modes 'toml-mode)


;; yaml-mode :: edit YAML files
;; https://github.com/yoshiki/yaml-mode
;; 
(use-package yaml-mode
  :mode  "\\.yml\\'"
  :mode "\\.yaml\\'"
  :config
  (add-to-list 'ac-modes 'yaml-mode)
  :hook
  (yaml-mode-hook . my-default-modes))


;; jinja2-mode :: edit Jinja2 templates
;; https://github.com/paradoxxxzero/jinja2-mode
;; 
(use-package jinja2-mode
  :mode "\\.j2\\'"
  :mode "\\.jinja\\'"
  :mode "\\.jinja2\\'"
  :config
  (add-to-list 'ac-modes 'jinja2-mode)
  :hook
  (jinja2-mode-hook . my-default-modes))


;; terraform-node :: mode for edit terraform manifests
;; https://github.com/syohex/emacs-terraform-mode
;; 
(use-package terraform-mode
  :mode "\\.tf\\'"
  :config
  (setq terraform-indent-level 2)
  :hook
  (terraform-mode-hook . my-default-modes))


;; json-mode :: edit JSON files
;; https://github.com/joshwnj/json-mode
;; 
(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-to-list 'ac-modes 'json-mode)
  :hook
  (json-mode-hook . my-default-modes))


;; dockerfile-mode :: edit Dockerfiles
;; https://github.com/spotify/dockerfile-mode
;; 
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :hook
  (dockerfile-mode-hook . my-default-modes))


;; docker-compose-mode :: edit docker-compose manifests
;; https://github.com/meqif/docker-compose-mode
;; 
(use-package docker-compose-mode
  :mode "docker\\-compose\\.yml\\'"
  :hook
  (docker-compose-mode-hook . my-default-modes))


;;; markupmodes.el ends here
