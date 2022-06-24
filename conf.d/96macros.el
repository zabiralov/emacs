;;; macros.el --- List of saved keyboard macroses
;;;
;;; Time-stamp: <2021-12-15 01:12:30 azabiralov>
;;;
;;; Commentary:

;;; Code:


;; keyboard macros :: https://www.emacswiki.org/emacs/KeyboardMacros

(bind-key "C-<f12>" 'kmacro-start-macro-or-insert-counter)
(bind-key "<f12>" 'kmacro-end-or-call-macro)



;; Вставка комментария и горизонтальной линии из '-' длиной 77 символов:
(fset 'macro-insert-commented-line
      [?\M-\; ?\C-u ?7 ?7 ?- return])


;; Переход на новую строку и вставка комментария:
(fset 'macro-return-then-insert-comment
			"\C-m\C-[;")


;; Выравнивание параграфа по обоим сторонам (по ширине):
(fset 'macro-fill-by-width
			"\C-u\C-[q")


;; Переключиться на другое окно:
(fset 'macro-switch-to-other-window
			"\C-xo")


;; Тab + Down
(fset 'macro-tab-and-down
      "\C-i\C-[OB")


;; Удаление содержимого всей строки
;; (для работы должен быть включен delete-selection-mode):
;; C-a, C-SPC, C-e, DEL
(fset 'macro-purge-line
      "\C-a\C-@\C-e\C-[[3~\C-[xend\C-ik\C-i")


(global-unset-key (kbd "C-v"))

(define-prefix-command 'my-macros-map)
(global-set-key (kbd "C-v") 'my-macros-map)

(define-key my-macros-map (kbd "-") 'macro-insert-commented-line)


;;; macros.el ends here
