;;; 06server.el --- settings for Emacs client and server -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2026-03-12 15:11:39 azabiralov>
;;;
;;; Commentary:

;;; Code:

(require 'server)
(unless (server-running-p)
  (server-start))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (my/set-faces)
		  (treemacs)
		  )))
  (my/set-faces))


;;; 06server.el ends here
