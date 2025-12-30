;;; 06server.el --- settings for Emacs client and server -*- lexical-binding: t -*-
;;;
;;; Time-stamp: <2025-12-30 11:02:29 azabiralov>
;;;
;;; Commentary:

;;; Code:

(server-start)

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (my/set-faces)
		  (treemacs)
		  )))
  (my/set-faces))


;;; 06server.el ends here
