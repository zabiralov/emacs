;;; 06server.el --- settings for Emacs client and server
;;;
;;; Time-stamp: <2025-05-23 17:52:55 azabiralov>
;;;
;;; Commentary:

;;; Code:

(server-start)

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (my-set-faces)
		  (treemacs)
		  )))
  (my-set-faces))


;;; 06server.el ends here
