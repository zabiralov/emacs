;;; 06server.el --- settings for Emacs client and server -*- lexical-binding: t -*-
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
