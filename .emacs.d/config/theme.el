;;; theme.el --- theme settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(add-hook 'after-init-hook
	  (lambda ()
	    (setq catppuccin-flavor 'mocha)
	    (load-theme 'catppuccin :no-confirm)))

(provide 'theme)
;;; theme.el ends here
