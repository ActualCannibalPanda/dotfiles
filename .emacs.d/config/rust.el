;;; rust.el --- rust settings -*- lexical-binding: t  -*-
;;; Commentary:
;;; Code:
(add-hook 'rust-mode-hook
	  (lambda ()
	    (setq rust-format-on-save t)
	    (setq indent-tabs-mode nil)
	    (prettify-symbols-mode)))

(use-package rust-mode
  :config
  (setq rust-mode-treesitter-derive t))

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(provide 'rust)
;;; rust.el ends here
