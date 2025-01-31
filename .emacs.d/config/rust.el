(add-hook 'rust-mode-hook
	  (lambda ()
	    (setq rust-format-on-save t)
	    (setq indent-tabs-mode nil)
	    (prettify-symbols-mode)))

(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
