(load "c")
(load "rust")
(load "python")

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
