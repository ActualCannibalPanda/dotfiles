(load "c")
(load "rust")
(load "python")

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)
