(require 'python)

(use-package pipenv
  :hook
  (python-mode . pipenv-mode))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda () (require 'lsp-pyright))))
