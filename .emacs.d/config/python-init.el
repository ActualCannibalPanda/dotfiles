(require 'python)

(use-package python)

(use-package pipenv
  :hook
  (python . pipenv-mode))
