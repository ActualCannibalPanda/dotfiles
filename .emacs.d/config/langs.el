;;; langs.el --- Language defs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(load "c")
(load "rust")
(load "python-init")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("jedi-language-server")))
  (add-to-list 'eglot-stay-out-of 'flymake))

(setq-default eglot-workspace-configuration
              '(:completions
                (:completeFunctionCalls t)))

(add-hook 'python-mode-hook 'eglot-ensure)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package jedi
  :after (epc pos-tip)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  :config)

(provide 'langs)
;;; langs.el ends here
