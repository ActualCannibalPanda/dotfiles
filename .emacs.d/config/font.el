;;; font.el --- font settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package fira-code-mode
  :config
  (global-fira-code-mode)
  (fira-code-mode-set-font)
  :hook prog-mode)

(provide 'font)
;;; font.el ends here
