;;; c.el --- c-mode settings -*- lexical-binding -*-
;;; Commentary:
;;; Code:
(setq c-offsets-alist '((member-init-intro . ++)))

(defconst my-c-style
  '((c-tab-always-indent . t)
    (c-comment-line-offset . 2)
    (c-echo-syntactic-information-p . t)))

(c-add-style "PERSONAL" my-c-style)

(defun my-c-mode-common-hook ()
  "My \='c-mode' settings."
  (c-set-style "PERSONAL")
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq indent-line-function 'insert-tab)
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(provide 'c)
;;; c.el ends here
