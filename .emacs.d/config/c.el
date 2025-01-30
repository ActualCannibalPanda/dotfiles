(setq c-offsets-alist '((member-init-intro . ++)))

(defconst my-c-style
  '((c-tab-always-indent . t)
    (c-comment-line-offset . 2)
    (c-echo-syntactic-information-p . t))

(c-add-style "PERSONAL" my-c-style)

(defun my-c-mode-common-hook ()
  (c-set-style "PERSONAL")
  (setq tab-width 8
	indent-tabs-mode nil)
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
