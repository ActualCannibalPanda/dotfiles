;;; myorg.el --- my org setup -*- lexical-binding -*-
;;; Commentary:
;;; Code:
(require 'org)
(keymap-global-set "C-c l"  'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)
(setq org-log-done t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
       ("STARTED" . "yellow")
       ("CANCELED" . (:foreground "blue" :weight bold))
       ("DONE" . "green")))
(provide 'myorg)
;;; myorg.el ends here
