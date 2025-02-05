;;; keymaps.el --- keymap settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)
    (forward-line 1)))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 0)
    (open-line -1)
    (forward-line -1)))

(keymap-global-set "M-n" 'insert-line-below)
(keymap-global-set "M-p" 'insert-line-above)

(provide 'keymaps)
;;; keymaps.el ends here
