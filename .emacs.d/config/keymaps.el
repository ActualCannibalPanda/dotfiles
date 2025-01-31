(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)
    (next-line)))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 0)
    (open-line 1)
    (previous-line)))

(keymap-global-set "M-n" 'insert-line-below)
(keymap-global-set "M-p" 'insert-line-above)

