;;; init --- Emacs initialisation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq inhibit-startup-screen t)

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca-no-symlink-mode)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(use-package expand-region
  :config
  (require 'expand-region)
  (keymap-global-set "C-=" 'er/expand-region))

(use-package change-inner
  :config
  (require 'change-inner)
  (keymap-global-set "M-i" 'change-inner)
  (keymap-global-set "M-o" 'change-outer))

(use-package siege-mode
  :ensure (siege-mode :fetcher github :repo "tslilc/siege-mode")
  :hook (prog-mode . siege-mode))

(use-package treesit-auto
  :demand t
  :ensure (treesit-auto :repo "renzmann/treesit-auto" :fetcher github)
  :init
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
	  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (lua "https://github.com/Azganoth/tree-sitter-lua")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (odin "https://github.com/ap29600/tree-sitter-odin")
	  (powershell "https://github.com/airbus-cert/tree-sitter-powershell.git")))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-auto-enabled nil
	highlight-indent-guides-auto-odd-face "darkgray"
	highlight-indent-guides-auto-even-face "dimgray"
	highlight-indent-guides-auto-character-face "white"))

(use-package multiple-cursors
  :config
  (require 'multiple-cursors)
  :bind (("C-c C-." . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

;; =============================================
;; theme
;; =============================================
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

;; =============================================
;; magit
;; =============================================
(use-package transient)
(use-package magit
  :after transient
  :init
  (setq magit-define-global-key-bindings "default")

  :config
  ;; This is to prevent company from overidding the TAB keybind
  (add-hook 'magit-status-mode-hook
	    (lambda ()
	      (company-mode -1)))
  
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

;; =============================================
;; font
;; =============================================
(use-package fira-code-mode
  :custom
  (fira-code-mode-disabled-ligatures '("x" "[]"))
  :config
  (global-fira-code-mode)
  (fira-code-mode-set-font)
  :hook prog-mode)

;; =============================================
;; keymaps
;; =============================================
(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (open-line 1)
  (forward-line 1))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (remove-if-not 'buffer-file-name (buffer-list)))))

(defadvice ibuffer
    (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name."
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-x M-b" 'kill-other-buffers)
(keymap-global-set "M-n" 'insert-line-below)
(keymap-global-set "M-p" 'insert-line-above)

;; =============================================
;; c
;; =============================================
(use-package cmake-ide
  :config
  (cmake-ide-setup))

;; =============================================
;; elisp
;; =============================================
(defun my/emacs-lisp-mode-hook ()
  "My hook for elisp mode."
  (define-key emacs-lisp-mode-map (kbd "C-S-b") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)

;; =============================================
;; rust
;; =============================================
(use-package rust-mode
  :hook
  rustic-mode)

(defun my/rustic-mode-hook ()
  "A hook for rustic-mode."
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package rustic
  :bind (:map rustic-mode-map
	      ("M-j" . lsp-ui-menu)
	      ("M-?" . lsp-find-references)
	      ("C-c C-c l" . flycheck-list-errors)
	      ("C-c C-c a" . lsp-execute-code-action)
	      ("C-c C-c r" . lsp-rename)
	      ("C-c C-c q" . lsp-workspace-restart)
	      ("C-c C-c Q" . lsp-workspace-shutdown)
	      ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  :hook
  (add-hook 'rustic-mode-hook 'my/rustic-mode-hook))

;; =============================================
;; lsp mode options
;; =============================================
(with-eval-after-load 'lsp-mode
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
	 (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
			 (fboundp 'json-parse-buffer))
                  'json-parse-buffer
		'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
	orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)

  :hook
  (prog-mode . lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-side 'right)
  (lsp-ui-doc-delay 0.7)
  (lsp-ui-doc-show-with-cursor t)
  (custom-set-faces '(markdown-code-face ((t (:inherit default))))))

(use-package lsp-pyright
  :after lsp-mode)

;; =============================================
;; company options
;; =============================================
(use-package company
  :custom
  (company-idle-delay 0.5)
  (company-backends
	'((company-capf :with company-yasnippet)
	  (company-clang :with company-yasnippet)
	  (company-files :with company-yasnippet)
          (company-yasnippet)))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  :bind (:map company-mode-map
	      ("M-/" . company-complete)
	      ("<tab>" . tab-indent-or-complete)
	      ("TAB" . tab-indent-or-complete)
	 :map company-active-map
	 ("TAB" . company-complete-common-or-cycle)
	 ("<backtab>" .
	  (lambda ()
	    (interactive)
	    (company-complete-common-or-cycle -1)))
	 ("C-SPC" . company-complete-common)))

(defun company-yasnippet-or-completion ()
  "Either expand a snippet or do a completion."
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  "Check if can expand."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  "Do a yasnipped expansion."
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "Either indent or complete."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	    (if (check-expansion)
		(company-complete-common)
	      (indent-for-tab-command)))))

;; =============================================
;; yasnippet options
;; =============================================
(use-package yasnippet
  :config
  (yas-reload-all)
  (setq yasnippets-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  :bind (:map yas-keymap
	      ("C-." . yas-next-field-or-maybe-expand)
	      ("C-," . yas-prev-field))
  :hook
  '((prog-mode . yas-minor-mode)
    (text-mode . yas-minor-mode)))

;; =============================================
;; zig
;; =============================================
(use-package reformatter)
(use-package zig-mode)

;; =============================================
;; python
;; =============================================
(use-package elpy
  :bind (("C-c C-b" . elpy-shell-send-buffer)
	 ("C-c g C-b" . elpy-shell-send-buffer-and-go)
	 ("C-c C-d" . elpy-shell-send-defun)
	 ("C-c s C-d" . elpy-shell-send-defun-and-step)
	 ("C-c g C-d" . elpy-shell-send-defun-and-go)
	 ("C-c C-c" . elpy-shell-send-declass)
	 ("C-c s C-c" . elpy-shell-send-declass-and-step)
	 ("C-c g C-c" . elpy-shell-send-declass-and-go)
	 ("C-c C-f" . elpy-shell-send-file)
	 ("C-c g C-f" . elpy-shell-send-file-and-go))
  :custom
  (setq python-indent-offset 4)
  :hook
  (((python-mode python-ts-mode) .
    (lambda ()
      (elpy-enable)
      (elpy-mode)))))

(use-package company-jedi
  :after company
  :config
  (add-to-list 'company-backends 'company-jedi))

;; =============================================
;; flycheck
;; =============================================
(use-package flycheck
  :init (global-flycheck-mode))

;; =============================================
;; powershell
;; =============================================
(use-package powershell
  :hook
  '((powershell-mode . powershell-ts-mode)))

;; =============================================
;; clojure
;; =============================================
(use-package clojure-mode
  :hook
  '(((clojure-mode clojurescript-mode clojuer-c-mode) . lsp)
    ((clojure-mode clojurescript-mode clojuer-c-mode) .
     (lambda ()
       (setq-local lsp-enable-completion-at-point nil
		   lsp-enable-indentation nil)))))

;; ==============================================
;; org-mode
;; ==============================================
(use-package org
  :config
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
	  ("DONE" . "green"))))

(use-package org-contrib
  :after org)

(use-package inf-clojure)
(use-package cider)

(use-package undo-fu
  :config
  (keymap-global-unset "C-z")
  (keymap-global-set "C-z" 'undo-fu-only-undo)
  (keymap-global-set "C-S-z" 'undo-fu-only-redo))

(use-package vundo)

(use-package helm
  :after async
  :config
  (keymap-global-set "M-x" 'helm-M-x))

(use-package helm-lsp
  :after '(lsp-mode helm)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package async
  :ensure (:host github :repo "jwiegley/emacs-async" :main "async.el")
  :config
  (dired-async-mode 1))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	      ("-" . dired-up-directory)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
