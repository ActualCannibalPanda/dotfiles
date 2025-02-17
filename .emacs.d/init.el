;;; init --- Emacs initialisation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)

(setq inhibit-startup-screen t)

(setq load-prefer-new t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq package-install-upgrade-built-in t)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("ELPA"      . "http://tromey.com/elpa/")
	("gnu"       . "http://elpa.gnu.org/packages/")
	("melpa"     . "https://melpa.org/packages/")
	("org"       . "https://orgmode.org/elpa/")
	("gnu-devel" . "https://elpa.gnu.org/devel/")
	("nongnu"    . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
		     
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(let ((packages
      (quote (; theme
	      catppuccin-theme
	      ; font
  	      fira-code-mode
	      ; completion
	      company
	      company-jedi
	      yasnippet
	      ; lsp
	      lsp-mode
	      lsp-ui
	      ; vcs
	      magit
	      ; features
	      flycheck
	      rainbow-delimiters
	      ; langs
	      powershell
	      cmake-ide
	      rustic
	      cargo
	      pipenv
	      ))))
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(defvar quelpa-update-melpa-p nil)
(defvar quelpa-checkout-melpa-p nil)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(eval-when-compile (require 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(use-package siege-mode
  :ensure quelpa
  :quelpa (siege-mode :repo "tslilc/siege-mode" :fetcher github)
  :hook (prog-mode . siege-mode))

(use-package treesit-auto
  :ensure quelpa
  :demand t
  :quelpa (treesit-auto :repo "renzmann/treesit-auto" :fetcher github)
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
	  (powershell "https://github.com/airbus-cert/tree-sitter-powershell.git")))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(add-hook 'after-init-hook
	  (lambda ()
	    (setq magit-define-global-key-bindings "recommended")))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; =============================================
;; theme
;; =============================================
(add-hook 'after-init-hook
	  (lambda ()
	    (setq catppuccin-flavor 'mocha)
	    (load-theme 'catppuccin :no-confirm)))

;; =============================================
;; font
;; =============================================
(use-package fira-code-mode
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

(keymap-global-set "M-n" 'insert-line-below)
(keymap-global-set "M-p" 'insert-line-above)

;; =============================================
;; c
;; =============================================
(use-package cmake-ide
  :config
  (cmake-ide-setup))

;; =============================================
;; rust
;; =============================================
(use-package rust-mode
  :ensure t
  :config
  (rustic-mode))

(use-package rustic
  :ensure t
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

(defun my/rustic-mode-hook ()
  "A hook for rustic-mode."
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; =============================================
;; lsp mode options
;; =============================================
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; =============================================
;; company options
;; =============================================
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.5)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
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
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "::" t nil))))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand))
	    (if (check-expansion)
		(company-complete-common)
	      (indent-for-tab-command))))))

;; =============================================
;; yasnippet options
;; =============================================
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  :hook
  '((prog-mode . yas-minor-mode)
    (text-mode . yas-minor-mode)))

;; =============================================
;; python
;; =============================================
(use-package pipenv
  :hook
  (python-mode . pipenv-mode))

(with-eval-after-load 'company
  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi)))

;; =============================================
;; flycheck
;; =============================================
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; =============================================
;; powershell
;; =============================================
(use-package powershell
  :hook
  '((powershell-mode . powershell-ts-mode)))

;; ==============================================
;; org-mode
;; ==============================================
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-contrib org catppuccin-theme company magit rust-mode cargo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
