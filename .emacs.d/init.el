;;; init --- Emacs initialisation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)

(setq inhibit-startup-screen t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

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
	      ; vcs
	      magit
	      ; features
	      flycheck
	      rainbow-delimiters
	      ; langs
	      cmake-ide
	      rust-mode
	      cargo
	      pipenv
	      ))))
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(setq quelpa-update-melpa-p nil)
(setq quelpa-checkout-melpa-p nil)

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
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")))
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
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)

(use-package cmake-ide
  :config
  (cmake-ide-setup))

;; =============================================
;; rust
;; =============================================
(use-package rust-mode
  :config
  (setq rust-mode-treesitter-derive t)
  :hook ((rust-mode . cargo-minor-mode)
	 (rust-mode . eglot-ensure)
	 (rust-mode . (lambda ()
	    (setq rust-format-on-save t)
	    (setq indent-tabs-mode nil)
	    (prettify-symbols-mode)))))

;; =============================================
;; company options
;; =============================================
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.1
	company-show-numbers t
	company-tooltip-idle-delay 0.1
	company-tooltip-limit 20
	company-require-match nil
	company-frontends '(company-pseudo-tooltip-frontend
			    company-preview-frontend
			    company-echo-metadata-frontend)
	company-backends '(company-capf))
  :bind (("M-/" . company-complete)
	 :map company-active-map
	 ("TAB" . company-complete-common-or-cycle)
	 ("<backtab>" .
	  (lambda ()
	    (interactive)
	    (company-complete-common-or-cycle -1)))
	 ("C-SPC" . company-complete-common)))

;; =============================================
;; python
;; =============================================
(use-package python
  :hook ((python-mode . eglot-ensure)))

(use-package pipenv
  :hook
  (python-mode . pipenv-mode))

(with-eval-after-load 'company
  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi)))

;; =============================================
;; eglot options
;; =============================================
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '((python-mode . ("jedi-language-server"))
		 (rust-mode . ("rustup" "run" "stable" "rust-analyzer" :initializationOptions
			       (:check (:command "clippy"))))
		 ((c++-mode c++-ts-mode) . ("clangd"))
		 ((c-mode c-ts-mode) . ("clangd"))))
  (add-to-list 'eglot-stay-out-of 'flymake))

;; =============================================
;; flycheck
;; =============================================
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
