;;; init --- Emacs initialisation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)

(setq inhibit-startup-screen t)

(setq load-prefer-new t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq use-package-always-ensure t)
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
	      highlight-indent-guides
	      multiple-cursors
	      ; langs
	      powershell
	      cmake-ide
	      rustic
	      cargo
	      pipenv
	      clojure-mode
	      cider
	      inf-clojure
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

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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

;; =============================================
;; theme
;; =============================================
(add-hook 'after-init-hook
	  (lambda ()
	    (setq catppuccin-flavor 'mocha)
	    (load-theme 'catppuccin :no-confirm)))

;; =============================================
;; magit
;; =============================================
(use-package magit
  :init
  (setq magit-define-global-key-bindings "recommended")
  :config
  ;; This is to prevent company from overidding the TAB keybind
  (add-hook 'magit-status-mode-hook
	    (lambda ()
	      (company-mode -1))))

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

(defun my/rustic-mode-hook ()
  "A hook for rustic-mode."
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; =============================================
;; lsp mode options
;; =============================================
(use-package lsp-mode
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
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

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
	  (company-rust :with company-yasnippet)
	  (company-emacs-lisp :with company-yasnippet)
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
	      ("C-," . yas-prev))
  :hook
  '((prog-mode . yas-minor-mode)
    (text-mode . yas-minor-mode)))

;; =============================================
;; python
;; =============================================
(use-package pipenv
  :hook
  ((python-mode python-ts-mode) . pipenv-mode))

(with-eval-after-load 'company
  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi)))

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
   '(multiple-cursors mutiple-cursors inf-clojure cider clojure-mode highlight-indent-guides org-contrib org catppuccin-theme company magit rust-mode cargo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
