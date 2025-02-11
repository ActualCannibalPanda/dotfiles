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

(setq packages
      (quote (; theme
	      catppuccin-theme
	      ; font
  	      fira-code-mode
	      ; completion
	      company
	      ; vcs
	      magit
	      ; features
	      lsp-mode
	      company
	      company-jedi
	      flycheck
	      lsp-treemacs
	      rainbow-delimiters
	      ; langs
	      rust-mode
	      cargo
	      pipenv
	      )))
		     
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

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

(with-eval-after-load 'company
  (define-key
   company-active-map
   (kbd "M-/") 'company-complete)
  (define-key
   company-active-map
   (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key
   company-active-map
   (kbd "<backtab>")
   (lambda ()
     (interactive)
     (company-complete-common-or-cycle -1))))

;; =============================================
;; c
;; =============================================
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

;; =============================================
;; rust
;; =============================================
(add-hook 'rust-mode-hook
	  (lambda ()
	    (setq rust-format-on-save t)
	    (setq indent-tabs-mode nil)
	    (prettify-symbols-mode)))

(use-package rust-mode
  :config
  (setq rust-mode-treesitter-derive t))

(add-hook 'rust-mode-hook 'cargo-minor-mode)


;; =============================================
;; python
;; =============================================
(use-package pipenv
  :hook
  (python-mode . pipenv-mode))

(use-package company-jedi
  :hook
  (python-mode .(lambda ()
		  (add-to-list 'company-backends 'company-jedi))))

;; =============================================
;; company options
;; =============================================
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay
	(lambda ()
	  (if (company-in-string-or-comment)
	      nil
	    0)))

;; =============================================
;; eglot options
;; =============================================
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("jedi-language-server")))
  (add-to-list 'eglot-stay-out-of 'flymake))

(setq-default eglot-workspace-configuration
              '(:completions
                (:completeFunctionCalls t)))

(add-hook 'python-mode-hook 'eglot-ensure)

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
