(require 'package)

(setq inhibit-startup-screen t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("ELPA"   . "http://tromey.com/elpa/")
	("gnu"    . "http://elpa.gnu.org/packages/")
	("melpa"  . "https://melpa.org/packages/")
	("org"    . "https://orgmode.org/elpa/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(setq package-selected-packages
      (quote (; theme
	      catppuccin-theme
	      ; font
	      fira-code-mode
	      ; completion
	      company
	      ; vcs
	      magit
	      ; langs
	      lsp-mode
	      rust-mode
	      cargo
	      )))
		     
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile (require 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(add-hook 'after-init-hook
	  (lambda ()
	    (setq magit-define-global-key-bindings "recommended")))

(setq files-to-load '("config"))

(dolist (file files-to-load)
  (add-to-list 'load-path
	       (concat user-emacs-directory file)))

(load "theme")
(load "font")
(load "keymaps")
(load "langs")
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
