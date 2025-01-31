(require 'package)

(setq inhibit-startup-screen t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("ELPA"  . "http://tromey.com/elpa/")
	("gnu"   . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org"   . "https://orgmode.org/elpa/")))

(package-initialize)

(setq package-selected-packages
      (quote (; theme
	      cyberpunk-theme
	      ; completion
	      company
	      ; vcs
	      magit
	      ; langs
	      rust-mode
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
	    (load-theme 'cyberpunk t)
	    (setq magit-define-global-key-bindings "recommended")))

(setq files-to-load '("config"))

(dolist (file files-to-load)
  (add-to-list 'load-path
	       (concat user-emacs-directory file)))

(load "keymaps")
(load "langs")
