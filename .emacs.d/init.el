(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("ELPA"  . "http://tromey.com/elpa/")
	("gnu"   . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org"   . "https://orgmode.org/elpa/")))

(setq package-selected-packages
      (quote (; theme
	      cyberpunk-theme
	      ; completion
	      company
	      ; vcs
	      magit
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

(load-file "./config/config.el")
