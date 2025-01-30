(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("ELPA"  . "http://tromey.com/elpa/")
	("gnu"   . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org"   . "https://orgmode.org/elpa/")))

(setq package-selected-packages
      (quote ( ; theme
	cyberpunk-theme)))
		     
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'cyberpunk t)))

(load-file "./config/config.el")
