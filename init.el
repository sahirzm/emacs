;;; init.el -- My Emacs config

;;; Commentary:

;;; Code:

(setq debug-on-error t)
;; disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; disable menu-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; disable tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; check connected to internet
(defvar my-onlinep)
(setq my-onlinep nil)
(unless
    (condition-case nil
        (delete-process
         (make-network-process
          :name "my-check-internet"
          :host "elpa.gnu.org"
          :service 80))
      (error t))
  (setq my-onlinep t))

;; list the packages you want
(defvar package-list)
(setq package-list '(auto-complete
		     highlight-indentation
		     smex
		     autopair
		     flycheck
		     xcscope
		     helm
		     powerline
		     magit
		     projectile
		     helm-projectile
		     gruvbox-theme
		     color-theme-sanityinc-solarized
		     linum-relative
		     php-mode
		     yasnippet
		     java-snippets
		     angular-snippets
		     django-snippets
		     restclient
		     neotree
		     disable-mouse
		     expand-region
		     fill-column-indicator
		     highlight-indent-guides
		     emmet-mode
		     web-mode
		     web-beautify
		     atom-one-dark-theme
		     dracula-theme
		     docker-compose-mode
		     dockerfile-mode
		     yaml-mode
		     json-mode
		     js2-mode
		     all-the-icons))

(when my-onlinep
    ;; list the repositories containing them
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "https://marmalade-repo.org/packages/")
			   ("elpa" . "http://tromey.com/elpa/")))

    ;; activate all the packages (in particular autoloads)
    (package-initialize)

    ;; fetch the list of packages available
    (unless package-archive-contents
    (package-refresh-contents))

    ;; install the missing packages
    (dolist (package package-list)
	(unless (package-installed-p package)
	    (package-install package))))

(add-hook 'after-init-hook '(lambda()
			      ;; load all package related settings
			      (load "~/.emacs.d/packages-settings")
			      (load "~/.emacs.d/my-custom-settings")
			      (load "~/.emacs.d/my-custom-bindings")))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (dracula-theme atom-one-dark-theme fill-column-indicator web-mode php-mode xcscope smex projectile powerline magit linum-relative highlight-indentation helm gruvbox-theme flycheck autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
