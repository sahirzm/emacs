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
(setq package-list '(company
		     highlight-indentation
		     highlight-indent-guides
		     smex
		     flycheck
		     xcscope
		     helm
		     powerline
		     airline-themes
		     magit
		     projectile
		     helm-projectile
		     gruvbox-theme
		     color-theme-sanityinc-solarized
		     php-mode
		     yasnippet
		     java-snippets
		     angular-snippets
		     django-snippets
		     restclient
		     treemacs
		     treemacs-projectile
		     disable-mouse
		     expand-region
		     fill-column-indicator
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
		     rjsx-mode
		     kotlin-mode
		     prettier-js
		     exec-path-from-shell
		     all-the-icons))

(when my-onlinep
    ;; list the repositories containing them
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
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
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq custom-file "~/.emacs.d/custom.el")
(add-hook 'after-init-hook '(lambda()
			      ;; load all package related settings
			      (load "~/.emacs.d/custom")
			      (load "~/.emacs.d/packages-settings")
			      (load "~/.emacs.d/my-custom-settings")
			      (load "~/.emacs.d/my-custom-bindings")))


(provide 'init)
;;; init.el ends here
