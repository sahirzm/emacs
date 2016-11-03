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
		     gruvbox-theme
		     linum-relative))

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
