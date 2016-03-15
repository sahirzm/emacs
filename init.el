;; disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; disable menu-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; disable tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; list the packages you want
(setq package-list '(color-theme-sanityinc-tomorrow monokai-theme))

;; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;; after everything is loaded load these so that they are not overriden by others
(add-hook 'after-init-hook '(lambda()
			      ;; load all package related settings
			      (load "packages-settings")
			      (load "my-custom-settings")
			      (load "my-custom-bindings")))
