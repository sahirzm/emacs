;;; packages-settings.el -- Settings related to packages installed

;;; Commentary:
;;; Code:
;; ~/.emacs.d/packages-settings.el

;; settings for highlight-indentation
(require 'highlight-indentation)
(highlight-indentation-mode)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;; settings for auto-complete
(require 'auto-complete)
(ac-config-default)

;; settings for org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode)))

;; settings related to smex
(require 'smex)
(defvar smex-save-file)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; settings related to ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; settings for autopair
;;(require 'autopair)
;;(autopair-global-mode)
;;(electric-pair-mode t)

;; settings for flycheck
(global-flycheck-mode)

;; cscope
(require 'xcscope)

;; helm mode
(require 'helm)
(require 'helm-config)

;; powerline
(require 'powerline)
(powerline-center-theme)

;; relative line numbers
(require 'linum-relative)
(linum-relative-on)

;; projectile for managing projects
(projectile-mode)

;; ya snippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'php-mode-hook #'yas-minor-mode)

;; enable web-mode and php-mode
(require 'web-mode)
(require 'php-mode)

;; neotree settings
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; disable mouse interactions
(require 'disable-mouse)
(global-disable-mouse-mode)

;; expand region on pressing Ctrl + =
(require 'expand-region)
(global-set-key (kbd "C-?") 'er/expand-region)

;; plugin to show column line at 120
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(add-hook 'prog-mode-hook 'fci-mode)
(setq fci-rule-column 80)
(setq fci-rule-color "yellow")

;; plugin to show indent guide lines
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; emmet mode for html and css
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)

;;; packages-settings ends here
