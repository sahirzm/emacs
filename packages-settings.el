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
(require 'autopair)
(autopair-global-mode)
(electric-pair-mode t)

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
(projectile-global-mode)

;;; packages-settings ends here
