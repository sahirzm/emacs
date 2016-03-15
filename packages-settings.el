
;;; Code:
;; ~/.emacs.d/packages-settings.el

;; settings for indent-guide
;;(require 'indent-guide)
;;(indent-guide-global-mode)

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
(defvar smex-save-file)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; settings related to ido
(ido-mode t)

(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; settings for autopair
(require 'autopair)
(autopair-global-mode 1)

;; settings for ansi-colors
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; settings for flycheck
(global-flycheck-mode)

;; cscope
(require 'xcscope)

;; helm mode
(require 'helm-config)

;; smart-mode-line
(require 'smart-mode-line)
;; set patched font
(set-default-font "Source Code Pro for Powerline Medium 13")
(setq sml/theme 'powerline)
(sml/setup)

;;; packages_settings.el ends here
