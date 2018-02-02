;;; my-custom-settings.el --- my settings
;;; Commentary:
;;; Code:
;; some of the settings have been copied from http://aaronbedra.com/emacs.d/. Thanks for Sharing

;; disable splash screen
(setq inhibit-splash-screen t)
;; set blank scratch message
(setq initial-scratch-message nil)

;; disable bell soun
(setq visible-bell 1)

;; set evil mode to default
;; (evil-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set Source code pro font
(set-frame-font "Source Code Pro 13")
;; disable backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; highlight current line in buffer
(global-hl-line-mode 1)

;; set tab width to 8
(setq-default tab-width 8)

;; load solarized theme
;;(require 'color-theme-sanityinc-solarized)
;;(color-theme-sanityinc-solarized--define-theme light)

;; load theme gruvbox
;;(load-theme 'gruvbox t)
;;(load-theme 'atom-one-dark t)
;;(load-theme 'spacemacs-dark)
(load-theme 'dracula)
;; set my name and email
(setq user-full-name "Sahir Maredia")
(setq user-mail-address "sahirzm@gmail.com")

;; settings for highlight and replace
(delete-selection-mode t)
(transient-mark-mode t)

;; sync both clipboards
(setq select-enable-clipboard t)

;; change yes-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; enable line number
(setq column-number-mode t)
(global-linum-mode t)

;; web-mode enable by default for following files
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; remove {} auto pairing in electric-pair-pairs for web-mode
(add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local electric-pair-inhibit-predicate
                  (lambda (c)
                    (if (char-equal c ?{) t (electric-pair-default-inhibit c))))))

(provide 'my-custom-settings)
;;; my-custom-settings ends here
