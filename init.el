(setq inhibit-startup-message t ; Don't show the splash screen
      visible-bell t)           ; Flash when the bell rings

(menu-bar-mode -1)              ; no menu bar
(tool-bar-mode -1)              ; no toolbar
(scroll-bar-mode -1)            ; no scrollbar

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; set firacode font
(set-face-attribute 'default nil :font "FiraCode Nerd Font")

;; Display line numbers in all buffers
(global-display-line-numbers-mode 1)

;; ensure to t for all packages
(setq use-package-always-ensure t)

;;treemacs
(use-package treemacs
  :ensure t
  :defer t)

;; make emacs write custom configuration to separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

;; remember last edited file
(recentf-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; disable backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; highlight current line in buffer
(global-hl-line-mode 1)

(setq user-full-name "Sahir Maredia"
      user-mail-address "sahirzm@gmail.com")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; fill-column
(setq-default fill-column 120)
(setq-default display-fill-column-indicator t)
(setq-default display-fill-column-indicator-character 9474)

;; sync both clipboards
(setq select-enable-clipboard t)

;; evil mode
(use-package evil
  :config
  (evil-mode 1))


