;;; packages-settings.el -- Settings related to packages installed

;;; Commentary:
;;; Code:
;; ~/.emacs.d/packages-settings.el

;; settings for highlight-indentation
(require 'highlight-indentation)
(highlight-indentation-mode)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

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
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; settings related to ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; settings for autopair
(electric-pair-mode t)

;; settings for flycheck
(require 'flycheck)
(global-flycheck-mode)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; cscope
(require 'xcscope)

;; helm mode
(require 'helm)
(require 'helm-config)

;; powerline
(require 'powerline)
(powerline-center-theme)
;; airline-theme
(require 'airline-themes)
(load-theme 'airline-gruvbox-dark t)

;; projectile for managing projects
(require 'projectile)
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'helm-projectile)
(helm-projectile-on)

;; ya snippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'php-mode-hook #'yas-minor-mode)

;; enable web-mode and php-mode
(require 'web-mode)
(require 'php-mode)

;; treemacs
(require 'treemacs)
(require 'treemacs-projectile)

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
(setq fci-rule-column 120)
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

;; web-beautify settings
(require 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(add-hook 'js-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
	     (lambda ()
	       (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

(require 'docker-compose-mode)
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "C-m" 'newline-and-indent)))

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; highlight-indent-guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; prettier-js
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;;; packages-settings ends here
