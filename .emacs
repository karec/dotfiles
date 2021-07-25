(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blacken-line-length 120 t)
 '(custom-safe-themes
   '("3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" default))
 '(package-selected-packages
   '(cargo rust-mode lsp-ivy lsp-ui blacken org-mode anaconda-eldoc-mode use-package zenburn-theme yasnippet yaml-mode virtualenvwrapper telephone-line pos-tip magit iedit golden-ratio flycheck-rust expand-region dash-functional counsel-projectile company comint-better-defaults better-defaults avy anaconda-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; built-in config

;; others
(setq custom-safe-themes t)
(setq-default show-trailing-whitespace t)
(electric-pair-mode 1)
(global-set-key (kbd "M-/") 'hippie-expand)

;; theming
(setq-default cursor-type 'box)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(global-display-line-numbers-mode)
(add-hook 'window-setup-hook '(lambda () (set-cursor-color "#00bfff")))
(add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color "#00bfff"))))

;; font
;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; mode-line
(setq column-number-mode t)
(setq-default mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-client
        mode-line-modified
        ""
        mode-line-directory
        mode-line-buffer-identification
        " "
        mode-line-position
        " "
        (moody-tab (vc-mode 1) nil 'up)
        vc-mode
        (flycheck-mode flycheck-mode-line)
        " "
        mode-line-modes
        mode-line-misc-info
        mode-line-end-spaces))

;; backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; kill-line
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))


;; Load MELPA
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(require 'use-package)

;; better defaults
(use-package better-defaults
  :ensure t)

;; theme
(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn))

;; magit
(use-package magit
  :ensure t
  :bind (("M-g s" . magit-status)
         ("M-g b" . magit-blame)
         ("M-g l" . magit-log-buffer-file)))

;; ivy
(use-package ivy
  :ensure t
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; projectile
(use-package projectile
  :ensure t
  :bind (("M-p f" . projectile-find-file)
         ("M-p s" . counsel-projectile-rg)
         ("M-p p" . projectile-switch-project))
  :config (projectile-mode))

;; counsel
(use-package counsel
  :ensure t
  :config (counsel-mode 1))

;; counsel projectile
(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode 1))

;; expand-regsion
(use-package expand-region
  :ensure t
  :bind (("C-'" . er/mark-inside-quotes)
         ("C-\"" . er/mark-outside-quotes)
         ("C-}" . er/mark-inside-pairs)))

;; iedit
(use-package iedit
  :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1)
  (setq flycheck-flake8-maximum-line-length 120)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (global-set-key (kbd "M-.") 'anaconda-mode-find-definitions)
  (global-set-key (kbd "M-,") 'anaconda-mode-find-assignments)
  (global-set-key (kbd "M-r") 'anaconda-mode-find-references))

(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save)
  (setq py-isort-options '("-sl")))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 120))

;; telephone line
(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode t)
  (defface git-face '((t (:foreground "black" :background "#6495ed"))) "")

  (setq telephone-line-faces
        '((git . (git-face . telephone-line-accent-inactive))
          (evil . telephone-line-evil-face)
          (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
          (nil . (mode-line . mode-line-inactive))))

  (setq telephone-line-lhs
        '((git   . (telephone-line-vc-segment))
          (accent . (telephone-line-flycheck-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (accent   . (telephone-line-airline-position-segment))))

  (setq telephone-line-primary-right-separator 'telephone-line-abs-left
        telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t))

;; avy
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)))

;; ansi-color
(use-package ansi-color
  :ensure t
  :hook ((compilation-filter-hook . colorize-compilation-buffer))
  :config
  (defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))))

;; org-mode
(use-package org
  :ensure t
  :config
  (setq org-default-notes-file "~/Documents/orga/agenda/notes.org")
  (setq org-log-done 'time))

;; company
(use-package company
  :ensure t
  :hook ((rust-mode . company-mode)
         (anaconda-mode . company-mode))
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))


;; yas
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((rust-mode . lsp))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references))
  :hook (rust-mode . lsp)
  :config (setq rust-format-on-save t)
          (add-hook 'rust-mode-hook
                    (lambda () (setq indent-tabs-mode nil))))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
