;; pacakges
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))


;; dot for graphviz
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;; built-ins config
(electric-pair-mode 1)
(setq-default show-trailing-whitespace t)
(global-display-line-numbers-mode)
(global-set-key (kbd "M-/") 'hippie-expand)


(require 'better-defaults)

;; flycheck
(require 'flycheck)
(global-flycheck-mode 1)
(setq flycheck-flake8-maximum-line-length 120)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; theming
(load-theme 'zenburn t)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)

;; magit
(require 'magit)
(global-set-key (kbd "M-g b") 'magit-blame)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g l") 'magit-log-buffer-file)
(global-set-key (kbd "M-g t") 'git-timemachine)


;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(counsel-projectile-mode 1)
(counsel-mode 1)

;; projectile
(require 'projectile)
(projectile-global-mode)

(global-set-key (kbd "M-p f") 'projectile-find-file)
(global-set-key (kbd "M-p s") 'counsel-projectile-rg)
(global-set-key (kbd "M-p p") 'projectile-switch-project)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/mark-inside-quotes)
(global-set-key (kbd "C-\"") 'er/mark-outside-quotes)
(global-set-key (kbd "C-}") 'er/mark-inside-pairs)

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

;; iedit
(require 'iedit)

;; backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; anaconda mode
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(global-set-key (kbd "M-.") 'anaconda-mode-find-definitions)
(global-set-key (kbd "M-,") 'anaconda-mode-find-assignments)
(global-set-key (kbd "M-r") 'anaconda-mode-find-references)

;; venv
(require 'virtualenvwrapper)
(setq projectile-switch-project-action 'venv-projectile-auto-workon)

;; kill-line
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; telephone line
(require 'telephone-line)

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
      telephone-line-evil-use-short-tag t)

(telephone-line-mode t)

;; avy
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)


;; golden ratio
(require 'golden-ratio)
(golden-ratio-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(electric-pair-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Documents/orga/agenda.org")))
 '(package-selected-packages
   (quote
    (golden-ratio minions use-package hcl-mode zenburn-theme rust-mode virtualenvwrapper anaconda-mode git-timemachine iedit sml-mode markdown-mode yaml-mode telephone-line expand-region magit flycheck counsel-projectile comint-better-defaults better-defaults badwolf-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
