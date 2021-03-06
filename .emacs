;; load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")

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
(setq-default cursor-type 'box)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(add-hook 'window-setup-hook '(lambda () (set-cursor-color "#00bfff")))
(add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color "#00bfff"))))

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

;; python config
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(global-set-key (kbd "M-.") 'anaconda-mode-find-definitions)
(global-set-key (kbd "M-,") 'anaconda-mode-find-assignments)
(global-set-key (kbd "M-r") 'anaconda-mode-find-references)

;; (require 'blacken)
;; (add-hook 'python-mode-hook 'blacken-mode)

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

;; ansi-color for m-x compile
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; confluence
(require 'ox-confluence)

;; org-mode
(require 'org)
(setq org-default-notes-file "/home/evalette/Documents/orga/agenda/notes.org")

(setq org-log-done 'time)

;; rust config
(require 'eglot)
(require 'rustic)
(require 'company)
(setq rustic-lsp-client 'eglot)
(setq rustic-lsp-server 'rust-analyzer)
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(add-hook 'rustic-mode-hook 'company-mode)

(with-eval-after-load 'rustic-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  )
(setq rustic-format-on-save t)


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
 '(org-agenda-files
   (quote
    ("~/Documents/orga/agenda/notes.org" "~/Documents/orga/agenda/work.org")))
 '(package-selected-packages
   (quote
    (eglot telephone-line expand-region magit flycheck counsel-projectile comint-better-defaults better-defaults zenburn-theme yaml-mode markdown-mode org-mode iedit virtualenvwrapper anaconda-mode rust-mode flycheck-rust avy blacken))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
