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

;; built-ins config
(electric-pair-mode 1)
(setq-default show-trailing-whitespace t)


(require 'better-defaults)

;; flycheck
(require 'flycheck)

(setq flycheck-flake8-maximum-line-length 120)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-flycheck-mode 1)

;; theming
(load-theme 'badwolf t)
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

;; telephone line
(require 'telephone-line)

;;
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

(require 'iedit)

;; anaconda mode
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(global-set-key (kbd "M-.") 'anaconda-mode-find-definitions)
(global-set-key (kbd "M-,") 'anaconda-mode-find-assignments)
(global-set-key (kbd "M-r") 'anaconda-mode-find-references)

;; venv
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support


(telephone-line-mode t)
(set-face-attribute 'mode-line nil :background "#000000")
