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

(require 'better-defaults)

;; flycheck
(global-flycheck-mode 1)

;; theming
(load-theme 'challenger-deep t)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)

;; magit
(require 'magit)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g l") 'magit-log-buffer-file)
(global-set-key (kbd "M-g t") 'git-timemachine)


;; projectile
(require 'projectile)
(projectile-global-mode)

(global-set-key (kbd "M-p f") 'projectile-find-file)
(global-set-key (kbd "M-p s") 'projectile-grep)
(global-set-key (kbd "M-p p") 'projectile-switch-project)

;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(counsel-projectile-mode 1)

;; nlinum
;; (require 'nlinum)
;; (global-nlinum-mode 1)
