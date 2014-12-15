;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; I had to disable marmalade because it was conflicting with melpa
;; and breaking cider, which I have to have:
;;(add-to-list 'package-archives
;;    '("marmalade" .
;;      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
; initial start won't work without this (package-refresh-contents)
(defvar my-packages '(ac-nrepl
		      cider
		      clojure-mode
		      clojure-test-mode
		      evil
		      zenburn-theme
		      projectile
		      coffee-mode
		      flx-ido
		      ido-ubiquitous
		      helm
		      helm-projectile
		      evil-tabs
		      flycheck
		      magit
		      puppet-mode
		      slamhound
		      markdown-mode
		      multi-term
		      helm-pydoc
		      rainbow-delimiters))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'evil)
(require 'cider)
(require 'zenburn-theme)
(require 'flx-ido)
(require 'ido-ubiquitous)
(require 'whitespace)
(require 'coffee-mode)
(require 'clojure-mode)
(require 'projectile)
(require 'helm)
(require 'helm-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-use-faces nil) ;; to see flx highlights
(setq org-agenda-files (list "~/org/agenda.org"))

(setq inhibit-startup-message t)
(setq scroll-margin 0 ;; can't really tell a difference with this
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

; don't let next-line add new lines at end of file
(setq-default next-line-add-newlines nil)

; make edited files end with a carriage return
(setq-default require-final-newline t)

(setq-default js-indent-level 2)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq
 backup-by-copying t
 backup-directory-alist
  '(("." . "~/.esaves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;
(whitespace-mode)
(ido-mode)
(flx-ido-mode)
(ido-ubiquitous-mode +1)
(evil-mode)
(tool-bar-mode 0)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFUN
;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
	(split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(defun helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm-mode t)
  (jit-lock-fontify-now) ;; https://groups.google.com/forum/#!topic/emacs-helm/YwqsyRRHjY4
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^(\\|testing\\|^;.*[A-Za-z]+")))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; won't mess with indentation
(add-hook 'text-mode-hook 'turn-on-auto-fill) ;; break long lines. TODO consider specific modes.
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'projectile-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'html-mode-hook
    (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))

;;;;;;;;;;;;;;;;;;;;;;;
;; KEYS
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f11] 'toggle-fullscreen)

(define-key evil-normal-state-map [escape] 'keyboard-quit)

(global-unset-key (kbd "C-x h"))
(global-set-key (kbd "C-x h") 'helm-clojure-headlines)
(global-set-key (kbd "C-<tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "C-x S-o") 'previous-multiframe-window) ;; for terminal, since C-tab breaks there

(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x 4") 'toggle-frame-split)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c C-f") 'helm-recentf)
(global-set-key (kbd "C-x p") 'proced)
(global-set-key (kbd "C-c t") 'ansi-term)
(global-set-key (kbd "C-c C-t") 'multi-term)

(global-set-key (kbd "C-c p s") 'projectile-switch-project)

;; Org mode keys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM SET VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-nntp-server "news.gmane.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
