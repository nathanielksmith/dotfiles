(tool-bar-mode 0)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'whitespace-cleanup)

; TODO do i need this?
(defun turn-on-soft-tabs ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
)
; dark table
; raw therapee

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

; don't let next-line add new lines at end of file
(setq-default next-line-add-newlines nil)

; make edited files end with a carriage return
(setq-default require-final-newline t)

(add-hook 'after-save-hook
    '(lambda ()
       (when (string-match "\\.coffee$" buffer-file-name)
         (coffee-compile-file))))

(add-hook 'html-mode-hook
    (lambda ()
        (set (make-local-variable 'sgml-basic-offset) 4)))

(setq-default js-indent-level 2)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defun show-trailing-ws ()
  (setq show-trailing-whitespace t)
)
(add-hook 'text-mode-hook 'show-trailing-ws)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . rainbow-delimiters-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;(setq-default show-trailing-whitespace t)
;(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
;(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(setq
 backup-by-copying t
 backup-directory-alist
  '(("." . "~/.esaves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

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

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'evil)
(require 'zenburn-theme)
(require 'ido)
(require 'whitespace)
(require 'coffee-mode)
(require 'clojure-mode)


(whitespace-mode)
(ido-mode)
(evil-mode)
;(crosshairs-toggle-when-idle)


(define-key evil-normal-state-map [escape] 'keyboard-quit)

(global-set-key (kbd "C-<tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window)

(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; Org mode keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/documents/org/schedule.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-nntp-server "news.gmane.org")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
