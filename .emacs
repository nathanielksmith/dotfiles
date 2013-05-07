(tool-bar-mode 0)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

; TODO do i need this?
(defun turn-on-soft-tabs ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
)
; dark table
; raw therapee

(add-hook 'after-save-hook
    '(lambda ()
       (when (string-match "\\.coffee$" buffer-file-name)
         (coffee-compile-file))))

(add-hook 'html-mode-hook
    (lambda ()
        (set (make-local-variable 'sgml-basic-offset) 4)))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defun show-trailing-ws ()
  (setq show-trailing-whitespace t)
)
(add-hook 'text-mode-hook 'show-trailing-ws)

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
(package-initialize)

(require 'evil)
(require 'zenburn-theme)
(require 'ido)
(require 'whitespace)
(require 'coffee-mode)

(whitespace-mode)
(ido-mode)
(evil-mode)

(global-set-key (kbd "C-<tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window)

(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

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
 '(default ((t (:family "Inconsolata for Powerline" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))
