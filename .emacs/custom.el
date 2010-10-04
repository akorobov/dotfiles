;-*- Mode: Emacs-Lisp -*-

; use cua mode
(require 'pc-select)
(pc-selection-mode)
(delete-selection-mode t)

; add yasnippet
(defun init-yas ()
   (require 'yasnippet)
   (yas/initialize)
   (yas/load-directory "~/.emacs/pkg/yasnippet-0.6.1c/snippets")
)
(init-yas)

(recentf-mode)

; no splash
(setq inhibit-startup-message t)

;(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(line-number-mode t)
(column-number-mode t)
(setq fill-column 78)

; keep backups
(setq version-control t)
(setq delete-old-versions t)

; fontlock
(global-font-lock-mode 1)
(setq font-lock-auto-fontify t)
(setq font-lock-verbose nil)
(setq lazy-lock-continuity-time 0.3)

; remove noisy yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; Default emacs's C mode indentation is annoying
(setq c-default-style "linux"
      c-basic-offset 4)

; nice colors
; (color-theme-charcoal-black-flat)
(color-theme-tangotango)

; customize things a bit
;(custom-set-faces)

