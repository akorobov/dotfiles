;-*- Mode: Emacs-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global settings
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

; fix default c- and cperl- mode indentations
(setq c-default-style "linux"
      c-basic-offset 4)
(setq cperl-continued-statement-offset 0
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; configure keyboard settings
(require 'pc-select)
(pc-selection-mode)
(delete-selection-mode t)

; keyboard shortcuts
(global-set-key [help] 'info)
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file-at-point)
(global-set-key [(shift f3)] 'revert-buffer)
(global-set-key [f5] 'other-window)
(global-set-key [(control tab)] 'other-window)

(global-set-key [f7] 'search-forward)
(global-set-key [(shift f7)] 'replace-string)
(global-set-key [(control f7)] 'replace-regexp)

(global-set-key [f10] 'color-theme-bw-flat-ak)
(global-set-key [(shift f10)] 'color-theme-slate-flat-ak)
(global-set-key [(control f10)] 'color-theme-black-ak)
(global-set-key [(shift f20)] 'color-theme-charcoal-black-flat) ; ctrl-shift-f10

(global-set-key [(meta j)] 'goto-line)
(global-set-key [(meta g)] 'grep)
(global-set-key [(meta q)] 'kill-buffer)
(global-set-key [(meta \`)] 'buffer-menu)
(global-set-key [(control \`)] 'buffer-menu)
(global-set-key [(control backspace)] 'undo)
(global-set-key [(control z)] 'undo)

; pc-selection-mode overrides m-backspace, restore it back
(global-set-key [(meta backspace)] 'backward-kill-word)


(global-set-key [(control \')] 'point-to-register)
(global-set-key [(meta \')] 'jump-to-register)

; quick commenting region
(global-set-key [(control \;)] 'comment-region)
(global-set-key [(control \:)] 'uncomment-region)

; gtags settings
(global-set-key [(meta \[)] 'gtags-pop-stack)
(global-set-key [(meta \])] 'gtags-find-tag)
(global-set-key [(control \})] 'gtags-find-symbol)
(global-set-key [(control \{)] 'gtags-find-rtag)


(defun shell-filter-region ( command )
  (interactive (list (read-shell-command "Filter to apply: ")))
  (shell-command-on-region (region-beginning) (region-end) command (current-buffer) t))
(global-set-key [(control |)] 'shell-filter-region)

;; (global-set-key [(control i)]
;;   '(lambda ()
;;      (interactive)
;;      (shell-command (read-string "Shell command to insert: ") t)))

(defun my-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command ()))))
;        (t (self-insert-command (or arg 1)))))

(global-set-key [(meta \\)] 'my-match-paren)       ; Bounce across parens, like in vi.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use yasnippet
(defun init-yas ()
   (require 'yasnippet)
   (yas/initialize)
   (yas/load-directory "~/.emacs/pkg/yasnippet-0.6.1c/snippets")
)
(init-yas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use recentf
(recentf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff buffers are read-only
(setq diff-default-read-only t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nice colors
(color-theme-tangotango)

