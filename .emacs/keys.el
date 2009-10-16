;-*- Mode: Emacs-Lisp -*-   ;Comment that puts emacs into lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;
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


;; function-menu
(if (featurep 'xemacs)
(progn
  (require 'func-menu)
  (add-hook 'find-file-hooks 'fume-add-menubar-entry)
  (global-set-key [(control c) l] 'fume-list-functions)
  (global-set-key [(control c) g] 'fume-prompt-function-goto)
  (global-set-key [(control meta up)] 'fume-goto-previous-function)
  (global-set-key [(control meta down)] 'fume-goto-next-function)
  (global-set-key "" 'fume-goto-next-function)
  (define-key fume-list-mode-map " " 'fume-prompt-function-goto-other-window)
)
nil
) 

; mappings for 2.5.x xemacs
(defun key25 ()
     (define-key global-window-system-map [(control z)] 'undo)
     (define-key global-map [(control c) l] 'fume-list-functions)
     (define-key global-map [(control meta up)] 'fume-goto-previous-function)
     (define-key global-map [(control meta down)] 'fume-goto-next-function)
     (define-key global-map [(control c) g] 'fume-prompt-function-goto)
)


(if (and (featurep 'xemacs) (and (>= emacs-major-version 21) (>= emacs-minor-version 5)) )
    (key25)
    nil
)

;; gtags bindings.
;(global-set-key "\M-[" 'pop-tag-mark)
;(global-set-key "\M-]" 'find-tag)
(global-set-key [(meta \[)] 'gtags-pop-stack)
(global-set-key [(meta \])] 'gtags-find-tag)
(global-set-key [(control \})] 'gtags-find-symbol)
(global-set-key [(control \{)] 'gtags-find-rtag)



(global-set-key [(control |)] 'shell-filter-region)

(defun shell-filter-region ( command )
  (interactive (list (read-shell-command "Filter to apply: ")))
  (shell-command-on-region (region-beginning) (region-end) command (current-buffer) t))

;; (global-set-key [(control i)]
;;   '(lambda ()
;;      (interactive)
;;      (shell-command (read-string "Shell command to insert: ") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is from the GNU Emacs FAQ:
(defun my-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command ()))))
;        (t (self-insert-command (or arg 1)))))

(global-set-key [(meta \\)] 'my-match-paren)       ; Bounce across parens, like in vi.
