(eval-after-load 'projectile
  '(progn
     (setq projectile-cache-project t)))

;; haskell
(eval-after-load 'haskell-mode
  '(custom-set-variables
    '(haskell-mode-hook '(turn-on-haskell-indentation))))

;; erlang
(defun ak/configure-distel ()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/distel/elisp")
  (when (require 'distel nil 'noerror)
     (distel-setup)
     ;; add distel shortcuts to erlang shell
     (defconst distel-shell-keys
       '(("\C-\M-i"   erl-complete)
         ("\M-?"      erl-complete)
         ("\M-."      erl-find-source-under-point)
         ("\M-,"      erl-find-source-unwind)
         ("\M-*"      erl-find-source-unwind)
         )
       "Additional keys to bind when in Erlang shell.")

     (add-hook 'erlang-shell-mode-hook
               (lambda ()
                 (dolist (spec distel-shell-keys)
                   (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

     ;; auto-completion for erlang/distel
     (add-to-list 'load-path "~/.emacs.d/site-lisp/company-distel")
     (require 'company-distel)
     (require 'company-distel-frontend)
     (add-to-list 'company-backends 'company-distel)
    ))

;; (add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
;; (add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))
(eval-after-load 'erlang
  '(progn
     (setq erlang-node-name "emacs@localhost")
     (setq erl-nodename-cache (intern erlang-node-name))
     (setq inferior-erlang-machine-options (list "-name" erlang-node-name "-sname" erlang-node-name))
     (ak/configure-distel)))

;; flycheck adjustments
(eval-after-load 'flycheck
  '(progn
     (setq flycheck-highlighting-mode 'lines)
     (load "flycheck-java")))

;; enable company mode by default (if present)
(when (require 'company nil 'noerror)
  (global-company-mode))

(eval-after-load 'company
  '(progn
     (setq company-tooltip-limit 20)
     (setq company-minimum-prefix-length 1)
     (setq company-idle-delay .25)
     (setq company-echo-delay 0)
     ;; do not convert to lower case dabbrev candidates
     (setq company-dabbrev-downcase nil)
     (setq company-begin-commands '(self-insert-command))
     (define-key company-active-map (kbd "C-c C-d") 'company-show-doc-buffer)
     (define-key company-active-map (kbd "C-/") 'company-complete-common)
     (define-key company-active-map (kbd "<tab>") 'company-complete)

     ; close on escape
     (define-key company-active-map (kbd "<escape>")
       '(lambda ()
          (interactive)
          (company-abort)
          (if (fboundp 'evil-normal-state)
              (evil-normal-state))))))

(add-hook 'go-mode-hook
  (lambda ()
    (when (require 'company-go nil 'noerror)
      (add-to-list 'company-backends 'company-go))))

(add-hook 'python-mode-hook
  (lambda ()
    (when (require 'jedi nil 'noerror)
      (jedi:setup))))

(when (require 'ggtags nil 'noerror)
  (global-set-key (kbd "C-x \\")  'ggtags-find-tag-dwim))

(when (require 'helm-projectile nil 'noerror)
  (global-set-key (kbd "C-x /")  'helm-projectile))

(when (require 'smartparens nil 'noerror)
  (progn
     (setq sp-highlight-pair-overlay nil)
     ;; do not autoinsert ' pair if the point is preceeded by word.
     (sp-pair "'" nil :unless '(sp-point-after-word-p))
     (sp-with-modes '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      lisp-interaction-mode
                      scheme-mode
                      lisp-mode
                      eshell-mode
                      slime-repl-mode
                      clojure-mode
                      cider-repl-mode
                      common-lisp-mode
                      )
       ;; disable ', it's the quote character!
       (sp-local-pair "'" nil :actions nil)
       (sp-local-pair "(" ")" :wrap "M-(")
       ;; also only use the pseudo-quote inside strings where it serve as
       ;; hyperlink.
       (sp-local-pair "`" "'" :when '(sp-in-string-p)))

     ;; key bindings
     (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
     (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
     (define-key sp-keymap (kbd "M-t") 'sp-transpose-sexp)
     (define-key sp-keymap (kbd "M-S-<left>") 'sp-backward-sexp)
     (define-key sp-keymap (kbd "M-S-<right>") 'sp-forward-sexp)
     (define-key sp-keymap (kbd "<delete>") 'sp-delete-char)
     (define-key sp-keymap (kbd "M-s") 'sp-unwrap-sexp)
     ))


;; (setq c-default-style "k&r"
;;       c-basic-offset 4)

;; to switch between single-line and multi-line comments
(defun ak/use-cpp-comments ()
  (interactive)
  (setq-local comment-start "//")
  (setq-local comment-end   ""))

(defun ak/use-c-comments ()
  (interactive)
  (setq-local comment-start "/*")
  (setq-local comment-end   "*/"))

;; configure irony-more and flycheck-irony
(defun use-irony-hook ()
  (progn
    ;; additional clang option to enable warnings on unused variables
    (irony-mode)
    ;; use flycheck-irony
    (flycheck-select-checker 'irony)
    ))

(when (require 'irony nil 'noerror)
  (progn
    (eval-after-load 'company
      '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    
    (setq irony-additional-clang-options '("-Wall"))
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
    (flycheck-irony-setup)
    ))

(defun enable-irony ()
  (add-hook 'c++-mode-hook 'use-irony-hook)
  (add-hook 'c-mode-hook 'use-irony-hook))

(defun disable-irony ()
  (remove-hook 'c++-mode-hook 'use-irony-hook)
  (remove-hook 'c-mode-hook 'use-irony-hook))
