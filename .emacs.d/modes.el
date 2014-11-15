;; adjust settings for various prog modes

(setq c-default-style "k&r"
      c-basic-offset 4)

(setq cperl-continued-statement-offset 0
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4 )


;; use company-mode by default
(global-company-mode)

(eval-after-load 'projectile
  '(progn
     (setq projectile-cache-project t)))

;;; scala
(defun init-scala ()
    (interactive)
    (add-to-list 'load-path "~/.emacs.d/pkg/scala")
    (require 'scala-mode-auto)
    
    (add-hook 'scala-mode-hook
              '(lambda ()
                 (scala-mode-feature-electric-mode)
               ))
    
    (add-to-list 'load-path "~/.emacs.d/pkg/ensime_2.9.2-0.9.8.1/elisp")
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
    (require 'ensime))

;;; haskell
(eval-after-load 'haskell-mode
  '(custom-set-variables
    '(haskell-mode-hook '(turn-on-haskell-indentation))))

;;; erlang
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))
(eval-after-load 'erlang
  '(progn
     (message "erlang extensions")
     (setq erlang-node-name "emacs@localhost")
     (setq erl-nodename-cache (intern erlang-node-name))
     (setq inferior-erlang-machine-options (list "-name" erlang-node-name "-sname" erlang-node-name))
     
     ;; (defun my-erlang-mode-hook ()
     ;;   ;; already defined node settings above
     ;;   ;; add to menu and more keyboard shortcuts
     ;;   (imenu-add-to-menubar "imenu")
     ;;   (local-set-key [return] 'newline-and-indent)
     ;; )
     ;; (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

     (add-to-list 'load-path "~/.emacs.d/pkg/distel/elisp")
     (require 'distel)
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
     (add-to-list 'load-path "~/.emacs.d/pkg/company-distel")
     (require 'company-distel)
     (require 'company-distel-frontend)
     (add-to-list 'company-backends 'company-distel)
     ))

;; flycheck adjustments
(eval-after-load 'flycheck
  '(progn
     (setq flycheck-highlighting-mode 'lines)
     (load "flycheck-java")
     ;; enable flycheck for java and c/c++
     (add-hook 'java-mode-hook #'flycheck-mode)
     (add-hook 'c++-mode-hook #'flycheck-mode)))

(eval-after-load 'company
  '(progn
     (require 'company-go)
     (setq company-tooltip-limit 20)
     (setq company-minimum-prefix-length 1)
     (setq company-idle-delay .7)
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
              (evil-normal-state))))
     ))

(eval-after-load 'go-mode
  '(progn
     (add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
     ))

;; to switch between single-line and multi-line comments
(defun cpp-comments ()
  (interactive)
  (setq-local comment-start "//")
  (setq-local comment-end   ""))

(defun c-comments ()
  (interactive)
  (setq-local comment-start "/*")
  (setq-local comment-end   "*/"))
