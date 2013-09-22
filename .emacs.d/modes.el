;; adjust settings for various prog modes


(load "pkg/gtags")

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
         ("\C-TAB"    auto-complete)
         ("\M-."      erl-find-source-under-point)
         ("\M-,"      erl-find-source-unwind)
         ("\M-*"      erl-find-source-unwind)
         )
       "Additional keys to bind when in Erlang shell.")

     (defconst my-distel-keys
       '(("\C-TAB"    auto-complete)
         ("\M-TAB"    auto-complete))
       "Additional key bindings for erlang-mode")

     (add-hook 'erlang-shell-mode-hook
               (lambda ()
                 (dolist (spec distel-shell-keys)
                   (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

                                        ; auto-complete mode in erlang
     (add-to-list 'ac-modes 'erlang-mode)

     (require  'auto-complete-distel)
     (add-hook 'erlang-mode-hook '(lambda () (setq ac-sources '(ac-source-distel ac-source-yasnippet ac-source-words-in-same-mode-buffers))))
     (define-key erlang-extended-mode-map (kbd "<C-tab>")  'auto-complete)))

;;; c/c++
;;; use flycheck with clang for syntax check and ac-clang for auto completion
(eval-after-load 'flycheck
  '(progn
     (setq flycheck-highlighting-mode 'lines)
     (defun make-clang-pattern (str level)
       (list (concat "^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): " str ": +\\(?4:.*\\)$") level))

     (flycheck-declare-checker c-clang-syntax
       "Check file using clang's syntax checker."
       :command '("clang"
                  "-fsyntax-only"
                  "-fno-diagnostics-show-option"
                  "-fno-caret-diagnostics"
                  source-inplace)
       :error-patterns
       (list (make-clang-pattern "warning" 'warning)
             (make-clang-pattern "note" 'warning)
             (make-clang-pattern "error" 'error)
             (make-clang-pattern "fatal error" 'error))
       :modes '(c-mode c++-mode cc-mode))
     (add-to-list 'flycheck-checkers 'c-clang-syntax)
     (load "flycheck-java")))


;; flymake hangs emacs on osx

;; enable clang completion 
(defun enable-clang-ac ()
  (interactive)
  (require 'auto-complete-clang)
  (setq ac-sources (append '(ac-source-clang) ac-sources)))

;;; java
