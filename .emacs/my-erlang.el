;-*- Mode: Emacs-Lisp -*-

;; do not forget to add distel ebin into ~/.erlang, i.e.:
; code:add_pathz("~/.emacs/pkg/distel/ebin").
; code:add_pathz("./ebin").

(add-to-list 'load-path "~/.emacs/pkg/erlang")
(add-to-list 'load-path "~/.emacs/pkg/distel/elisp")

(require 'erlang-start)

(if (eq system-type 'windows-nt)
    (progn 
      (setq exec-path (cons "d:/applications/erlang-r14b/bin" exec-path))
      (setq erlang-root-dir "d:/applications/erlang-r14b")  )
      (setq erlang-man-root-dir "d:/applications/erlang-r14b/man")
    (progn
      (setq erlang-root-dir "/opt/erlang")
      (add-to-list 'exec-path "/opt/erlang/bin")
      (setq erlang-man-root-dir "/opt/erlang/man")
      )
)

(setq erlang-node-name "emacs@localhost")
(setq erl-nodename-cache (intern erlang-node-name))
(setq inferior-erlang-machine-options (list "-name" erlang-node-name "-sname" erlang-node-name))

(defun my-erlang-mode-hook ()
  ;; already defined node settings above
  ;; add to menu and more keyboard shortcuts
  (imenu-add-to-menubar "imenu")
  (local-set-key [return] 'newline-and-indent)
)
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

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
              (define-key erlang-shell-mode-map (car spec) (cadr spec))))
)
