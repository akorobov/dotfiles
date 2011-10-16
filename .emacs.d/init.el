;-*- Mode: Emacs-Lisp -*-

; setup paths
(add-to-list 'load-path "~")
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/pkg")
(add-to-list 'load-path "~/.emacs.d/pkg/org")
(add-to-list 'load-path "~/.emacs.d/pkg/yasnippet-0.6.1c")
(add-to-list 'load-path "~/.emacs.d/pkg/magit")

(load "pkg/protbuf")
(load "pkg/gtags")

; color themes
(load "pkg/color-theme")
(load "pkg/color-theme-tangotango")
(load "pkg/color-theme-zenburn")


; customize emacs a bit
(load "misc")
(load "custom")

; start server
(progn
  (server-start)
  (add-hook 'server-switch-hook
            (lambda nil
              (let ((server-buf (current-buffer)))
                (bury-buffer)
                (switch-to-buffer-other-frame server-buf))))
  
  (add-hook 'server-done-hook 'delete-frame)
)

; load language-specific packages when needed
(defun load-scala ()
    (interactive)
    (add-to-list 'load-path "~/.emacs.d/pkg/scala")
    (require 'scala-mode-auto)
    (add-to-list 'load-path "~/.emacs.d/pkg/ensime/elisp")
    (require 'ensime)
 )

(defun load-haskell ()
    (interactive)
    (add-to-list 'load-path "~/.emacs.d/pkg/haskell-mode")
    (load "haskell-site-file")
 )

(defun load-erlang ()
    (interactive)
    (load "my-erlang")
    (erlang-shell)
)

; elpa/package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

