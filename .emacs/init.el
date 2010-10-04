;-*- Mode: Emacs-Lisp -*-

; setup paths
(add-to-list 'load-path "~")
(add-to-list 'load-path "~/.emacs")
(add-to-list 'load-path "~/.emacs/pkg")
(add-to-list 'load-path "~/.emacs/pkg/org")
(add-to-list 'load-path "~/.emacs/pkg/yasnippet-0.6.1c")


; load libraries
; precompile(disabled atm)
(defun compile-if-newer-and-load (file)
  (load file)
)

(compile-if-newer-and-load "~/.emacs/keys")
(compile-if-newer-and-load "~/.emacs/modes")
(compile-if-newer-and-load "~/.emacs/misc")

(compile-if-newer-and-load "~/.emacs/pkg/protbuf")
(compile-if-newer-and-load "~/.emacs/pkg/gtags")

; color themes
(compile-if-newer-and-load "~/.emacs/pkg/color-theme")
(compile-if-newer-and-load "~/.emacs/pkg/color-theme-tangotango")
(compile-if-newer-and-load "~/.emacs/themes")


; customize emacs a bit
(load-file "~/.emacs/custom.el")

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

; misc development modes(right now start manually)
; haskell
;(add-to-list 'load-path "~/.emacs/pkg/haskell-mode-2.8.0")
;(compile-if-newer-and-load "~/.emacs/pkg/haskell-mode-2.8.0/haskell-site-file")

; scala 
;(add-to-list 'load-path "~/.emacs/pkg/scala")
;(add-to-list 'load-path "~/.emacs/pkg/ensime/elisp")
;(require 'scala-mode-auto)
;(require 'ensime)

; erlang
;(add-to-list 'load-path "~/.emacs/pkg/erlang")
;(add-to-list 'load-path "~/.emacs/pkg/distel")
;(require 'distel)
