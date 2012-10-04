;-*- Mode: Emacs-Lisp -*-

; setup paths
(add-to-list 'load-path "~")
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/pkg")
(add-to-list 'load-path "~/.emacs.d/pkg/org")
(add-to-list 'load-path "~/.emacs.d/pkg/yasnippet")
(add-to-list 'load-path "~/.emacs.d/pkg/magit")

(load "pkg/protbuf")
(load "pkg/gtags")

; customize emacs a bit
(load "misc")
(load "custom")

; start server
(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
    (progn
      (server-start)
      (add-hook 'server-switch-hook
                (lambda nil
                  (let ((server-buf (current-buffer)))
                    (bury-buffer)
                    (switch-to-buffer-other-frame server-buf))))))


; load language-specific packages when needed
(defun load-scala ()
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

(defun load-haskell ()
    (interactive)
    (add-to-list 'load-path "~/.emacs.d/pkg/haskell-mode")
    (load "haskell-site-file"))

(defun load-erlang ()
    (interactive)
    (load "my-erlang")
    (erlang-shell))

; elpa/package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

