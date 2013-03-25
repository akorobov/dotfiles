;-*- Mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/pkg")

;; start server
(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
    (progn
      (server-start)
      (add-hook 'server-switch-hook
                (lambda nil
                  (let ((server-buf (current-buffer)))
                    (bury-buffer)
                    (switch-to-buffer-other-frame server-buf))))))

(require 'package)
(package-initialize)

;; elpa/package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar my-packages
  '(flymake-cursor
    paredit
    haskell-mode
    sml-mode 
    go-mode
    erlang
    clojure-mode
    nrepl
    geiser
    yasnippet 
    flycheck
    auto-complete
    auto-complete-clang)
  "elpa packages")

;; install on demand
(defun fetch-my-packages ()
  (interactive)
  (when (not package-archive-contents)
  (package-refresh-contents))

  (dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
)

(load "custom")
(load "modes")
