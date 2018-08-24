;;; init.el --- emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'exec-path "~/bin")

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; turn off menu/scrollbar/toolbar
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not window-system))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq inhibit-startup-message t)
(fset 'yes-or-no-p #'y-or-n-p)

(line-number-mode)
(column-number-mode)

;; remove noisy yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; use spaces
(setq-default indent-tabs-mode nil
              tab-width 4)

(setq ring-bell-function 'ignore)

;; don't get lost
(show-paren-mode t)

;; delete selected region on edit
(delete-selection-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq display-buffer-alist
      `(("." nil (reusable-frames . visible))))

(use-package recentf
  :config (recentf-mode))

(use-package files
  :init (setq version-control t
              delete-old-versions t)
  :bind (("C-c r" . revert-buffer)))

(use-package which-key :ensure t :defer t)
;; color themes
(setq custom-theme-directory "~/.emacs.d/site-lisp/themes")

(setq gnutls-min-prime-bits 1024)

(use-package ido
  :init (ido-mode t)
  :config (progn
            (setq ido-auto-merge-delay-time 1)
            (defun ak-ido-setup ()
              (define-key ido-completion-map (kbd "SPC") 'ido-select-text))
            (add-hook 'ido-setup-hook 'ak-ido-setup)))

;; window navigation
(use-package window
  :bind (("M-o" . other-window)
         ("M-0" . delete-window)
         ("M-1" . delete-other-windows)
         ("M-2" . split-window-vertically)
         ("M-3" . split-window-horizontally)))

(use-package windmove
  :defer t
  :bind (("C-c <left>"  . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>"    . windmove-up)
         ("C-c <down>"  . windmove-down)))

(use-package ace-window
  :ensure t :defer t
  ;; relies on terminal sending [27;5;39~ escape sequence for C-'
  :bind (("C-'" . ace-window)
         ("M-=" . ace-window))
  :config (setq aw-scope 'frame))

(use-package simple
  :bind (("M-j" . goto-line)
         ([(meta backspace)] . backward-kill-word)))

(use-package projectile
  :ensure t :defer t
  :config (progn
            (setq projectile-cache-project t)
            (add-to-list 'projectile-project-root-files "compile_commands.json")
            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".cquery")
            (add-to-list 'projectile-project-root-files ".cquery.in")))

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("M-`" . ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package helm
  :config (setq ibuffer-expert t)
  :bind (("C-x C-b" . helm-buffers-list)
         ("<f2>" . helm-buffers-list)))

(use-package helm-projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'projectile (helm-projectile-on))
  :bind ("C-x /" . helm-projectile))

(use-package imenu
  :bind ("M-i" . imenu))

(use-package smartparens
  :ensure t
  :config (progn
            (smartparens-global-mode)
            (show-smartparens-global-mode)
            (setq sp-highlight-pair-overlay nil)
            ;; do not autoinsert ' pair if the point is preceeded by word.
            (sp-with-modes '(emacs-lisp-mode
                             inferior-emacs-lisp-mode
                             lisp-interaction-mode
                             scheme-mode
                             lisp-mode
                             rust-mode
                             eshell-mode
                             slime-repl-mode
                             clojure-mode
                             cider-repl-mode
                             common-lisp-mode)
              ;; disable ', it's the quote character!
              (sp-local-pair "'" nil :actions nil)
              (sp-local-pair "(" ")" :wrap "M-(")
              ;; also only use the pseudo-quote inside strings where it serve as
              ;; hyperlink.
              (sp-local-pair "`" "'" :when '(sp-in-string-p)))

            ;; key bindings
            (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
            (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
            (define-key smartparens-mode-map (kbd "M-t") 'sp-transpose-sexp)
            (define-key smartparens-mode-map (kbd "M-S-<left>") 'sp-backward-sexp)
            (define-key smartparens-mode-map (kbd "M-S-<right>") 'sp-forward-sexp)
            (define-key smartparens-mode-map (kbd "<delete>") 'sp-delete-char)
            (define-key smartparens-mode-map (kbd "M-s") 'sp-unwrap-sexp)))

(use-package man
  :bind ("<f1>" . manual-entry))

(use-package ggtags
  :ensure t
  :defer t
  :config (setq gtags-path-style 'relative)
  :bind ("C-x \\" . ggtags-find-tag-dwim))

;; interface to platinum searcher
(use-package pt
  :ensure t :defer t
  :bind ("C-c /" . pt-regexp))

(use-package term
  :config (define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(use-package company
  :ensure t
  :config (progn
            (setq company-global-modes '(not fundamental-mode)
                  company-tooltip-limit 20
                  company-minimum-prefix-length 1
                  company-idle-delay .25
                  company-echo-delay 0
                  company-show-numbers t
                  ;; do not convert to lower case dabbrev candidates
                  company-dabbrev-downcase nil
                  company-begin-commands '(self-insert-command))
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
            (global-company-mode t)))

(use-package company-quickhelp
  :ensure t :defer t
  :init (with-eval-after-load 'company
          (when (fboundp window-system) (company-quickhelp-mode))))

(use-package yasnippet :ensure t :defer t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package diff-mode
  :defer t
  :bind ([(control c) (control q)] . diff-apply-hunk)
  :config (setq diff-default-read-only t))

(use-package eldoc)

(use-package flycheck
  :ensure t :defer t
  :init (global-flycheck-mode)
  :config (setq flycheck-highlighting-mode 'lines))

;;; languages support
(use-package lsp-mode
  :ensure t :defer t)

(use-package lsp-ui
  :ensure t
  :config
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config
  (setq company-lsp-enable-recompletion t)
  (add-to-list 'company-backends 'company-lsp))

;; golang
(use-package go-mode
  :ensure t :defer t
  :bind ("C-c C-g d" . godoc-at-point)
  :bind ("M-." . godef-jump)
  :bind ("M-*" . pop-tag-mark)
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go
  :ensure t :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc
  :ensure t :defer t
  :init (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package go-guru
  :ensure t :defer t)

(use-package go-errcheck
  :ensure t :defer t)

;; rust
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

;; Rust
(use-package rust-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package lsp-rust
  :ensure t
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-mode))

;; `toml' support for `cargo' files
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

;; haskell
(use-package haskell-mode
  :ensure t :defer t
  :config (custom-set-variables
           '(haskell-mode-hook '(turn-on-haskell-indentation))))

(use-package erlang
  :ensure t :defer t
  :config (setq erlang-node-name "emacs@localhost"
                erl-nodename-cache (intern erlang-node-name)
                inferior-erlang-machine-options
                (list "-name" erlang-node-name "-sname" erlang-node-name)))

;; clojure
(use-package clojure-mode
  :defer t :ensure t)

(use-package cider
  :defer t :ensure t)

;; c/c++
(use-package cquery
  :ensure t
  :commands (lsp-cquery-enable)
  :hook (c-mode-common . lsp-cquery-enable)
 :config
  ;; (add-hook 'c-mode-common-hook #'lsp-cquery-enable)
  ;; (add-hook 'c++-mode-hook (lambda() (lsp-cquery-enable)))
 (setq cquery-executable "/opt/cquery/bin/cquery")
 ;;(setq cquery-extra-init-params '(:completion (:detailedLabel t)))
 )

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;; python
(use-package python
  :defer t)

(use-package anaconda-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :ensure t :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-anaconda)))

(use-package flycheck-java
  :load-path "lisp/"
  ;; ensure s package is installed
  :init (progn
          (unless (package-installed-p 's)
                 (package-refresh-contents)
                 (package-install 's))
          (add-to-list 'flycheck-checkers 'java)))

(use-package ak-python-virtualenv
  :load-path "lisp/"
  :commands (ak-flycheck-python-setup)
  :commands (ak-python-venv-setup)
  :init (progn
          (add-hook 'python-mode-hook #'ak-python-venv-setup)
          (add-hook 'flycheck-mode-hook #'ak-flycheck-python-setup)))

;; yang models
(use-package yang-mode :ensure t :defer t)

;; web
(use-package json-mode :ensure t :defer t
  :init (setq js-indent-level 2))

(use-package js2-mode
  :diminish js2-minor-mode
  :commands (js2-mode js2-minor-mode)
  :mode ("\\.js\\'" . js-mode)
  :mode ("\\.ts\\'" . js-mode)
  :interpreter ("node" . js-mode)
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)
  (setq js2-highlight-level 3
        js2-mode-assume-strict t
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil)

  (use-package tern
    :ensure t :defer t
    :diminish " T"
    :commands (tern-mode)
    :init
    (add-hook 'js-mode-hook 'tern-mode))

  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern))

  (use-package js-doc :ensure t :defer t
)

  (use-package js2-refactor
    :ensure t :defer t
    :diminish js2-refactor-mode
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    ;; :config
    ;; (js2r-add-keybindings-with-prefix "C-c r")
    )
  )

(use-package web-mode
  :ensure t :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2))

(use-package emmet-mode
  :ensure t :defer t)

;; version control
(use-package magit
  :ensure t :defer t)

(use-package multiple-cursors
  :ensure t :defer t
  :bind ("C-c m" . mc/mark-all-like-this-dwim))

(use-package powerline :ensure t
     :config
     (setq-default ns-use-srgb-colorspace nil
                   powerline-default-separator 'utf-8)
     :init (powerline-default-theme))

(use-package ak-utils
  :load-path "lisp/"
  :bind ("M-\\" . goto-match-paren)
  :bind ([(control \|)] . shell-filter-region))

;; reset theme on change
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable all custom themes before loading new one."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; themes
(use-package custom
  :init (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes"))

(use-package zenburn-theme :ensure t :defer t)
(use-package flatui-theme :ensure t :defer t)
(use-package subatomic-theme :ensure t :defer t)
(use-package mono-dark-theme :defer t)
(use-package mono-light-theme :defer t)
(use-package green-phosphor-theme :defer t)

(use-package spacemacs-theme :ensure t :defer t
  :init (setq spacemacs-theme-comment-bg nil))


;;; init.el ends here
