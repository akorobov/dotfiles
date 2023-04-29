;;; init.el --- emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

;; A list of package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
;;(setq use-package-always-ensure t)


(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/opt/llvm/bin")
(add-to-list 'exec-path "/opt/go/bin")
(add-to-list 'exec-path "~/dev/go/bin")

(setq load-prefer-newer t)

;;(eval-when-compile
;;  (require 'use-package))

;; turn off menu/scrollbar/toolbar
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

(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

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

(use-package project
  :config
  (setq project-vc-extra-root-markers '(".project.el")))

(use-package simple
  :bind (("M-j" . goto-line)
         ([(meta backspace)] . backward-kill-word)))

(use-package windmove
  :defer t
  :bind (("C-c <left>"  . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>"    . windmove-up)
         ("C-c <down>"  . windmove-down)))

(use-package which-key :ensure t :defer t)

(use-package powerline :ensure t
     :config
     (setq-default ns-use-srgb-colorspace nil
                   powerline-default-separator 'utf-8)
     :init (powerline-default-theme))

(use-package emacs
 :init
 (setq completion-cycle-threshold t)
 ;;(setq tab-always-indent 'complete)
 )

;; version control
(use-package magit
  :ensure t :defer t)

;; completion
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  ;; (corfu-cycle t)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.5)
  :bind (:map corfu-map
              ("M-/" . corfu-next)
              ("M-d" . #'corfu-info-documentation)
         )
  )

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex)))))
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

;; searching
(use-package rg
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)
  :bind ("C-c /" . ripgrep-regexp))

;; lsp
(setq ak/lsp-eglot t)
(if ak/lsp-eglot
    (use-package eglot
      :ensure t
      :defer t
      :config
      (setq read-process-output-max (* 1024 1024))
      ;; non-blocking connects
      (setq eglot-sync-connect 0)
      (push :documentHighlightProvider eglot-ignored-server-capabilities)
      :bind (("C-c a" 'eglot-code-actions)
             ("C-c r" 'eglot-rename)
             ("C-c C-r" 'eglot-reconnect)
             )
      
      )

  (use-package lsp-mode
    :ensure t
    :defer t
    :custom
    (lsp-completion-provider :none)
    :init
    (defun ak/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless
    :hook (prog-mode . lsp)
    :hook (lsp-completion-mode . ak/lsp-mode-setup-completion))
)

(use-package sideline
  :ensure t
  :init
  (setq sideline-backends-right
        '((sideline-lsp      . up)
          (sideline-flymake . down)
          )))

;; language modes

;; go
(use-package go-mode
  :ensure t :defer t
  :bind ("C-c C-g d" . godoc-at-point)
  
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  )


;; rust
(use-package rust-mode
  :ensure t)
;; `toml' support for `cargo' files
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

;; c/c++
(use-package clang-format
  :ensure t
  :bind
  (:map c++-mode-map
        ("C-c i" . clang-format-region)
        ("C-c u" . clang-format-buffer)))


;; python
(use-package pyvenv
  :defer t
  :ensure t)
(use-package python
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook 'lsp))


;; misc
(use-package powerline
  :ensure t
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
  :init (add-to-list 'custom-theme-load-path "~/.config/emacs/lisp/themes"))

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode)
    (show-smartparens-global-mode)
    (setq sp-highlight-pair-overlay nil)
    ;; do not autoinsert ' pair if the point is preceeded by word.
    (sp-with-modes
     '(emacs-lisp-mode
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

(use-package multiple-cursors
  :ensure t :defer t
  :bind ("C-c m" . mc/mark-all-like-this-dwim))

;; fonts

(cond 
 ((find-font (font-spec :name "Iosevka SS08"))
  (setq ak/default-font "Iosevka SS08"))
 ((find-font (font-spec :name "PragmataPro Liga"))
  (setq ak/default-font "PragmataPro Liga"))
 (t (setq ak/default-font "courier"))
 )

(use-package frame
  :config
  (letrec ((font-name ak/default-font)
           (font-size (if (> (display-pixel-width) 1920) 12 12))
           (font (format "%s-%d" font-name font-size)))
    (set-frame-font font)
    (add-to-list 'default-frame-alist `(font . ,font))))


;; (use-package zenburn-theme :ensure t :defer t)
;; (use-package flatui-theme :ensure t :defer t)
;; (use-package subatomic-theme :ensure t :defer t)
;; (use-package mono-dark-theme :defer t)
;; (use-package mono-light-theme :defer t)
;; (use-package green-phosphor-theme :defer t)

;; (use-package nord-theme
;;   :init (setq nord-comment-brightness 20))

(use-package spacemacs-theme :ensure t :defer t
  :init (setq spacemacs-theme-comment-bg nil))
