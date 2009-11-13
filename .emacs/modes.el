;-*- Mode: Emacs-Lisp -*-   ;Comment that puts emacs into lisp mode

; set automodes 
(setq auto-mode-alist '(
    ("\\.erl$"                    . erlang-mode)
	("\\.hs$"                     . haskell-mode)
    ("\\.c$"                      . c++-mode)
	("\\.h$"                      . c++-mode)
	("\\.C$"                      . c++-mode)
	("\\.H$"                      . c++-mode)
	("\\.hh$"                     . c++-mode)
	("\\.cc$"                     . c++-mode)
	("\\.cpp$"                    . c++-mode)
	("\\.hpp$"                    . c++-mode)
	("\\.diff$"                   . diff-mode)
	("\\.patch$"                  . diff-mode)
	("\\.rej$"                    . diff-mode)
	("\\.y$"                      . c-mode) ; in lieu of yacc mode
	("\\.a$"                      . c-mode)
	("\\.[pP][Llm]$"              . cperl-mode)
	("\\.java$"                   . java-mode)
	("\\.[sS]$"                   . asm-mode)
	("\\.asm$"                    . asm-mode)
	("\\.tex$"                    . latex-mode)
	("\\.txi$"                    . texinfo-mode)
	("\\.z[A-Za-z]*$"             . ksh-mode)
	("\\.[A-ya-y]*shrc$"          . ksh-mode)
	("\\.\\(tcl\\|tk\\)$"         . tcl-mode)
	("\\.\\(sgml\\|sgm\\|xml\\)$" . sgml-mode)
	("\\.m[vesmn]$"               . nroff-mode)
	("\\.\\([1-9]\\|man\\)$"      . nroff-mode)
	("\\.tar"                     . tar-mode)
	("\\.lisp$"                   . emacs-lisp-mode)
	("\\.el$"                     . emacs-lisp-mode)
	("\\.emacs$"                  . emacs-lisp-mode)
	("\\.emacs.[A-Za-z]*$"        . emacs-lisp-mode)
	("\\.bib$"                    . bibtex-mode)
	("\\.ps$"                     . postscript-mode)
	("\\.fvwm[0-9]*rc"            . fvwm-mode)
	("\\.html?$"                  . html-mode)
	("\\.jsp?$"                   . html-mode)
	("\\.mk$\\|[Mm]akefile"       . makefile-mode)
	("\\.cs$"                     . java-mode) ; use java-mode for csharp files
	("$"                          . text-mode))
)

;; ; programming modes

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                    (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (statement-block-intro . 4)
                                   (substatement-open . 0)
                                   (defun-block-intro . 4)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (inline-open       . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "my c programming style")

(defconst my-java-style
  '(
    (c-tab-always-indent        . t)
    (c-basic-offset . 4)
    (c-echo-syntactic-information-p . t)
    (c-offsets-alist             . ((inline-open . 0)
                                    (topmost-intro-cont    . +)
                                    (statement-block-intro . +)
                                    (knr-argdecl-intro     . 5)
                                    (substatement-open     . 0)
                                    (label                 . +)
                                    (statement-case-open   . +)
                                    (statement-cont        . +)
                                    (arglist-intro  . c-lineup-arglist-intro-after-paren)
                                    (arglist-close  . c-lineup-arglist)
                                    (access-label   . 0)
                                    (inher-cont     . c-lineup-java-inher)
                                    (func-decl-cont . c-lineup-java-throws)
                                    ))
    )
  "my java programming style"
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook 'ak-c-hook)
(add-hook 'c++-mode-common-hook 'ak-c-hook)
(add-hook 'java-mode-hook 'ak-java-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ak-c-hook ()
  ;; ctypes is mighty (add support for font-locking user defined types)
  (require 'ctypes)
  (setq ctypes-write-types-at-exit t)
  (ctypes-read-file "~/emacs/ctypes.c++" nil t t)
  (setq ctypes-auto-parse-mode 1)


  ;; still do ~1~ style backups, even if we are
  ;; looking at a cvs file.
  (setq backup-inhibited nil)  
  
  ;; evil cc-mode defines its own keymap. 
  (define-key c-mode-base-map "\M-q"  'kill-buffer)
  (define-key c-mode-base-map "\M-j"  'goto-line)

  (c-add-style "my-c-style" my-c-style t)
  (c-add-style "my-java-style" my-java-style t)
  (c-set-style "my-c-style")
)

(defun ak-java-hook ()
  
  (c-set-style "my-java-style")
  (define-key c-mode-base-map "\M-q"  'kill-buffer)
)
					; end of defun ak-cc-mode-common-hook


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; (global-set-key "\C-cvi"  'ffap-cvs-diff);; same cvs diff keystroke...
;; ; (require 'cvs)
;; ; (defun ffap-cvs-diff (ignore-me)
;; ;   "Run cvs diff with version REV of the current buffer."
;; ;   (interactive "i")
;; ;   (cvs:call-command cvs-command "*CVS Diff*" "diff" 
;; ; 		    (list "diff" "-u" "-r" "HEAD" (ffap-string-at-point)))
;; ; 					;  (switch-to-buffer-other-window)
;; ; )


(require 'ffap)				; load the package
(ffap-bindings)				; do default key bindings
;; triple click on a file to open it...
(add-hook 'mouse-track-click-hook 'ffap-mouse-track-click)


(require 'cperl-mode)  
; fix broken cperl indentation
(setq-default cperl-extra-newline-before-brace t)
(setq-default cperl-indent-level 2)
(setq-default cperl-continued-statement-offset cperl-indent-level)
(setq-default cperl-continued-brace-offset (- cperl-indent-level))
(setq-default cperl-label-offset (- cperl-indent-level))
(setq-default cperl-electric-parens-string "")


