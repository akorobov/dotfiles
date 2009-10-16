;-*- Mode: Emacs-Lisp -*-   ;Comment that puts emacs into lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add home directory to library path
(add-to-list 'load-path "~")
(add-to-list 'load-path "~/.emacs")
(add-to-list 'load-path "~/.emacs/pkg")
(add-to-list 'load-path "~/.emacs/pkg/org") ; org mode
(add-to-list 'load-path "~/.emacs/pkg/erlang")
(add-to-list 'load-path "~/.emacs/pkg/haskell-mode-2.4/")
(add-to-list 'load-path "~/.emacs/pkg/distel/")
(add-to-list 'load-path "~/.emacs/pkg/yasnippet-0.6.1c/")

;(add-to-list 'load-path "~/.dot/emacs/pkg/speedbar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; load libraries

; cua mode(shift/ctrl-ins|del + arrows)
(require 'pc-select)

(if (featurep 'xemacs)
  (pc-select-mode t)
  (pc-selection-mode)
)
(delete-selection-mode t)
;(pc-select-mode t)


; instead of loading lisp files we can precompile them on demand
; and load bytecode(much faster)
; disabled because xemacs/emacs collide
(defun compile-if-newer-and-load (file)
  (load file)
)
;;   (if (featurep 'xemacs)
;;       "Byte compile FILE.el if newer than file.elc."
;;     (if (file-newer-than-file-p (concat file ".el") (concat file ".elc"))
;;         (byte-compile-file (concat file ".el")))
;;         ()
;;     )

(compile-if-newer-and-load "~/.emacs/keys")
(compile-if-newer-and-load "~/.emacs/modes")
(compile-if-newer-and-load "~/.emacs/misc")
(compile-if-newer-and-load "~/.emacs/pkg/color-theme")
(compile-if-newer-and-load "~/.emacs/themes")
(compile-if-newer-and-load "~/.emacs/pkg/protbuf")
(compile-if-newer-and-load "~/.emacs/pkg/gtags")

(compile-if-newer-and-load "~/.emacs/pkg/haskell-mode-2.4/haskell-site-file")

;(require 'yasnippet-bundle)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs/pkg/yasnippet-0.6.1c/snippets")

;(compile-if-newer-and-load "~/.emacs/pkg/gtags")
;(compile-if-newer-and-load "~/.emacs/pkg/locate")
;(compile-if-newer-and-load "~/.emacs/pkg/diff-mode")
;(compile-if-newer-and-load "~/.emacs/pkg/align")
;(compile-if-newer-and-load "~/.emacs/c-mode")
;(compile-if-newer-and-load "~/.dot/emacs/mymisc")
;(compile-if-newer-and-load "~/.dot/emacs/variables")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start with the same buffers, major modes and buffer positions:
; You must do a M-x desktop-save the first time it's used. Emacs
; must be started in the same current directory.
;(load "desktop")
;(setq desktop-basefilename "~/.dot/.emacs.desktop")
;(desktop-load-default)
;(desktop-read)

;(setq custom-file "~/.emacs/custom.el")
(load-file "~/.emacs/custom.el")

; gnuserv
(if (featurep 'xemacs)
  (progn
   (load-library "gnuserv")
   (unless (gnuserv-running-p) (gnuserv-start))
  )
  nil
)

(if (featurep 'xemacs)
    (progn
      (require 'recent-files)
      (recent-files-initialize)
    )
    (recentf-mode)
)

