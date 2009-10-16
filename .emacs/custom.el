; customize [x]emacs settings

(if (featurep 'xemacs)
  (progn
    (custom-set-variables
     '(paren-mode (quote sexp) nil (paren))
     '(delete-auto-save-files nil)
     '(font-lock-mode t nil (font-lock))
     '(setq font-lock-verbose nil) ; to avoid the annoying messages
     '(setq lazy-lock-continuity-time 0.3) ; refontify after 0.3 inactivity
    )
    (set-specifier horizontal-scrollbar-visible-p nil)
    (set-specifier vertical-scrollbar-visible-p nil)
    (custom-set-faces)
    ; no toolbar, menubar, scrollbars
    (set-specifier menubar-visible-p nil 'global)
    (set-specifier default-toolbar-visible-p nil)
    (set-specifier horizontal-scrollbar-visible-p nil)
    (set-specifier vertical-scrollbar-visible-p nil)
  )
  (progn
    ;(menu-bar-mode -1)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)
    (global-font-lock-mode 1) ; always fontify
    (line-number-mode t)
    (column-number-mode t)
  )
)

; fix me and put it right spot
(setq font-lock-verbose nil) ; to avoid the annoying messages
(setq lazy-lock-continuity-time 0.3) ; refontify after 0.3 inactivity

; remove noisy yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(setq recent-files-save-file "~/.recent-files")

(setq fill-column 78)

; save the spaces    
(setq-default unspace-p nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

; use versioned control
(setq version-control t)
(setq delete-old-versions t)
 
;; favorite themes
;(color-theme-black-ak)
;(color-theme-white-ak)
;(color-theme-gnome2)
(color-theme-charcoal-black-flat)


(if (eq system-type 'windows-nt)
  (if (featurep 'xemacs)
    (custom-set-faces  '(default ((t (:size "8pt" :family "Consolas"))) t))
    ; emacs
    (set-default-font "-outline-Consolas-normal-r-normal-normal-12-*-96-96-c-*-iso10646-1")
    ;(set-default-font "-outline-Bitstream Vera Sans Mono-normal-r-normal-normal-11-*-96-96-c-*-iso10646-1")
  )
  ; else unix
  (if (featurep 'xemacs)
    (set-default-font "-outline-Consolas-normal-r-normal-normal-11-*-96-96-c-*-iso10646-1")
    (set-face-font 'default "-microsoft-consolas-*-*-normal--13-0-96-96-c-0-iso8859-1")
  )
)

