; customize [x]emacs settings

(if (featurep 'xemacs)
    (progn
      (require 'recent-files)
      (recent-files-initialize)
      (setq recent-files-save-file "~/.emacs.d/recent-files")
      (recent-files-toggle-save-list-on-exit)
    )
    (recentf-mode)
)

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
    (set-specifier menubar-visible-p nil 'global)
    (set-specifier default-toolbar-visible-p nil)
    (set-specifier horizontal-scrollbar-visible-p nil)
    (set-specifier vertical-scrollbar-visible-p nil)
    (line-number-mode 1)
    (column-number-mode 1)
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

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs


; fix me and put it right spot
(setq font-lock-verbose nil) ; to avoid the annoying messages
(setq lazy-lock-continuity-time 0.3) ; refontify after 0.3 inactivity

; remove noisy yes/no
(fset 'yes-or-no-p 'y-or-n-p)
(setq fill-column 78)

; save the spaces    
(setq-default unspace-p nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; no splash
(setq inhibit-startup-message t)

; use versioned control
(setq version-control t)
(setq delete-old-versions t)
 
;; color themes
;(color-theme-black-ak)
;(color-theme-white-ak)
;(color-theme-gnome2)
(color-theme-charcoal-black-flat)

(defun reset-font ()
  (if (eq system-type 'windows-nt)
      (if (featurep 'xemacs)
	(custom-set-faces  '(default ((t (:size "8pt" :family "Consolas"))) t))
	(set-default-font "-outline-Consolas-normal-r-normal-normal-12-*-96-96-c-*-iso10646-1")
	;(set-default-font "-outline-Bitstream Vera Sans Mono-normal-r-normal-normal-11-*-96-96-c-*-iso10646-1")
      )
					; else unix
    (if (featurep 'xemacs)
        (set-face-font 'default "-microsoft-consolas-*-*-normal--10-0-96-96-c-0-iso8859-1")
        ()
      )
    )
  )

(reset-font)

;(set-face-font 'default "-microsoft-dejavu sans mono-*-*-normal--9-90-96-96-c-0-iso8859-1")
;(set-face-font 'default "-microsoft-dejavu sans mono-*-*-normal--10-100-96-96-c-0-iso8859-1")
;(set-default-font "-outline-Consolas-normal-r-normal-normal-11-*-96-96-c-*-iso10646-1")
; (set-face-font 'default "-microsoft-consolas-*-*-normal--9-0-96-96-c-0-iso8859-1")
; (set-face-font 'default "-microsoft-consolas-*-*-normal--10-0-96-96-c-0-iso8859-1")
; (set-face-font 'default "-microsoft-droid sans mono-*-*-normal--10-100-96-96-c-0-iso8859-1")
; (set-face-font 'default "-microsoft-monospac821 bt-*-*-normal--9-90-96-96-c-0-iso8859-1")
