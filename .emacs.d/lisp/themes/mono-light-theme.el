;;; mono-light-theme.el --- Light monochrome theme for Emacs.

;;; Commentary:
;;
;; Light monochrome theme for Emacs
;; mono: white F4F4F4 BFBFBF 8B8B8B 59595A  black 282828

;;; Code:
(deftheme mono-light
  "Light monochrome theme")

(let ((class '((class color) (min-colors 10)))
      (white "#F4F4F4")
      (lgray "#BFBFBF")
      (mgray "#8B8B8B")
      (dgray "#59595A")
      (black "#282828")

      (orange "#FF9912")
      (green  "#00f900")
      (red    "#E82C0C")
      (dgreen "#008700"))

  (custom-theme-set-faces
   'mono-light

   `(default ((t (:foreground ,black :background ,white :weight normal))))
   `(cursor ((t (:background ,dgreen))))

   ;; Highlighting faces
   `(fringe ((t (:background ,white))))
   `(highlight ((t (:foreground ,white :background ,lgray))))
   `(region ((t (:foreground ,white :background ,mgray))))
   `(secondary-selection ((t (:foreground: ,white :background ,mgray))))
   `(isearch ((t (:foreground ,white :background ,orange))))
   `(lazy-highlight ((t (:foreground ,white :background ,dgray))))
   `(trailing-whitespace ((t (:background ,red))))

   ;; powerline faces
   `(powerline-active1 ((t :background ,dgray :foreground ,lgray :weight bold)))
   `(powerline-active2 ((t :background ,dgray :foreground ,lgray)))
   `(powerline-inactive1 ((t :background ,dgray :foreground ,lgray)))
   `(powerline-inactive2 ((t :background ,dgray :foreground ,lgray)))
  
   ;; Escape and prompt faces
   `(minibuffer-prompt ((t (:weight bold :foreground ,dgray))))
   `(escape-glyph ((t (:foreground ,dgray))))
   `(error ((t (:background nil :weight bold :foreground ,red))))
   `(warning ((t (:background nil :foreground ,orange))))
   `(success ((t (:inherit default :foreground ,green))))
   `(info ((t (:inherit default :foreground ,green :underline t))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,black))))
   `(font-lock-comment-face ((t (:foreground ,dgray))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,dgray))))
   `(font-lock-doc-face ((t (:foreground ,dgray))))
   `(font-lock-doc-string-face ((t (:foreground ,dgray))))
   `(font-lock-special-keyword-face ((t (:foreground ,dgray))))
   `(font-lock-constant-face ((t (:foreground ,dgray))))
   `(font-lock-function-name-face ((t (:foreground ,black))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,dgray))))
   `(font-lock-string-face ((t (:foreground ,dgray))))
   `(font-lock-type-face ((t (:foreground ,dgray))))
   `(font-lock-variable-name-face ((t ( :foreground ,black))))
   `(font-lock-warning-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face ((t (:foreground ,dgray))))
   `(sh-quoted-exec ((t (:foreground ,dgray))))

   ;; compilation
   `(compilation-column-face ((t (:foreground ,dgray))))
   `(compilation-enter-directory-face ((t (:foreground ,dgray))))
   `(compilation-error-face ((t (:foreground ,dgray :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,dgray))))
   `(compilation-info-face ((t (:foreground ,dgray))))
   `(compilation-info ((t (:background ,white :foreground ,dgray))))
   `(compilation-leave-directory-face ((t (:foreground ,dgray))))
   `(compilation-line-face ((t (:background nil :foreground ,dgray))))
   `(compilation-line-number ((t (:background ,white :foreground ,dgray))))
   `(compilation-message-face ((t (:foreground ,dgray))))
   `(compilation-warning-face ((t (:foreground ,dgray :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,dgray))))
   `(grep-error-face ((t (:foreground ,dgray :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,dgray))))
   `(grep-match-face ((t (:foreground ,dgray))))
   `(match ((t (:foreground ,dgray))))

   ;; diff
   `(diff-header ((t (:background nil :foreground ,dgray))))
   `(diff-file-header ((t (:background nil :foreground ,dgray :bold t))))
   `(diff-index-face ((t (:background nil :foreground ,dgray :bold t))))
   `(diff-context ((t (:background nil :foreground ,lgray))))
   `(diff-added ((t (:background nil :foreground ,black :weight normal))))
   `(diff-removed ((t (:background nil :foreground ,mgray :weight normal))))
   `(diff-changed ((t (:background nil :foreground ,orange))))
   `(diff-refine-added ((t (:weight bold :foreground "#ff5f00" :background nil))))
   `(diff-refine-removed ((t (:weight bold :foreground "#ff5f00" :background nil))))
   `(diff-refine-change ((t (:weight bold :foreground "#ff5f00" :background nil))))


   ;; Button and link faces
   `(link ((t (:underline t :foreground ,dgray))))
   `(link-visited ((t (:underline t :foreground ,dgray))))

   ;; ido
   `(ido-first-match ((t (:foreground ,dgray))))
   `(ido-only-match ((t (:foreground ,dgray))))
   `(ido-subdir ((t (:weight bold :foreground ,black))))

   `(magit-section-title ((t (:foreground ,dgray :weight bold))))
   `(magit-branch ((t (:foreground ,orange :weight bold))))
   `(magit-item-highlight ((t (:background nil))))

   `(company-tooltip ((t (:background ,lgray))))
   `(company-scrollbar-bg ((t (:background ,lgray))))
   ;; flycheck
   `(flycheck-error ((t (:foreground ,red :weight bold :underline t))))
   `(flycheck-warning ((t (:foreground ,orange :weight bold :underline t))))
   `(flycheck-info ((t (:inherit default :foreground ,dgreen :underline t ))))
   )

  ;; smartparens
  `(sp-show-pair-mismatch-face ((t (:background ,red))))
  `(sp-pair-overlay-face ((t (:background ,lgray))))

  (custom-theme-set-variables
   'mono-light
   `(ansi-color-names-vector [,white ,dgray ,lgray ,mgray ,black ,red ,green ,orange])))

(provide-theme 'mono-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mono-light-theme.el ends here
