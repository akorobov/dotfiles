;;; mono-dark-theme.el --- Dark monochrome theme for Emacs.

;;; Commentary:
;;
;; Dark monochrome theme for Emacs
;; mono: white F4F4F4 BFBFBF 8B8B8B 59595A black 484848

;;; Code:
(deftheme mono-dark
  "Dark monochrome theme")

(let ((class `((class color) (min-colors 10)))
      (white "#F4F4F4")
      (lgray "#BFBFBF")
      (mgray "#8B8B8B")
      (dgray "#59595A")
      (black "#404357") ;; "#484848"

      (orange "#FF9912")
      (red  "#ff2600")
      (green  "#00f900")
      (dgreen "#009800")
      )

  (custom-theme-set-faces
   'mono-dark

   `(default ((,class (:foreground ,white  :background ,black :weight normal))))
   `(cursor ((,class (:background ,orange))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,black))))
   `(highlight ((,class (:foreground ,white :background ,dgray))))
   `(region ((,class (:foreground ,white :background ,dgray))))
   `(secondary-selection ((,class (:foreground: ,white :background ,mgray))))
   `(isearch ((,class (:foreground ,white :background ,orange))))
   `(lazy-highlight ((,class (:foreground ,dgray :background ,lgray))))
   `(trailing-whitespace ((,class (:background ,red))))

   `(minibuffer-prompt ((t (:foreground ,orange))))

   ;; powerline faces
   `(powerline-active1 ((t :background ,dgray :foreground ,lgray :weight bold)))
   `(powerline-active2 ((t :background ,dgray :foreground ,lgray)))
   `(powerline-inactive1 ((t :background ,dgray :foreground ,lgray)))
   `(powerline-inactive2 ((t :background ,dgray :foreground ,lgray)))
   
   ;; Escape and prompt faces
    `(minibuffer-prompt ((,class (:weight bold :foreground ,orange))))
    `(escape-glyph ((,class (:foreground ,dgray))))
    `(error ((,class (:background nil :weight bold :underline t :foreground ,red))))
    `(warning ((,class (:background nil :foreground ,orange))))
    `(success ((,class (:background nil :foreground ,green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,white))))
   `(font-lock-comment-face ((t (:foreground ,lgray))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,lgray))))
   `(font-lock-doc-face ((t (:foreground ,lgray))))
   `(font-lock-doc-string-face ((t (:foreground ,mgray))))
   `(font-lock-special-keyword-face ((t (:foreground ,white))))
   `(font-lock-constant-face ((t (:foreground ,white))))
   `(font-lock-function-name-face ((t (:foreground ,white))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,white))))
   `(font-lock-string-face ((t (:foreground ,lgray))))
   `(font-lock-type-face ((t (:foreground ,white))))
   `(font-lock-variable-name-face ((t ( :foreground ,white))))
   `(font-lock-warning-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face ((t (:foreground ,white))))
   `(sh-quoted-exec ((t (:foreground ,lgray))))

   ;; compilation
   `(compilation-column-face ((t (:foreground ,white))))
   `(compilation-enter-directory-face ((t (:foreground ,dgray))))
   `(compilation-error-face ((t (:foreground ,dgray :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,dgray))))
   `(compilation-info-face ((t (:foreground ,dgray))))
   `(compilation-info ((t (:foreground ,white :background nil :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,dgray))))
   `(compilation-line-face ((t (:background ,black :foreground ,white))))
   `(compilation-line-number ((t (:background ,black :foreground ,white))))
   `(compilation-message-face ((t (:foreground ,lgray))))
   `(compilation-warning-face ((t (:foreground ,dgray :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,lgray))))
   `(grep-error-face ((t (:foreground ,lgray :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,lgray))))
   `(grep-match-face ((t (:foreground ,orange :weight bold))))
   `(match ((t (:background ,lgray :foreground ,dgray ))))

   ;; diff
   `(diff-header ((t (:background nil :foreground ,lgray))))
   `(diff-file-header ((t (:inherit default :foreground ,lgray :bold t))))
   `(diff-index-face ((t (:background nil :foreground ,lgray :bold t))))
   `(diff-context ((t (:foreground ,mgray :background nil))))
   `(diff-added ((t (:foreground ,white :background nil :weight normal))))
   `(diff-removed ((t (:foreground ,lgray :background nil))))
   `(diff-changed ((t (:foreground ,orange :background nil))))
   `(diff-refine-added ((t (:foreground ,orange :bold t :background nil))))
   `(diff-refine-removed ((t (:background nil :foreground ,lgray :bold t))))
   `(diff-refine-change ((t (:weight bold :foreground ,orange :background ,black))))


   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,green))))
   `(link-visited ((,class (:underline t :foreground ,lgray))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,white))))
   `(ido-only-match ((,class (:foreground ,lgray))))
   `(ido-dir ((,class (:foreground ,lgray))))
   `(ido-subdir ((,class (:weight bold :foreground ,white))))

   ;; magit
   `(magit-section-title ((t (:foreground ,dgray :weight bold))))
   `(magit-branch ((t (:foreground ,orange :weight bold))))
   `(magit-item-highlight ((t (:background ,black))))

   `(company-tooltip ((,class (:background ,dgray))))
   `(company-scrollbar-bg ((,class (:background ,dgray))))

   ;; flycheck
   ;; `(flycheck-error-face ((t (:foreground ,red :weight bold :underline t))))
   ;; `(flycheck-warning-face ((t (:foreground ,orange :weight bold :underline t :background nil))))
   )

  (custom-theme-set-variables
   'mono-dark
   `(ansi-color-names-vector [,white ,dgray ,lgray ,mgray ,black ,red ,green ,orange])))

(provide-theme 'mono-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mono-dark-theme.el ends here
