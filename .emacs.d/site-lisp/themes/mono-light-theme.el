;;; mono-light.el --- Light monochrome theme for Emacs.

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
      (green  "##00f900")
      (red  "#ff2600")
      (dgreen "#009800")
      )

  (custom-theme-set-faces
   'mono-light

   `(default ((,class (:foreground ,black :background ,white))))
   `(cursor ((,class (:background ,dgreen))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,white))))
   `(highlight ((,class (:foreground ,white :background ,lgray))))
   `(region ((,class (:foreground ,white :background ,mgray))))
   `(secondary-selection ((,class (:foreground: ,white :background ,mgray))))
   `(isearch ((,class (:foreground ,white :background ,orange))))
   `(lazy-highlight ((,class (:foreground ,white :background ,dgray))))
   `(trailing-whitespace ((,class (:background ,red))))

   ;; Mode line faces
   `(mode-line-buffer-id ((t (:foreground ,black :weight bold))))
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
                              :background ,lgray :foreground ,dgray))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
                                       :background ,white
                                       :foreground ,black))))

   ;; Escape and prompt faces
    `(minibuffer-prompt ((,class (:weight bold :foreground ,dgray))))
    `(escape-glyph ((,class (:foreground ,dgray))))
    `(error ((,class (:background nil :weight bold :foreground ,red))))
    `(warning ((,class (:background nil :foreground ,orange))))
    `(success ((,class (:background nil :foreground ,green))))

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
   `(diff-context ((t (:foreground ,lgray))))
   `(diff-added ((t (:foreground ,black))))
   `(diff-removed ((t (:foreground ,mgray))))
   `(diff-changed ((t (:foreground ,orange))))
   `(diff-refine-added ((t (:weight bold :foreground "#ff5f00" :background nil))))
   `(diff-refine-removed ((t (:weight bold :foreground "#ff5f00" :background nil))))
   `(diff-refine-change ((t (:weight bold :foreground "#ff5f00" :background nil))))


   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,dgray))))
   `(link-visited ((,class (:underline t :foreground ,dgray))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,dgray))))
   `(ido-only-match ((,class (:foreground ,dgray))))
   `(ido-subdir ((,class (:weight bold :foreground ,black))))

   `(flymake-errline ((t (:underline t :background ,white :foreground ,red))))
   `(flymake-warnline ((t (:underline t :background ,white :foreground ,green))))

   `(magit-section-title ((t (:foreground ,dgray :weight bold))))
   `(magit-branch ((t (:foreground ,orange :weight bold))))
   `(magit-item-highlight ((t (:background nil))))

   `(company-tooltip ((,class (:background ,lgray))))
   `(company-scrollbar-bg ((,class (:background ,lgray))))
   ;; flycheck
   `(flycheck-error-face ((t (:foreground ,red :weight bold :underline t))))
   `(flycheck-warning-face ((t (:foreground ,orange :weight bold :underline t :background nil))))
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
