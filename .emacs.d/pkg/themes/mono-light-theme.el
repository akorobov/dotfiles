(deftheme mono-light
  "Light monochrome theme")

(let ((class '((class color) (min-colors 10)))
      (black "#080808")
      (white "#fafafa")
      (white-1 "#dadada")
      (lgray "#b3b3b3")
      (lgray+1 "#c3c3c3")
      (dgray "#303030")
      (sgray "#606060")
      (sgray+1 "#707070")
      (orange "#FF9912")
      (green  "#5f5f00"))

  (custom-theme-set-faces
   'mono-light

   `(default ((,class (:foreground ,black :background ,white))))
   `(cursor ((,class (:background ,green))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,white))))
   `(highlight ((,class (:foreground ,white :background ,sgray))))
   `(region ((,class (:foreground ,white :background ,sgray+1))))
   `(secondary-selection ((,class (:foreground: ,white :background ,sgray))))
   `(isearch ((,class (:foreground ,white :background ,orange))))
   `(lazy-highlight ((,class (:foreground ,white :background ,sgray))))
   `(trailing-whitespace ((,class (:background "red"))))

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
    `(error ((,class (:background ,white :weight bold :foreground "red"))))
    `(warning ((,class (:background ,white :foreground ,orange))))
    `(success ((,class (:background ,dgray :foreground ,green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,black))))
   `(font-lock-comment-face ((t (:foreground ,sgray))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,sgray))))
   `(font-lock-doc-face ((t (:foreground ,sgray))))
   `(font-lock-doc-string-face ((t (:foreground ,sgray))))
   `(font-lock-special-keyword-face ((t (:foreground ,sgray))))
   `(font-lock-constant-face ((t (:foreground ,dgray))))
   `(font-lock-function-name-face ((t (:foreground ,black))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,dgray))))
   `(font-lock-string-face ((t (:foreground ,sgray))))
   `(font-lock-type-face ((t (:foreground ,dgray))))
   `(font-lock-variable-name-face ((t ( :foreground ,dgray))))
   `(font-lock-warning-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face ((t (:foreground ,dgray))))

   ;; compilation
   `(compilation-column-face ((t (:foreground ,dgray))))
   `(compilation-enter-directory-face ((t (:foreground ,dgray))))
   `(compilation-error-face ((t (:foreground ,dgray :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,dgray))))
   `(compilation-info-face ((t (:foreground ,dgray))))
   `(compilation-info ((t (:foreground ,dgray :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,dgray))))
   `(compilation-line-face ((t (:foreground ,dgray))))
   `(compilation-line-number ((t (:foreground ,dgray))))
   `(compilation-message-face ((t (:foreground ,dgray))))
   `(compilation-warning-face ((t (:foreground ,dgray :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,dgray))))
   `(grep-error-face ((t (:foreground ,dgray :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,dgray))))
   `(grep-match-face ((t (:foreground ,orange))))
   `(match ((t (:background ,orange :foreground ,dgray))))

   ;; diff
   `(diff-added ((,class (:foreground ,dgray))
                 (t (:foreground ,dgray))))
   `(diff-changed ((t (:foreground ,orange))))
   `(diff-refine-change ((t (:weight bold :foreground "#ff5f00" :background ,white))))
   `(diff-removed ((,class (:foreground ,sgray))
                   (t (:foreground ,lgray))))
   `(diff-header ((,class (:background ,white))
                  (t (:background ,white :foreground ,dgray))))
   `(diff-file-header
     ((,class (:background ,white :foreground ,dgray :bold t))
      (t (:background ,white :foreground ,dgray :bold t))))
   `(diff-index-face
     ((,class (:background ,white :foreground ,dgray :bold t))
      (t (:background ,white :foreground ,dgray :bold t))))


   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,dgray))))
   `(link-visited ((,class (:underline t :foreground ,dgray))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,dgray))))
   `(ido-only-match ((,class (:underline ,dgray :foreground ,dgray))))
   `(ido-subdir ((,class (:weight bold :foreground ,black))))

   `(flymake-errline ((t (:underline t :background "#8B0000"))))
   `(flymake-warnline ((t (:underline t :background "#00BFBF"))))

   `(magit-section-title ((t (:foreground ,dgray :weight bold))))
   `(magit-branch ((t (:foreground ,orange :weight bold))))
   `(magit-item-highlight ((t (:background ,white-1))))

   )

  (custom-theme-set-variables
   'mono-light
   `(ansi-color-names-vector [,white ,dgray ,lgray ,sgray])))

(provide-theme 'mono-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mono-light-theme.el ends here
