(deftheme mono-dark
  "Dark monochrome theme")

(let ((class '((class color) (min-colors 10)))
      (bg "#383838")
      (bg+1 "#434343")
      (black "#080808") 
      (white "#eaeaea") 
      (lgray "#b3b3b3")
      (lgray+1 "#c3c3c3")
      (lgray+2 "#d3d3d3")
      (dgray "#303030")
      (dgray+1 "#404040")
      (sgray "#606060")
      (sgray+1 "#707070")
      (sgray+2 "#808080")
      (orange "#d75f00")
      (green  "#008B00")
      (red  "#B51816"))
  
  (custom-theme-set-faces
   'mono-dark

   `(default ((,class (:foreground ,white  :background ,bg))))
   `(cursor ((,class (:background ,green))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,bg))))
   `(highlight ((,class (:foreground ,white :background ,dgray))))
   `(region ((,class (:foreground ,white :background ,sgray+1))))
   `(secondary-selection ((,class (:foreground: ,white :background ,sgray))))
   `(isearch ((,class (:foreground ,white :background ,orange))))
   `(lazy-highlight ((,class (:foreground ,dgray :background ,lgray))))
   `(trailing-whitespace ((,class (:background ,red))))

   ;; Mode line faces
   `(minibuffer-prompt ((t (:foreground ,orange))))
   `(mode-line-buffer-id ((t (:foreground ,lgray+1 :weight bold))))
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
                              :background ,sgray :foreground ,lgray+1))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
                                       :background ,bg
                                       :foreground ,lgray))))

   ;; Escape and prompt faces
    `(minibuffer-prompt ((,class (:weight bold :foreground ,orange))))
    `(escape-glyph ((,class (:foreground ,dgray))))
    `(error ((,class (:background ,bg :weight bold :underline t :foreground ,red))))
    `(warning ((,class (:background ,dgray :foreground "yellow"))))
    `(success ((,class (:background ,dgray :foreground "green"))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,white))))
   `(font-lock-comment-face ((t (:foreground ,sgray+2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,sgray+2))))
   `(font-lock-doc-face ((t (:foreground ,sgray+2))))
   `(font-lock-doc-string-face ((t (:foreground ,sgray+2))))
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
   `(compilation-info ((t (:foreground ,lgray+1 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,dgray))))
   `(compilation-line-face ((t (:background ,bg :foreground ,lgray+1))))
   `(compilation-line-number ((t (:background ,bg :foreground ,lgray+1))))
   `(compilation-message-face ((t (:foreground ,lgray))))
   `(compilation-warning-face ((t (:foreground ,dgray :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,lgray))))
   `(grep-error-face ((t (:foreground ,lgray :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,lgray))))
   `(grep-match-face ((t (:foreground ,orange :weight bold))))
   `(match ((t (:background ,orange :foreground ,dgray ))))

   ;; diff
   `(diff-header ((t (:background ,bg :foreground ,lgray))))
   `(diff-file-header ((t (:background ,bg :foreground ,lgray :bold t))))
   `(diff-index-face ((t (:background ,bg :foreground ,lgray :bold t))))
   `(diff-context ((t (:foreground ,lgray))))
   `(diff-added ((t (:foreground ,white))))
   `(diff-removed ((t (:foreground ,lgray+2))))
   `(diff-changed ((t (:foreground ,orange :background ,bg))))
   `(diff-refine-added ((t (:background ,bg :foreground ,orange :bold t))))
   `(diff-refine-removed ((t (:background ,bg :foreground ,lgray :bold t))))
   `(diff-refine-change ((t (:weight bold :foreground ,orange :background ,bg))))


   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,green))))
   `(link-visited ((,class (:underline t :foreground ,lgray))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,dgray))))
   `(ido-only-match ((,class (:underline ,dgray :foreground ,dgray))))
   `(ido-subdir ((,class (:weight bold :foreground ,black))))

   ;; magit
   `(magit-section-title ((t (:foreground ,dgray :weight bold))))
   `(magit-branch ((t (:foreground ,orange :weight bold))))
   `(magit-item-highlight ((t (:background ,bg+1))))
)

  (custom-theme-set-variables
   'mono-dark
   `(ansi-color-names-vector [,white ,dgray ,lgray ,sgray])))

(provide-theme 'mono-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mono-dark-theme.el ends here
