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
   `(mode-line-buffer-id ((t (:foreground ,dgray :weight bold))))
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
                              :background ,sgray :foreground ,lgray+2))))
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

   ;; compilation
   `(compilation-column-face ((t (:foreground ,white))))
   `(compilation-enter-directory-face ((t (:foreground ,dgray))))
   `(compilation-error-face ((t (:foreground ,dgray :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,dgray))))
   `(compilation-info-face ((t (:foreground ,dgray))))
   `(compilation-info ((t (:foreground ,lgray+1 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,dgray))))
   `(compilation-line-face ((t (:foreground ,lgray+1))))
   `(compilation-line-number ((t (:foreground ,lgray+1))))
   `(compilation-message-face ((t (:foreground ,lgray))))
   `(compilation-warning-face ((t (:foreground ,dgray :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,lgray))))
   `(grep-error-face ((t (:foreground ,lgray :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,lgray))))
   `(grep-match-face ((t (:foreground ,orange :weight bold))))
   `(match ((t (:background ,orange :foreground ,dgray ))))

   ;; diff
   `(diff-added ((,class (:foreground ,white))
                 (t (:foreground ,white))))
   `(diff-changed ((t (:foreground ,orange :background ,bg))))
   `(diff-refine-change ((t (:weight bold :foreground ,orange :background ,bg))))
   `(diff-removed ((,class (:foreground ,lgray))
                   (t (:foreground ,lgray))))
   `(diff-header ((,class (:background ,bg))
                  (t (:background ,bg :foreground ,dgray))))
   `(diff-file-header ((,class (:background ,bg :foreground ,lgray+2 :bold t))
                       (t (:background ,bg :foreground ,sgray :bold t))))
   `(diff-index-face ((,class (:background ,bg :foreground ,dgray :bold t))
                      (t (:background ,bg :foreground ,sgray :bold t))))


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
