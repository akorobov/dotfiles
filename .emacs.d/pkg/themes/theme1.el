(deftheme theme1 "The Theme1 color theme")

; base color theme
(let ((class '((class color) (min-colors 89)))
      ;; Theme1 palette
      ;; colors with +x are lighter, colors with -x are darker
      (theme1-bg "#293134")
      (theme1-bg+1 "#394144")
      (theme1-bg-1 "#192124")
      (theme1-fg "#E0E2E4")


      ; color faces
      (theme1-builtin "#93C763")
      (theme1-keyword "#93C763")
      (theme1-comment "#7D8C93")
      (theme1-doc "#7D8C93")
      (theme1-doc-string "#7D8C93")
      (theme1-constant "#A082BD")
      (theme1-negation-char "#E0E2E4") ; same as fg
      (theme1-preprocessor "#93C763")
      (theme1-string "#EC7600")
      (theme1-type "#E0E2E4")
      (theme1-variable "#E0E2E4")
      (theme1-warning "#FFCD22")


      ; colors
      ;; (theme1-red "#")
      ;; (theme1-blue "#")
      ;; (theme1-orange "#")
      ;; (theme1-yellow "#")
      ;; (theme1-green "#")

      ;; (theme1-fg "#dcdccc")
      ;; (theme1-fg-1 "#656555")
      ;; (theme1-bg-1 "#2b2b2b")
      ;; (theme1-bg-05 "#383838")
      ;; (theme1-bg "#3f3f3f")
      ;; (theme1-bg+1 "#4f4f4f")
      ;; (theme1-bg+2 "#5f5f5f")
      ;; (theme1-bg+3 "#6f6f6f")
      (theme1-red+1 "#dca3a3")
      (theme1-red "#cc9393")
      (theme1-red-1 "#bc8383")
      (theme1-red-2 "#ac7373")
      (theme1-red-3 "#9c6363")
      (theme1-red-4 "#8c5353")
      (theme1-orange "#dfaf8f")
      (theme1-yellow "#f0dfaf")
      (theme1-yellow-1 "#e0cf9f")
      (theme1-yellow-2 "#d0bf8f")
      (theme1-green-1 "#5f7f5f")
      (theme1-green "#7f9f7f")
      (theme1-green+1 "#8fb28f")
      (theme1-green+2 "#9fc59f")
      (theme1-green+3 "#afd8af")
      (theme1-green+4 "#bfebbf")
      (theme1-cyan "#93e0e3")
      (theme1-blue+1 "#94bff3")
      (theme1-blue "#8cd0d3")
      (theme1-blue-1 "#7cb8bb")
      (theme1-blue-2 "#6ca0a3")
      (theme1-blue-3 "#5c888b")
      (theme1-blue-4 "#4c7073")
      (theme1-blue-5 "#366060")
      (theme1-magenta "#dc8cc3")
      (theme1-sel-bg "#184259")
      )

  (custom-theme-set-faces
   'theme1
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,theme1-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,theme1-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((t (:foreground ,theme1-fg :background ,theme1-bg))))
   `(cursor ((t (:foreground ,theme1-fg :background "white"))))
   `(escape-glyph ((t (:foreground ,theme1-yellow :bold t))))
   `(fringe ((t (:foreground ,theme1-fg :background ,theme1-bg))))
   `(header-line ((t (:foreground ,theme1-yellow
                                  :background ,theme1-bg
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,theme1-bg))))

   ;;; compilation
   `(compilation-column-face ((t (:foreground ,theme1-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,theme1-green))))
   `(compilation-error-face ((t (:foreground ,theme1-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,theme1-fg))))
   `(compilation-info-face ((t (:foreground ,theme1-blue))))
   `(compilation-info ((t (:foreground ,theme1-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,theme1-green))))
   `(compilation-line-face ((t (:foreground ,theme1-yellow))))
   `(compilation-line-number ((t (:foreground ,theme1-yellow))))
   `(compilation-message-face ((t (:foreground ,theme1-blue))))
   `(compilation-warning-face ((t (:foreground ,theme1-orange :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,theme1-fg))))
   `(grep-error-face ((t (:foreground ,theme1-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,theme1-blue))))
   `(grep-match-face ((t (:foreground ,theme1-orange :weight bold))))
   `(match ((t (:background ,theme1-bg-1 :foreground ,theme1-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((t (:foreground ,theme1-yellow :background ,theme1-orange))))
   `(isearch-fail ((t (:foreground ,theme1-fg :background ,theme1-red-4))))
   `(lazy-highlight ((t (:foreground ,theme1-yellow :background ,theme1-bg))))

   `(menu ((t (:foreground ,theme1-fg :background ,theme1-bg))))
   `(minibuffer-prompt ((t (:foreground ,theme1-yellow))))
   `(mode-line
     ((,class (:foreground ,theme1-green+1
                           :background ,theme1-bg
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,theme1-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,theme1-green-1
                      :background ,theme1-bg
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,theme1-bg+1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background, theme1-sel-bg))))
   `(trailing-whitespace ((t (:background ,theme1-red))))
   `(vertical-border ((t (:foreground ,theme1-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,theme1-fg))))
   `(font-lock-comment-face ((t (:foreground ,theme1-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,theme1-green))))
   `(font-lock-constant-face ((t (:foreground ,theme1-green+4))))
   `(font-lock-doc-face ((t (:foreground ,theme1-green+1))))
   `(font-lock-doc-string-face ((t (:foreground ,theme1-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,theme1-blue))))
   `(font-lock-keyword-face ((t (:foreground ,theme1-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,theme1-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,theme1-blue+1))))
   `(font-lock-string-face ((t (:foreground ,theme1-red))))
   `(font-lock-type-face ((t (:foreground ,theme1-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,theme1-orange))))
   `(font-lock-warning-face ((t (:foreground ,theme1-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
   ;; show-paren
   `(show-paren-mismatch ((t (:foreground ,theme1-red-3 :background ,theme1-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,theme1-blue-1 :background ,theme1-bg :weight bold))))

   `(fci-rule-color ,theme1-bg)))

(provide-theme 'theme1)

