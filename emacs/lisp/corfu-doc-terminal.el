;;; corfu-doc-terminal.el --- `corfu-doc' popup on terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-05-28
;; Version: 0.9
;; Package-Requires: ((emacs "25.1") (corfu-doc "0.5.1") (corfu-terminal "0.2") (popon "0.3"))
;; Keywords: convenience
;; Homepage: https://codeberg.org/akib/emacs-corfu-doc-terminal

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `corfu-doc' uses child frames to display candidates.  This makes
;; `corfu-doc' unusable on terminal.  This package replaces that with
;; popup/popon, which works everywhere.  Use M-x corfu-doc-terminal-mode to
;; enable.  You'll probably want to enable it only on terminal.  In that
;; case, put the following in your init file:

;;   (unless (display-graphic-p)
;;     (corfu-doc-terminal-mode +1))

;; To use this package, you need to enable `corfu-terminal-mode'.

;;; Code:

(require 'popon)
(require 'corfu-doc)
(require 'corfu-terminal)
(require 'avl-tree)

(defgroup corfu-doc-terminal nil
  "`corfu-doc' popup on terminal."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-corfu-doc-terminal")
  :prefix "corfu-doc-terminal-")

(defcustom corfu-doc-terminal-position-right-margin 0
  "Number of columns of margin at the right of window.

Always keep the popup atleast this many columns away from the right edge of
the window.

Note: If the popup breaks or crosses the right edge of window, you may set
this variable to warkaround it.  But remember, that's a *bug*, so if that
ever happens to you please report the issue at
https://codeberg.org/akib/emacs-corfu-doc-terminal/issues."
  :type 'integer)

(defcustom corfu-doc-terminal-decoration-threshold 10
  "Don't do decorations if it decreases content width to less that this.

Decorations include margins, scroll bar, containuation and non-continuation
string."
  :type 'integer)

(defcustom corfu-doc-terminal-left-margin " "
  "String to show on the left margin."
  :type 'string)

(defcustom corfu-doc-terminal-right-margin " "
  "String to show on the right margin."
  :type 'string)

(defcustom corfu-doc-terminal-scroll-bar " "
  "String to show on the scroll bar.

If this string occupies less columns that
`corfu-doc-terminal-right-margin', the scroll bar shown on the right side.
For example if this is set to \"12\" and `corfu-doc-terminal-right-margin'
is set to \"abcd\", \"ab12\" will appear in the popon.

This should ideally occupy less or equal number of columns as
`corfu-doc-terminal-right-margin'."
  :type 'string)

(defcustom corfu-doc-terminal-continuation-string "\\"
  "Show this string at the end of wrapped lines.

This string should occupy the same number of columns as
`corfu-doc-terminal-non-continuation-string', otherwise the popon would
break."
  :type 'string)

(defcustom corfu-doc-terminal-non-continuation-string " "
  "Show this string at the end of lines that aren't wrapped.

This string should occupy the same number of columns as
`corfu-doc-terminal-continuation-string', otherwise the popon would break."
  :type 'string)

(defcustom corfu-doc-terminal-auto-resize-function
  #'corfu-doc-terminal-auto-resize
  "Function to return the size of popon.

The function should take a two arguments CONTENTS, MAX-SIZE, where CONTENTS
is the contents of the popon and MAX-SIZE is a cons cell of form (MAX-WIDTH
. MAX-HEIGHT), which specifies the maximum size of popon.  It should
calculate the width WIDTH and height HEIGHT of the popon from it and return
it as (WIDTH . HEIGHT).  The function should be pure and side-effect-free."
  :type 'function)

(defface corfu-doc-terminal-continuation
  '((t :inherit corfu-default))
  "Face for continuation string at the end of wrapped line.")

(defface corfu-doc-terminal-scroll-bar
  '((t :inherit corfu-bar))
  "Face for scroll bar.")

(defvar corfu-doc-terminal--popon nil
  "Popon used to show documentation.")

(defvar corfu-doc-terminal--popon-text nil
  "The text of popon.")

(defvar corfu-doc-terminal--popon-position nil
  "The position of popon.")

(defvar corfu-doc-terminal--doc-string nil
  "The current documentation visible in popon.")

(defvar corfu-doc-terminal--doc-start-position nil
  "The start position of the visible part of documentation.")

(defvar corfu-doc-terminal--doc-scroll nil
  "The number of line the documentation has been scroll so far.")

(defvar corfu-doc-terminal--doc-scroll-tree nil
  "AVL tree of scrolled line counts and corresponding start positions.")

(defun corfu-doc-terminal--use-child-frame-p ()
  "Return non-nil if child frame should be used."
  (or (not corfu-terminal-mode)
      (and corfu-terminal-disable-on-gui
           (display-graphic-p))))

(defun corfu-doc-terminal--popup-visible-p (fn)
  "Check whether the popon is visible.

Call FN if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (popon-live-p corfu-doc-terminal--popon)))

(defun corfu-doc-terminal--cf-popup-visible-p (fn)
  "Check whether the popon is visible.

Call FN if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (popon-live-p corfu-terminal--popon)))

(defun corfu-doc-terminal--get-cf-popup-edges (fn)
  "Get the edges of the `corfu-terminal' popon.

The edges are relative to window's origin point.

Call FN Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (let ((position (popon-position corfu-terminal--popon))
          (size (popon-size corfu-terminal--popon)))
      (list (car position) (cdr position) (+ (car position) (car size))
            (+ (cdr position) (cdr size))))))

(defun corfu-doc-terminal--should-refresh-popup (fn candidate)
  "Return whether popon should be refreshed.

CANDIDATE should be the selected candidate.

Call FN with CANDIDATE if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn candidate)
    (and (string= candidate corfu-doc--candidate)
         (eq (selected-window) corfu-doc--cf-window)
         (popon-live-p corfu-doc-terminal--popon))))

(defun corfu-doc-terminal--clear-buffer (fn)
  "Clear popon.

Call FN if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (when (popon-live-p corfu-doc-terminal--popon)
      (let* ((size (popon-size corfu-doc-terminal--popon))
             (line (make-string (car size) ? )))
        (setq corfu-doc-terminal--popon-text
              (mapconcat (lambda (_) line)
                         (number-sequence 0 (1- (cdr size))) "\n"))
        (popon-kill corfu-doc-terminal--popon)
        (setq corfu-doc-terminal--popon
              (popon-create corfu-doc-terminal--popon-text
                            corfu-doc-terminal--popon-position
                            nil nil -1))))))

(defun corfu-doc-terminal--make-popup-invisible (fn)
  "Make popon invisible.

Call FN if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (when (popon-live-p corfu-doc-terminal--popon)
      (popon-kill corfu-doc-terminal--popon)
      (setq corfu-doc-terminal--popon nil))))

(defun corfu-doc-terminal--popup-hide (fn)
  "Hide popon.

Call FN if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (when (popon-live-p corfu-doc-terminal--popon)
      (popon-kill corfu-doc-terminal--popon)
      (setq corfu-doc-terminal--popon nil)
      (setq corfu-doc-terminal--popon-text nil)
      (setq corfu-doc-terminal--popon-position nil)
      (setq corfu-doc-terminal--popon nil)
      (setq corfu-doc-terminal--doc-string nil)
      (setq corfu-doc-terminal--doc-start-position nil)
      (setq corfu-doc-terminal--doc-scroll nil)
      (setq corfu-doc-terminal--doc-scroll-tree nil)
      (corfu-doc--set-vars nil nil nil))))

(defun corfu-doc-terminal--calc-popon-position-size ()
  "Calculate the position and size of the popon.

The return value is a cons cell whose car is the position and cdr is the
size."
  ;; Adapted from `corfu-doc--calculate-doc-frame-position' in corfu-doc.el
  (let* ((x nil)
         (y nil)
         (space 0)
         (cf-window (popon-window corfu-terminal--popon))
         (cf-popon--pos (popon-position corfu-terminal--popon))
         (cf-popon-x (car cf-popon--pos))  ; corfu--frame x pos
         (cf-popon-y (cdr cf-popon--pos))
         (cf-popon--size (popon-size corfu-terminal--popon))
         (cf-popon-width (car cf-popon--size))
         (cf-popon-height (cdr cf-popon--size))
         (cf-window-width (window-max-chars-per-line cf-window))
         (cf-window-height
          (max 0 (- (window-body-height cf-window)
                    corfu-doc-terminal-position-right-margin)))
         (cf-doc-popon-max-width (min corfu-doc-max-width cf-window-width))
         (cf-doc-popon-max-height (min corfu-doc-max-height
                                       cf-window-height))
         (cf-doc-popon-auto-size
          (when corfu-doc-terminal-auto-resize-function
            (funcall corfu-doc-terminal-auto-resize-function
                     corfu-doc-terminal--doc-string
                     (cons cf-doc-popon-max-width
                           cf-doc-popon-max-height))))
         (cf-doc-popon-width (min (car cf-doc-popon-auto-size)
                                  cf-doc-popon-max-width))
         (cf-doc-popon-height (min (cdr cf-doc-popon-auto-size)
                                   cf-doc-popon-max-height))
         (cf-point-y
          (if (< (point) (point-max))
              (cdr (popon-x-y-at-pos (point)))
            ;; This is needed, because if `point' is at `point-max', the
            ;; cursor will be after overlay.  To workaround that, we move
            ;; to the beginning of line, and then calculate the Y value.
            (save-excursion
              (goto-char (vertical-motion 0))
              (cdr (popon-x-y-at-pos (point))))))
         (cf-window-space-right
          (- cf-window-width (+ cf-popon-x cf-popon-width space)))
         (cf-window-space-left (- cf-popon-x space))
         (cf-window-space-top (min (- cf-popon-y space) cf-point-y))
         (cf-window-space-bottom
          (- cf-window-height (max (+ cf-popon-y cf-popon-height space)
                                   (+ cf-point-y 1)))))
    (cond
     ((or (>= cf-window-space-right cf-doc-popon-width)
          (and (>= cf-window-space-right cf-window-space-left)
               (>= cf-doc-popon-height cf-window-space-top)
               (>= cf-doc-popon-height cf-window-space-bottom)))
      (setq x (+ cf-popon-x cf-popon-width space))
      (setq y cf-popon-y))
     ((or (>= cf-window-space-left cf-doc-popon-width)
          (and (> cf-window-space-left cf-window-space-right)
               (>= cf-doc-popon-height cf-window-space-top)
               (>= cf-doc-popon-height cf-window-space-bottom)))
      (setq x (- cf-popon-x space cf-doc-popon-width))
      (setq y cf-popon-y))
     (t
      (setq x cf-popon-x)
      (setq y (cond
               ((< cf-popon-y cf-point-y)
                (if (and (< cf-window-space-top cf-doc-popon-width)
                         (>= (1+ cf-point-y) 0))
                    (1+ cf-point-y)
                  (max 0 (- cf-popon-y space cf-doc-popon-height))))
               (t
                (if (and (< cf-window-space-bottom cf-doc-popon-width)
                         (>= (- cf-point-y cf-doc-popon-height) 0))
                    (- cf-point-y cf-doc-popon-height)
                  (+ cf-popon-y cf-popon-height space)))))))
    (when (>= (+ x cf-doc-popon-width) cf-window-width)
      (if (= y cf-popon-y)
          (setq cf-doc-popon-width (- cf-window-width x))
        (setq x (max (- cf-window-width cf-doc-popon-width) 0))
        (when (< cf-window-width cf-doc-popon-width)
          (setq cf-doc-popon-width cf-window-width))))
    (when (< x 0)
      (setq cf-doc-popon-width (+ cf-doc-popon-width x))
      (setq x 0))
    (when (and (= y cf-popon-y)
               (> cf-point-y cf-popon-y)
               (> cf-doc-popon-height cf-popon-height))
      (setq y (max (- cf-point-y cf-doc-popon-height) 0))
      (setq cf-doc-popon-height (- cf-point-y y)))
    (when (> (+ y cf-popon-height) cf-window-height)
      (setq cf-popon-height (- cf-window-height y)))
    `((,x . ,y) . (,cf-doc-popon-width . ,cf-doc-popon-height))))

(defun corfu-doc-terminal-auto-resize (contents max-size)
  "Calculate popon size from CONTENTS but don't exceed MAX-SIZE."
  (let ((lines (split-string contents "\n")))
    (cons (min (+ (apply #'max
                         (last (nreverse (mapcar #'string-width lines))
                               (cdr max-size)))
                  (string-width corfu-doc-terminal-left-margin)
                  (string-width corfu-doc-terminal-continuation-string)
                  (string-width corfu-doc-terminal-right-margin))
               (car max-size))
          (max (cdr max-size) (length lines)))))

(defun corfu-doc-terminal--calc-popon-text (size)
  "Calculate the text to show given the SIZE of popon."
  (let* ((width (car size))
         (height (cdr size))
         (decoration-width
          (+ (string-width corfu-doc-terminal-left-margin)
             (string-width corfu-doc-terminal-continuation-string)
             (string-width corfu-doc-terminal-right-margin)))
         (show-decorations-p (> (- width decoration-width)
                                corfu-doc-terminal-decoration-threshold))
         (lines nil)
         (i corfu-doc-terminal--doc-start-position))
    (when show-decorations-p
      (setq width (- width decoration-width)))
    (dotimes (j height)
      (let ((line "")
            (column 0)
            (break nil))
        (when (>= i (length corfu-doc-terminal--doc-string))
          (avl-tree-enter corfu-doc-terminal--doc-scroll-tree
                          (cons i (+ corfu-doc-terminal--doc-scroll j))))
        (while (not break)
          (let ((char (if (>= i (length corfu-doc-terminal--doc-string))
                          "\n"
                        (substring corfu-doc-terminal--doc-string i
                                   (1+ i)))))
            (if (or (> (+ column (string-width char)) width)
                    (string= char "\n"))
                (progn
                  (let* ((extend-attr
                          (lambda (face)
                            (if (facep face)
                                (face-attribute face :extend)
                              (or (cadr (plist-member face :extend))
                                  'unspecified))))
                         (extend 'unspecified))
                    (let ((faceprop (get-char-property 0 'face char)))
                      (when (< column width)
                        (setq line (concat line (propertize " " 'face
                                                            faceprop)))
                        (setq column (1+ column)))
                      (catch 'done
                        (if (face-list-p faceprop)
                            (dolist (face faceprop)
                              (setq extend (funcall extend-attr face))
                              (when (booleanp extend)
                                (throw 'done extend)))
                          (setq extend (funcall extend-attr faceprop))
                          (when (booleanp extend)
                            (throw 'done extend)))
                        (setq extend (funcall extend-attr 'default)))
                      (let ((space (make-string (- width column) ? )))
                        (setq line (concat line (if extend
                                                    (propertize space 'face
                                                                faceprop)
                                                  space))))
                      (setq break t)))
                  (when (or (string= char "\n")
                            (string= char "\t"))
                    (setq i (1+ i)))
                  (when show-decorations-p
                    (setq
                     line
                     (concat
                      line
                      (if (string= char "\n")
                          (propertize
                           corfu-doc-terminal-non-continuation-string
                           'face 'corfu-doc-terminal-continuation)
                        (propertize
                         corfu-doc-terminal-continuation-string
                         'face 'corfu-doc-terminal-continuation))))))
              (setq line (concat line (if (string= char "\t")
                                          (make-string tab-width ? )
                                        char)))
              (setq column (+ column (string-width char)))
              (setq i (1+ i)))))
        (push line lines)))
    (let* ((right-margin-with-scroll-bar
            (with-temp-buffer
              (insert corfu-doc-terminal-right-margin)
              (move-to-column
               (max (- (string-width corfu-doc-terminal-right-margin)
                       (string-width corfu-doc-terminal-scroll-bar))
                    0))
              (insert (propertize corfu-doc-terminal-scroll-bar 'face
                                  'corfu-doc-terminal-scroll-bar))
              (move-to-column
               (string-width corfu-doc-terminal-right-margin))
              (buffer-substring (point-min) (point))))
           (k -1)
           (text
            (mapconcat
             (lambda (line)
               (if (not show-decorations-p)
                   line
                 (setq k (1+ k))
                 (concat
                  corfu-doc-terminal-left-margin
                  line
                  (if (<= (floor
                           (* (/ corfu-doc-terminal--doc-start-position
                                 (float (length
                                         corfu-doc-terminal--doc-string)))
                              height))
                          k
                          (ceiling
                           (*
                            (/ i (float (length
                                         corfu-doc-terminal--doc-string)))
                            height)))
                      right-margin-with-scroll-bar
                    corfu-doc-terminal-right-margin))))
             (nreverse lines) "\n")))
      (add-face-text-property 0 (length text) 'corfu-default t text)
      text)))

(defun corfu-doc-terminal--popon-show ()
  "Show the popon."
  (corfu-doc--make-popup-invisible)
  (let* ((position-size (corfu-doc-terminal--calc-popon-position-size)))
    (setq corfu-doc-terminal--popon-text
          (corfu-doc-terminal--calc-popon-text (cdr position-size)))
    (setq corfu-doc-terminal--popon-position (car position-size))
    (setq corfu-doc-terminal--popon
          (popon-create corfu-doc-terminal--popon-text
                        corfu-doc-terminal--popon-position nil nil -1))))

(defun corfu-doc-terminal--refresh-popup (fn)
  "Update the position of popon.

Call FN if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn)
    (corfu-doc-terminal--popon-show)
    (setq corfu-doc--cf-popup-edges (corfu-doc--get-cf-popup-edges))))

(defun corfu-doc-terminal--preprocess-docstring (str)
  "Preprocess STR to remove any problematic text properties."

  ;; Remove any text with `invisible' text property.
  (let ((pos 0))
    (while (< pos (length str))
      (let ((change (next-single-property-change pos 'invisible str
                                                 (length str))))
        (if (not (get-text-property pos 'invisible str))
            (setq pos change)
          (setq str (concat (substring str 0 pos)
                            (substring str change)))))))

  ;; Handle `display' property.
  (let ((pos 0))
    (while (< pos (length str))
      (let ((change (next-single-property-change pos 'display str
                                                 (length str))))
        (if (not (stringp (get-text-property pos 'display str)))
            (setq pos change)
          (let ((replacement (get-text-property pos 'display str)))
            (add-text-properties 0 (length replacement)
                                 (text-properties-at pos str) replacement)
            (setq str (concat (substring str 0 pos)
                              (propertize replacement 'display nil)
                              (substring str change)))
            (setq pos (+ pos (length replacement))))))))
  str)

(defun corfu-doc-terminal--update-popup (fn doc)
  "Update the position of popon.

Call FN with DOC if on graphical display and
`corfu-doc-terminal-disable-on-gui' is non-nil."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn doc)
    (setq corfu-doc-terminal--doc-string
          (corfu-doc-terminal--preprocess-docstring doc))
    (setq corfu-doc-terminal--doc-start-position 0)
    (setq corfu-doc-terminal--doc-scroll 0)
    (setq corfu-doc-terminal--doc-scroll-tree
          (avl-tree-create #'car-less-than-car))
    (avl-tree-enter corfu-doc-terminal--doc-scroll-tree '(0 . 0))
    (corfu-doc-terminal--popon-show)))

(defun corfu-doc-terminal--popup-scroll (fn arg)
  "Scroll up ARG lines.

ARG is same as in `scroll-up' (which see).

Call FN with ARG if Corfu is using child frame."
  (if (corfu-doc-terminal--use-child-frame-p)
      (funcall fn arg)
    (when (popon-live-p corfu-doc-terminal--popon)
      (let* ((proposed-scroll corfu-doc-terminal--doc-scroll)
             (size (popon-size corfu-doc-terminal--popon))
             (width (car size))
             (height (cdr size)))
        (cond
         ((eq arg nil)
          (setq proposed-scroll
                (+ proposed-scroll (- height (min next-screen-context-lines
                                                  (1- height))))))
         ((eq arg '-)
          (setq proposed-scroll
                (- proposed-scroll (- height (min next-screen-context-lines
                                                  (1- height))))))
         (t
          (setq proposed-scroll (+ proposed-scroll arg))))
        (when (and (>= proposed-scroll 0)
                   (/= proposed-scroll corfu-doc-terminal--doc-scroll))
          (let ((i nil)
                (start-line nil)
                (j nil)
                (cached-i (cdr (avl-tree-member
                                corfu-doc-terminal--doc-scroll-tree
                                (cons proposed-scroll nil)))))
            (cond
             (cached-i
              (setq i cached-i)
              (setq start-line proposed-scroll)
              (setq j 0))
             ((< proposed-scroll corfu-doc-terminal--doc-scroll)
              (setq i 0)
              (setq start-line 0)
              (setq j proposed-scroll))
             (t
              (setq i corfu-doc-terminal--doc-start-position)
              (setq start-line corfu-doc-terminal--doc-scroll)
              (setq j (- proposed-scroll corfu-doc-terminal--doc-scroll))))
            (dotimes (k j)
              (let ((column 0)
                    (break nil))
                (when (< i (length corfu-doc-terminal--doc-string))
                  (avl-tree-enter corfu-doc-terminal--doc-scroll-tree
                                  (cons (+ start-line k) i)))
                (while (and (< i (length corfu-doc-terminal--doc-string))
                            (not break))
                  (let ((char (aref corfu-doc-terminal--doc-string i)))
                    (if (or (> (+ column (char-width char)) width)
                            (eq char ?\n))
                        (progn
                          (when (or (eq char ?\n)
                                    (eq char ?\t))
                            (setq i (1+ i)))
                          (setq break t))
                      (setq column (+ column (char-width char)))
                      (setq i (1+ i)))))))
            (when (< i (length corfu-doc-terminal--doc-string))
              (setq corfu-doc-terminal--doc-scroll proposed-scroll)
              (setq corfu-doc-terminal--doc-start-position i)
              (corfu-doc-terminal--popon-show))))))))

(defun corfu-doc-terminal--increase-corfu-doc-advice-precedence ()
  "Increase the precedence of `corfu-doc''s advices on Corfu."
  (when (advice-member-p #'corfu-doc--popup-show #'corfu--popup-show)
    (advice-remove #'corfu--popup-show #'corfu-doc--popup-show)
    (advice-add #'corfu--popup-show :after #'corfu-doc--popup-show
                '((depth . -1))))
  (when (advice-member-p #'corfu-doc--popup-hide #'corfu--popup-hide)
    (advice-remove #'corfu--popup-hide #'corfu-doc--popup-hide)
    (advice-add #'corfu--popup-hide :after #'corfu-doc--popup-hide
                '((depth . -1)))))

;;;###autoload
(define-minor-mode corfu-doc-terminal-mode
  "`corfu-doc' popup on terminal."
  :global t
  (if corfu-doc-terminal-mode
      (progn
        (advice-add #'corfu-doc--popup-visible-p :around
                    #'corfu-doc-terminal--popup-visible-p)
        (advice-add #'corfu-doc--cf-popup-visible-p :around
                    #'corfu-doc-terminal--cf-popup-visible-p)
        (advice-add #'corfu-doc--get-cf-popup-edges :around
                    #'corfu-doc-terminal--get-cf-popup-edges)
        (advice-add #'corfu-doc--should-refresh-popup :around
                    #'corfu-doc-terminal--should-refresh-popup)
        (advice-add #'corfu-doc--clear-buffer :around
                    #'corfu-doc-terminal--clear-buffer)
        (advice-add #'corfu-doc--make-popup-invisible :around
                    #'corfu-doc-terminal--make-popup-invisible)
        (advice-add #'corfu-doc--popup-hide :around
                    #'corfu-doc-terminal--popup-hide)
        (advice-add #'corfu-doc--refresh-popup :around
                    #'corfu-doc-terminal--refresh-popup)
        (advice-add #'corfu-doc--update-popup :around
                    #'corfu-doc-terminal--update-popup)
        (advice-add #'corfu-doc--popup-scroll :around
                    #'corfu-doc-terminal--popup-scroll)
        (corfu-doc-terminal--increase-corfu-doc-advice-precedence)
        (add-hook
         'corfu-doc-mode-hook
         #'corfu-doc-terminal--increase-corfu-doc-advice-precedence))
    (advice-remove #'corfu-doc--popup-visible-p
                   #'corfu-doc-terminal--popup-visible-p)
    (advice-remove #'corfu-doc--cf-popup-visible-p
                   #'corfu-doc-terminal--cf-popup-visible-p)
    (advice-remove #'corfu-doc--get-cf-popup-edges
                   #'corfu-doc-terminal--get-cf-popup-edges)
    (advice-remove #'corfu-doc--should-refresh-popup
                   #'corfu-doc-terminal--should-refresh-popup)
    (advice-remove #'corfu-doc--clear-buffer
                   #'corfu-doc-terminal--clear-buffer)
    (advice-remove #'corfu-doc--make-popup-invisible
                   #'corfu-doc-terminal--make-popup-invisible)
    (advice-remove #'corfu-doc--popup-hide
                   #'corfu-doc-terminal--popup-hide)
    (advice-remove #'corfu-doc--refresh-popup
                   #'corfu-doc-terminal--refresh-popup)
    (advice-remove #'corfu-doc--update-popup
                   #'corfu-doc-terminal--update-popup)
    (advice-remove #'corfu-doc--popup-scroll
                   #'corfu-doc-terminal--popup-scroll)
    (remove-hook
     'corfu-doc-mode-hook
     #'corfu-doc-terminal--increase-corfu-doc-advice-precedence)
    ;; Decrease precedence of the advices that we increased.
    (when (advice-member-p #'corfu-doc--popup-show #'corfu--popup-show)
      (advice-remove #'corfu--popup-show #'corfu-doc--popup-show)
      (advice-add #'corfu--popup-show :after #'corfu-doc--popup-show))
    (when (advice-member-p #'corfu-doc--popup-hide #'corfu--popup-hide)
      (advice-remove #'corfu--popup-hide #'corfu-doc--popup-hide)
      (advice-add #'corfu--popup-hide :after #'corfu-doc--popup-hide))))

(provide 'corfu-doc-terminal)
;;; corfu-doc-terminal.el ends here
