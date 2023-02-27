;;; package: --- misc utils
;;; Commentary:
;;;
;;; Code:

(defun select-all ()
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max)))

; Insert the date
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

(defun read-lines (file)
  "returns a list of lines read from a file with given name."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

;; protect Messages and scratch buffers
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (not (member (buffer-name) '("*scratch*" "*Messages*")))
                t
              (bury-buffer)
              nil)))

(defun shell-filter-region (command)
  (interactive (list (read-shell-command "Filter command to apply: ")))
  (shell-command-on-region (region-beginning) (region-end) command (current-buffer) t))


;; stackoverflow http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region between BEGIN to END positions.
The function inserts linebreaks to separate tags that have
nothing but wh2itespace between them.   It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))


(provide 'ak-utils)
;;; ak-utils.el ends here
