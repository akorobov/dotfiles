;; Author: akorobov@gmail.com

;; Java syntax checking using flymake with eclipse batch compiler. Uses source/class-path information extracted from eclipse classpath project file
(require 'xml)
(require 'flymake)

(defvar fmj/ecj-path
  (expand-file-name  "~/dev/tools/ecj-4.2.jar")
  "location of ecj batch compiler jar")

(defvar fmj/source-version "1.6")
(defvar fmj/opts "-warn:+over-ann,uselessTypeCheck -proceedOnError")

(defcustom fmj/classpath-file-name ".classpath"
  "name of eclipse classpath file")


(defun fmj/find-classpath-file ()
  (setq classpath (locate-dominating-file default-directory fmj/classpath-file-name))
  (if classpath
      (concat classpath fmj/classpath-file-name)
      nil))

(defun fmj/parse-classpath-file (cp-file)
  (if cp-file
      (progn  
        (setq classpath-entries 
              (xml-get-children (car (xml-parse-file cp-file)) 'classpathentry))
        (setq classpath ())
        (setq sourcepath ())
        (mapc (lambda (e)
                (let* ((attrs (xml-node-attributes e))
                       (path (cdr (assq 'path attrs)))
                       (output (cdr (assq 'output attrs)))
                       (kind (cdr (assq 'kind attrs))))
                  (cond ((string= "src" kind)
                         (add-to-list 'sourcepath path)
                         (add-to-list 'classpath output))
                        ((string= "lib" kind)
                         (add-to-list 'classpath path))))) classpath-entries)

        `((sourcepath . ,sourcepath) (classpath . ,classpath)))
    '((sourcepath . (".")) (claspath . (".")))))
 
(defun fmj/get-abs-path (p e) (expand-file-name (concat p "/" e)))
(defun fmj/add-to-path  (s e) (concat s ":" e))

(defun fmj/project-root () 
  (let ((cpfile (fmj/find-classpath-file)))
    (if cpfile
        (file-name-directory (fmj/find-classpath-file))
        nil)))

(defun fmj/get-abs-sourcepath (root sc)
  (let* ((sps (cdr (assoc 'sourcepath sc)))
         (s (concat root "/" (car sps)))
         (tail (cdr sps)))
    (mapc (lambda (e) (setq s (fmj/add-to-path s (fmj/get-abs-path root e)))) tail)
    s))

(defun fmj/get-abs-classpath (root sc)
  (let* ((sps (cdr (assoc 'classpath sc)))
         (s (concat root "/" (car sps)))
         (tail (cdr sps)))
    (mapc (lambda (e) (setq s (fmj/add-to-path s (fmj/get-abs-path root e)))) tail)
    s))

(defun fmj/init ()
  "Initialize flymake and generate ecj command for current buffer"
  (setq sc (fmj/parse-classpath-file (fmj/find-classpath-file)))
  (setq root (fmj/project-root))
  (setq class-path (fmj/get-abs-classpath root sc))
  (setq source-path (fmj/get-abs-sourcepath root sc))

  (setq cmd
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'fmj/create-temp-file))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    ; do not generate class but only check syntax, one drawback is 
    ; ecj work for single file and won't pick up changes in other buffers/files
    (list "java" (list "-jar" fmj/ecj-path 
                       "-source" fmj/source-version 
                       "-target" fmj/source-version 
                       "-d" "none"
                       fmj/opts
                       "-Xemacs" 
                       "-classpath" class-path
                       (expand-file-name local-file))))) 
  cmd)

(defun fmj/cleanup ()
  "Delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))
 
(defun fmj/create-temp-file (file-name prefix)
  "Create temporary directory file "
  (file-truename (expand-file-name 
                  (file-name-nondirectory file-name)
                  (expand-file-name  (int-to-string (abs (random))) (flymake-get-temp-dir)))))

(push '(".+\\.java$" fmj/init fmj/cleanup) flymake-allowed-file-name-masks)

(provide 'flymake-ecj)
