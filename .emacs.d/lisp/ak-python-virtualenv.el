;;; init.el --- emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'projectile)
(require 'python)

(defvar ak-venv-root-dir (expand-file-name "~/.virtualenvs"))

(declare-function python-shell-calculate-exec-path "python")

(defun ak-flycheck-set-python-executables ()
  "Set Python executables for the current buffer."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq-local flycheck-python-pylint-executable
                (executable-find "pylint"))
    (setq-local flycheck-python-flake8-executable
                (executable-find "flake8"))))

(defun ak-flycheck-python-setup ()
  (when (derived-mode-p 'python-mode)
    (add-hook 'hack-local-variables-hook
              #'ak-flycheck-set-python-executables  'local)))

(defun ak-python-venv-setup ()
  (let* ((name (projectile-project-name))
         (venv-dir (expand-file-name name ak-venv-root-dir)))
    (when (file-directory-p venv-dir)
      (setq-local python-shell-virtualenv-path venv-dir))))

(provide 'ak-python-virtualenv)
;;; ak-python-virtualenv.el ends here
