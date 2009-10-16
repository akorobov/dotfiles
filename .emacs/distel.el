; erlang configuration(move to different file)
(if (eq system-type 'windows-nt)
    (progn 
      (setq exec-path (cons "c:/Applications/erlang/bin" exec-path))
      (setq erlang-root-dir "C:/Applications/erlang")  )
    nil
)


(setq derl-cookie "cookie-monster-was-here")
(require 'erlang-start)
(require 'distel)

(distel-setup)
(add-hook 'erlang-mode-hook
            '(lambda ()
               (unless erl-nodename-cache
                 (distel-load-shell))))
(defun distel-load-shell ()
    "Load/reload the erlang shell connection to a distel node"
    (interactive)
    (setq erl-nodename-cache 'emacs@akorobov-xp)
    (setq distel-modeline-node "emacs")
    (force-mode-line-update)
    (let ((file-buffer (current-buffer))
          (file-window (selected-window)))
      (setq inferior-erlang-machine-options '("-sname" "emacs@akorobov-xp"))
      (switch-to-buffer-other-window file-buffer)
      (inferior-erlang)
      (select-window file-window)
      (switch-to-buffer file-buffer)))
(add-hook 'erlang-mode-hook
	  (lambda ()
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    (imenu-add-to-menubar "imenu")))
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)	
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-*"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")
(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
