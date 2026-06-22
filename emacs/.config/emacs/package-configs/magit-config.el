;; magit-config.el starts here

;; Ensure Magit is installed
(use-package magit
  :ensure t
  :bind (("C-x g s" . magit-status)
         ("C-x g c" . magit-clone)
         ("C-x g i" . magit-init))
  :init
  (setq magit-git-executable "/usr/bin/git")
  (setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))
  (setenv "GIT_ASKPASS" "")
  (setq magit-log-arguments '("--graph" "--oneline" "--decorate" "--color"))
  :config
  ;; Keybindings inside magit buffers
  (define-key magit-mode-map (kbd "M-p") 'magit-section-backward-sibling)
  (define-key magit-mode-map (kbd "M-n") 'magit-section-forward-sibling)
  (define-key magit-mode-map (kbd "^")   'magit-section-up)

  ;; Prevent Magit from inheriting direnv environment
  (remove-hook 'magit-status-mode-hook #'direnv-update-environment)
  (remove-hook 'magit-process-mode-hook #'direnv-update-environment)

  (evil-define-key 'normal magit-mode-map (kbd "C-f") 'isearch-forward)
  (evil-define-key 'visual magit-mode-map (kbd "C-f") 'isearch-forward))

;; Faces (kept outside, same behavior as before)
(custom-set-faces
 '(magit-hash ((t (:foreground "green")))))


(use-package forge
  :ensure t
  :after magit
  :bind (("C-x g f" . forge-pull))
  :config
  (defun my/magit-review-origin-master ()
    "Review current branch against origin/master."
    (interactive)
    (magit-diff-range "origin/master...HEAD"))

  (global-set-key (kbd "C-x g r") 'my/magit-review-origin-master))

(with-eval-after-load 'magit
  (require 'xref)

  (defvar my/magit-xref-forward-stack nil
    "Forward stack for Magit review navigation.")

  (defun my/magit-xref--xref-back ()
    (if (fboundp 'xref-go-back)
        (xref-go-back)
      (xref-pop-marker-stack)))

  (define-minor-mode my/magit-xref-target-mode
    "Temporary Xref-style navigation for files visited from Magit."
    :lighter " Magit-Xref"
    :keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-,") #'my/magit-xref-go-back)
      (define-key map (kbd "M-.") #'my/magit-xref-go-forward)
      map))

  (defun my/magit-xref-visit-file (&optional other-window)
    "Visit file from Magit and push the Magit location onto the Xref stack."
    (interactive "P")
    (setq my/magit-xref-forward-stack nil)
    (xref-push-marker-stack)
    (magit-diff-visit-file other-window)
    (my/magit-xref-target-mode 1))

  (defun my/magit-xref-visit-worktree-file (&optional other-window)
    "Visit worktree file from Magit and push the Magit location onto the Xref stack."
    (interactive "P")
    (setq my/magit-xref-forward-stack nil)
    (xref-push-marker-stack)
    (magit-diff-visit-worktree-file other-window)
    (my/magit-xref-target-mode 1))

  (defun my/magit-xref-go-back ()
    "Go back to the Magit buffer, saving current file position for forward nav."
    (interactive)
    (push (point-marker) my/magit-xref-forward-stack)
    (my/magit-xref--xref-back))

  (defun my/magit-xref-go-forward ()
    "Go forward to the file location previously left by `my/magit-xref-go-back'."
    (interactive)
    (unless my/magit-xref-forward-stack
      (user-error "No Magit/Xref forward location"))
    (let ((marker (pop my/magit-xref-forward-stack)))
      (xref-push-marker-stack)
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (my/magit-xref-target-mode 1)))

  ;; Plain Emacs bindings in all Magit buffers.
  (define-key magit-mode-map (kbd "M-.") #'my/magit-xref-visit-file)
  (define-key magit-mode-map (kbd "RET") #'my/magit-xref-visit-file)
  (define-key magit-mode-map (kbd "C-<return>")
              #'my/magit-xref-visit-worktree-file)

  ;; Evil can override `magit-mode-map', so bind explicitly for Evil too.
  (with-eval-after-load 'evil
    (evil-define-key 'normal magit-mode-map
      (kbd "M-.") #'my/magit-xref-visit-file
      (kbd "RET") #'my/magit-xref-visit-file
      (kbd "C-<return>") #'my/magit-xref-visit-worktree-file)))

;; magit-config.el ends here
