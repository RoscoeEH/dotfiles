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
  (remove-hook 'magit-process-mode-hook #'direnv-update-environment))

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

;; magit-config.el ends here
