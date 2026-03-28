;;; scad.el starts here

(use-package scad-mode
  :ensure t)

;; TODO flycheck to lint for closed parens and missing semi-colons

(defun format-scad ()
  (when (and (eq major-mode 'scad-mode)
             buffer-file-name)
    (shell-command (concat (expand-file-name "~/Tools/format-scad.py") " " 
                           (shell-quote-argument buffer-file-name)))
    (revert-buffer t t t)))

(add-hook 'after-save-hook #'format-scad)

;;; scad.el ends here
