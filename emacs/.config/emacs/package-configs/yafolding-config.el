;;; yafolding-config.el starts here

(use-package yafolding
  :ensure t
  :hook (prog-mode . yafolding-mode)
  :config

  (evil-define-key 'normal yafolding-mode-map
    (kbd "z a") 'yafolding-toggle-element
    (kbd "z A") 'yafolding-toggle-all      ;; Toggles all folds in the buffer
    (kbd "z o") 'yafolding-show-all
    (kbd "z c") 'yafolding-hide-all))

;;; yafolding-config.el ends here
