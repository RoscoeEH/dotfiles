;;; rust-config.el starts here

(use-package flycheck-rust
  :ensure t)

(add-hook 'rust-mode-hook #'flycheck-mode)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . rust-enable-format-on-save))

(use-package toml-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook 'eglot-ensure)


;;; rust-config.el ends here
