;;; bazel-config.el starts here

(use-package bazel
  :ensure t
  :hook (bazel-mode . bazel-format-on-save-mode))

;; Check if we use buildifier
;; (use-package bazel
;;   :ensure t
;;   :hook (bazel-mode . bazel-format-on-save-mode))


;;; bazel-config.el ends here
