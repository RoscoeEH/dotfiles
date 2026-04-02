;;; python-config.el starts here


;; Ensure Python mode specifically only uses dabbrev
(with-eval-after-load 'python
  (setq-local company-backends '(company-dabbrev)))

(add-hook 'python-mode-hook 'eglot-ensure)

;; Maybe move this?
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'python-mode apheleia-mode-alist) nil)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) nil))

;; Commented out to not cause massive changes to repos
;; (with-eval-after-load 'apheleia
;;   (setf (alist-get 'ruff-format apheleia-formatters)
;;         '("ruff" "format" "-"))

;;   (setf (alist-get 'python-mode apheleia-mode-alist)
;;         '(ruff-format)))

;;; python-config.el ends here
