;;; markdown-config.el starts here

;; md-mode is installed with package-vc-install.
(use-package md-mode
  :ensure nil
  :mode ("\\.md\\'" . md-mode)
  :custom
  ;; Keep source tables unchanged when opening files.
  (md-mode-auto-align-tables nil)

  ;; Wrap text and table cells in the rendered view.
  (md-render-wrap-lines t)
  (md-render-table-wrap-columns t)

  ;; Use md-mode's own faces.
  (md-mode-use-markdown-mode-faces nil)

  ;; Table of contents.
  (md-mode-toc-side 'left)
  (md-mode-toc-width 30)

  :config
  ;; Preserve your original heading colors.
  (set-face-foreground 'md-render-header-1 "DeepSkyBlue1")
  (set-face-foreground 'md-render-header-2 "DarkOrange1")
  (set-face-foreground 'md-render-header-3 "MediumOrchid1")
  (set-face-foreground 'md-render-header-4 "YellowGreen")
  (set-face-foreground 'md-render-header-5 "SpringGreen")
  (set-face-foreground 'md-render-header-6 "SlateBlue1")

  ;; Keep header colors but use normal text size.
  (dolist (level '(1 2 3 4 5 6))
    (set-face-attribute
     (intern (format "md-render-header-%d" level))
     nil
     :height 1.0))

  ;; Toggle source / rendered view.
  (define-key md-mode-map (kbd "C-c p")
              #'md-mode-toggle-markup))


;;; List handling

(defun markdown-list-dwim ()
  "Continue Markdown lists or remove an empty list delimiter."
  (interactive)
  (let* ((current-line (thing-at-point 'line t))
         (indent (if (string-match "^[ \t]*" current-line)
                     (match-string 0 current-line)
                   ""))
         new-prefix)
    (if (or (string-match "^[ \t]*[-*]\\s-*$" current-line)
            (string-match "^[ \t]*[0-9]+[)][ \t]*$" current-line)
            (string-match "^[ \t]*[0-9]+[.][ \t]*$" current-line)
            (string-match "^[ \t]*-\\s-*\\[[xX ]\\][ \t]*$" current-line))
        (delete-region (line-beginning-position) (line-end-position))
      (cond
       ;; Checklist
       ((string-match "^[ \t]*-\\s-*\\[\\([xX ]\\)\\]\\s-+" current-line)
        (setq new-prefix (concat indent "- [ ] ")))

       ;; Numbered list: 1)
       ((string-match "^[ \t]*\\([0-9]+\\)[)][ \t]+" current-line)
        (let ((num (string-to-number (match-string 1 current-line))))
          (setq new-prefix
                (concat indent (number-to-string (1+ num)) ") "))))

       ;; Numbered list: 1.
       ((string-match "^[ \t]*\\([0-9]+\\)\\.[ \t]+" current-line)
        (let ((num (string-to-number (match-string 1 current-line))))
          (setq new-prefix
                (concat indent (number-to-string (1+ num)) ". "))))

       ;; Unordered list
       ((string-match "^[ \t]*\\([-*]\\)\\s-+" current-line)
        (setq new-prefix
              (concat indent (match-string 1 current-line) " "))))

      (newline)
      (when new-prefix
        (insert new-prefix)))))


(defun markdown-outdent ()
  "Outdent the current Markdown list item by two spaces."
  (interactive)
  (when (derived-mode-p 'md-mode)
    (let* ((current-indent (current-indentation))
           (cursor-offset (- (current-column) current-indent))
           (outdent-step 2)
           (new-indent (max 0 (- current-indent outdent-step))))
      (indent-line-to new-indent)
      (move-to-column (+ new-indent cursor-offset)))))


;;; Checklist handling

(defun toggle-markdown-check ()
  "Toggle a Markdown checklist item, preserving cursor position."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\s-*\\(- \\[\\)\\([ xX]?\\)\\(\\]\\)")
      (replace-match
       (if (string-match-p "[xX]" (match-string 2)) " " "x")
       t t nil 2))))


;;; Evil keybindings

(with-eval-after-load 'evil
  (with-eval-after-load 'md-mode
    ;; Only affect md-mode buffers.
    (evil-define-key 'insert md-mode-map
      (kbd "RET") #'markdown-list-dwim
      (kbd "<backtab>") #'markdown-outdent)

    (evil-define-key 'normal md-mode-map
      (kbd "z m") #'toggle-markdown-check)))


;;; Pandoc integration

(use-package pandoc-mode
  :ensure t
  :hook (md-mode . pandoc-mode))


;;; Export

(defun export-markdown (format &optional extra-args)
  "Export current Markdown buffer using Pandoc to FORMAT.
Optionally pass EXTRA-ARGS."
  (interactive "sExport format (e.g., html, pdf): ")
  (unless buffer-file-name
    (user-error "Buffer must be visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((output (concat
                  (file-name-sans-extension buffer-file-name)
                  "." format))
         (args (append
                (list "-f" "markdown"
                      "-t" format
                      "-o" output)
                extra-args))
         (status (apply #'call-process
                        "pandoc" nil "*Pandoc Output*" t
                        buffer-file-name args)))
    (if (and (integerp status) (zerop status))
        (message "Exported to %s" output)
      (error "Pandoc export failed; see *Pandoc Output*"))))


(defun export-markdown-to-html ()
  "Export current Markdown buffer to HTML using Pandoc."
  (interactive)
  (export-markdown "html"))


(defun export-markdown-to-pdf ()
  "Export current Markdown buffer to PDF using Pandoc."
  (interactive)
  (export-markdown "pdf" '("--pdf-engine=xelatex")))


(global-set-key (kbd "M-c e h") #'export-markdown-to-html)
(global-set-key (kbd "M-c e p") #'export-markdown-to-pdf)


;;; markdown-config.el ends here
