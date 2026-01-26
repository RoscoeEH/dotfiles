;;; scad.el starts here

(use-package scad-mode
  :ensure t)

(with-eval-after-load 'flycheck
  (flycheck-define-checker openscad
    "A syntax checker for OpenSCAD files."
    :command ("openscad" "--export-format" "stl" source "-o" "/dev/null")
    :error-patterns
    ((error line-start
            "ERROR:" (message) " at line " line
            line-end))
    :modes scad-mode)

  (add-to-list 'flycheck-checkers 'openscad))

;;; scad.el ends here
