;; Enable abbrev mode globally
(setq-default abbrev-mode t)

;; Define global abbreviations
(define-abbrev-table 'global-abbrev-table
  '())

(define-abbrev-table 'rust-mode-abbrev-table
  '(("pln" "println!")
    ("rbe" "-> Result<(), Box<dyn Error>>")
    ("devmode" "cfg!(debug_assertions)")))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("dk" "define-key")
    ("enm" "evil-normal-state-map")
    ("eim" "evil-insert-state-map")
    ("evm" "evil-visual-state-map")))

(setq save-abbrevs 'silently)
