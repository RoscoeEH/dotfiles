;; Enable abbrev mode globally
(setq-default abbrev-mode t)

;; Define global abbreviations
(define-abbrev-table 'global-abbrev-table
  '())

(define-abbrev-table 'rust-mode-abbrev-table
  '(("pln" "println!")
    ("rbe" "->Result<(), Box<dyn Error>>")
    ("devmode" "cfg!(debug_assertions)")))


(setq save-abbrevs 'silently)
