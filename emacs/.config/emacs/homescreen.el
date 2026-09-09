;;; homescreen.el starts here

(require 'subr-x)

(defvar my-recent-branches nil)

(defvar my-recent-branches-file
  (expand-file-name "~/.recent-branches.el"))

(defvar my-recent-branches-limit 12)

(load my-recent-branches-file t)

(defvar my-recent-branches-excluded-repos
  (list (expand-file-name "~/.dotfiles")))

(defun my-dashboard-switch-to-branch (repo branch)
  (let ((default-directory (file-name-as-directory
                            (expand-file-name repo))))
    (unless (zerop (process-file "git" nil nil nil
                                 "switch" branch))
      (user-error "Could not switch %s to %s" repo branch))
    (dired default-directory)))

(defun my-dashboard-insert-branches (list-size)
  (dashboard-insert-section
   "Recent Branches:"
   my-recent-branches
   list-size
   'branches
   (dashboard-get-shortcut 'branches)
   `(lambda (&rest _)
      (my-dashboard-switch-to-branch
       ,(car el)
       ,(cdr el)))
   (format "%s - %s"
           (cdr el)
           (abbreviate-file-name (car el)))))

(use-package
  dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner
        (concat CONFIG_PATH "emacs_image.png"))
  (custom-set-faces
   '(dashboard-banner-logo-title
     ((t (:foreground "#2957b0" :weight bold)))))

  (setq dashboard-banner-logo-title
        (let ((title "
******** ****     ****     **       ******   ********
/**///// /**/**   **/**    ****     **////** **////// 
/**      /**//** ** /**   **//**   **    // /**       
/******* /** //***  /**  **  //** /**       /*********
/**////  /**  //*   /** **********/**       ////////**
/**      /**   /    /**/**//////**//**    **       /**
/********/**        /**/**     /** //******  ******** 
//////// //         // //      //   //////  ////////  
"))
          (propertize title 'face '(:foreground "red" :weight bold))))

  (add-to-list 'dashboard-item-generators
               '(branches . my-dashboard-insert-branches))
  (add-to-list 'dashboard-item-shortcuts
               '(branches . "m"))

  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 8) (branches . 10)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))


(defun my-current-git-repo ()
  (when-let ((root
              (locate-dominating-file default-directory ".git")))
    (directory-file-name (expand-file-name root))))

(defun my-current-git-branch ()
  (when-let ((repo (my-current-git-repo)))
    (let ((default-directory repo))
      (string-trim
       (with-output-to-string
         (with-current-buffer standard-output
           (process-file
            "git" nil t nil
            "branch" "--show-current")))))))

(defun my-save-recent-branches ()
  (let ((tmp (make-temp-file "recent-branches-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";; Automatically generated. Do not edit manually.\n\n")
            (prin1 `(setq my-recent-branches ',my-recent-branches)
                   (current-buffer))
            (insert "\n"))
          (rename-file tmp my-recent-branches-file t))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun my-prune-recent-branches ()
  (setq my-recent-branches
        (seq-filter
         (lambda (entry)
           (file-directory-p
            (expand-file-name (car entry))))
         my-recent-branches)))

(defun my-record-current-branch ()
  (my-prune-recent-branches)

  (when-let* ((repo (my-current-git-repo))
              (branch (my-current-git-branch))
              ((not (string-empty-p branch)))
              ((not (member repo my-recent-branches-excluded-repos))))
    (let ((entry (cons repo branch)))
      (setq my-recent-branches
            (delete entry my-recent-branches))

      (push entry my-recent-branches)

      (when (> (length my-recent-branches)
               my-recent-branches-limit)
        (setcdr
         (nthcdr (1- my-recent-branches-limit)
                 my-recent-branches)
         nil))

      (my-save-recent-branches))))

(add-hook 'find-file-hook #'my-record-current-branch)
(add-hook 'after-save-hook #'my-record-current-branch)

;;; homescreen.el ends here
