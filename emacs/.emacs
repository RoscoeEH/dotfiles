(load "~/.config/emacs/init.el")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032"
     "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     default))
 '(package-selected-packages
   '(## ace-window adaptive-wrap aggressive-indent bm company evil
        evil-collection flycheck free-keys goto-last-change key-chord
        lsp-mode magit minimap modus-themes rainbow-delimiters
        rust-mode vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-banner-logo-title ((t (:foreground "#2957b0" :weight bold))))
 '(magit-hash ((t (:foreground "green")))))
(put 'list-timers 'disabled nil)
