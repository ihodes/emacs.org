(require 'org)
;; Bootstrap use-package
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load the configuration from org file
(org-babel-load-file "~/.emacs.d/emacs.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ocamlformat-enable 'enable-outside-detected-project)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(ace-jump-mode ace-window adaptive-wrap ag browse-kill-ring cider consult deft
                   embark embark-consult exec-path-from-shell git-timemachine
                   gptel haskell-mode magit marginalia markdown-mode
                   material-theme mmm-mode orderless paredit projectile
                   rainbow-delimiters scss-mode smart-mode-line solarized-theme
                   swiper typescript-mode undo-tree vertico visual-fill-column
                   visual-regexp web-mode yaml-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))
(put 'narrow-to-region 'disabled nil)
