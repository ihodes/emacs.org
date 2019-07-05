(package-initialize)
(require 'package)
(require 'org)
(org-babel-load-file "~/.emacs.d/emacs.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ocamlformat-enable (quote enable-outside-detected-project))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (typescript-mode adaptive-wrap visual-fill-column mmm-mode web-mode visual-regexp use-package undo-tree tuareg smex smart-mode-line rainbow-delimiters projectile paredit merlin magit hydra haskell-mode git-timemachine fullframe flycheck flx exec-path-from-shell ensime counsel clojure-mode browse-kill-ring ag ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
