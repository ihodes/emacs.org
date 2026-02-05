(load-theme 'modus-vivendi :no-confirm)

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

;; Disable custom by making it disposable
(setq custom-file (make-temp-file "emacs-custom-"))

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default show-trailing-whitespace t)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package and use straight.el by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Packages

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-repository-directories '(("~/workspace/" . 1))))

(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind ("C-x C-u" . undo-tree-visualize))

(use-package markdown-mode)

;; Vertico completion stack
(use-package savehist
  :straight nil
  :init (savehist-mode))

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  :bind (:map vertico-map
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g o"   . consult-outline)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-find))
  :custom
  (consult-narrow-key "<")
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :init (global-corfu-mode))

(use-package avy
  :straight t
  :bind (("M-j"   . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)))

(global-set-key (kbd "<f5>") 'revert-buffer-quick)
