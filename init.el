(load-theme 'modus-vivendi :no-confirm)

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

;; Disable custom by making it disposable
(setq custom-file (make-temp-file "emacs-custom-"))

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-auto-revert-mode 1)
(setq auto-revert-use-notify t)
(setq auto-revert-avoid-polling t)

(setq-default show-trailing-whitespace t)
(global-set-key (kbd "<f1>") 'delete-trailing-whitespace)
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'special-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

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

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Install use-package and use straight.el by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package diminish
  :config
  (with-eval-after-load 'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'autorevert (diminish 'auto-revert-mode))
  (with-eval-after-load 'simple (diminish 'visual-line-mode))
  (with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
  (with-eval-after-load 'paredit (diminish 'paredit-mode)))

(which-function-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(repeat-mode 1)

(use-package winner
  :straight nil
  :init (winner-mode 1)
  :bind (("C-c <left>"  . winner-undo)
         ("C-c <right>" . winner-redo))
  :config
  (defvar w/winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<left>") 'winner-undo)
      (define-key map (kbd "<right>") 'winner-redo)
      map)
    "Keymap to repeat `winner' key sequences.  Used in `repeat-mode'.")
  (put 'winner-undo 'repeat-map 'w/winner-repeat-map)
  (put 'winner-redo 'repeat-map 'w/winner-repeat-map))

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Packages

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x [" . magit-diff-buffer-file))
  :config
  (setq magit-repository-directories '(("~/workspace/" . 1))))

(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  :config
  (defun my/diff-hl-update-all ()
    "Update diff-hl in all buffers when Emacs gains focus."
    (when (frame-focus-state)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and diff-hl-mode buffer-file-name)
            (diff-hl-update))))))
  (add-function :after after-focus-change-function #'my/diff-hl-update-all))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind ("C-x C-u" . undo-tree-visualize))

(use-package vterm
  :straight t
  :bind ("<f2>" . vterm)
  :hook (vterm-mode . (lambda () (setq-local show-trailing-whitespace nil)))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/bin/zsh"))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode)))

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.vue\\'"))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package emmet-mode
  :hook ((html-mode  . emmet-mode)
         (css-mode   . emmet-mode)
         (web-mode   . emmet-mode)
         (jsx-mode   . emmet-mode)))

(use-package eglot
  :hook ((python-mode     . eglot-ensure)
         (js-mode         . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
                   nil nil #'equal)
        '("pyright-langserver" "--stdio")))

(use-package flymake
  :straight nil
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-counters)))

(use-package python
  :straight nil
  :custom
  (python-shell-interpreter "uv")
  (python-shell-interpreter-args "run python")
  :hook (inferior-python-mode . (lambda () (setq-local show-trailing-whitespace nil)))
  :bind (:map python-mode-map
         ("M-n" . python-nav-forward-defun)
         ("M-p" . python-nav-backward-defun)))


(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 100)
  :hook ((text-mode . visual-line-mode)
         (text-mode . visual-fill-column-mode)))

(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode scheme-mode) . paredit-mode))

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
         ("M-l" . avy-goto-line)))

(use-package gptel
  :config
  (setq gptel-model 'claude-sonnet-4-20250514)
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key #'gptel-api-key-from-auth-source))
  :bind (("C-c RET"       . gptel-send)
         ("C-c <C-return>" . gptel-menu)
         ("C-c ]"         . gptel-rewrite)
         ("C-c g"         . gptel)))

(global-set-key (kbd "<f5>") 'revert-buffer-quick)
