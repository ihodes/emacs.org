(add-to-list 'default-frame-alist '(undecorated . t))
(setq frame-resize-pixelwise t)

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

;; Disable custom by making it disposable
(setq custom-file (make-temp-file "emacs-custom-"))

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "<C-tab>") 'ignore)
(global-set-key (kbd "<M-tab>") 'ignore)
(global-set-key (kbd "s-s") 'ignore)
(global-set-key (kbd "s-t") 'ignore)

(global-auto-revert-mode 1)
(setq auto-revert-use-notify nil)
(setq auto-revert-interval 1)


(which-function-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(repeat-mode 1)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(setq-default show-trailing-whitespace t)
(global-set-key (kbd "<f1>") 'delete-trailing-whitespace)
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'special-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

(global-set-key (kbd "<f5>") 'revert-buffer-quick)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package catppuccin-theme
  :straight t)


(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))


(use-package vterm
  :straight t
  :hook (vterm-mode . (lambda () (setq-local show-trailing-whitespace nil)))
  :custom
  (vterm-max-scrollback 100000)
  (vterm-shell "/bin/zsh")
  :config
  (defun ihds/vterm-paste-image ()
    "Save clipboard image to a temp file and insert the path into vterm."
    (interactive)
    (let* ((filename (format-time-string "/tmp/emacs-clipboard-%Y%m%dT%H%M%S.png"))
           (result (string-trim
                    (shell-command-to-string
                     (format "osascript -l JavaScript -e '
ObjC.import(\"AppKit\");
var pb = $.NSPasteboard.generalPasteboard;
var data = pb.dataForType($.NSPasteboardTypePNG);
if (data.isNil()) {
  var tiff = pb.dataForType($.NSPasteboardTypeTIFF);
  if (!tiff.isNil()) {
    var img = $.NSBitmapImageRep.imageRepWithData(tiff);
    data = img.representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $());
  }
}
if (!data.isNil()) {
  data.writeToFileAtomically(\"%s\", true);
  \"ok\";
} else { \"no image\"; }
'" filename)))))
      (if (string-prefix-p "ok" result)
          (vterm-insert filename)
        (message "No image found on clipboard"))))

  (defun ihds/vterm-smart-paste ()
    "Paste image path if clipboard has an image, otherwise normal vterm yank."
    (interactive)
    (let ((has-image (string-prefix-p "true"
                      (string-trim
                       (shell-command-to-string
                        "osascript -l JavaScript -e '
ObjC.import(\"AppKit\");
var pb = $.NSPasteboard.generalPasteboard;
!pb.dataForType($.NSPasteboardTypePNG).isNil() || !pb.dataForType($.NSPasteboardTypeTIFF).isNil();
'")))))
      (if has-image
          (ihds/vterm-paste-image)
        (vterm-yank))))

  :bind (:map vterm-mode-map
              ("s-v" . ihds/vterm-smart-paste)
              ("C-M-s-v" . ihds/vterm-paste-image)))

(use-package multi-vterm
  :straight t
  :bind ("<f2>" . multi-vterm))


(use-package project
  :defer t
  :config
  (project-remember-projects-under "~/workspace/" nil))


(use-package diminish
  :config
  (with-eval-after-load 'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'autorevert (diminish 'auto-revert-mode))
  (with-eval-after-load 'simple (diminish 'visual-line-mode))
  (with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
  (with-eval-after-load 'paredit (diminish 'paredit-mode)))


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


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x |" . magit-diff-buffer-file))
  :config
  (setq magit-repository-directories '(("~/workspace/" . 1)))
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-worktrees nil t)
  (add-to-list 'magit-section-initial-visibility-alist '(recent . show))
  (add-to-list 'magit-section-initial-visibility-alist '(untracked . show))

  (defvar ihds/magit--refresh-timer nil)

  (defun ihds/magit-refresh-all-debounced ()
    "Refresh all magit buffers, debounced."
    (when ihds/magit--refresh-timer
      (cancel-timer ihds/magit--refresh-timer))
    (setq ihds/magit--refresh-timer
          (run-with-timer 0.5 nil
            (lambda ()
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (derived-mode-p 'magit-mode)
                    (magit-refresh-buffer))))))))

  (add-hook 'after-save-hook #'ihds/magit-refresh-all-debounced)
  (add-hook 'after-revert-hook #'ihds/magit-refresh-all-debounced))


(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  :bind
  (("s-[" . diff-hl-previous-hunk)
   ("s-]" . diff-hl-next-hunk)
   ("s-{" . diff-hl-show-hunk-previous)
   ("s-}" . diff-hl-show-hunk-next)
   ("s-\\" . ihds/diff-hl-toggle-show-hunk))
  :config
  (require 'diff-hl-show-hunk-inline)
  (defun ihds/diff-hl-toggle-show-hunk ()
    "Toggle the diff-hl inline hunk popup."
    (interactive)
    (if diff-hl-show-hunk-inline--current-popup
	(diff-hl-show-hunk-inline-hide)
      (diff-hl-show-hunk)))
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
  (setq undo-tree-auto-save-history nil)
  (define-key undo-tree-map (kbd "C-x u") #'undo)
  :bind
  ("C-x C-u" . undo-tree-visualize))

(use-package adaptive-wrap
  :straight t
  :hook (markdown-mode . adaptive-wrap-prefix-mode))

(use-package edit-indirect
  :straight t)

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode))
  :config
  (defun ihds/markdown-insert-code-block ()
    "Insert a fenced code block and place cursor after opening fence."
    (interactive)
    (insert "```\n\n```")
    (forward-line -1)
    (end-of-line))
  :bind (:map markdown-mode-map
              ("s-b" . markdown-insert-bold)
              ("s-i" . markdown-insert-italic)
              ("s-k" . markdown-insert-link)
              ("s-`" . ihds/markdown-insert-code-block)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico completion stack ;;
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
         ("M-s f"   . consult-find)
         ("C-x p b" . consult-project-buffer))
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

;; end vertico stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package tabspaces
  :straight t
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tabspaces-session-project-session-store nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-name-function #'tabspaces--name-tab-by-project-or-default)
  :config
  ;; Window management
  (customize-set-variable 'display-buffer-base-action
    '((display-buffer-reuse-window display-buffer-same-window)
      (reusable-frames . t)))
  (customize-set-variable 'even-window-sizes nil)

  ;; Consult integration: scope consult-buffer to current workspace
  (with-eval-after-load 'consult
    (plist-put consult-source-buffer :hidden t)
    (plist-put consult-source-buffer :default nil)
    (defvar consult--source-workspace
      (list :name "Workspace Buffers"
            :narrow ?w
            :history 'buffer-name-history
            :category 'buffer
            :state #'consult--buffer-state
            :default t
            :items (lambda () (consult--buffer-query
                               :predicate #'tabspaces--local-buffer-p
                               :sort 'visibility
                               :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))

  ;; Track previous workspace for quick switching
  (defvar ihds/tabspaces--previous-tab nil)
  (defun ihds/tabspaces--track-previous (&rest _)
    (setq ihds/tabspaces--previous-tab (tabspaces--current-tab-name)))
  (advice-add 'tab-bar-switch-to-tab :before #'ihds/tabspaces--track-previous)

  (defun ihds/tabspaces-switch-with-default ()
    "Like `tabspaces-switch-or-create-workspace' but defaults to previous workspace."
    (interactive)
    (let* ((tabs (tabspaces--list-tabspaces))
           (default ihds/tabspaces--previous-tab)
           (prompt (if default
                      (format "Switch to workspace (default %s): " default)
                    "Switch to workspace: "))
           (choice (completing-read prompt tabs nil t nil nil default)))
      (tab-bar-switch-to-tab choice)))

  ;; Transient menu
  (require 'transient)
  (defun ihds/tabspaces-overview ()
    "Return a string showing current workspace and a table of all workspaces."
    (let* ((current (tabspaces--current-tab-name))
           (tabs (tabspaces--list-tabspaces))
           (header (format "Workspace: %s\n\n" (propertize current 'face 'transient-heading)))
           (rows (mapconcat
                  (lambda (name)
                    (let* ((idx (tab-bar--tab-index-by-name name))
                           (bufs (length (tabspaces--buffer-list nil idx)))
                           (marker (if (string= name current) "*" " ")))
                      (format " %s %-20s %3d buffers" marker name bufs)))
                  tabs "\n")))
      (concat header rows)))

  (defun ihds/tabspaces-move-buffer-to-tab (buf-name tab-name)
    "Move buffer BUF-NAME to workspace TAB-NAME, removing it from the current one."
    (interactive
     (list (buffer-name (current-buffer))
           (completing-read "Move to workspace: " (tabspaces--list-tabspaces) nil t)))
    (let ((buf (get-buffer buf-name))
          (current-tab (tabspaces--current-tab-name)))
      (when (and buf (not (string= tab-name current-tab)))
        ;; Switch to target tab, add buffer, switch back, remove from current
        (tab-bar-switch-to-tab tab-name)
        (switch-to-buffer buf)
        (tab-bar-switch-to-tab current-tab)
        (tabspaces-remove-current-buffer buf))))

  (transient-define-prefix ihds/tabspaces-transient ()
    "Workspace management."
    [:description ihds/tabspaces-overview
     ["Switch"
      ("s" "switch/create" tabspaces-switch-or-create-workspace)
      ("o" "open project" tabspaces-open-or-create-project-and-workspace)
      ("t" "buffer & tab" tabspaces-switch-buffer-and-tab)]
     ["Buffers"
      ("b" "switch buffer" tabspaces-switch-to-buffer)
      ("m" "move to workspace" ihds/tabspaces-move-buffer-to-tab)
      ("r" "remove current" tabspaces-remove-current-buffer)
      ("R" "remove selected" tabspaces-remove-selected-buffer)
      ("C" "clear all" tabspaces-clear-buffers)]
     ["Manage"
      ("d" "close workspace" tabspaces-close-workspace)
      ("k" "kill & close" tabspaces-kill-buffers-close-workspace)]
     ["Session"
      ("C-s" "save session" tabspaces-save-session)
      ("C-l" "restore session" tabspaces-restore-session)
      ("C-p" "save project session" tabspaces-save-current-project-session)]]
    (interactive)
    (transient-setup 'ihds/tabspaces-transient))

  (defun ihds/tabspaces-close-workspace-confirm ()
    "Close current workspace after confirmation."
    (interactive)
    (if (<= (length (tabspaces--list-tabspaces)) 1)
        (message "Can't close the last workspace")
      (when (y-or-n-p (format "Close workspace '%s'? " (tabspaces--current-tab-name)))
        (tabspaces-close-workspace))))

  ;; s-1 … s-9 jump to workspace by position
  (dotimes (i 9)
    (let ((n (1+ i)))
      (global-set-key (kbd (format "s-%d" n))
                      `(lambda () (interactive) (tab-bar-select-tab ,n)))))

  :bind
  ("s-w" . ihds/tabspaces-close-workspace-confirm)
  ("C-M-s-p" . ihds/tabspaces-transient)
  ("C-M-s-s" . ihds/tabspaces-switch-with-default)
  ("C-x p p" . tabspaces-open-or-create-project-and-workspace))

(with-eval-after-load 'tab-bar
  (set-face-attribute 'tab-bar-tab nil
                      :weight 'bold
                      :underline t
                      :background (face-background 'highlight))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :weight 'normal
                      :underline nil))

(use-package powerline
  :config
  (powerline-default-theme))

(defun ihds/apply-catppuccin-powerline ()
  "Set powerline faces for the current catppuccin flavor."
  (pcase catppuccin-flavor
    ('latte
     (set-face-attribute 'mode-line nil :background "#dc8a78" :foreground "#4c4f69")
     (set-face-attribute 'powerline-active1 nil :background "#bcc0cc" :foreground "#4c4f69")
     (set-face-attribute 'powerline-active2 nil :background "#ccd0da" :foreground "#4c4f69")
     (set-face-attribute 'which-func nil :foreground "#6c6f85" :weight 'bold))
    ('frappe
     (set-face-attribute 'mode-line nil :background "#f2d5cf" :foreground "#232634")
     (set-face-attribute 'powerline-active1 nil :background "#626880" :foreground "#c6d0f5")
     (set-face-attribute 'powerline-active2 nil :background "#414559" :foreground "#c6d0f5")
     (set-face-attribute 'which-func nil :foreground "#a5adce" :weight 'bold))
    ('macchiato
     (set-face-attribute 'mode-line nil :background "#f4dbd6" :foreground "#181926")
     (set-face-attribute 'powerline-active1 nil :background "#5b6078" :foreground "#cad3f5")
     (set-face-attribute 'powerline-active2 nil :background "#363a4f" :foreground "#cad3f5")
     (set-face-attribute 'which-func nil :foreground "#a5adcb" :weight 'bold))
    ('mocha
     (set-face-attribute 'mode-line nil :background "#f5e0dc" :foreground "#11111b")
     (set-face-attribute 'powerline-active1 nil :background "#585b70" :foreground "#cdd6f4")
     (set-face-attribute 'powerline-active2 nil :background "#313244" :foreground "#cdd6f4")
     (set-face-attribute 'which-func nil :foreground "#a6adc8" :weight 'bold)))
  (powerline-reset))

(use-package auto-dark
  :straight t
  :custom
  (auto-dark-dark-theme nil)
  (auto-dark-light-theme nil)
  :config
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'mocha)
              (load-theme 'catppuccin :no-confirm)
              (ihds/apply-catppuccin-powerline)))
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'frappe)
              (load-theme 'catppuccin :no-confirm)
              (ihds/apply-catppuccin-powerline)))
  (auto-dark-mode t))

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


(use-package windmove
  :config
  (windmove-default-keybindings 'super))


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

