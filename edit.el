;;; edit.el --- editing utilities (lean & fast) -*- lexical-binding: t; -*-

;; Smart parentheses management
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  ;; Evil integration
  (with-eval-after-load 'evil
    ;; Use smartparens for text objects
    (setq sp-navigate-consider-symbols nil)
    ;; Better Evil integration
    (setq sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil))
  ;; Don't insert space before delimiters in some modes
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  ;; Python-specific
  (sp-local-pair 'python-mode "'" nil :unless '(sp-point-after-word-p))
  ;; LaTeX-specific
  (sp-local-pair 'LaTeX-mode "$" "$")
  (sp-local-pair 'LaTeX-mode "\\[" "\\]")
  :diminish smartparens-mode)

;; iedit: multiple identical region editing (loaded on demand)
(use-package iedit
  :ensure t
  :commands (iedit-mode iedit-mode-toggle-on-function))

;; evil-mc: multiple cursors for Evil (manual cursor placement)
(use-package evil-mc
  :ensure t
  :after (evil general)
  :config
  (global-evil-mc-mode 1)

  ;; Evil style: gm prefix (multiple cursors - consistent with other packages)
  (general-define-key
   :states 'normal
   "gmm" 'evil-mc-make-all-cursors
   "gmu" 'evil-mc-undo-all-cursors
   "gmn" 'evil-mc-make-and-goto-next-match
   "gmp" 'evil-mc-make-and-goto-prev-match
   "gmN" 'evil-mc-skip-and-goto-next-match
   "gmP" 'evil-mc-skip-and-goto-prev-match
   "gmq" 'evil-mc-pause-cursors
   "gmr" 'evil-mc-resume-cursors)

  ;; Line-wise cursor addition (normal & visual)
  (general-define-key
   :states '(normal visual)
   "C-M-j" 'evil-mc-make-cursor-move-next-line
   "C-M-k" 'evil-mc-make-cursor-move-prev-line)

  ;; Emacs style: C-c m prefix (global)
  (general-define-key
   "C-c m j" 'evil-mc-make-cursor-move-next-line
   "C-c m k" 'evil-mc-make-cursor-move-prev-line
   "C-c m n" 'evil-mc-make-and-goto-next-match
   "C-c m p" 'evil-mc-make-and-goto-prev-match
   "C-c m m" 'evil-mc-make-all-cursors
   "C-c m u" 'evil-mc-undo-all-cursors
   "C-c m q" 'evil-mc-pause-cursors
   "C-c m r" 'evil-mc-resume-cursors))

(use-package ialign
  :ensure t)

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode wgrep-finish-edit)
  :config
  (setq wgrep-change-readonly-file t
        wgrep-auto-save-buffer t))

;; EditorConfig support for consistent coding styles across editors
(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode)
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :diminish editorconfig-mode)

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :bind (("M-=" . er/expand-region)
         ("M-+" . er/expand-region)
         ("M--" . er/contract-region)))

;; Snippet expansion system
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (LaTeX-mode . yas-minor-mode))
  :init
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-reload-all)
  ;; Use TAB only when at word end/beginning for better completion integration
  (setq yas-triggers-in-field t
        yas-wrap-around-region t
        yas-verbosity 1)
  ;; Keybindings
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        "i s" '(yas-insert-snippet :which-key "insert snippet")
        "i n" '(yas-new-snippet :which-key "new snippet")
        "i v" '(yas-visit-snippet-file :which-key "visit snippet")))))

;; Collection of snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; edit.el ends here
