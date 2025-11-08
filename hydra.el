;;; hydra.el --- Hydra transient keymaps -*- lexical-binding: t; -*-

;; Hydra: sticky keybindings for repeated commands
(use-package hydra
  :ensure t
  :defer 1)

;; Pretty Hydra: more elegant hydra definition syntax
(use-package pretty-hydra
  :ensure t
  :after hydra)

;; Major Mode Hydra: auto-generate hydras for major modes
(use-package major-mode-hydra
  :ensure t
  :after (hydra pretty-hydra)
  :config
  (with-eval-after-load 'general
    (sleepy/leader-def
      "," '(major-mode-hydra :which-key "major mode"))))

;;; ========================================================================
;;; Window Management Hydra (가장 유용!)
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-window (:color pink :hint nil)
    "
╔════════════════════════════════════════════════════════════════╗
║                    Window Management                           ║
╠════════════════════════════════════════════════════════════════╣
║  Move      │ _h_ ←  _j_ ↓  _k_ ↑  _l_ →     _o_ other          ║
║  Resize    │ _H_ ←  _J_ ↓  _K_ ↑  _L_ →     _=_ balance        ║
║  Split     │ _v_ vert   _s_ horiz   _m_ maximize               ║
║  Delete    │ _d_ delete   _D_ delete-other                     ║
║  Rotate    │ _r_ rotate                                        ║
║  Quit      │ _q_ quit   _RET_ quit                             ║
╚════════════════════════════════════════════════════════════════╝
"
    ;; Movement
    ("h" evil-window-left)
    ("j" evil-window-down)
    ("k" evil-window-up)
    ("l" evil-window-right)
    ("o" other-window)

    ;; Resize (더 크게 조절: 5 단위)
    ("H" (evil-window-decrease-width 5))
    ("J" (evil-window-increase-height 5))
    ("K" (evil-window-decrease-height 5))
    ("L" (evil-window-increase-width 5))
    ("=" balance-windows)

    ;; Split & Delete
    ("v" split-window-right)
    ("s" split-window-below)
    ("d" delete-window)
    ("D" delete-other-windows)
    ("m" delete-other-windows :exit t)

    ;; Rotate
    ("r" evil-window-rotate-downwards)

    ;; Exit
    ("q" nil :exit t)
    ("RET" nil :exit t))

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "w ." '(hydra-window/body :which-key "window hydra"))))

;;; ========================================================================
;;; Text Scale Hydra
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-text-scale (:color pink :hint nil)
    "
╔═══════════════════════════════════╗
║         Text Scale                ║
╠═══════════════════════════════════╣
║  _+_ increase   _-_ decrease      ║
║  _0_ reset      _q_ quit          ║
╚═══════════════════════════════════╝
"
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ("0" (text-scale-set 0) :exit t)
    ("q" nil :exit t)
    ("RET" nil :exit t))

  ;; Global binding
  (global-set-key (kbd "C-c t s") 'hydra-text-scale/body)

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "t s" '(hydra-text-scale/body :which-key "text scale"))))

;;; ========================================================================
;;; Git/Magit Hydra
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-git (:color blue :hint nil)
    "
╔═══════════════════════════════════════════════════════════════╗
║                         Git/Magit                             ║
╠═══════════════════════════════════════════════════════════════╣
║  Main      │ _s_ status   _d_ diff   _l_ log   _b_ blame     ║
║  Actions   │ _c_ commit   _p_ push   _f_ pull  _F_ fetch     ║
║  Time      │ _t_ timemachine                                  ║
║  Quit      │ _q_ quit                                         ║
╚═══════════════════════════════════════════════════════════════╝
"
    ("s" magit-status :exit t)
    ("d" magit-diff-buffer-file :exit t)
    ("l" magit-log-buffer-file :exit t)
    ("b" magit-blame :exit t)
    ("c" magit-commit :exit t)
    ("p" magit-push :exit t)
    ("f" magit-pull :exit t)
    ("F" magit-fetch :exit t)
    ("t" git-timemachine :exit t)
    ("q" nil :exit t))

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "g ." '(hydra-git/body :which-key "git hydra"))))

;;; ========================================================================
;;; Workspace/Perspective Hydra
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-perspective (:color pink :hint nil)
    "
╔═══════════════════════════════════════════════════════════════╗
║                      Workspaces                               ║
╠═══════════════════════════════════════════════════════════════╣
║  Navigate  │ _n_ next   _p_ prev   _l_ list                   ║
║  Switch    │ 0-9 workspace by number                          ║
║  Create    │ _c_ create   _r_ rename                          ║
║  Delete    │ _d_ delete   _k_ kill                            ║
║  Quit      │ _q_ quit                                         ║
╚═══════════════════════════════════════════════════════════════╝
"
    ("n" persp-next)
    ("p" persp-prev)
    ("l" persp-list-buffers :exit t)
    ("c" persp-switch :exit t)
    ("r" persp-rename :exit t)
    ("d" persp-kill)
    ("k" persp-kill)
    ("q" nil :exit t))

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "TAB ." '(hydra-perspective/body :which-key "workspace hydra"))))

;;; ========================================================================
;;; LSP/Eglot Hydra
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-lsp (:color blue :hint nil)
    "
╔═══════════════════════════════════════════════════════════════╗
║                         LSP/Eglot                             ║
╠═══════════════════════════════════════════════════════════════╣
║  Actions   │ _r_ rename   _f_ format   _o_ organize imports   ║
║  Navigate  │ _d_ definition   _D_ references   _i_ impl       ║
║  Info      │ _h_ hover   _s_ symbols   _a_ code action        ║
║  Server    │ _S_ shutdown   _R_ restart                       ║
║  Quit      │ _q_ quit                                         ║
╚═══════════════════════════════════════════════════════════════╝
"
    ("r" eglot-rename :exit t)
    ("f" eglot-format :exit t)
    ("o" eglot-code-action-organize-imports :exit t)
    ("d" xref-find-definitions :exit t)
    ("D" xref-find-references :exit t)
    ("i" eglot-find-implementation :exit t)
    ("h" eldoc-doc-buffer :exit t)
    ("s" consult-eglot-symbols :exit t)
    ("a" eglot-code-actions :exit t)
    ("S" eglot-shutdown :exit t)
    ("R" eglot-reconnect :exit t)
    ("q" nil :exit t))

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "c ." '(hydra-lsp/body :which-key "lsp hydra"))))

;;; ========================================================================
;;; Navigation Hydra
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-navigate (:color pink :hint nil)
    "
╔═══════════════════════════════════════════════════════════════╗
║                       Navigation                              ║
╠═══════════════════════════════════════════════════════════════╣
║  Scroll    │ _j_ down   _k_ up   _d_ page-down  _u_ page-up  ║
║  Jump      │ _g_ top    _G_ bottom   _m_ middle              ║
║  Recenter  │ _c_ center   _t_ top    _b_ bottom              ║
║  Quit      │ _q_ quit                                         ║
╚═══════════════════════════════════════════════════════════════╝
"
    ("j" evil-scroll-line-down)
    ("k" evil-scroll-line-up)
    ("d" evil-scroll-page-down)
    ("u" evil-scroll-page-up)
    ("g" evil-goto-first-line :exit t)
    ("G" evil-goto-line :exit t)
    ("m" evil-window-middle)
    ("c" recenter-top-bottom)
    ("t" (recenter-top-bottom 0))
    ("b" (recenter-top-bottom -1))
    ("q" nil :exit t))

  ;; Global binding
  (global-set-key (kbd "C-c n") 'hydra-navigate/body))

;;; ========================================================================
;;; Edit Hydra (expand-region, iedit, multiple cursors)
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-edit (:color pink :hint nil)
    "
╔═══════════════════════════════════════════════════════════════╗
║                         Editing                               ║
╠═══════════════════════════════════════════════════════════════╣
║  Select    │ _+_ expand   _-_ contract                        ║
║  Multi     │ _m_ mark-all   _n_ next   _p_ prev              ║
║  Iedit     │ _e_ iedit-mode                                   ║
║  Quit      │ _q_ quit                                         ║
╚═══════════════════════════════════════════════════════════════╝
"
    ("+" er/expand-region)
    ("-" er/contract-region)
    ("m" evil-mc-make-all-cursors :exit t)
    ("n" evil-mc-make-and-goto-next-match :exit t)
    ("p" evil-mc-make-and-goto-prev-match :exit t)
    ("e" iedit-mode :exit t)
    ("q" nil :exit t))

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "e ." '(hydra-edit/body :which-key "edit hydra"))))

;;; ========================================================================
;;; Zoom Hydra (Fontaine integration)
;;; ========================================================================

(with-eval-after-load 'hydra
  (defhydra hydra-zoom (:color pink :hint nil)
    "
╔═══════════════════════════════════════════════════════════════╗
║                         Zoom                                  ║
╠═══════════════════════════════════════════════════════════════╣
║  Buffer    │ _+_ increase   _-_ decrease   _0_ reset          ║
║  Frame     │ _f_ fontaine-preset                              ║
║  Quit      │ _q_ quit                                         ║
╚═══════════════════════════════════════════════════════════════╝
"
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ("0" (text-scale-set 0) :exit t)
    ("f" fontaine-set-preset :exit t)
    ("q" nil :exit t))

  ;; Leader key binding
  (with-eval-after-load 'general
    (sleepy/leader-def
      "t z" '(hydra-zoom/body :which-key "zoom hydra"))))

;;; hydra.el ends here
