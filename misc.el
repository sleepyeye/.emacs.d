;;; misc.el --- odds & ends -*- lexical-binding: t; -*-

;; --------------------------------------------
;; Hide mixed DOS EOL (^M) or convert to UNIX
;; --------------------------------------------
(defun remove-dos-eol ()
  "Hide ^M when visiting files with mixed CRLF/LF endings.
Only hides display. For actual conversion, use `set-buffer-file-coding-system`."
  (interactive)
  (setq buffer-display-table (or buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

;; Note: To actually convert CRLF→LF, run this
(defun convert-dos-to-unix ()
  "Convert current buffer to UNIX line endings."
  (interactive)
  (set-buffer-file-coding-system 'unix))


;; -------------------------
;; Spell checking (jinx)
;; -------------------------
(defun sleepy--jinx-ready-p ()
  "Return non-nil when Jinx native module prerequisites seem available."
  (and (fboundp 'module-load)
       (or (executable-find "enchant-2")
           (executable-find "enchant"))))

(use-package jinx
  :if (sleepy--jinx-ready-p)
  :hook
  ;; Enable globally
  (text-mode . jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("M-i" . jinx-correct)
         ("M-o" . jinx-previous)
         ("M-p" . jinx-next))
  :custom
  ;; Language: can add "en ko" etc. if needed (requires dictionary installation)
  (jinx-languages "en")
  :config
  ;; Readable underline (change color as desired)
  (set-face-attribute 'jinx-misspelled nil
                      :underline '(:color "#006800" :style wave)));; -------------------------
;; GC tuning (gcmh)
;; -------------------------
(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :custom
  ;; Auto-calculate idle time
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  ;; Safe thresholds (low/high). Previous value `minimal-emacs-gc-cons-threshold`
  ;; could cause void-variable error if not in environment, so set explicitly.
  (gcmh-low-cons-threshold  (* 1 1024 1024))    ;; 1MB
  (gcmh-high-cons-threshold (* 128 1024 1024))) ;; 128MB

;; Mitigate rendering bottlenecks from long lines (built-in Emacs)
(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'global-so-long-mode)
              (global-so-long-mode 1))))

;; -------------------------
;; Terminal (vterm)
;; -------------------------
(defun sleepy--vterm-ready-p ()
  "Return non-nil when vterm native module build tools are available."
  (and (boundp 'module-file-suffix)
       module-file-suffix
       (executable-find "cmake")
       (executable-find "make")
       (executable-find "gcc")))

(use-package vterm
  :if (sleepy--vterm-ready-p)
  :ensure t
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (vterm-max-scrollback 5000)
  (vterm-timer-delay 0.01))
