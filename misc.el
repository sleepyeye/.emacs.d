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
(use-package jinx
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
  :hook (after-init . gcmh-mode)
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
