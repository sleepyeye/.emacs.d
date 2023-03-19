(use-package explain-pause-mode
  :elpaca (explain-pause-mode :host github :repo "lastquestion/explain-pause-mode")
  :defer t)


(use-feature simple
  :general
  (+general-global-toggle
    "f" 'auto-fill-mode)
  :custom
  (eval-expression-debug-on-error nil)
  (fill-column 80 "Wrap at 80 columns."))

(use-feature autorevert
  :defer 2
  :custom
  (auto-revert-interval 0.01 "Instantaneously revert")
  :config
  (global-auto-revert-mode t))

(use-package default-text-scale
  :commands ( default-text-scale-increase
              default-text-scale-decrease
              default-text-scale-reset
              default-text-scale-increment))


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
