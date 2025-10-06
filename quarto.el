(use-package quarto-mode
  :mode (("\\.Rmd" . poly-quarto-mode))
  :config
  (setq quarto-preview-display-buffer nil)
  (define-key poly-quarto-mode-map (kbd "C-c C-c") 'quarto-preview))
