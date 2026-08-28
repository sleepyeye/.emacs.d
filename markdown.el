;;; markdown.el --- Markdown mode configuration -*- lexical-binding: t; -*-

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t
        markdown-hide-markup nil
        markdown-make-gfm-checkboxes-buttons t))

(defun sleepy/markdown-leader-keys (keymap)
  "Bind markdown leader keys on KEYMAP (markdown-mode or markdown-ts-mode)."
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def
      :keymaps keymap
      "m p" '(markdown-preview :which-key "preview")
      "m e" '(markdown-export :which-key "export")
      "m i" '(markdown-insert-link :which-key "insert link")
      "m I" '(markdown-insert-image :which-key "insert image"))))

(with-eval-after-load 'general
  (sleepy/markdown-leader-keys 'markdown-mode-map)
  (with-eval-after-load 'markdown-ts-mode
    (sleepy/markdown-leader-keys 'markdown-ts-mode-map)))

;;; markdown.el ends here
