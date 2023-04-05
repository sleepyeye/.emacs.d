(use-package citar
  :demand t
  :commands (citar-open org-roam-capture org-roam-dailies-capture-today)
  :custom
  (citar-library-paths '("~/Dropbox/org/citar-lib/"))
  (citar-notes-paths '("~/Dropbox/org/roam/note/"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :config
  (setq citar-library-file-extensions (list "pdf" "jpg" "svg" "mp4" "key" "png")
		citar-file-additional-files-separator "-")
  :hook
  (LaTeX-mode . citar-capf-setup)

  (org-mode . citar-capf-setup))

(use-package citar-org-roam
  :demand t
  :after (citar org-roam)
  :config (citar-org-roam-mode))


;; (use-package citar-org-roam
;;   :demand t
;;   :config
;;   (citar-org-roam-mode)
;;   (setq citar-org-roam-note-title-template "${author} - ${title}")
;;   (setq citar-org-roam-capture-template-key "n"
;; 		citar-org-roam-subdir citar-notes-paths))
