(use-feature dired
  :commands (dired)
  :custom
  (dired-listing-switches "-alh" "Human friendly file sizes.")
  (dired-kill-when-opening-new-dired-buffer t))
