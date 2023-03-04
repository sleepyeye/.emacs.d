(use-package org
  :mode ("\\.org" . org-mode))

(use-package org-contrib
	:defer 10)

(use-package doct
  :elpaca (doct :branch "development" :protocol ssh :depth nil)
  :commands (doct))


(use-package valign
	:hook (org-mode . valign-mode))
