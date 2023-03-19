(use-package org
  :mode ("\\.org" . org-mode))

(use-package org-contrib
	:defer 10)

(use-package doct
  :elpaca (doct :branch "development" :protocol ssh :depth nil)
  :commands (doct))


(use-package valign
	:hook (org-mode . valign-mode))


(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "~/Dropbox/org/roam/"))
  :bind
	(("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; (use-package org-gcal
;; 	:init
;; 	(setq epg-pinentry-mode 'loopback)
;; 	(setq org-gcal-client-id "593814277796-khurk1t6vjs9rmq96dkiqmjislsgvpep.apps.googleusercontent.com"
;; 				org-gcal-client-secret "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"
;; 				org-gcal-file-alist '( ("wonjunlee.0729@gmail.com" . "~/Sync/org/gcal.org")))
;; 	(setq plstore-cache-passphrase-for-symmetric-encryption t)
;; 	:config
;; 	(org-gcal-reload-client-id-secret))




