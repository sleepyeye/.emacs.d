(defun sleepy/org-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-super-capf
					 #'tempel-complete
					 #'cape-file
					 #'cape-dabbrev))))
(use-package org
  :defer 1
  :mode ("\\.org" . org-mode)
  :hook (org-mode . sleepy/org-capf)
  :init
  (setq org-cite-global-bibliography '("~/Dropbox/roam/ref.bib")))

(use-package org-contrib
  :defer 10)

(use-package doct
  :elpaca (doct :branch "development" :protocol ssh :depth nil)
  :commands (doct))


(use-package valign
  :hook (org-mode . valign-mode))


(use-package org-roam
  :defer 2
  :custom
  (org-roam-directory (expand-file-name "~/Dropbox/roam"))
  :general
  (sleepy/leader-def
	"rf" #'org-roam-node-find
	"ri" #'org-roam-node-insert
	"rc" #'org-roam-capture
	"r]" #'org-roam-dailies-capture-today
	"rt" #'org-roam-dailies-goto-today
	"rT" #'org-roam-dailies-goto-tomorrow
	"ry" #'org-roam-dailies-goto-yesterday)

  :config
  (setq org-roam-completion-everywhere t)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-dailies-capture-templates
		'(("d" "default" entry "\n\n* %<%H:%M> %?"
		   :empty-lines 1
		   :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n\n"))
		  ))
  (org-roam-db-autosync-mode))


(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
