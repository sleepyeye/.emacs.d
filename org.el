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


(use-feature org-indent
  :hook (org-mode . org-indent-mode))

(use-package org-starless
  :elpaca (org-starless :repo "TonCherAmi/org-starless" :host github)
  :hook (org-mode . org-starless-mode))


(with-eval-after-load 'org-faces
  (set-face-attribute 'org-level-1 nil :foreground "white" :background nil :box nil :weight 'heavy      :height 1.2)
  (set-face-attribute 'org-level-2 nil :foreground "white" :background nil :box nil :weight 'ultra-bold :height 1.1)
  (set-face-attribute 'org-level-3 nil :foreground "white" :background nil :box nil :weight 'bold       :height 1.05)
  (set-face-attribute 'org-level-4 nil :foreground "white" :background nil :box nil :weight 'semi-bold  :height 1.0)
  (set-face-attribute 'org-level-5 nil :foreground "white" :background nil :box nil :weight 'medium     :height 1.0)
  (set-face-attribute 'org-level-6 nil :foreground "white" :background nil :box nil :weight 'medium     :height 1.0)
  (set-face-attribute 'org-level-7 nil :foreground "white" :background nil :box nil :weight 'medium     :height 1.0)
  (set-face-attribute 'org-level-8 nil :foreground "white" :background nil :box nil :weight 'medium     :height 1.0))
