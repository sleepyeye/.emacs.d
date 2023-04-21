;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")
  :config
  (defvar sleepy/tempel-templates
	'((templates "Global template"))
	"My templates.")
  (add-to-list 'tempel-template-sources 'sleepy/tempel-templates))
