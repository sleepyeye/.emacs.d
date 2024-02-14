(use-package yaml-mode
  :mode (("\\.yml" . yaml-mode)
		 ("\\.yaml" . yaml-mode)))


(use-package yaml-pro
  :after yaml-mode
  :general
  (general-evil-define-key '(normal) yaml-pro-ts-mode-map
	">" #'yaml-pro-ts-indent-subtree
	"<" #'yaml-pro-ts-unindent-subtree
	"gh" #'yaml-pro-ts-up-level
	"gj" #'yaml-pro-ts-next-subtree
	"gk" #'yaml-pro-ts-prev-subtree)

  (general-define-key
   :keymaps 'yaml-pro-ts-mode-map
   "M-j" #'yaml-pro-ts-move-subtree-up
   "M-k" #'yaml-pro-ts-move-subtree-down))
