(use-package org-ai
  :commands (org-ai-mode org-ai-global-mode)
  :after (auth-source org)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-3.5-turbo") ; if you are on the gpt-4 beta:
  (setq org-ai-openai-api-token (sleepy/get-openai-api-key))
)

(use-package chatgpt-shell
  :commands (chatgpt-shell dall-e-shell)
  :general
  (sleepy/leader-def
	"aa" #'chatgpt-shell
	"ac" #'chatgpt-shell-explain-code
	"ar" #'chatgpt-shell-refactor-code
	"ap" #'chatgpt-shell-proofread-region
	)

  (general-define-key
   "M-o" #'chatgpt-shell)
  :config

  (general-define-key
   :keymaps 'shell-maker-mode-map
   "C-[" #'chatgpt-shell-previous-item
   "C-]" #'chatgpt-shell-next-item)

  (setq chatgpt-shell-insert-queries-inline nil)
  (setq chatgpt-shell-openai-key (sleepy/get-openai-api-key)))
