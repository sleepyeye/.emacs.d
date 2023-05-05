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
  (setq chatgpt-shell-insert-queries-inline nil)
  (setq chatgpt-shell-openai-key (sleepy/get-openai-api-key)))
