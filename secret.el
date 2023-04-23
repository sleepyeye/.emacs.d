(use-package auth-source
  :demand t
  :elpaca nil
  :config
  (defun sleepy/get-openai-api-key ()
	"Get the OpenAI API key from the authinfo file, or return nil if not found."
	(let* ((auth-info (auth-source-search :host "openai.emacs" :max 1))
		   (api-key (when auth-info (plist-get (car auth-info) :apikey))))
	  (if api-key
		  api-key
		nil)))

  )
