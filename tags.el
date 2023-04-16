;; (use-package citre
;;   :defer t
;;   :after projectile
;;   :init
;;   ;; This is needed in `:init' block for lazy load to work.
;;   (require 'citre-config)
;;   ;; Bind your frequently used commands.  Alternatively, you can define them
;;   ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
;;   (global-set-key (kbd "C-x c j") 'citre-jump)
;;   (global-set-key (kbd "C-x c J") 'citre-jump-back)
;;   (global-set-key (kbd "C-x c p") 'citre-ace-peek)
;;   (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

;;   :config
;;   (setq
;;    ;; Set these if readtags/ctags is not in your PATH.
;;    citre-readtags-program (executable-find "readtags")
;;    citre-ctags-program (executable-find "ctags")
;;    ;; Set this if you use project management plugin like projectile.  It's
;;    ;; used for things like displaying paths relatively, see its docstring.
;;    citre-project-root-function #'projectile-project-root
;;    ;; Set this if you want to always use one location to create a tags file.
;;    citre-default-create-tags-file-location 'project-cache
;;    ;; See the "Create tags file" section above to know these options
;;    citre-use-project-root-when-creating-tags t
;;    citre-prompt-language-for-ctags-command t
;;    ;; By default, when you open any file, and a tags file can be found for it,
;;    ;; `citre-mode' is automatically enabled.  If you only want this to work for
;;    ;; certain modes (like `prog-mode'), set it like this.
;;    citre-auto-enable-citre-mode-modes '(prog-mode)))


(use-package dumb-jump
  :demand t
  :config
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

  
