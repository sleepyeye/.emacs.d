(use-package vertico
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode)
  :init
  (setq vertico-count 10
		vertico-cycle t))

(use-package consult
  :demand t
  :config
  (add-to-list 'consult-buffer-filter ".*roam.org$")
  (add-to-list 'consult-buffer-filter "\\`\\*Messages\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Completions\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Welcome\\*\\'")
  (add-to-list 'consult-buffer-filter "magit*")

  (sleepy/leader-def
	"bd" 'kill-current-buffer
	"br" 'revert-buffer
	"bb" 'consult-buffer
	"bB" 'consult-buffer-other-window
	"bp" 'consult-project-buffer)

  ;; TODO fix name
  (defun consult-ripgrep-current ()
	(interactive)
	(consult-ripgrep default-directory))

  (sleepy/leader-def
	"si" 'consult-imenu
	"sI" 'consult-imenu-multi
	"sb" 'consult-line
	"sB" 'consult-line-multi
	"sd" 'consult-ripgrep-current
	"sp" 'consult-ripgrep
	"sg" 'consult-git-grep)

  (sleepy/leader-def
	"ji" 'consult-outline
	"jl" 'consult-goto-line
	"jo" 'consult-outline
	"jd" 'consult-ripgrep
	"jg" 'consult-git-grep)

  (sleepy/leader-def
	"uy" 'consult-yank-pop)

  (sleepy/leader-def
	"fr" 'consult-recent-file)

  :config
  ;; Turn off live-preview
  (consult-customize
   consult-theme
   consult-recent-file
   :preview-key nil)


  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

(use-package orderless
  :demand t
  :init
  ;;; I found list of completion categories from port's config
  ;;; For more details, see
  ;;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs.org
  ;; A list of known completion categories:
  ;;; - `bookmark'
  ;;; - `buffer'
  ;;; - `charset'
  ;;; - `coding-system'
  ;;; - `color'
  ;;; - `command' (e.g. `M-x')
  ;;; - `customize-group'
  ;;; - `environment-variable'
  ;;; - `expression'
  ;;; - `face'
  ;;; - `file'
  ;;; - `function' (the `describe-function' command bound to `C-h f')
  ;;; - `info-menu'
  ;;; - `imenu'
  ;;; - `input-method'
  ;;; - `kill-ring'
  ;;; - `library'
  ;;; - `minor-mode'
  ;;; - `multi-category'
  ;;; - `package'
  ;;; - `project-file'
  ;;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;;; - `theme'
  ;;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;;; - `consult-grep'
  ;;; - `consult-isearch'
  ;;; - `consult-kmacro'
  ;;; - `consult-location'
  ;;; - `embark-keybinding'

  ;;; Not only that he summarized completion styles
  ;;; emacs22
  ;;; Prefix completion that only operates on the text before point. If we are in prefix|suffix, with | representing the cursor, it will consider everything that expands prefix and then add back to it the suffix.
  ;;; basic
  ;;; Prefix completion that also accounts for the text after point. Using the above example, this one will consider patterns that match all of emacs22 as well as anything that completes suffix.
  ;;; partial-completion
  ;;; This is used for file navigation. Instead of typing out a full path like ~/.local/share/fonts, we do ~/.l/s/f or variants thereof to make the matches unique such as ~/.l/sh/fon. It is a joy to navigate the file system in this way.
  ;;; substring
  ;;; Matches the given sequence of characters literally regardless of where it is in a word. So pro will match professional as well as reproduce.
  ;;; flex
  ;;; Completion of an in-order subset of characters. It does not matter where the charactes are in the word, so long as they are encountered in the given order. The input lad will thus match list-faces-display as well as pulsar-highlight-dwim.
  ;;; initials
  ;;; Completion of acronyms and initialisms. Typing lfd will thus match list-faces-display. This completion style can also be used for file system navigation, though I prefer to only have partial-completion handle that task.
  ;;; orderless
  ;;; This is the only completion style I use which is not built into Emacs. It matches patterns out-of-order. Patterns are typically words separated by spaces, though they can also be regular expressions, and even styles that are the same as the aforementioned flex and initials.
  (setq completion-styles '(orderless partial-completion basic)
		completion-ignore-case t
		completion-category-defaults nil
		completion-category-overrides '((file (styles . (partial-completion orderless)))
										(command (styles . (substring orderless)))
										(theme (styles . (substring orderless)))
										(variable (styles . (partial-completion orderless)))
										(symbol (styles . (basic substring orderless)))
										(eglot (styles . (orderless partial-completion basic)))))
  )


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :commands (marginalia-mode marginalia-cycle)
  :init
  (setq marginalia-annotators-heavy t))

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 80)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode 1))

(use-package cape
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))


;;; Completion candidate sorting with prescient.
;;;
;;; Corfu/Vertico/Consult provides really nice completion systems.
;;; But it is quite annoying without candidate sorting methods.
;;; For instance, I want to type use-package and I partially typed
;;; (use-pa|
;;;   candidate-1
;;;   candidate-2
;;;   candidate-3
;;;   candidate-4
;;; where '|' cursor.
;;; If you don't use appropriate sorting method candidate-1 would not be 'use-package'.
;;; I confused this concept with 'orderless' package.
;;; 'orderless' support fancy filtering algorithm's for completion candidates.
;;; However it is nothing related to sorting the result of them.
;;; The package 'prescient' provides methods for sorting these results
(use-package prescient
  :ensure t
  :init
  (setq prescient-sort-full-matches-first t))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (setq corfu-prescient-enable-filtering nil
		corfu-prescient-override-sorting t
		corfu-prescient-enable-sorting t))
