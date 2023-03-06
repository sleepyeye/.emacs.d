;; Checkout this awesome posts about font system in emacs-lisp
;; https://idiocy.org/emacs-fonts-and-fontsets.html
;; The symbol name of Korean in =script-representative-chars= is 'hangul


;; First we define fall-back Korean font which is Noto Sans CJK.
;; Append to default fontsets of hangul
(set-fontset-font t 'hangul "Noto Sans CJK KR Regular" nil 'append)
;; and other East-Asian fontsets as well
(set-fontset-font t 'han "Noto Sans CJK SC Regular" nil 'append)
(set-fontset-font t 'kana "Noto Sans CJK JP Regular" nil 'append)
(set-fontset-font t 'cjk-misc "Noto Sans CJK KR Regular" nil 'append)


;; My preference is IBM Plex Sans
(set-fontset-font t 'hangul "IBM Plex Sans KR")


(use-package fontaine
  :demand t
  :after modus-themes
  :init
  (add-hook 'modus-themes-after-load-theme-hook #'fontaine-apply-current-preset)
  :config
  (setq fontaine-presets
	'((regular
	   :default-family "Fira Code"
	   :default-height 140  ;; x10 of font size in other programs
	   :default-weight regular
	   :fixed-pitch-family "Fira Code"
	   :fixed-pitch-serif-family "IBM Plex Serif"
	   :variable-pitch-family "IBM Plex Sans"
	   :italic-family "JuliaMono"
	   :italic-slant italic
	   :bold-weight bold
	   :line-spacing 1)))
  (fontaine-set-preset 'regular))
