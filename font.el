;;; font.el --- Font configuration -*- lexical-binding: t; -*-

;; Checkout this awesome post about font system in emacs-lisp
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
	   :line-spacing 1)
	  (iosevka
	   :default-family "Iosevka"
	   :default-weight regular
	   :default-height 140
	   :fixed-pitch-family "Iosevka Fixed"
	   :fixed-pitch-weight nil
	   :fixed-pitch-height 1.0
	   :fixed-pitch-serif-family nil ; falls back to :default-family
	   :fixed-pitch-serif-weight nil ; falls back to :default-weight
	   :fixed-pitch-serif-height 1.0
	   :variable-pitch-family "Iosevka Aile"
	   :variable-pitch-weight nil
	   :variable-pitch-height 1.0
	   :bold-family nil
	   :bold-weight bold
	   :italic-family nil
	   :italic-slant italic
	   :line-spacing 1)))
  (fontaine-set-preset 'iosevka)
  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; Persist font configurations while switching themes
  (add-hook 'modus-themes-after-load-theme-hook #'fontaine-apply-current-preset))

;;; font.el ends here
