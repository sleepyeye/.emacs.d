;;; media.el --- PDF and image viewing configuration -*- lexical-binding: t; -*-

;; =============================================================================
;; BINARY FILE HANDLING
;; =============================================================================
;; Prevent encoding prompts for binary files (PDF, images, etc.)

;; Add common binary extensions to auto-coding-alist
(dolist (ext '("\\.pdf\\'" "\\.png\\'" "\\.jpg\\'" "\\.jpeg\\'" "\\.gif\\'"
               "\\.bmp\\'" "\\.tiff\\'" "\\.webp\\'" "\\.ico\\'" "\\.svg\\'"
               "\\.eps\\'" "\\.ps\\'" "\\.dvi\\'"
               "\\.mp3\\'" "\\.mp4\\'" "\\.mkv\\'" "\\.avi\\'" "\\.mov\\'"
               "\\.zip\\'" "\\.tar\\'" "\\.gz\\'" "\\.bz2\\'" "\\.xz\\'"
               "\\.exe\\'" "\\.dll\\'" "\\.so\\'" "\\.dylib\\'"
               "\\.o\\'" "\\.a\\'" "\\.elc\\'" "\\.eln\\'"
               "\\.sqlite\\'" "\\.db\\'"))
  (add-to-list 'auto-coding-alist (cons ext 'no-conversion)))

;; =============================================================================
;; PDF VIEWING - pdf-tools
;; =============================================================================
;; pdf-tools provides better PDF viewing than doc-view:
;; - Faster rendering
;; - Text selection and copying
;; - Annotations
;; - Isearch in PDF
;; - HiDPI/Retina support

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  ;; Silence compilation warnings
  (setq pdf-view-use-scaling t)  ; HiDPI support
  :config
  ;; Install pdf-tools without prompting (builds epdfinfo if needed)
  (pdf-tools-install :no-query)

  ;; Display settings
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1
        pdf-view-continuous t)

  ;; Midnight mode (dark background) - disabled by default
  ;; Use `pdf-view-midnight-minor-mode' to toggle
  (setq pdf-view-midnight-colors '("#ffffff" . "#1a1a1a"))

  ;; Prevent duplicate windows when opening PDFs
  ;; pdf-view already handles this, but ensure display-buffer doesn't interfere
  (add-to-list 'display-buffer-alist
               '("\\.pdf\\(<[^>]+>\\)?\\'"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t)))

  ;; Performance: disable blinking cursor in PDF mode
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq-local cursor-type nil)
              (setq-local blink-cursor-mode nil))))

;; Evil integration for pdf-tools (already in evil.el, but ensure bindings work)
(with-eval-after-load 'pdf-tools
  (with-eval-after-load 'evil
    ;; Ensure motion state is used (already set in evil.el)
    (evil-set-initial-state 'pdf-view-mode 'motion)

    ;; PDF-specific evil bindings in motion state
    (evil-define-key 'motion pdf-view-mode-map
      ;; Navigation
      "j" 'pdf-view-next-line-or-next-page
      "k" 'pdf-view-previous-line-or-previous-page
      "J" 'pdf-view-next-page
      "K" 'pdf-view-previous-page
      "h" 'image-backward-hscroll
      "l" 'image-forward-hscroll
      "gg" 'pdf-view-first-page
      "G" 'pdf-view-last-page
      (kbd "C-d") 'pdf-view-scroll-up-or-next-page
      (kbd "C-u") 'pdf-view-scroll-down-or-previous-page
      (kbd "C-f") 'pdf-view-scroll-up-or-next-page
      (kbd "C-b") 'pdf-view-scroll-down-or-previous-page

      ;; Zoom
      "+" 'pdf-view-enlarge
      "-" 'pdf-view-shrink
      "0" 'pdf-view-scale-reset
      "W" 'pdf-view-fit-width-to-window
      "H" 'pdf-view-fit-height-to-window
      "P" 'pdf-view-fit-page-to-window

      ;; Search
      "/" 'isearch-forward
      "?" 'isearch-backward
      "n" 'isearch-repeat-forward
      "N" 'isearch-repeat-backward

      ;; Other
      "o" 'pdf-outline
      "m" 'pdf-view-midnight-minor-mode
      "q" 'quit-window
      (kbd "RET") 'pdf-links-action-perform)))

;; =============================================================================
;; IMAGE VIEWING - image-mode
;; =============================================================================
;; Built-in image-mode with proper configuration

(use-package image-mode
  :ensure nil
  :mode (("\\.png\\'" . image-mode)
         ("\\.jpe?g\\'" . image-mode)
         ("\\.gif\\'" . image-mode)
         ("\\.bmp\\'" . image-mode)
         ("\\.webp\\'" . image-mode)
         ("\\.svg\\'" . image-mode)
         ("\\.tiff?\\'" . image-mode)
         ("\\.ico\\'" . image-mode))
  :config
  ;; Prevent duplicate windows for images
  (add-to-list 'display-buffer-alist
               '("\\(?:\\.png\\|\\.jpe?g\\|\\.gif\\|\\.bmp\\|\\.webp\\|\\.svg\\|\\.tiff?\\|\\.ico\\)\\(<[^>]+>\\)?\\'"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t)))

  ;; Image display settings
  (setq image-auto-resize t
        image-auto-resize-on-window-resize 1)  ; Resize when window changes

  ;; Animate GIFs automatically
  (setq image-animate-loop t)

  ;; Hook for image-mode specific settings
  (add-hook 'image-mode-hook
            (lambda ()
              ;; Disable line numbers in image buffers
              (display-line-numbers-mode -1)
              ;; Disable cursor blinking
              (setq-local cursor-type nil)
              (setq-local blink-cursor-mode nil))))

;; Evil bindings for image-mode
(with-eval-after-load 'image-mode
  (with-eval-after-load 'evil
    (evil-set-initial-state 'image-mode 'motion)

    (evil-define-key 'motion image-mode-map
      ;; Navigation
      "j" 'image-next-line
      "k" 'image-previous-line
      "h" 'image-backward-hscroll
      "l" 'image-forward-hscroll
      "J" 'image-scroll-up
      "K" 'image-scroll-down
      (kbd "C-d") 'image-scroll-up
      (kbd "C-u") 'image-scroll-down

      ;; Zoom
      "+" 'image-increase-size
      "-" 'image-decrease-size
      "0" 'image-transform-reset
      "r" 'image-rotate
      "W" 'image-transform-fit-to-width
      "H" 'image-transform-fit-to-height

      ;; Animation (for GIFs)
      "a" 'image-toggle-animation
      "{" 'image-previous-frame
      "}" 'image-next-frame
      (kbd "SPC") 'image-toggle-animation

      ;; File navigation
      "n" 'image-next-file
      "p" 'image-previous-file

      ;; Other
      "q" 'quit-window)))

;; =============================================================================
;; AUTO-COMPRESSION FOR VIEWING COMPRESSED FILES
;; =============================================================================
;; Already enabled by default, but ensure it's on
(auto-compression-mode 1)

;;; media.el ends here
