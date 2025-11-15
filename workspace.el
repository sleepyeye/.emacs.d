;;;; workspace.el --- perspective + consult integration -*- lexical-binding: t; -*-

(use-package perspective
  :demand t
  :init
  (setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory)
        persp-suppress-no-prefix-key-warning t
        persp-initial-frame-name "1")  ; Start with workspace "1" instead of "main"
  :config
  (persp-mode 1)

  ;; Session save/restore
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (when (file-exists-p persp-state-default-file)
    (ignore-errors (persp-state-load persp-state-default-file)))

  ;; Numeric workspace switching
  (defun sleepy/persp--switch (name)
    "Switch to perspective NAME (create if missing)."
    (interactive) (persp-switch name))

  ;; Generate workspace switching functions for 0-9
  (dotimes (i 10)
    (let ((num (number-to-string i)))
      (defalias (intern (format "sleepy/persp-%s" num))
        (lambda ()
          (interactive)
          (sleepy/persp--switch num))
        (format "Switch to workspace %s." num))))

  ;; Auto workspace when entering a project
  (defun sleepy/persp-for-project ()
    "Switch to a workspace named after the current project."
    (when-let ((proj (project-current)))
      (let ((name (file-name-nondirectory
                   (directory-file-name (project-root proj)))))
        (persp-switch name))))
  (add-hook 'project-switch-project-hook #'sleepy/persp-for-project)

  ;; Switch by name
  (defun sleepy/persp-switch-completing ()
    "Switch to workspace chosen via minibuffer completion."
    (interactive)
    (persp-switch (completing-read "Switch to workspace: " (persp-names))))

  ;; Consult: Prioritize current workspace buffers
  (with-eval-after-load 'consult
    ;; Don't hide default buffer source, just lower its priority
    (consult-customize consult--source-buffer :hidden nil :default nil)
    (defvar consult--source-perspective
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda ()
                        (if (and (fboundp 'persp-curr)
                                 (persp-curr))
                            (mapcar #'buffer-name
                                    (persp-buffers (persp-curr)))
                          ;; Return empty list instead of nil
                          '()))))
    ;; Add perspective source to the front
    ;; Ensure consult-buffer-sources is initialized first
    (unless (boundp 'consult-buffer-sources)
      (require 'consult))
    (setq consult-buffer-sources
          (cons 'consult--source-perspective
                (delq 'consult--source-perspective consult-buffer-sources))))


  ;; Workspace leader: SPC TAB ...
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        ;; Numeric workspace switching
      "TAB 1" #'sleepy/persp-1
      "TAB 2" #'sleepy/persp-2
      "TAB 3" #'sleepy/persp-3
      "TAB 4" #'sleepy/persp-4
      "TAB 5" #'sleepy/persp-5
      "TAB 6" #'sleepy/persp-6
      "TAB 7" #'sleepy/persp-7
      "TAB 8" #'sleepy/persp-8
      "TAB 9" #'sleepy/persp-9
      "TAB 0" #'sleepy/persp-0

        ;; Workspace utilities
        "TAB TAB" #'sleepy/persp-switch-completing))))
