;;; note.el --- org-mode + denote configuration -*- lexical-binding: t; -*-

;;; --- Denote ---------------------------------------------------------

(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "notes" "~/org"))
  (denote-file-type 'org)
  (denote-known-keywords '("project" "ref" "idea" "meeting" "journal"))
  (denote-prompts '(title keywords))
  :config
  (unless (file-directory-p denote-directory)
    (make-directory denote-directory t))

  (defun sleepy/journal--file-today ()
    "Return today's journal file path, or nil if it doesn't exist."
    (car (denote-directory-files
          (format "%s.*journal" (format-time-string "%Y-%m-%d")))))

  (defun sleepy/journal--template ()
    "Return the template for a new journal entry."
    "* Daily\n- [ ] \n\n* Journal\n")

  (defun sleepy/denote-journal ()
    "Open or create today's journal entry."
    (interactive)
    (if-let ((file (sleepy/journal--file-today)))
        (find-file file)
      ;; Create new journal with template
      (denote (format-time-string "%Y-%m-%d")
              '("journal")
              nil nil nil)
      (goto-char (point-max))
      (insert (sleepy/journal--template))
      (save-buffer)))

  (defun sleepy/journal-add-entry ()
    "Add a timestamped entry to today's journal in a popup window."
    (interactive)
    ;; Ensure today's journal exists
    (unless (sleepy/journal--file-today)
      (save-window-excursion
        (sleepy/denote-journal)))
    ;; Open journal in popup (bottom side window)
    (let ((buf (find-file-noselect (sleepy/journal--file-today))))
      (pop-to-buffer buf
                     '((display-buffer-in-side-window)
                       (side . bottom)
                       (window-height . 0.3)))
      ;; Find or create Journal section
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Journal$" nil t)
        (goto-char (point-max))
        (insert "\n* Journal\n"))
      ;; Go to end of Journal section (before next heading or EOF)
      (if (re-search-forward "^\\* " nil t)
          (beginning-of-line)
        (goto-char (point-max)))
      ;; Insert timestamped entry with cursor on same line
      (insert (format "\n** %s " (format-time-string "%H:%M")))
      (evil-insert-state))))

(use-package consult-denote
  :ensure t
  :after denote
  :config
  (consult-denote-mode 1))

;;; --- Org-mode -------------------------------------------------------

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "CANCEL(c@)")))
  (org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-archive-location (expand-file-name "archive.org::datetree/" org-directory)))

(use-package org-agenda
  :ensure nil
  :after org
  :commands org-agenda
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t))

;;; --- Org-capture ----------------------------------------------------

(use-package org-capture
  :ensure nil
  :after org
  :commands org-capture
  :config
  (setq org-capture-templates
        '(("i" "Inbox" entry
           (file+headline org-default-notes-file "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)
          ("t" "Task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)
          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1))))

;;; --- Evil integration -----------------------------------------------

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; --- Keybindings ----------------------------------------------------

(with-eval-after-load 'general
  (sleepy/leader-def
    "n"   '(:ignore t :which-key "Notes")
    ;; Org: lowercase = action/popup, UPPERCASE = open file
    "na"  '(org-agenda :which-key "agenda")
    "nc"  '(org-capture :which-key "capture")
    "nC"  '((lambda () (interactive)
              (find-file (expand-file-name "inbox.org" org-directory)))
            :which-key "open inbox")
    "nj"  '(sleepy/journal-add-entry :which-key "journal entry")
    "nJ"  '(sleepy/denote-journal :which-key "open journal")
    ;; Denote: lowercase = action, UPPERCASE = open location
    "nn"  '(denote :which-key "new note")
    "nN"  '((lambda () (interactive)
              (find-file denote-directory))
            :which-key "open notes dir")
    "nf"  '(consult-denote-find :which-key "find note")
    "ng"  '(consult-denote-grep :which-key "grep notes")
    "nl"  '(denote-link-or-create :which-key "insert link")
    "nL"  '(org-store-link :which-key "store org link")
    "nb"  '(denote-backlinks :which-key "backlinks")
    "nr"  '(denote-rename-file :which-key "rename")))

;;; note.el ends here
