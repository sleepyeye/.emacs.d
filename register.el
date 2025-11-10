;;; register.el --- Enhanced register system from Prot -*- lexical-binding: t; -*-

;; Based on prot-register.el and prot-simple.el from Protesilaos Stavrou's dotfiles

;;; Commentary:
;; This package extends Emacs registers with:
;; - File + position register type (jump to file:line even after closing buffer)
;; - DWIM register add (context-aware: region/number/frameset/file)
;; - DWIM register use (smart: jump or insert based on content)

;;; Code:

(require 'cl-lib)

;;;; 1. Custom register type: file + position

(defvar sleepy/file-register-functions nil
  "Hook run after jumping to a file register.")

(cl-defstruct (sleepy-file-register
               (:constructor nil)
               (:constructor sleepy-file-register-make (file position)))
  "Register type storing file path and cursor position."
  file position)

(cl-defmethod register-val-describe ((val sleepy-file-register) _verbose)
  "Describe file register VAL in human-readable format."
  (format "File %s at position %d"
          (sleepy-file-register-file val)
          (sleepy-file-register-position val)))

(cl-defmethod register-val-jump-to ((val sleepy-file-register) _arg)
  "Jump to file and position stored in register VAL."
  (let ((file (sleepy-file-register-file val))
        (pos (sleepy-file-register-position val)))
    (find-file file)
    (goto-char pos)
    (run-hooks 'sleepy/file-register-functions)))

;;;; 2. Save current file + position to register

(defun sleepy/register-save-file-position (register)
  "Save current file and position to REGISTER."
  (interactive
   (list (register-read-with-preview "File to register: ")))
  (unless buffer-file-name
    (user-error "Buffer not visiting a file"))
  (set-register register
                (sleepy-file-register-make buffer-file-name (point)))
  (message "Saved %s:%d to register %c"
           (file-name-nondirectory buffer-file-name)
           (point)
           register))

;;;; 3. DWIM register add (context-aware)

(defun sleepy/register-add-dwim (register)
  "Add to REGISTER based on context:
- Active region: store text
- Numeric prefix: store number
- Multiple windows: store frameset
- Default: store file + position"
  (interactive
   (list (register-read-with-preview "Add to register: ")))
  (cond
   ;; Active region -> store text
   ((use-region-p)
    (copy-to-register register (region-beginning) (region-end))
    (message "Stored region text to register %c" register))

   ;; Numeric prefix -> store number
   (current-prefix-arg
    (let ((num (prefix-numeric-value current-prefix-arg)))
      (set-register register num)
      (message "Stored number %d to register %c" num register)))

   ;; Multiple windows -> store frameset
   ((or (> (length (window-list)) 1)
        (> (length (tab-bar-tabs)) 1))
    (frameset-to-register register)
    (message "Stored frameset to register %c" register))

   ;; Default -> store file + position
   (buffer-file-name
    (sleepy/register-save-file-position register))

   (t
    (point-to-register register)
    (message "Stored point to register %c" register))))

;;;; 4. DWIM register use (smart retrieval)

(defun sleepy/register-use-dwim (register)
  "Use REGISTER intelligently:
- Jumpable (position, file, frameset): jump to it
- Insertable (text, number): insert it"
  (interactive
   (list (register-read-with-preview "Use register: ")))
  (let ((val (get-register register)))
    (cond
     ;; File register -> jump
     ((sleepy-file-register-p val)
      (register-val-jump-to val nil))

     ;; Marker/position -> jump
     ((or (markerp val) (and (consp val) (markerp (car val))))
      (jump-to-register register))

     ;; Frameset -> restore
     ((and (consp val) (eq 'frameset (car val)))
      (jump-to-register register))

     ;; Text/number -> insert
     ((or (stringp val) (numberp val))
      (insert-register register))

     ;; Fallback
     (t
      (jump-to-register register)))))

;;;; Hook configuration

;; Recenter screen after jumping to file register
(add-hook 'sleepy/file-register-functions #'recenter)

;;;; Enhanced completion integration

;; Command to store register with completion of existing registers
(defun sleepy/register-add-with-preview ()
  "Add to register with preview of existing registers."
  (interactive)
  (let ((register (register-read-with-preview "Add to register: ")))
    (sleepy/register-add-dwim register)))

;; Command to use register with narrowing by type
(defun sleepy/register-jump ()
  "Jump to a register (positions/files only) with completion."
  (interactive)
  (let* ((registers (cl-remove-if-not
                     (lambda (r)
                       (let ((val (cdr r)))
                         (or (markerp val)
                             (sleepy-file-register-p val)
                             (and (consp val)
                                  (or (markerp (car val))
                                      (eq 'frameset (car val)))))))
                     register-alist))
         (register (if registers
                       (register-read-with-preview "Jump to register: ")
                     (user-error "No position/file registers"))))
    (sleepy/register-use-dwim register)))

(defun sleepy/register-insert ()
  "Insert from register (text/numbers only) with completion."
  (interactive)
  (let* ((registers (cl-remove-if-not
                     (lambda (r)
                       (let ((val (cdr r)))
                         (or (stringp val) (numberp val))))
                     register-alist))
         (register (if registers
                       (register-read-with-preview "Insert from register: ")
                     (user-error "No text/number registers"))))
    (insert-register register)))

;; Better register listing with consult integration
(with-eval-after-load 'consult
  ;; Enhance consult-register preview for file registers
  (defun sleepy/consult-register-format (register)
    "Format REGISTER for consult display with file register support."
    (let ((val (get-register register)))
      (cond
       ((sleepy-file-register-p val)
        (format "File: %s:%d"
                (file-name-nondirectory (sleepy-file-register-file val))
                (sleepy-file-register-position val)))
       (t
        (register-describe-oneline register)))))

  ;; Add to consult-register-format if needed
  (advice-add 'register-describe-oneline :before-until #'sleepy/consult-register-format)

  ;; Optional: Add file registers as a source in consult-buffer
  (defvar sleepy/consult--source-register-file
    `(:name     "File Registers"
      :narrow   ?r
      :category register
      :face     consult-bookmark
      :history  register-alist
      :items    ,(lambda ()
                   (mapcar (lambda (reg)
                             (let ((key (car reg))
                                   (val (cdr reg)))
                               (when (sleepy-file-register-p val)
                                 (propertize
                                  (format "%c: %s:%d"
                                          key
                                          (file-name-nondirectory
                                           (sleepy-file-register-file val))
                                          (sleepy-file-register-position val))
                                  'register key))))
                           (seq-filter (lambda (r)
                                         (sleepy-file-register-p (cdr r)))
                                       register-alist)))
      :action   ,(lambda (cand)
                   (when-let ((reg (get-text-property 0 'register cand)))
                     (sleepy/register-use-dwim reg))))
    "Consult source for file registers.")

  ;; Uncomment to add file registers to consult-buffer:
  ;; (add-to-list 'consult-buffer-sources 'sleepy/consult--source-register-file 'append)
  )

(provide 'register)
;;; register.el ends here
