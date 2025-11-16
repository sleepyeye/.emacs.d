;;; profile-init.el --- Profile Emacs startup and command execution -*- lexical-binding: t; -*-

;; Usage:
;; 1. Startup profiling: emacs -Q -l ~/.emacs.d/profile-init.el
;; 2. Command profiling: M-x profiler-start RET cpu RET
;;                       (do your slow operations)
;;                       M-x profiler-report RET

;; Measure init time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Load your normal init
(load user-init-file)

;;; profile-init.el ends here
