;;; misc.el --- odds & ends -*- lexical-binding: t; -*-

;; --------------------------------------------
;; Hide mixed DOS EOL (^M) or convert to UNIX
;; --------------------------------------------
(defun remove-dos-eol ()
  "Hide ^M when visiting files with mixed CRLF/LF endings.
표시만 가리는 것. 실제 변환은 `set-buffer-file-coding-system`을 사용."
  (interactive)
  (setq buffer-display-table (or buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

;; 참고: 실제로 CRLF→LF 변환하려면 이걸 실행
(defun convert-dos-to-unix ()
  "Convert current buffer to UNIX line endings."
  (interactive)
  (set-buffer-file-coding-system 'unix))

;; -------
;; vterm
;; -------
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell (or (executable-find "fish")
                        (getenv "SHELL")              ;; fallback to login shell
                        (user-shell))
        vterm-max-scrollback 10000)
  (with-eval-after-load 'general
    (sleepy/leader-def
      "ot" #'vterm)))   ;; SPC o t 로 터미널 열기

;; -------------------------
;; Spell checking (jinx)
;; -------------------------
(use-package jinx
  :hook
  ;; 전역 활성화
  (text-mode . jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("M-i" . jinx-correct)
         ("M-o" . jinx-previous)
         ("M-p" . jinx-next))
  :custom
  ;; 언어: 필요시 "en ko" 등 추가 가능(사전 설치 필요)
  (jinx-languages "en")
  :config
  ;; 가독성 좋은 밑줄(원하는 색으로 바꿔도 됨)
  (set-face-attribute 'jinx-misspelled nil
                      :underline '(:color "#006800" :style wave)));; -------------------------
;; GC tuning (gcmh)
;; -------------------------
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  ;; 유휴 시점 자동 계산
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  ;; 안전한 임계값(낮음/높음). 이전 값 `minimal-emacs-gc-cons-threshold`는
  ;; 환경에 없으면 void-variable가 날 수 있어 명시적으로 설정.
  (gcmh-low-cons-threshold  (* 1 1024 1024))    ;; 1MB
  (gcmh-high-cons-threshold (* 128 1024 1024))) ;; 128MB

;; 긴 줄로 인한 렌더링 병목 완화(Emacs 내장)
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))
