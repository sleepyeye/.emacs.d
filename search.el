;;; search.el --- ripgrep / dumb-jump setup -*- lexical-binding: t; -*-

;; dumb-jump (xref 백엔드로 사용)
(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  ;; ripgrep을 우선 사용 (속도 ↑)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-force-searcher  'rg)
  :config
  ;; xref 백엔드에 등록 → 기본 xref UI/키와 연동됨
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;; --- Imenu: only classes & functions ---------------------------------

;; consult-imenu에서 계층을 한 줄로(예: Class.method)
(with-eval-after-load 'consult
  (setq consult-imenu-namespace 'concat))

;; 공통: 최신화/정확도 옵션
(setq imenu-auto-rescan t
	  imenu-use-markers t)
