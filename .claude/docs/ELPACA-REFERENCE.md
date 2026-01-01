# Elpaca 레퍼런스 (elisp-reviewer 참조용)

> 원본: https://github.com/progfolio/elpaca/blob/master/doc/manual.md

## 핵심 개념: 비동기 처리

Elpaca는 init 파일을 읽은 **후에** 비동기적으로 패키지 큐를 처리한다.

```elisp
(elpaca package-a (message "First"))   ;; 큐에 추가
(message "Second")                      ;; 즉시 실행 (First보다 먼저!)
(elpaca package-b (message "Third"))   ;; 큐에 추가
(elpaca-process-queues)                 ;; First, Third 순서로 처리
```

**결과**: "Second" -> "First" -> "Third"

---

## use-package 통합

### 활성화

```elisp
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
```

### `:ensure` 키워드

| 값 | 의미 |
|----|------|
| `t` 또는 생략 | Elpaca가 패키지 설치 (기본값, `use-package-always-ensure t` 설정시) |
| `nil` | Elpaca 우회 - 빌트인 패키지용 |
| `(:wait t)` | 동기 로딩 - 설치 완료까지 블로킹 |
| `(:host github :repo "user/repo")` | 커스텀 recipe |
| `(:build (:not compile))` | 빌드 옵션 커스터마이징 |

### 예시

```elisp
;; 일반 패키지 (elpaca가 설치)
(use-package magit
  :commands magit-status)

;; 빌트인 패키지 (elpaca 우회)
(use-package dired
  :ensure nil
  :config ...)

;; 동기 로딩 필요시 (init에서 직접 사용하는 패키지)
(use-package general
  :ensure (:wait t)
  :demand t
  :config ...)

;; 커스텀 recipe
(use-package some-package
  :ensure (:host github :repo "user/repo" :branch "develop"))
```

---

## Hook: elpaca-after-init-hook

### 왜 필요한가?

- `after-init-hook`: init 파일 읽은 직후 실행 (패키지 로딩 **전**)
- `elpaca-after-init-hook`: 모든 패키지 활성화 **후** 실행

### 언제 사용하는가?

1. 패키지에 의존하는 초기화 코드
2. custom.el 로딩
3. 전역 모드 활성화

### 예시

```elisp
;; custom.el 로딩
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook
  (lambda () (load custom-file 'noerror)))

;; use-package에서 사용
(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode))

(use-package global-corfu-mode
  :hook (elpaca-after-init . global-corfu-mode))
```

---

## Recipe 키워드

| 키워드 | 설명 | 예시 |
|--------|------|------|
| `:host` | 호스팅 서비스 | `github`, `gitlab`, `codeberg` |
| `:repo` | 저장소 | `"user/repo"` |
| `:branch` | 브랜치 | `"develop"` |
| `:tag` | 릴리즈 태그 | `"v1.0.0"` |
| `:ref` | 커밋 해시 | `"abc1234"` |
| `:pin` | 업데이트 방지 | `t` |
| `:depth` | clone depth | `1`, `nil` (full) |
| `:files` | 링크할 파일 | `(:defaults "extensions/*")` |
| `:build` | 빌드 명령 | `(:not compile)`, `(:not elpaca--compile-info)` |
| `:wait` | 동기 로딩 | `t` |

---

## 흔한 실수와 해결

### 1. after-init-hook 사용

```elisp
;; 잘못됨 - 패키지 로딩 전에 실행될 수 있음
:hook (after-init . some-mode)

;; 올바름
:hook (elpaca-after-init . some-mode)
```

### 2. 패키지 간 의존성

```elisp
;; 잘못됨 - general이 아직 로딩 안됐을 수 있음
(use-package general)
(use-package evil
  :general ...)  ;; 에러!

;; 올바름 - :wait로 동기 로딩
(use-package general
  :ensure (:wait t)
  :demand t)
(use-package evil
  :general ...)  ;; OK
```

### 3. 빌트인 패키지

```elisp
;; 잘못됨 - elpaca가 설치 시도
(use-package recentf
  :config ...)

;; 올바름
(use-package recentf
  :ensure nil
  :config ...)
```

### 4. 즉시 실행 코드

```elisp
;; 잘못됨 - 패키지 로딩 전에 실행
(use-package some-package)
(some-package-function)  ;; 에러!

;; 올바름 - :config 안에서 실행
(use-package some-package
  :config
  (some-package-function))
```

---

## Obsolete 변수

| 구 변수 | 신 변수 | 변경일 |
|---------|---------|--------|
| `elpaca-use-package-by-default` | `use-package-always-ensure` | 2024-02-08 |

---

## 유용한 명령어

| 명령어 | 설명 |
|--------|------|
| `M-x elpaca-info` | 패키지 정보 조회 |
| `M-x elpaca-try` | 패키지 임시 테스트 |
| `M-x elpaca-update` | 패키지 업데이트 |
| `M-x elpaca-update-all` | 전체 업데이트 |
| `M-x elpaca-log` | 빌드 로그 확인 |
| `M-x elpaca-manager` | 패키지 매니저 UI |
