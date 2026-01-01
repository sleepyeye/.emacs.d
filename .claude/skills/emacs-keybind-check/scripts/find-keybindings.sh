#!/bin/bash
# find-keybindings.sh - Emacs 설정에서 키바인딩을 검색하는 스크립트
# 사용법: ./find-keybindings.sh [디렉토리] [옵션]
#
# 옵션:
#   --key <키>      특정 키 시퀀스 검색 (예: --key "C-c p")
#   --cmd <명령>    특정 명령어 검색 (예: --cmd "projectile")
#   --conflicts     중복 키바인딩 탐지
#   --output <파일> 결과를 파일에 저장 (--conflicts와 함께 사용시 상세 정보 포함)
#   --json          JSON 형식으로 출력

set -e

# 기본값
SEARCH_DIR="${1:-.}"
MODE="all"
SEARCH_KEY=""
SEARCH_CMD=""
OUTPUT_FILE=""
JSON_OUTPUT=false

# 인자 파싱
shift 2>/dev/null || true
while [[ $# -gt 0 ]]; do
    case $1 in
        --key)
            MODE="key"
            SEARCH_KEY="$2"
            shift 2
            ;;
        --cmd)
            MODE="cmd"
            SEARCH_CMD="$2"
            shift 2
            ;;
        --conflicts)
            MODE="conflicts"
            shift
            ;;
        --output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        *)
            shift
            ;;
    esac
done

# 키바인딩 패턴 (주요 함수들)
PATTERNS='(global-set-key|define-key|local-set-key|bind-key|evil-define-key|general-define-key)'

# :bind 패턴 (use-package)
BIND_PATTERN=':bind\s*\(?'

# 검색 함수
find_all_keybindings() {
    echo "=== Emacs 키바인딩 검색 결과 ==="
    echo "검색 경로: $SEARCH_DIR"
    echo ""

    echo "### 함수형 키바인딩 (global-set-key, define-key 등)"
    rg --no-heading -n --color=never \
        -e "$PATTERNS" \
        --type-add 'elisp:*.el' --type elisp \
        "$SEARCH_DIR" 2>/dev/null || echo "(결과 없음)"

    echo ""
    echo "### use-package :bind 키바인딩"
    rg --no-heading -n --color=never \
        -e "$BIND_PATTERN" \
        --type-add 'elisp:*.el' --type elisp \
        "$SEARCH_DIR" 2>/dev/null || echo "(결과 없음)"
}

find_by_key() {
    echo "=== 키 시퀀스 검색: $SEARCH_KEY ==="
    # 키 시퀀스를 regex-safe하게 이스케이프
    ESCAPED_KEY=$(echo "$SEARCH_KEY" | sed 's/[[\.*^$()+?{|]/\\&/g')

    rg --no-heading -n --color=never \
        -e "\"$ESCAPED_KEY\"" \
        -e "'$ESCAPED_KEY" \
        -e "(kbd \"$ESCAPED_KEY" \
        --type-add 'elisp:*.el' --type elisp \
        "$SEARCH_DIR" 2>/dev/null || echo "(결과 없음)"
}

find_by_cmd() {
    echo "=== 명령어 검색: $SEARCH_CMD ==="

    rg --no-heading -n --color=never \
        -e "(global-set-key|define-key|local-set-key|bind-key).*$SEARCH_CMD" \
        -e ":bind.*$SEARCH_CMD" \
        -e "\".*\"[[:space:]]*\.[[:space:]]*.*$SEARCH_CMD" \
        --type-add 'elisp:*.el' --type elisp \
        "$SEARCH_DIR" 2>/dev/null || echo "(결과 없음)"
}

find_conflicts() {
    echo "=== 중복 키바인딩 탐지 ==="
    echo ""

    # 중복 키 목록 추출
    DUPLICATE_KEYS=$(rg -o --no-filename --color=never \
        '\(kbd "([^"]+)"\)' -r '$1' \
        --type-add 'elisp:*.el' --type elisp \
        "$SEARCH_DIR" 2>/dev/null | sort | uniq -c | sort -rn | \
        awk '$1 > 1 {print $2}')

    if [[ -z "$DUPLICATE_KEYS" ]]; then
        echo "(중복된 키바인딩 없음)"
        return
    fi

    # 화면 출력: 간단한 요약
    echo "$DUPLICATE_KEYS" | head -20 | while read -r key; do
        COUNT=$(rg -c --no-filename --color=never \
            "\\(kbd \"$key\"\\)" \
            --type-add 'elisp:*.el' --type elisp \
            "$SEARCH_DIR" 2>/dev/null | awk '{s+=$1} END {print s}')
        echo "[${COUNT}회] $key"
    done

    echo ""
    echo "(상위 20개만 표시, 2회 이상 등장하는 키만)"

    # 파일 출력: 상세 정보 포함
    if [[ -n "$OUTPUT_FILE" ]]; then
        {
            echo "# Emacs 키바인딩 충돌 보고서"
            echo "# 생성일시: $(date '+%Y-%m-%d %H:%M:%S')"
            echo "# 검색 경로: $SEARCH_DIR"
            echo ""

            for key in $DUPLICATE_KEYS; do
                echo "## 키: $key"
                echo ""
                # 해당 키가 정의된 모든 위치 출력
                rg --no-heading -n --color=never \
                    "\\(kbd \"$key\"\\)" \
                    --type-add 'elisp:*.el' --type elisp \
                    "$SEARCH_DIR" 2>/dev/null | while read -r line; do
                    echo "- $line"
                done
                echo ""
            done
        } > "$OUTPUT_FILE"

        echo ""
        echo "상세 보고서 저장됨: $OUTPUT_FILE"
    fi
}

output_json() {
    echo "["
    rg --no-heading -n --color=never --json \
        -e "$PATTERNS" \
        --type-add 'elisp:*.el' --type elisp \
        "$SEARCH_DIR" 2>/dev/null | \
        jq -s '[.[] | select(.type == "match") | {file: .data.path.text, line: .data.line_number, text: .data.lines.text}]' 2>/dev/null || echo "[]"
}

# 메인 실행
case $MODE in
    all)
        if $JSON_OUTPUT; then
            output_json
        else
            find_all_keybindings
        fi
        ;;
    key)
        find_by_key
        ;;
    cmd)
        find_by_cmd
        ;;
    conflicts)
        find_conflicts
        ;;
esac
