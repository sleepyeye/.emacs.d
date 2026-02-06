# Agentic Coding Guidelines for .emacs.d

This repository contains a personal Emacs configuration managed with the
[Elpaca](https://github.com/progfolio/elpaca) package manager.

## 1. Build, Lint, and Test

There is no formal build pipeline. "Building" means reloading the
configuration or restarting Emacs.

### Common verification commands
- **Reload current file**: `M-x eval-buffer`
- **Reload entire config**: restart Emacs (or `M-x load-file` on `init.el`)
- **Byte compile a file (interactive)**: `M-x byte-compile-file`
- **Byte compile a file (CLI)**:
  ```sh
  emacs --batch -l path/to/file.el -f batch-byte-compile
  ```
- **Lint docstrings**: `M-x checkdoc`
- **Flymake diagnostics**: `M-x flymake-mode`

### Tests (ERT)
- There is no automated test suite in this repo.
- If you add ERT tests, run a single test via:
  ```sh
  emacs --batch -l ert -l path/to/tests.el \
    --eval "(ert-run-tests-batch-and-exit 'test-name)"
  ```
- Run all tests in a file via:
  ```sh
  emacs --batch -l ert -l path/to/tests.el \
    --eval "(ert-run-tests-batch-and-exit t)"
  ```

### Environment
- **Package Manager**: Elpaca (bootstrapped in `bootstraps.el`).
- **Platform constants**: `IS-MAC`, `IS-LINUX`, `IS-WINDOWS`, `IS-BSD` in
  `early-init.el`. Use them for OS-specific logic.

## 2. Project Layout and Load Order

- `early-init.el`: GC tuning, UI, platform constants, bootstrap setup.
- `bootstraps.el`: Elpaca install + `elpaca-use-package-mode`.
- `init.el`: loads all modules via `load`.
- Module files live at repo root (examples: `python.el`, `ui.el`, `edit.el`).
- Keep `init.el` as a loader; put real logic in modules.

## 3. Package Configuration

- Use `use-package` for all third-party packages.
- `elpaca-use-package-mode` is enabled, so `:ensure t` is optional.
- Use `:elpaca` only for custom recipes or non-standard sources.
- Local packages live in `local-packages/`; keep changes minimal and documented.
- Prefer `:hook`, `:init`, and `:config` sections instead of ad-hoc
  `add-hook` or `setq` outside `use-package` blocks.
- Prefer `with-eval-after-load` for optional integration glue.

## 4. Code Style (Emacs Lisp)

### File headers
- Always enable lexical binding:
  ```elisp
  ;;; filename.el --- Description -*- lexical-binding: t; -*-
  ```
- End files with `;;; filename.el ends here`.

### Formatting
- Indentation: 2 spaces, no tabs.
- Keep lines ~80-100 chars when reasonable.
- Prefer multi-line `setq` for related variables.
- Align nested `setq` or `setf` blocks for readability.

### Docstrings and comments
- Every defun/defcustom needs a docstring.
- Use `;;;` for file headers, `;;` for section headers and comments.
- Avoid inline comments unless logic is non-obvious.

### Namespacing
- Public functions/variables must use `sleepy/` prefix.
- Private helpers must use `sleepy--` prefix.
- Examples:
  - Good: `sleepy/python-run-file-async`, `sleepy/default-frame-width`
  - Bad: `run-python`, `default-frame-width`

### Variables and types
- Prefer `defconst` for true constants.
- Prefer `defcustom` for user-facing config with a `:type`.
- Use `defvar` for mutable globals with clear docstrings.
- Use `setq-local` for buffer-local settings.

### Imports and loading
- Use `load` to pull in local modules (as done in `init.el`).
- Use `require` for built-in libraries when needed by `use-package` config.
- Avoid `require` for optional packages; prefer `use-package`.

### Error handling
- Use `user-error` for expected user-facing errors.
- Use `condition-case` or `ignore-errors` for external calls.
- Guard external tooling with `executable-find`.

### Hooks and advice
- Prefer `:hook` in `use-package` over global `add-hook`.
- When using `add-hook`, keep hook functions named and prefixed.
- Use `with-eval-after-load` for optional integrations.
- Use `advice-add` sparingly and document intent in a comment.

### Data and state
- Prefer `setq` for simple bindings; `setf` for `alist-get`/`plist-get` edits.
- Keep globals minimal; use buffer-local state where possible.
- Use `defgroup` + `defcustom` for user-facing settings.
- Keep interactive commands `sleepy/` namespaced.

## 5. Keybindings

- Use `general.el` and `sleepy/leader-def` for leader key mappings.
- Avoid `define-key` or `global-set-key` for user-facing bindings when
  a leader binding is appropriate.
- Keep new keybindings grouped by feature area (see `general.el`).

## 6. Platform Awareness

- Gate OS-specific logic with `IS-MAC`, `IS-LINUX`, `IS-WINDOWS`.
- Keep macOS-only settings in `macos.el` when possible.
- Respect `user-emacs-directory` when forming paths.

## 7. Files and Paths

- Respect `user-emacs-directory` when forming file paths.
- Use `expand-file-name` for user paths and project files.
- When reading external files, guard with `file-readable-p`.
- Prefer `locate-dominating-file` or `project-root` for project discovery.
- Keep temporary buffers named and reusable (e.g., `*Python Run*`).

## 8. Suggested Workflow for Agents

1. Read `init.el` to choose the right module.
2. Confirm OS-specific logic with constants in `early-init.el`.
3. Implement changes in the relevant module file.
4. Byte-compile the modified file.
5. Check `*Messages*` for warnings on reload.

## 9. External Agent Rules

- No `.cursor/rules/` or `.cursorrules` found.
- No `.github/copilot-instructions.md` found.
