# KNOWLEDGE - Documentation Index

**Purpose**: Central index for all project documentation
**Audience**: Claude Code instances for quick navigation
**Details**: Full technical documentation in docs/ directory

## Quick Start

**New to this config?** Start here:
1. Read CLAUDE.md - Guide for Claude Code instances
2. Review PLANNING.md - System architecture overview
3. Check docs/quick_reference.md - Common workflows
4. Explore docs/architecture_overview.md - Deep dive

## Documentation Structure

```
~/.emacs.d/
├── CLAUDE.md           → Guide for Claude Code (how to work with this config)
├── PLANNING.md         → System architecture and design decisions
├── RULES.md            → Coding standards and conventions
├── TASK.md             → Current tasks and priorities
├── KNOWLEDGE.md        → This file - documentation index
└── docs/
    ├── architecture_overview.md  → Complete system architecture (12KB)
    ├── module_reference.md       → All 27 modules documented
    ├── quick_reference.md        → Common patterns and workflows
    ├── cheatsheet.md            → General keybinding cheatsheet
    ├── keybindings.md           → Detailed keybinding reference
    ├── latex-workflow.md        → LaTeX editing workflow
    └── python-workflow.md       → Python development workflow
```

## Document Summaries

### Root Files (Overview - No Technical Details)

**CLAUDE.md**
- How Claude Code should work with this configuration
- File naming conventions and project structure
- Common development tasks
- Special integrations (Claude Code IDE, Consult, Evil)

**PLANNING.md**
- 5-layer architecture overview
- Design principles and absolute rules
- Technology stack with rationale
- Future plans and technical debt

**RULES.md**
- Naming conventions (functions, variables, files)
- Code quality standards
- Integration patterns
- Module creation checklist

**TASK.md**
- Active and pending tasks
- Completed work log
- Blocking issues
- Priority categorization

### docs/ Files (Detailed Technical Content)

**architecture_overview.md** (12KB)
- Project identity and statistics
- Architecture philosophy and principles
- 5-layer module organization
- Bootstrap sequence (3 stages)
- Dependency graph (hard and soft)
- Package management (Elpaca patterns)
- Keybinding architecture (leader keys, Evil)
- Completion stack (Vertico/Consult/Corfu)
- LSP architecture (Eglot configuration)
- Workspace management (Perspective + Projectile)
- Register system innovation
- Performance optimization (multi-stage)
- Platform abstraction
- AI integration (Claude Code IDE)
- Architectural patterns and gaps
- Quality assessment (92/100)

**module_reference.md**
- Complete reference for all 27 modules
- Per-module: purpose, dependencies, key functions, integration points
- Module loading patterns
- Cross-module integration details
- Evil state configurations
- Leader key bindings per module

**quick_reference.md**
- Common keybindings (leader, Evil, global)
- Common workflows (projects, search, git, registers, LSP)
- Code patterns (adding packages, functions, keybindings)
- Module creation checklist
- Troubleshooting guide
- External tool dependencies

**cheatsheet.md**
- General keybinding cheatsheet

**keybindings.md**
- Detailed keybinding reference

**latex-workflow.md**
- LaTeX editing workflow with AUCTeX
- LaTeX-specific keybindings and commands

**python-workflow.md**
- Python development workflow
- Python-specific keybindings and LSP commands

## Quick Navigation

**Want to...**

Add a new package? → RULES.md (patterns) + docs/quick_reference.md (examples)
Understand architecture? → PLANNING.md (overview) + docs/architecture_overview.md (details)
Find keybindings? → docs/quick_reference.md
Understand a module? → docs/module_reference.md
See current work? → TASK.md
Learn conventions? → RULES.md

## For Claude Code Instances

**First time in this project?**
1. Read CLAUDE.md for context
2. Review PLANNING.md for architecture
3. Check TASK.md for active work

**Starting new work?**
1. Update TASK.md with new tasks
2. Follow RULES.md conventions
3. Reference docs/ for technical details
4. Update docs if architecture changes

**Need technical details?**
- System design → docs/architecture_overview.md
- Module info → docs/module_reference.md
- Common patterns → docs/quick_reference.md

## Serena Memory Integration

SuperClaude users with Serena MCP have additional memories:
- `architecture_overview` - Same as docs/architecture_overview.md
- `module_reference` - Detailed module documentation
- `quick_reference` - Common patterns and workflows

Load with: `/sc:load`

## Documentation Maintenance

**Managed by**: Claude instances
**Update frequency**: As needed when architecture changes
**Human editable**: Yes, but Claude should manage technical content
**Version control**: All files tracked in git

**When to update**:
- New modules added → Update module_reference.md + PLANNING.md
- Architecture changes → Update architecture_overview.md + PLANNING.md
- New tasks → Update TASK.md
- New conventions → Update RULES.md
- New workflows → Update quick_reference.md

## External References

**SuperClaude Framework**: ~/.claude/ (global Claude Code instructions)
**Original CLAUDE.md**: Already exists, will be updated to reference this structure

---

This documentation structure ensures Claude Code web instances can quickly understand and work with this configuration without SuperClaude access.
