# SuperClaude to Claude Code Documentation Guide

**Purpose**: Systematic process to prepare comprehensive documentation for Claude Code web instances using SuperClaude's advanced capabilities.

**Target Audience**: Repository owners with SuperClaude access who want to create documentation accessible to Claude Code web instances (without SuperClaude/Serena access).

---

## Overview

This guide helps you create a **hybrid documentation system** that works for:
- **SuperClaude users**: Access via Serena MCP memories (fast, persistent)
- **Claude Code web instances**: Access via markdown files (portable, version-controlled)

The system creates:
1. **Root-level overview files**: Simple, high-level guidance (CLAUDE.md, PLANNING.md, RULES.md, TASK.md, KNOWLEDGE.md)
2. **docs/ technical files**: Detailed architecture, modules, workflows, references

---

## Prerequisites

- SuperClaude framework installed (~/.claude/ directory)
- Serena MCP server configured and working
- Repository with existing codebase
- Git repository (for version control)

---

## Step-by-Step Process

### Phase 1: Discovery and Analysis

**Command**: `/sc:brainstorm`

**What to say**:
```
I want to document this project thoroughly so that Claude Code web instances can understand it better.
What command should I use?
```

**Expected outcome**: Claude will ask clarifying questions about:
- Documentation scope (architecture, modules, workflows)
- Target audience (developers, maintainers, Claude instances)
- Documentation location (docs/ folder, root files)
- Level of detail (overview vs technical deep-dive)

**Your responses should specify**:
1. You want documentation in `docs/` folder with root-level overview files
2. Root files should be simple overviews without technical details
3. `docs/` files should contain detailed technical content
4. Documentation should be accessible to both Serena users and web instances
5. Documentation managed by Claude instances, version-controlled in git

---

### Phase 2: Comprehensive Analysis and Memory Creation

**Command**: `/sc:index --serena --focus architecture --think-hard`

**What this does**:
- Uses Sequential MCP for systematic multi-step analysis
- Creates Serena memories for cross-session persistence
- Analyzes architecture, dependencies, patterns, conventions
- Generates quality assessment and technical debt analysis

**Expected Serena memories created**:
- `architecture_overview` - Complete system architecture
- `module_reference` - All modules/components documented
- `quick_reference` - Common patterns and workflows

**Time**: 2-5 minutes depending on codebase size

**Output**: You'll see Sequential thinking process (10-20 thoughts) followed by memory creation confirmations.

---

### Phase 3: Documentation Structure Creation

**Command**: `/sc:document --comprehensive`

**What to specify** (if Claude asks):
```
Create the following documentation structure:

Root files (simple overviews):
- CLAUDE.md: Update with documentation references (if exists, create if not)
- PLANNING.md: System architecture and design decisions overview
- RULES.md: Coding standards and conventions overview
- TASK.md: Current tasks and priorities
- KNOWLEDGE.md: Central documentation index

Technical documentation in docs/:
- architecture_overview.md: Export from Serena memory, complete system details
- module_reference.md: All modules/components documented
- quick_reference.md: Common patterns, keybindings, workflows
- (any existing workflow docs should be moved here)

Requirements:
- Root files are overviews only, point to docs/ for details
- docs/ files contain full technical content
- Export Serena memories to corresponding markdown files
- Maintain consistency between Serena memories and markdown files
```

**Expected outcome**:
- 5 root-level files created/updated
- 3+ files in docs/ directory
- All Serena memories exported to markdown
- KNOWLEDGE.md acts as navigation index

---

### Phase 4: Directory Cleanup (if needed)

**Check for multiple documentation directories**:
```bash
ls -la | grep -E "^d.*doc"
```

**If you find both `doc/` and `docs/`**:

**Command**: Tell Claude:
```
Can you merge doc and docs? Pick one of them and fix corresponding files.
```

**Recommendation**: Choose `docs/` (plural) as it's more conventional.

**Expected outcome**:
- All content moved to single directory
- Empty directory removed
- KNOWLEDGE.md updated to reflect all files
- No broken references

---

### Phase 5: Git Commit and Push

**Command**: `/sc:git Commit and push`

**What this does**:
- Checks git status
- Stages all documentation files
- Creates descriptive commit message
- Commits changes
- Pushes to remote repository

**Expected commit message format**:
```
Add comprehensive documentation structure for Claude Code instances

Create hybrid documentation system with root-level overviews and detailed technical docs:

Root files (simple overviews):
- CLAUDE.md: [description]
- PLANNING.md: [description]
- RULES.md: [description]
- TASK.md: [description]
- KNOWLEDGE.md: [description]

Technical documentation (docs/):
- architecture_overview.md: [description]
- module_reference.md: [description]
- quick_reference.md: [description]
- [other files]: [descriptions]

Changes:
- [specific changes made]
- [Serena memory exports]
- [directory consolidations]
```

---

## Final Documentation Structure

After completion, your repository should have:

```
your-repo/
├── CLAUDE.md           # Guide for Claude Code instances (updated/created)
├── PLANNING.md         # Architecture overview (created)
├── RULES.md            # Coding standards (created)
├── TASK.md             # Current tasks (created)
├── KNOWLEDGE.md        # Documentation index (created)
├── docs/
│   ├── architecture_overview.md  # Complete system architecture
│   ├── module_reference.md       # All modules documented
│   ├── quick_reference.md        # Common patterns/workflows
│   └── [workflow-specific].md    # Language/tool workflows
└── [your source code]
```

---

## Verification Checklist

After completing the process, verify:

- [ ] All 5 root files exist (CLAUDE.md, PLANNING.md, RULES.md, TASK.md, KNOWLEDGE.md)
- [ ] Root files are concise overviews (not full technical content)
- [ ] docs/ directory exists with technical documentation
- [ ] Serena memories exist: `architecture_overview`, `module_reference`, `quick_reference`
- [ ] Markdown files in docs/ match Serena memory content
- [ ] KNOWLEDGE.md indexes all documentation files
- [ ] Only one documentation directory exists (docs/, not doc/)
- [ ] No broken references to old directory names
- [ ] All changes committed to git
- [ ] Changes pushed to remote repository

---

## Customization Options

### For Different Project Types

**Backend API Projects**:
- Add `docs/api-reference.md` for endpoint documentation
- Add `docs/database-schema.md` for data model documentation
- Focus on service architecture, data flow, API design

**Frontend Projects**:
- Add `docs/component-library.md` for UI components
- Add `docs/state-management.md` for state patterns
- Focus on component hierarchy, styling system, routing

**Library/Package Projects**:
- Add `docs/public-api.md` for exported functions/classes
- Add `docs/usage-examples.md` for integration examples
- Focus on API design, usage patterns, migration guides

**Infrastructure/DevOps Projects**:
- Add `docs/deployment.md` for deployment procedures
- Add `docs/monitoring.md` for observability setup
- Focus on architecture, configuration, operational procedures

### Analysis Depth Options

Replace `--think-hard` with:
- `--think` for standard analysis (~4K tokens, faster)
- `--think-hard` for deep analysis (~10K tokens, recommended)
- `--ultrathink` for maximum depth (~32K tokens, complex projects)

### Focus Areas

Add `--focus` flags to `/sc:index`:
- `--focus architecture` for system design emphasis
- `--focus security` for security-focused documentation
- `--focus performance` for performance patterns
- `--focus quality` for code quality standards

---

## Troubleshooting

### Issue: Serena memories not created

**Solution**: Ensure Serena MCP is configured correctly:
```bash
# Check Serena is available
mcp list | grep serena

# If not, add to ~/.claude/mcp.json
```

### Issue: Documentation too verbose or too brief

**Solution**: Provide explicit guidance:
```
Make root files concise (1-3 pages each)
Make docs/ files comprehensive (5-15 pages each)
```

### Issue: Existing CLAUDE.md conflicts

**Solution**: Tell Claude:
```
Preserve existing CLAUDE.md content, only add documentation structure references
```

### Issue: Too many scattered documentation files

**Solution**:
```
Consolidate all documentation into docs/ directory
Update KNOWLEDGE.md to index everything
```

---

## Advanced: Multi-Repository Strategy

For organizations with many repositories:

1. **Create template documentation structure** in one repository
2. **Use this guide** to generate full documentation
3. **Extract common patterns** to ~/.claude/PROJECT_DOCS_TEMPLATE.md
4. **Replicate across repositories** with customization per project

**Template sections**:
- Root file structure (consistent across all projects)
- Common documentation categories
- Naming conventions
- Git commit message format
- Quality standards

---

## Time Estimates

Based on project complexity:

- **Small projects** (<5K LOC, <10 files): 10-15 minutes
- **Medium projects** (5K-20K LOC, 10-50 files): 20-30 minutes
- **Large projects** (20K-100K LOC, 50-200 files): 30-60 minutes
- **Enterprise projects** (>100K LOC, >200 files): 1-2 hours

Time breakdown:
- Phase 1 (Brainstorm): 2-5 minutes
- Phase 2 (Analysis): 3-10 minutes (depends on --think level)
- Phase 3 (Documentation): 5-20 minutes
- Phase 4 (Cleanup): 1-3 minutes
- Phase 5 (Git): 1-2 minutes

---

## Benefits

**For SuperClaude users**:
- Fast access via Serena memories
- Cross-session persistence
- Pattern learning and reuse

**For Claude Code web instances**:
- Complete markdown documentation
- No Serena dependency
- Version-controlled alongside code

**For both**:
- Consistent documentation structure
- Clear navigation via KNOWLEDGE.md
- Root-level guidance + technical deep-dives
- Managed by Claude, reviewed by humans

---

## Example Session Transcript

```
User: I want to document this project thoroughly so that Claude Code web
      instances can understand it better.

Claude: [Activates brainstorming mode, asks clarifying questions]

User: [Answers: wants docs/ folder, root overviews, hybrid approach]

Claude: I recommend using /sc:index with Serena to analyze and create memories,
        then exporting to markdown files.

User: /sc:index --serena --focus architecture --think-hard

Claude: [Sequential thinking process, creates 3 Serena memories]

User: Now create the documentation structure with root files and docs/ folder

Claude: [Creates PLANNING.md, RULES.md, TASK.md, KNOWLEDGE.md, updates CLAUDE.md,
        exports Serena memories to docs/]

User: Can you merge doc and docs directories?

Claude: [Moves all files to docs/, removes doc/, updates KNOWLEDGE.md]

User: /sc:git Commit and push

Claude: [Stages files, commits with descriptive message, pushes to remote]

Done! Your repository now has comprehensive documentation for Claude Code instances.
```

---

## Questions?

If Claude asks questions during the process:
- Be specific about structure (root files vs docs/ files)
- Specify level of detail (overview vs technical)
- Clarify target audience (developers, maintainers, AI assistants)
- Request examples if unclear

---

## Next Steps After Documentation

1. **Test with Claude Code web**: Open project in claude.ai/code and verify documentation is accessible
2. **Iterate**: Update documentation as code evolves
3. **Share patterns**: Extract common patterns to ~/.claude/ for reuse
4. **Team adoption**: Share this guide with team members for consistent documentation

---

**Version**: 1.0
**Last Updated**: 2025-11-14
**Based on**: .emacs.d documentation creation process
**Tested with**: SuperClaude framework + Serena MCP + Sequential MCP
