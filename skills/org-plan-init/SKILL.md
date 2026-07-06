---
description: Create PLAN.org, ISSUES.org, RESOLVED.org, and COMPLETED.org at a project's root with the correct #+TODO headers and state-machine conventions, for any of the four that don't already exist. Use when the user asks to set up planning files in a project, or when another org-plan-*/org-issue-* skill finds one of these files missing.
argument-hint: "[project root, or none for current directory]"
allowed-tools: Read, Write, Edit, Bash(ls:*), Bash(test:*)
---

# Initialize the planning files

Goal: create the private, per-project planning files described in CLAUDE.md
— `PLAN.org` (a depth-3 epic/feature/task tree), `ISSUES.org` (a flat triage
log), and their two archives, `RESOLVED.org` and `COMPLETED.org` — with a
consistent header so the rest of the `org-plan-*`/`org-issue-*` skills and
the `org-plan-reviewer` agent can rely on the format.

## Scope

These four files live at the root of whatever project is open, one set per
project — not a single shared set across projects. Determine the project
root from the argument given, or default to the current working directory.

## Steps

1. For each of the four filenames, check with `test -f` whether it already
   exists at the project root. Only create the ones that are missing —
   never overwrite an existing file, even if its header looks wrong (leave
   fixing an existing file's header to a direct edit, not this skill).

2. Create `PLAN.org` (if missing) with:
   ```
   #+TITLE: Plan
   #+TODO: TODO(t) IN-PROGRESS(i) HELD(h) | DONE(d) CANCELED(c)

   # Depth-3 tree: depth 1 = epic, depth 2 = feature, depth 3 = task.
   # A theme is an ordinary org tag on any heading. Every heading has a
   # :CUSTOM_ID: property (epic-/feature-/task-<slug>) so ISSUES.org can
   # link to it via :PLAN-REF:. See the org-plan-add skill.
   ```

3. Create `ISSUES.org` (if missing) with:
   ```
   #+TITLE: Issues
   #+TODO: TRIAGE(t) PLANNED(p) | RESOLVED(r) CANCELED(c)

   # Flat list (depth 1 only). Proposed solutions are body text, not
   # sub-headings. Each heading has a :CUSTOM_ID: issue-<slug> property.
   # TRIAGE -> PLANNED when an accepted fix is added to PLAN.org (see the
   # org-issue-plan skill), PLANNED -> RESOLVED when that PLAN.org node is
   # DONE (see org-plan-sync), or -> CANCELED if it won't be fixed.
   ```

4. Create `RESOLVED.org` (if missing) with:
   ```
   #+TITLE: Resolved issues
   #+TODO: TRIAGE(t) PLANNED(p) | RESOLVED(r) CANCELED(c)

   # Archive for RESOLVED entries moved out of ISSUES.org. Append-only —
   # see the org-plan-archive skill.
   ```

5. Create `COMPLETED.org` (if missing) with:
   ```
   #+TITLE: Completed work
   #+TODO: TODO(t) IN-PROGRESS(i) HELD(h) | DONE(d) CANCELED(c)

   # Archive for DONE epics moved out of PLAN.org. Append-only — see the
   # org-plan-archive skill.
   ```

6. Check for a `.gitignore` at the project root with `test -f`. If one
   exists and doesn't already cover these filenames, add the four filenames
   to it (they're private working notes per CLAUDE.md, not checked-in
   project files). If no `.gitignore` exists, don't create one just for
   this — report that these files are currently untracked-but-not-ignored
   and let the user decide.

7. Report which files were created, which already existed and were left
   alone, and whether `.gitignore` was updated.
