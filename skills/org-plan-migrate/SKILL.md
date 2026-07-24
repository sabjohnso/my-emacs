---
description: Convert a project's old-format planning files (flat unnumbered ISSUES.org with prose "Proposed solutions" lists, PLAN.org nodes carrying :ISSUE-REF: and body prose) to the current format — numbered issues with Description/Proposed Solutions subtrees, :CLOSES: links, and backlog-only PLAN.org. Use once per project when its planning files predate the numbered-issue format.
argument-hint: "[project root, or none for current directory]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*), Bash(cp:*)
---

# Migrate planning files to the numbered-issue format

Goal: a one-shot, idempotent upgrade of a project's four planning files from
the old conventions to the current ones, without losing content or breaking
a single cross-link. The old format had unnumbered flat issues with a prose
`Proposed solutions:` list, and PLAN.org nodes that used `:ISSUE-REF:` and
carried explanatory body prose. The new format is what `org-plan-init`
documents: numbered issues (`* TRIAGE <n> Title`, `:CUSTOM_ID: issue-<n>`)
with `** Description` / `** Proposed Solutions` subtrees, a backlog-only
PLAN.org, and a `:CLOSES:` link on the node that resolves an issue.

## When to run

Run this once, per project, when its files predate the new format. It is
not part of the normal `add`/`plan`/`sync`/`archive` cycle.

## Idempotence and safety

1. **No-op if already migrated.** Before changing anything, detect new-format
   markers: any numbered issue heading
   (`grep -qE '^\*+ (TRIAGE|PLANNED|RESOLVED|CANCELED) [0-9]+' ISSUES.org
   RESOLVED.org`) or any `:CLOSES:` property
   (`grep -q '^:CLOSES:' PLAN.org COMPLETED.org`). If either is present, the
   files are already (at least partly) migrated — stop and report rather
   than risk double-converting. A second run is thus a no-op.
2. **Back up first.** Copy each file this skill will touch to
   `<name>.org.pre-migrate` before editing (`cp -n`, so an existing backup
   from a prior aborted run is not clobbered). Mention the backups in the
   report.
3. **Never delete content silently.** Body prose that can't be placed
   automatically (see PLAN.org step) is reported, not dropped.

## Number assignment during migration

Assign numbers to existing issues in file order — process `ISSUES.org` top
to bottom, then `RESOLVED.org` top to bottom, handing out `1, 2, 3, …` so
numbers are unique across both files and stable (an earlier-listed issue
gets a lower number). Record the old-`CUSTOM_ID` → new-`issue-<n>` mapping;
every link rewrite below keys off it.

## Steps

1. **Detect and guard** (idempotence check above); **back up** the files.

2. **Renumber and reshape each issue** in `ISSUES.org`, then `RESOLVED.org`:
   - Insert the assigned number after the TODO keyword:
     `* TRIAGE Title` → `* TRIAGE <n> Title`.
   - Replace `:CUSTOM_ID: issue-<old-slug>` with `:CUSTOM_ID: issue-<n>`,
     recording the mapping.
   - Move the problem prose under a new `** Description` heading.
   - If the old body has a `Proposed solutions:` numbered list, convert it
     to a `** Proposed Solutions` subtree: each list item becomes a
     `*** Solution <k> <title>` heading (first line/clause → title, any
     remaining prose → the solution's body), numbered from 1. If there was
     no such list, omit `** Proposed Solutions` entirely.

3. **Mark the accepted solution.** For each `PLANNED` issue, determine which
   solution was chosen from its old `:PLAN-REF:` (the PLAN.org node it
   pointed at, whose old `:ISSUE-REF:` names this issue back). If the chosen
   solution can be matched to one `*** Solution` heading, suffix it
   ` (Accepted)`. If it can't be matched unambiguously (no proposed-
   solutions list, or the plan node doesn't correspond to a listed
   solution), leave none accepted and record the issue in the report for a
   human to mark.

4. **Convert PLAN.org / COMPLETED.org nodes:**
   - Rename each `:ISSUE-REF: [[file:ISSUES.org::#issue-<old-slug>][...]]`
     to `:CLOSES:` with the same file, retargeted to the new
     `#issue-<n>` and relabeled `<n> <issue title>`. (Where the old format
     put `:ISSUE-REF:` on several nodes of one fix, keep `:CLOSES:` on only
     the single closing node — the one the issue's `:PLAN-REF:` pointed at —
     and drop the redundant references on the others, noting this in the
     report.)
   - Do **not** auto-strip body prose. Backlog-only PLAN.org forbids it, but
     the prose may be worth folding into an issue `Description` — so **flag
     every PLAN.org/COMPLETED.org node that still has a body**, with its
     heading and the text, and let the user decide. Leave the prose in place
     until they do.

5. **Rewrite the back-links.** Update every issue's `:PLAN-REF:` label and
   any moved `file:` component using the recorded id mapping, so
   `:PLAN-REF:`/`:CLOSES:` still reciprocate and resolve after renumbering.

6. **Report:**
   - the old-slug → `issue-<n>` mapping for every issue,
   - which issues got an `(Accepted)` solution and which need a human to
     mark one,
   - every PLAN.org/COMPLETED.org node still carrying body prose (the manual
     backlog-only cleanup left to do),
   - any redundant `:ISSUE-REF:` dropped,
   - the backup files written.

## After migrating

Run the `org-plan-reviewer` agent — it now checks every new-format rule and
will surface anything the migration flagged for a human (unmarked accepted
solution, leftover PLAN.org prose) as a concrete finding.
