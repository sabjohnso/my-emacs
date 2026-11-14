---
description: Convert a project's old-format planning files (flat unnumbered ISSUES.org with prose "Proposed solutions" lists, PLAN.org nodes carrying :ISSUE-REF:) to the current format — numbered issues with Description/Proposed Solutions subtrees, :CLOSES: links, a PLAN.org that contains only backlog items with descriptions, and requirements in REQUIREMENTS.org. Use once per project when its planning files predate the numbered-issue format, or when PLAN.org contains requirements.
argument-hint: "[project root, or none for current directory]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*), Bash(cp:*)
---

# Migrate planning files to the numbered-issue format

Goal: a one-shot, idempotent upgrade of a project's planning files from
the old conventions to the current ones, without losing content or breaking
a single cross-link. The old format had unnumbered flat issues with a prose
`Proposed solutions:` list, and PLAN.org nodes that used `:ISSUE-REF:` and
carried explanatory body prose. The new format is what `org-plan-init`
documents: numbered issues (`* TRIAGE <n> Title`, `:CUSTOM_ID: issue-<n>`)
with `** Description` / `** Proposed Solutions` subtrees, a `PLAN.org`
that contains only backlog items with short descriptions, requirements
in `REQUIREMENTS.org`, and a `:CLOSES:` link on the node that resolves an
issue.

## When to run

Run this once per project when its files predate the new format. Also run
it on a project that has numbered issues but has requirements in
`PLAN.org`, or has epics and features with no descriptions. It is not part
of the normal `add`/`plan`/`sync`/`archive` cycle.

## Language

Write all new text (descriptions, headings, and the report) in Simplified
Technical English (ASD-STE100). Load the
`communication:simplified-technical-english` skill before you write it.
Do not rewrite text that exists only to change its language.

## Idempotence and safety

1. **Do not number the issues two times.** Before changing anything, detect
   new-format markers: any numbered issue heading
   (`grep -qE '^\*+ (TRIAGE|PLANNED|RESOLVED|CANCELED) [0-9]+' ISSUES.org
   RESOLVED.org`) or any `:CLOSES:` property
   (`grep -q '^:CLOSES:' PLAN.org COMPLETED.org`). If either is present,
   the issues are already numbered. Skip steps 2, 3, 4, and 7, and do
   steps 5, 6, and 8 only. Steps 5 and 6 are safe to do again: after
   step 5, `PLAN.org` has no requirements to move, and step 6 changes
   nothing.
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
   - Do not remove body text automatically. A description is permitted.
     Flag each body that is not a description: a body of more than 5
     lines, or a body that contains requirements or design discussion.
     Flag each epic and each feature that has no description. Report the
     heading and the text, and let the user decide. Do not change the
     text until the user decides.

5. **Move requirements out of PLAN.org.** Find each top-level heading in
   `PLAN.org` that has no TODO keyword (for example, a `Requirements`
   heading). If `REQUIREMENTS.org` does not exist, run `org-plan-init`
   first. Move the heading and its subtree to `REQUIREMENTS.org`. Remove
   the top-level heading itself if it only groups the requirements, and
   promote its children by one level. Change each `CUSTOM_ID` prefix to
   `req-`. Change each link to these headings in `PLAN.org`,
   `ISSUES.org`, `RESOLVED.org`, and `COMPLETED.org` to the new file and
   `CUSTOM_ID`. Remove a `PLAN.org` header comment that tells about the
   requirements exception. Report each heading that you moved.

6. **Flag issues that are not issues.** An issue must be a problem in
   code, builds, or tests that exist (see `org-issue-add`). Flag each
   issue that is a design question, a missing feature, or work that was
   not started when the issue was written. Do not move or change these
   issues. Report them, and let the user decide.

7. **Rewrite the back-links.** Update every issue's `:PLAN-REF:` label and
   any moved `file:` component using the recorded id mapping, so
   `:PLAN-REF:`/`:CLOSES:` still reciprocate and resolve after renumbering.

8. **Report:**
   - the old-slug → `issue-<n>` mapping for every issue,
   - which issues got an `(Accepted)` solution and which need a human to
     mark one,
   - each PLAN.org/COMPLETED.org body that is not a description, and each
     epic or feature with no description,
   - each heading moved to `REQUIREMENTS.org`,
   - each issue that is not a problem in code that exists,
   - any redundant `:ISSUE-REF:` dropped,
   - the backup files written.

## After migrating

Run the `org-plan-reviewer` agent. It checks each new-format rule. It
reports each item that the migration flagged for a human (an accepted
solution with no mark, a body that is not a description, an epic or a
feature with no description) as a finding.
