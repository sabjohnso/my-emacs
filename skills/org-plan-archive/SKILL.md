---
description: Periodic maintenance — sync ISSUES.org states from PLAN.org, then move every RESOLVED issue into RESOLVED.org and every fully-DONE epic from PLAN.org into COMPLETED.org, rewriting any cross-file links that moved. Use when the user asks to archive completed planning work, or on a periodic cleanup pass over PLAN.org/ISSUES.org.
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Archive resolved issues and completed epics

Goal: the periodic cleanup pass CLAUDE.md describes — move finished work out
of the active `PLAN.org`/`ISSUES.org` files and into their append-only
archives, `COMPLETED.org` and `RESOLVED.org`, without breaking any
`:PLAN-REF:`/`:ISSUE-REF:` link in the process.

## Preconditions

All four files must exist — run `org-plan-init` first if any are missing.

## Steps

1. **Sync first.** Run the same logic as the `org-plan-sync` skill before
   doing anything else, so no issue that's actually done gets left behind
   in `PLANNED` state. Fold its anomaly report (dangling links, canceled-
   but-still-planned issues) into the final report from this skill.

2. **Identify archivable issues.** Every `ISSUES.org` heading now in
   `RESOLVED` state is archivable — there's no further condition to check
   (unlike epics, an issue has no descendants).

3. **Identify archivable epics.** A depth-1 `PLAN.org` heading is archivable
   only if:
   - its own state is `DONE`, **and**
   - every depth-2/depth-3 descendant under it is `DONE` or `CANCELED`
     (mixed terminal states are fine; anything still `TODO`/`IN-PROGRESS`/
     `HELD` blocks the whole epic from archiving), **and**
   - no `CUSTOM_ID` anywhere in that epic's subtree is the target of a
     `:PLAN-REF:` from an ISSUES.org heading still in `TRIAGE` or `PLANNED`
     state (check this *after* step 1's sync, using the same `grep`-based
     lookup as `org-plan-sync`).
   An epic that fails any of these is **not** archived this pass — record
   it in the final report with the specific reason (which descendant is
   still open, or which still-active issue references it) rather than
   silently skipping it.

4. **Move the archivable issues.** Cut each qualifying `ISSUES.org` heading
   (including its `:PROPERTIES:` drawer and body text) and append it, in
   order, to the end of `RESOLVED.org`.

5. **Move the archivable epics.** Cut each qualifying `PLAN.org` depth-1
   subtree (the epic and all its features/tasks, properties, and body text
   intact) and append it, in order, to the end of `COMPLETED.org`.

6. **Rewrite moved cross-links.** For every `:PLAN-REF:`/`:ISSUE-REF:` link
   whose *target* just moved files in steps 4–5 (i.e. both the issue and
   the epic it references were archived in this same pass), rewrite the
   link's `file:` component to point at the new file (`ISSUES.org` →
   `RESOLVED.org`, `PLAN.org` → `COMPLETED.org`) so the link still resolves.
   A link whose target did *not* move in this pass needs no change — its
   `CUSTOM_ID` is unaffected regardless of which file currently holds it.

7. **Report** exactly what moved (issue → `RESOLVED.org`, epic →
   `COMPLETED.org`), what links were rewritten, what was skipped and why,
   and the sync anomalies from step 1.

## What this skill does not do

It never invents a state transition beyond what `org-plan-sync` already
performs — an issue must already be `RESOLVED` and an epic already fully
`DONE` before this skill will move them. It does not delete anything; a
move is always a cut-and-append, preserving the full subtree.
