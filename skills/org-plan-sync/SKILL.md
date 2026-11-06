---
description: Set each open PLAN.org feature or epic whose children are all DONE or CANCELED to DONE, then scan PLAN.org (and COMPLETED.org) for DONE nodes carrying a :CLOSES: link, and flip each linked ISSUES.org issue to RESOLVED. Flags (without auto-changing) a closing node that's CANCELED or a :CLOSES:/:PLAN-REF: link that no longer resolves. Use right after marking a PLAN.org item DONE, or whenever ISSUES.org's states might be stale relative to PLAN.org.
allowed-tools: Read, Edit, Bash(grep:*), Bash(test:*)
---

# Sync ISSUES.org states from PLAN.org

Goal: keep `ISSUES.org`'s state honest relative to `PLAN.org` without
requiring the user to remember to flip an issue by hand every time its
linked plan work finishes. Also keep each parent heading in `PLAN.org`
consistent with its children. This skill writes to `ISSUES.org`, and to
`PLAN.org` only for the parent completion in step 0. It never modifies
`COMPLETED.org`.

## Preconditions

Both `PLAN.org` and `ISSUES.org` must exist. If either is missing, there is
nothing to sync — report that and stop rather than creating anything (this
skill is not `org-plan-init`).

## Steps

The `:CLOSES:` link on the closing PLAN.org node is authoritative — this
skill is driven from the plan side, not the issue side.

0. **Complete the parents in `PLAN.org`.** A parent is a feature (depth 2)
   or an epic (depth 1) that has one or more child headings. A child is
   terminal when its state is `DONE` or `CANCELED`. Examine the features
   first, then the epics, so that a feature that this step completes
   counts as a `DONE` child of its epic. For each parent whose children
   are all terminal, apply exactly one of:
   - **One or more children are `DONE`** and the parent is `TODO` or
     `IN-PROGRESS` → set the parent to `DONE`. Change only the keyword in
     the headline. Keep the title, the tags, and the `:PROPERTIES:` drawer.
   - **All children are `CANCELED`** → do not change the parent. Report it
     as needing a human decision (set the parent to `CANCELED`, or add new
     work under it).
   - **The parent is `HELD`** → do not change the parent. A person set
     `HELD` on purpose. Report it as needing a human decision.
   - **The parent is already `DONE` or `CANCELED`** → no change.
   Do this step before step 1, because a parent can carry a `:CLOSES:` link.
   A parent that this step sets to `DONE` then closes its issue in step 3.
1. Find every node in `PLAN.org` (and `COMPLETED.org`, for a fix whose epic
   was archived before its issue) that has a `:CLOSES:` property, and read
   the TODO state of its enclosing heading:
   `grep -B<n> '^:CLOSES:' PLAN.org COMPLETED.org` (or read the files) to
   pair each `:CLOSES:` with the heading above it.
2. For each, extract the target issue from the link
   (`[[file:ISSUES.org::#issue-<n>][...]]`) and locate that issue in
   `ISSUES.org`: `grep -B2 '^:CUSTOM_ID: issue-<n>$' ISSUES.org` (anchored
   to line start so a mention of the string in prose or a comment doesn't
   false-positive) to find the enclosing heading and read its state.
3. Apply exactly one of, keyed on the *closing node's* state:
   - **Closing node is `DONE`** and the issue is `PLANNED` → flip the issue
     to `RESOLVED`. This is the only automatic transition this skill makes.
   - **Closing node is `CANCELED`** → do *not* change the issue's state.
     Canceling the chosen approach doesn't mean the underlying problem is
     gone — it may need a different fix, or a deliberate cancellation of
     its own. Report it as needing a human decision (re-plan a different
     approach, or manually move the issue to `CANCELED`).
   - **Closing node still `TODO`/`IN-PROGRESS`/`HELD`** → no change; the
     work isn't finished yet.
   - **Closing node is `DONE` but the issue is not `PLANNED`** (already
     `RESOLVED`/`CANCELED`, or still `TRIAGE`) → no state change; report
     `TRIAGE`-with-a-done-closer as an anomaly (a `:CLOSES:` should only
     ever target a `PLANNED` issue).
   - **`:CLOSES:` target `issue-<n>` not found in `ISSUES.org`** (nor, if
     the issue may have been archived, in `RESOLVED.org`) → do not guess or
     silently drop the link. Report it as a dangling `:CLOSES:` needing
     manual repair.
4. Also flag any reciprocity break for a human: a `PLANNED` issue with a
   `:PLAN-REF:` but no PLAN.org node `:CLOSES:`-ing it back, or a
   `:PLAN-REF:` whose `CUSTOM_ID` no longer resolves in `PLAN.org`.
5. Report a summary: each parent that step 0 set to `DONE` (with its
   `CUSTOM_ID`), how many issues flipped to `RESOLVED`, and a list of
   every anomaly found (a parent whose children are all `CANCELED`, a
   `HELD` parent whose children are all terminal, canceled-but-still-planned,
   dangling link, reciprocity break), each with the `CUSTOM_ID` or the issue
   number so the user can act on it directly.

## What this skill does not do

It does not archive anything — a `RESOLVED` issue stays in `ISSUES.org`
until `org-plan-archive` moves it to `RESOLVED.org`. It does not touch
`TRIAGE` issues (that transition is `org-issue-plan`'s job) or issues that
no PLAN.org node `:CLOSES:` at all. In `PLAN.org` it changes only the
keyword of a parent heading in step 0. It never sets a parent to `DONE`
while one or more of its children are still `TODO`, `IN-PROGRESS`, or
`HELD`.
