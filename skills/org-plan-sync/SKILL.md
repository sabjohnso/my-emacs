---
description: Scan ISSUES.org for PLANNED entries linked to PLAN.org via :PLAN-REF:, and flip each to RESOLVED if its linked PLAN.org node is now DONE. Flags (without auto-changing) a linked node that's CANCELED or a link that no longer resolves. Use right after marking a PLAN.org item DONE, or whenever ISSUES.org's states might be stale relative to PLAN.org.
allowed-tools: Read, Edit, Bash(grep:*), Bash(test:*)
---

# Sync ISSUES.org states from PLAN.org

Goal: keep `ISSUES.org`'s state honest relative to `PLAN.org` without
requiring the user to remember to flip an issue by hand every time its
linked plan work finishes. This skill only ever writes to `ISSUES.org` —
it never modifies `PLAN.org`.

## Preconditions

Both `PLAN.org` and `ISSUES.org` must exist. If either is missing, there is
nothing to sync — report that and stop rather than creating anything (this
skill is not `org-plan-init`).

## Steps

1. Find every `ISSUES.org` heading currently in `PLANNED` state that has a
   `:PLAN-REF:` property.
2. For each, extract the target `CUSTOM_ID` from the link
   (`[[file:PLAN.org::#<custom-id>][...]]`) and look it up in `PLAN.org`:
   `grep -B2 '^:CUSTOM_ID: <custom-id>$' PLAN.org` (anchored to line start
   so a mention of the string in prose or a comment doesn't false-positive)
   to find the enclosing heading and read its TODO state.
3. Apply exactly one of:
   - **Linked node is `DONE`** → flip the issue's state to `RESOLVED`. This
     is the only automatic transition this skill makes.
   - **Linked node is `CANCELED`** → do *not* change the issue's state.
     Canceling the chosen approach doesn't mean the underlying problem is
     gone — it may need a different fix, or a deliberate cancellation of
     its own. Report it as needing a human decision (re-plan a different
     approach, or manually move the issue to `CANCELED`).
   - **Linked node still `TODO`/`IN-PROGRESS`/`HELD`** → no change; the work
     isn't finished yet.
   - **`CUSTOM_ID` not found anywhere in `PLAN.org`** → do not guess or
     silently drop the link. Report it as a dangling `:PLAN-REF:` needing
     manual repair (the target may have been renamed, or archived without
     rewriting the link — see `org-plan-archive`).
4. Report a summary: how many issues flipped to `RESOLVED`, and a list of
   every anomaly found (canceled-but-still-planned, dangling link), each
   with the issue's `CUSTOM_ID` so the user can act on it directly.

## What this skill does not do

It does not archive anything — a `RESOLVED` issue stays in `ISSUES.org`
until `org-plan-archive` moves it to `RESOLVED.org`. It does not touch
`TRIAGE` issues (that transition is `org-issue-plan`'s job) or issues
without a `:PLAN-REF:` at all.
