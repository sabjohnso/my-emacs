---
description: Accept a proposed solution for a TRIAGE issue by adding the corresponding node(s) to PLAN.org, marking the chosen solution (Accepted), cross-linking the closing PLAN.org node to the ISSUES.org entry, and flipping that issue from TRIAGE to PLANNED. Use when a specific approach to fixing a logged issue has been chosen and should now be tracked as planned work.
argument-hint: "\"<issue number, CUSTOM_ID, or title>\" [solution <k>] -> <epic|feature|task> \"<title>\" [under \"<parent title>\"]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Accept an issue's fix into PLAN.org

Goal: this is the one place `ISSUES.org`'s `TRIAGE -> PLANNED` transition
happens — an accepted approach gets recorded in `PLAN.org`, its `Solution`
heading is marked `(Accepted)`, and the two files are cross-linked (the
closing PLAN.org node `:CLOSES:` the issue; the issue `:PLAN-REF:`s back) so
later tooling (`org-plan-sync`, `org-plan-archive`, `org-plan-reviewer`) can
follow the relationship without re-deriving it.

## Preconditions

Both `PLAN.org` and `ISSUES.org` must already exist (run `org-plan-init`
first if not), and the named issue must currently be in `TRIAGE` state — if
it's already `PLANNED`/`RESOLVED`/`CANCELED`, stop and report that instead
of creating a duplicate link.

## Steps

1. Locate the issue in `ISSUES.org` by number (e.g. `748`), `CUSTOM_ID`
   (`issue-748`), or matching heading text, and confirm it's `TRIAGE`.
2. Identify which proposed solution is being accepted (by its `Solution <k>`
   number or title). Append ` (Accepted)` to exactly that `*** Solution`
   heading's title. If the issue has no `** Proposed Solutions` subtree,
   add one first with a single `*** Solution 1 <title>` heading describing
   the accepted approach, marked `(Accepted)`. Refuse if a solution is
   already marked `(Accepted)` — that issue has already been planned; stop
   and report rather than accepting a second.
3. Add the PLAN.org node(s) for the accepted approach, using the exact same
   depth rules, `CUSTOM_ID` slugging, and collision-checking as the
   `org-plan-add` skill (reuse that logic rather than a different scheme —
   the two files must stay consistent). PLAN.org nodes carry no body prose
   (see `org-plan-add`); the rationale stays in the issue's `Description`.
4. Cross-link the two nodes. Choose the single PLAN.org node that, when
   `DONE`, means the issue is resolved — the *closing* node (for a
   multi-node fix this is the last/umbrella node, not every node):
   - On that closing `PLAN.org` node, add
     `:CLOSES: [[file:ISSUES.org::#issue-<n>][<n> <issue title>]]`
     inside its `:PROPERTIES:` drawer. This is the authoritative
     "mark this issue RESOLVED when I am DONE" instruction (`org-plan-sync`
     reads it). At most one PLAN.org node may `:CLOSES:` a given issue.
   - On the `ISSUES.org` entry, add
     `:PLAN-REF: [[file:PLAN.org::#<plan-custom-id>][<plan heading>]]`
     inside its `:PROPERTIES:` drawer, pointing back at that same closing
     node.
   Both links are relative `file:` links with a `CUSTOM_ID` target, so they
   resolve regardless of the user's home directory and stay clickable in
   Emacs.
5. Change the issue's heading state from `TRIAGE` to `PLANNED`. Do not
   change anything else about the issue heading (keep its existing number,
   `CUSTOM_ID`, and other properties).
6. Report: the issue transitioned, the solution marked `(Accepted)`, the
   PLAN.org node(s) created, and the `:CLOSES:`/`:PLAN-REF:` link pair
   added.

## What this skill does not do

It does not decide which proposed solution is correct — that decision is
the caller's (typically the user, after discussion). This skill only
records the decision once made. It also never touches `RESOLVED.org` or
`COMPLETED.org` — see `org-plan-sync` and `org-plan-archive` for the later
transitions.
