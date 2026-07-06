---
description: Accept a proposed solution for a TRIAGE issue by adding the corresponding node(s) to PLAN.org, cross-linking it to the ISSUES.org entry, and flipping that issue from TRIAGE to PLANNED. Use when a specific approach to fixing a logged issue has been chosen and should now be tracked as planned work.
argument-hint: "\"<issue CUSTOM_ID or description>\" -> <epic|feature|task> \"<title>\" [under \"<parent title>\"]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Accept an issue's fix into PLAN.org

Goal: this is the one place `ISSUES.org`'s `TRIAGE -> PLANNED` transition
happens — an accepted approach gets recorded in `PLAN.org`, and the two
files are cross-linked so later tooling (`org-plan-sync`,
`org-plan-archive`, `org-plan-reviewer`) can follow the relationship without
re-deriving it.

## Preconditions

Both `PLAN.org` and `ISSUES.org` must already exist (run `org-plan-init`
first if not), and the named issue must currently be in `TRIAGE` state — if
it's already `PLANNED`/`RESOLVED`/`CANCELED`, stop and report that instead
of creating a duplicate link.

## Steps

1. Locate the issue in `ISSUES.org` by `CUSTOM_ID` or matching heading text,
   and confirm it's `TRIAGE`.
2. Add the PLAN.org node(s) for the accepted approach, using the exact same
   depth rules, `CUSTOM_ID` slugging, and collision-checking as the
   `org-plan-add` skill (reuse that logic rather than a different scheme —
   the two files must stay consistent).
3. Cross-link the two nodes:
   - On the new (or targeted existing) `PLAN.org` node, add
     `:ISSUE-REF: [[file:ISSUES.org::#<issue-custom-id>][<issue heading>]]`
     inside its `:PROPERTIES:` drawer.
   - On the `ISSUES.org` entry, add
     `:PLAN-REF: [[file:PLAN.org::#<plan-custom-id>][<plan heading>]]`
     inside its `:PROPERTIES:` drawer.
   Both links are relative `file:` links with a `CUSTOM_ID` target, so they
   resolve regardless of the user's home directory and stay clickable in
   Emacs.
4. Change the issue's heading state from `TRIAGE` to `PLANNED`. Do not
   change anything else about the issue heading (keep its existing
   `CUSTOM_ID`, body text, and any other properties).
5. Report: the issue transitioned, the PLAN.org node(s) created, and both
   link properties added.

## What this skill does not do

It does not decide which proposed solution is correct — that decision is
the caller's (typically the user, after discussion). This skill only
records the decision once made. It also never touches `RESOLVED.org` or
`COMPLETED.org` — see `org-plan-sync` and `org-plan-archive` for the later
transitions.
