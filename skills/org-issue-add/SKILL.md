---
description: Log a new known problem as a numbered TRIAGE issue in ISSUES.org, with a required Description heading and an optional Proposed Solutions subtree. Use when a problem is identified and should be recorded for later triage, not fixed immediately.
argument-hint: "\"<problem description>\" [proposed solutions...]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Log a new issue in ISSUES.org

Goal: record a known problem in `ISSUES.org` so it can be triaged later,
without jumping straight to planning a fix.

## Preconditions

If `ISSUES.org` doesn't exist yet at the project root, run the
`org-plan-init` skill first — do not hand-roll a header here.

## Issue schema

Each issue is a depth-1 `TRIAGE` heading whose title is the issue number
followed by a descriptive title, plus a fixed subtree:

- A required `** Description` heading holding the problem text.
- An optional `** Proposed Solutions` heading whose `*** Solution <k> Title`
  children hold the ideas, numbered from 1 per issue. Omit the whole
  `Proposed Solutions` heading if the caller has no fix ideas yet — never
  write it empty.

Depth stops at 3, and depth 3 exists only under `Proposed Solutions`. Do
not nest anything under a `Solution` heading beyond its body text.

## Number assignment

Issue numbers are unique across `ISSUES.org` **and** `RESOLVED.org` and are
never reused. Allocate the next one statelessly as `max(existing) + 1`
(first issue is `1`):

```
grep -ohE '^\*+ (TRIAGE|PLANNED|RESOLVED|CANCELED) [0-9]+' ISSUES.org RESOLVED.org
```

Take the largest trailing number across both files; if neither file has any
issue, start at `1`. The `:CUSTOM_ID:` is `issue-<n>` — the number is the
sole identity, so no slug is needed and links survive retitling. (Because
allocation reads both files, archiving an issue can never free a number for
reuse.)

## New heading format

```
* TRIAGE 748 Theme cycle skips my-nord under heavy GC pressure
:PROPERTIES:
:CUSTOM_ID: issue-748
:END:
** Description
Cycling themes rapidly while a large GC pause is in flight sometimes lands
on my-tron instead of the expected next theme in my-theme-cycle.
** Proposed Solutions
*** Solution 1 Debounce my-toggle-theme
Ignore a cycle request issued while a pause is in flight.
*** Solution 2 Snapshot the cycle index
Read the cycle index once at entry instead of live after the pause.
```

No solution is marked `(Accepted)` at this stage — that suffix is applied
only when a fix is accepted into `PLAN.org` (see the `org-issue-plan`
skill).

## Steps

1. Confirm `ISSUES.org` exists (see Preconditions).
2. Allocate the next issue number (see above) and set `:CUSTOM_ID: issue-<n>`.
3. Append the new issue, in `TRIAGE` state, as the last entry in the file —
   with its `** Description` heading and, if any solutions were given, its
   `** Proposed Solutions` subtree. Preserve all existing entries and their
   order.
4. Report the issue number, heading, and `CUSTOM_ID` added.
