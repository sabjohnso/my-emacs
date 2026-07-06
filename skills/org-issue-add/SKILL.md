---
description: Log a new known problem as a TRIAGE heading in ISSUES.org, with an optional numbered list of proposed solutions as body text. Use when a problem is identified and should be recorded for later triage, not fixed immediately.
argument-hint: "\"<problem description>\" [proposed solutions...]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Log a new issue in ISSUES.org

Goal: record a known problem in `ISSUES.org` so it can be triaged later,
without jumping straight to planning a fix.

## Preconditions

If `ISSUES.org` doesn't exist yet at the project root, run the
`org-plan-init` skill first — do not hand-roll a header here.

## Flat-list rule

`ISSUES.org` is depth 1 only. Proposed solutions are plain text (a
numbered list) under the heading, never sub-headings — do not create a
child TODO for each proposed solution.

## CUSTOM_ID assignment

Slug the issue description (lowercase, hyphenated, punctuation stripped),
prefixed `issue-`. Check for collisions first, across both `ISSUES.org` and
`RESOLVED.org` (an issue could reuse wording from one already archived):
`grep -o '^:CUSTOM_ID: .*' ISSUES.org RESOLVED.org` — anchored to line start
so a mention of the string in prose or a comment doesn't false-positive.
Disambiguate with a
numeric suffix on collision.

## New heading format

```
* TRIAGE Theme cycle skips my-nord under heavy GC pressure
:PROPERTIES:
:CUSTOM_ID: issue-theme-cycle-skips-nord-under-gc
:END:

Problem: cycling themes rapidly while a large GC pause is in flight
sometimes lands on my-tron instead of the expected next theme in
my-theme-cycle.

Proposed solutions:
1. Debounce my-toggle-theme so a cycle can't be triggered mid-GC.
2. Snapshot the cycle index before the pause instead of reading it live.
```

The "Proposed solutions" list is optional — omit it entirely if the caller
has no fix ideas yet, rather than writing an empty list.

## Steps

1. Confirm `ISSUES.org` exists (see Preconditions).
2. Slug and de-duplicate the `CUSTOM_ID` (see above).
3. Append the new heading, in `TRIAGE` state, as the last entry in the file
   — preserve all existing entries and their order.
4. Report the heading added and its `CUSTOM_ID`.
