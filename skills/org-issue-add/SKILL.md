---
description: Log a problem in code, builds, or tests that exist as a numbered TRIAGE issue in ISSUES.org, with a Description heading and an optional Proposed Solutions subtree. Use when you find a defect in existing code and must record it for triage, not repair it now. Do not use it for a design question, a missing feature, or work that is not started. Use org-plan-add for those.
argument-hint: "\"<problem description>\" [proposed solutions...]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Log a new issue in ISSUES.org

Goal: record a problem in `ISSUES.org` for triage at a later time. Do not
plan a repair here.

## Language

Write the title, the description, and the solutions in Simplified
Technical English (ASD-STE100). Load the
`communication:simplified-technical-english` skill before you write them.

## What is an issue

An issue is a problem in code, a build, or tests that exist. Examples:

- A function gives an incorrect result.
- A test fails, or a test is not reliable.
- The build fails with one compiler or one Message Passing Interface (MPI)
  implementation.
- Code that exists does not obey a requirement.

These items are not issues:

- A design question about code that does not exist.
- A missing feature, or work that is not started.
- A requirement.
- A concern about a risk in a future design.

If the problem is not an issue, do not add it to `ISSUES.org`. Tell the
caller why. Put a design question, a missing feature, or future work in
`PLAN.org` with the `org-plan-add` skill. Put a requirement in
`REQUIREMENTS.org`.

If the project has no code yet, there can be no issues.

## Preconditions

If `ISSUES.org` does not exist at the project root, run the
`org-plan-init` skill first. Do not write a header here.

## Issue schema

Each issue is a depth-1 `TRIAGE` heading. The title is the issue number
and then a descriptive title. Each issue has these subheadings:

- A `** Description` heading. It is necessary. It tells where the
  problem is (a file, a function, a test, or a build step) and what the
  incorrect result is.
- A `** Proposed Solutions` heading. It is optional. Its
  `*** Solution <k> Title` children contain the ideas, with numbers from
  1 in each issue. If the caller has no ideas for a repair, do not write
  this heading. Do not write it empty.

The maximum depth is 3. Use depth 3 only under
`Proposed Solutions`. A `Solution` heading has only body text, with no
child headings.

## Number assignment

An issue number is unique across `ISSUES.org` and `RESOLVED.org`. Do not
use a number again. The next number is the largest number in the two
files plus 1. The first issue is `1`.

```
grep -ohE '^\*+ (TRIAGE|PLANNED|RESOLVED|CANCELED) [0-9]+' ISSUES.org RESOLVED.org
```

The `:CUSTOM_ID:` is `issue-<n>`. The number is the identity, so a slug
is not necessary, and links stay correct when the title changes. The
calculation reads the two files, so an archived issue never makes its
number available again.

## New heading format

```
* TRIAGE 748 Theme cycle skips my-nord during a long garbage collection pause
:PROPERTIES:
:CUSTOM_ID: issue-748
:END:
** Description
In my-theme-cycle, a fast series of theme changes during a long garbage
collection pause sometimes selects my-tron. The correct result is the
next theme in the cycle.
** Proposed Solutions
*** Solution 1 Ignore requests during a pause in my-toggle-theme
Ignore a request to change the theme while a pause continues.
*** Solution 2 Read the cycle index one time
Read the cycle index when the function starts, not after the pause.
```

At this step, no solution has the ` (Accepted)` suffix. The
`org-issue-plan` skill adds that suffix when it accepts a solution into
`PLAN.org`.

## Steps

1. Make sure that the problem is an issue (see What is an issue). If it
   is not, stop and tell the caller where it goes.
2. Make sure that `ISSUES.org` exists (see Preconditions).
3. Get the next issue number (see above). Set `:CUSTOM_ID: issue-<n>`.
4. Add the new issue in the `TRIAGE` state at the end of the file, with
   its `** Description` heading. If the caller gives solutions, add the
   `** Proposed Solutions` subtree. Keep all other issues and their
   order.
5. Report the issue number, the heading, and the `CUSTOM_ID`.
