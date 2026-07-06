---
description: Add an epic, feature, or task heading to PLAN.org at the correct depth, creating missing parent epic/feature headings as needed, assigning a unique CUSTOM_ID, and applying a theme tag if given. Use whenever new planned work — an epic, a feature under one, or a task under a feature — should be recorded in PLAN.org.
argument-hint: "<epic|feature|task> \"<title>\" [under \"<parent title>\"] [:theme:]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Add a heading to PLAN.org

Goal: record a new epic, feature, or task in `PLAN.org` at the right depth,
following the file's fixed conventions, without disturbing existing
headings.

## Preconditions

If `PLAN.org` doesn't exist yet at the project root, run the
`org-plan-init` skill first — do not hand-roll a header here.

## Depth rules

`PLAN.org` is a strict depth-3 tree: depth 1 = epic, depth 2 = feature,
depth 3 = task. No labels like "Epic:"/"Feature:"/"Task:" are needed —
depth alone conveys the level.

- Adding an **epic**: append a new depth-1 heading at the end of the file.
- Adding a **feature**: requires a parent epic. If the named epic doesn't
  exist yet, create it first (depth 1), then add the feature as its last
  depth-2 child.
- Adding a **task**: requires a parent feature (and transitively its epic).
  Create any missing ancestor the same way, then add the task as the last
  depth-3 child under that feature.
- Refuse to create a heading deeper than depth 3 (e.g. a "sub-task" under a
  task) — flag that this tree is depth-3 by design and suggest folding the
  extra detail into the task's body text or splitting it into a sibling
  task instead.

## CUSTOM_ID assignment

Every heading gets a `:CUSTOM_ID:` property in a `:PROPERTIES:` drawer,
slugged from the heading text (lowercase, hyphenated, punctuation stripped)
and prefixed by level: `epic-<slug>`, `feature-<slug>`, `task-<slug>`.

1. Before assigning, check for collisions across the whole file (and,
   defensively, `COMPLETED.org` too, since an epic could have the same slug
   as one already archived): `grep -o '^:CUSTOM_ID: .*' PLAN.org
   COMPLETED.org` — anchored to line start so a mention of the string in
   prose or a comment doesn't false-positive.
2. If the slug collides, disambiguate with a numeric suffix (`-2`, `-3`, ...).

## New heading format

```
*** TODO Audit use-package :defer usage                              :performance:
:PROPERTIES:
:CUSTOM_ID: task-audit-use-package-defer-usage
:END:
```

New headings always start in the `TODO` state (or `TODO`-equivalent depth-1
"open" state) — do not backdate a heading to `DONE`/`IN-PROGRESS` on
creation. A theme tag is optional; apply it only if the caller specified
one, as an ordinary org tag at the end of the headline.

## Steps

1. Confirm `PLAN.org` exists (see Preconditions).
2. Determine the target depth and locate (or create) the required
   ancestor(s), per the depth rules above.
3. Slug and de-duplicate the `CUSTOM_ID` (see above).
4. Insert the new heading as the last child at its level, preserving all
   existing content and ordering.
5. Report the heading added, its full path (epic > feature > task), and its
   `CUSTOM_ID`.
