---
description: Add an epic, feature, or task heading with its description to PLAN.org at the correct depth. Create missing parent epic and feature headings, assign a unique CUSTOM_ID, and apply a theme tag if one is given. Use when new planned work (an epic, a feature under an epic, or a task under a feature) must go in PLAN.org. Also use it for design work and for work that is not started, because these are not issues.
argument-hint: "<epic|feature|task> \"<title>\" [under \"<parent title>\"] [:theme:] [\"<description>\"]"
allowed-tools: Read, Edit, Write, Bash(grep:*), Bash(test:*)
---

# Add a heading to PLAN.org

Goal: put a new epic, feature, or task in `PLAN.org` at the correct depth.
Obey the conventions of the file. Do not change the headings that
exist.

## Language

Write the title and the description in Simplified Technical English
(ASD-STE100). Load the `communication:simplified-technical-english` skill
before you write them.

## Only backlog items

`PLAN.org` is the backlog. It contains only epics, features, and tasks.
Do not put these items in it:

- Requirements. They go in `REQUIREMENTS.org`. A description can link to
  a requirement.
- Problems in code that exists. They go in `ISSUES.org` (see the
  `org-issue-add` skill).
- Design discussion, a comparison of alternatives, or status notes.

Do not add a heading that has no TODO keyword.

## Descriptions

Each epic and each feature has a description. A task has a description
only when its title is not sufficient.

A description is plain text after the `:PROPERTIES:` drawer. It does
two things:

1. It states what the item delivers.
2. It states when the item is complete.

A description has 5 lines or fewer. If you need more lines, the text
contains something that is not a description. Put a requirement in
`REQUIREMENTS.org`. Link to it from the description. Divide a large item into child items.

If the caller does not give a description for an epic or a feature,
write one from the conversation. If the conversation does not tell you
what the item delivers, ask the caller.

## Preconditions

If `PLAN.org` does not exist at the project root, run the `org-plan-init`
skill first. Do not write a header here.

## Depth rules

`PLAN.org` is a tree with a maximum depth of 3: depth 1 is an epic,
depth 2 is a feature, and depth 3 is a task. Do not use labels such as
"Epic:", "Feature:", or "Task:". The depth shows the level.

- **Epic**: add a new depth-1 heading at the end of the file.
- **Feature**: the feature must have a parent epic. If the epic does not
  exist, create it first (with a description). Then add the feature as
  the last depth-2 child of the epic.
- **Task**: the task must have a parent feature and a parent epic. Create
  each missing parent in the same way. Then add the task as the last
  depth-3 child of the feature.
- Do not create a heading at depth 4 or more. Tell the caller that the
  tree has a maximum depth of 3. Recommend a sibling task, or a task
  description, for the extra detail.

## CUSTOM_ID assignment

Each heading has a `:CUSTOM_ID:` property in a `:PROPERTIES:` drawer.
Make a slug from the heading text. Use lowercase. Use hyphens between
words. Remove punctuation. Add a prefix for the level: `epic-<slug>`,
`feature-<slug>`, or `task-<slug>`.

1. Look for the same value in `PLAN.org` and `COMPLETED.org`, because an
   archived epic can have the same slug:
   `grep -o '^:CUSTOM_ID: .*' PLAN.org COMPLETED.org`.
   The `^` anchor prevents a match on the text in a description or a
   comment.
2. If the slug exists, add a number suffix (`-2`, `-3`, and so on).

## New heading format

```
** TODO Load themes on demand                                     :performance:
:PROPERTIES:
:CUSTOM_ID: feature-load-themes-on-demand
:END:
Emacs loads a theme file only when the user selects that theme. The
feature is complete when startup loads no theme file other than the
default theme.
```

A new heading always starts in the `TODO` state. Do not give a new
heading the `DONE` or `IN-PROGRESS` state. Add a theme tag only if the
caller gives one. Put it as an org tag at the end of the headline.

This skill does not add a `:CLOSES:` link to an issue. The
`org-issue-plan` skill adds that link when it accepts a solution to an
issue. Use this skill for backlog items that are not related to an issue.

## Steps

1. Make sure that `PLAN.org` exists (see Preconditions).
2. Make sure that the item is a backlog item (see Only backlog items).
3. Find the depth, and find or create each parent (see Depth rules).
4. Make the `CUSTOM_ID` and make sure that it is unique (see above).
5. Write the description (see Descriptions).
6. Add the new heading as the last child at its level. Keep all other
   text and the order of the headings.
7. Report the heading, its path (epic > feature > task), and its
   `CUSTOM_ID`.
