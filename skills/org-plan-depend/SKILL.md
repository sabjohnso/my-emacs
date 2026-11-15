---
description: Add or remove a dependency between two PLAN.org items. A :BLOCKED-BY: link records that an item must wait for a blocker, and the item goes to BLOCKED while a blocker is not DONE. Refuse a dependency that makes a cycle, that points at the item itself or at an ancestor of the item, or that points at a heading that does not exist. Use when one epic, feature, or task cannot start until another one is DONE, or when that condition stops.
argument-hint: "<add|remove> \"<item>\" blocked-by \"<blocker>\""
allowed-tools: Read, Edit, Bash(grep:*), Bash(test:*)
---

# Add or remove a dependency in PLAN.org

Goal: record that one `PLAN.org` item cannot start until another item is
`DONE`, and keep the state of the blocked item correct. The
`org-plan-sync` skill sets a `BLOCKED` item back to `TODO` when all its
blockers are `DONE`. This skill only adds and removes the links.

## Language

Write the report in Simplified Technical English (ASD-STE100). Load the
`communication:simplified-technical-english` skill before you write it.

## Terms

- The **item** is the heading that must wait.
- A **blocker** is a heading that the item waits for.
- A blocker is **open** when its state is not `DONE` and not `CANCELED`.
- An item is **blocked** when one or more of its blockers is open.

## Data format

The item has a `:BLOCKED-BY:` property in its `:PROPERTIES:` drawer.
The value is one or more org links on one line, with one space between
the links. Each link goes to the `CUSTOM_ID` of a blocker:

```
*** BLOCKED Apply the config
:PROPERTIES:
:CUSTOM_ID: task-apply-the-config
:BLOCKED-BY: [[file:PLAN.org::#task-parse-the-config][Parse the config]] [[file:PLAN.org::#task-load-the-schema][Load the schema]]
:END:
```

The link label is the title of the blocker, with no TODO keyword and no
tags. A link to a blocker in `COMPLETED.org` names `file:COMPLETED.org`.

## Preconditions

1. `PLAN.org` must exist. If it does not exist, report that and stop.
   This skill is not `org-plan-init`.
2. The `#+TODO:` line of `PLAN.org` must contain `BLOCKED(b)`. If it does
   not, report that the file has an old header, and recommend the
   `org-plan-migrate` skill. Then stop.

## Find the headings

The caller can name a heading by its `CUSTOM_ID` or by its title. Look
for the name as a `CUSTOM_ID` first. If no heading has that
`CUSTOM_ID`, look for it as a title. `COMPLETED.org` is optional. Use
`test -f COMPLETED.org` first, and if the file does not exist, search
only `PLAN.org`.

1. Find a `CUSTOM_ID` with `grep -n '^:CUSTOM_ID: <id>$' PLAN.org
   COMPLETED.org`. The `^` and `$` anchors prevent a match on text in a
   description or a comment.
2. Find a title with `grep -nE '^\*{1,3} [A-Z-]+ <title>( +:[^ ]+:)?$'
   PLAN.org COMPLETED.org`. The end of the pattern permits only tags
   after the title, so a longer title does not match. If more than one heading has the title, do not
   guess. Report the matches with their `CUSTOM_ID` values and stop.
3. The item must be in `PLAN.org`. A blocker can be in `PLAN.org` or in
   `COMPLETED.org`.

If a heading does not exist, report the name and stop. Do not make a link
to a heading that does not exist. The remove action does not need the
blocker heading. It can remove a link to a heading that does not exist.

## Add a dependency

Steps 1 to 6 are checks. Do them in this order. Stop at the first check
that fails. Do not change the file. Report which check failed and why.
If all checks pass, do steps 7 and 8.

1. **Item state.** The item must not be `DONE` or `CANCELED`. Finished
   work cannot wait.
2. **Blocker state.** The blocker must not be `CANCELED`. Canceled work
   never becomes `DONE`, so the item would wait forever.
3. **Self.** The blocker must not be the item.
4. **Ancestor.** The blocker must not be an ancestor of the item (its
   feature or its epic). A parent becomes `DONE` only after all its
   children are finished. Thus a child that waits for its own parent
   waits forever. A blocker that is a descendant of the item is
   permitted.
5. **Duplicate.** If the `:BLOCKED-BY:` value of the item already has a
   link to the blocker, do not add a second link. Report that the
   dependency exists. Do not do steps 6 and 7. Go to step 8 (Set the
   state), because the state can be stale.
6. **Cycle.** A heading waits for each of its `:BLOCKED-BY:` blockers.
   A parent also waits for each of its children, because it becomes
   `DONE` only after them. Start at the blocker. Follow both kinds of
   wait from each heading that you find: its `:BLOCKED-BY:` links and
   its children. Search through `PLAN.org` and `COMPLETED.org`. Read each file one time, and keep a list of the
   `CUSTOM_ID` values that you visited. Do not follow a heading two
   times. The walk stops when you get to the item, or when no heading
   remains to visit. If you get to the item, the new link makes a
   cycle. Report the path of the cycle as a list of `CUSTOM_ID` values.
7. **Write the link.** Add the link at the end of the `:BLOCKED-BY:`
   value. If the item has no `:BLOCKED-BY:` property, add the property
   as the last line of the drawer, before `:END:`. Do not change other
   properties.
8. **Set the state.** Apply exactly one of these rules:
   - If the blocker is open and the item is `TODO`, set the item to
     `BLOCKED`.
   - If the blocker is open and the item is `BLOCKED`, do not change the
     item.
   - If the blocker is open and the item is `IN-PROGRESS` or `HELD`, do
     not change the item. Report that a person must decide if the work
     stops. Somebody started that work or stopped it on purpose.
   - If the blocker is `DONE`, do not change the state. Report that the
     item is not blocked by this blocker. If the item is `BLOCKED` and
     all its blockers are `DONE`, also report that the state is stale,
     and recommend the `org-plan-sync` skill.

   Change only the keyword in the headline. Keep the title, the tags,
   and the drawer.

## Remove a dependency

1. Find the link to the blocker in the `:BLOCKED-BY:` value of the item.
   Match the `CUSTOM_ID` in the link (`#<id>`). If there is no such link,
   report that and stop.
2. Remove the link. If no link remains, remove the `:BLOCKED-BY:` line.
3. If the item is not `BLOCKED`, do not change its state. If the item
   is `BLOCKED`, read the state of each remaining blocker, and apply
   exactly one of these rules:
   - If all remaining blockers are `DONE`, or no link remains, set the
     item to `TODO`.
   - If one or more remaining blockers are `CANCELED`, do not change the
     item. Report that a person must decide.
   - If one or more remaining blockers are open, do not change the item.

## Report

Report the item and the blocker (title and `CUSTOM_ID`), the action, the
new state of the item, and the check that failed, if one failed. If the
state did not change, tell why.

## What this skill does not do

- It does not set a `BLOCKED` item back to `TODO` after a blocker
  becomes `DONE`. The `org-plan-sync` skill does that. This skill sets
  an item to `TODO` only when a remove action leaves no open blocker.
- It does not add a dependency in `ISSUES.org`. An issue waits through
  the `:CLOSES:` link of its plan node.
- It does not change the order of headings, the depth of a heading, or
  any heading other than the item.
