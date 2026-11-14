---
description: Create PLAN.org, ISSUES.org, RESOLVED.org, COMPLETED.org, and REQUIREMENTS.org at the root of a project with the correct #+TODO headers and conventions, for each of the five files that do not exist. Use when the user asks to set up planning files in a project, or when another org-plan-*/org-issue-* skill finds that one of these files is missing.
argument-hint: "[project root, or none for current directory]"
allowed-tools: Read, Write, Edit, Bash(ls:*), Bash(test:*)
---

# Initialize the planning files

Goal: create the private planning files of a project with a consistent
header. The other `org-plan-*` and `org-issue-*` skills and the
`org-plan-reviewer` agent use this header.

| File               | Contents                                                     |
|--------------------|--------------------------------------------------------------|
| `PLAN.org`         | The backlog: epics, features, and tasks. Nothing else.        |
| `ISSUES.org`       | Numbered problems in code, builds, or tests that exist.       |
| `RESOLVED.org`     | The archive of resolved issues.                               |
| `COMPLETED.org`    | The archive of completed epics.                               |
| `REQUIREMENTS.org` | The requirements of the project, as prose. No TODO keywords. |

## Language

Write all prose in these files in Simplified Technical English
(ASD-STE100). This includes the header comments below, descriptions,
and requirements. Load the `communication:simplified-technical-english`
skill before you write prose.

## Scope

Each project has its own set of these five files at its root. Get the
project root from the argument. If there is no argument, use the current
working directory.

## Version control

Do not change `.gitignore`. The user ignores all `.org` files at the user
level. The user tells you when to track a planning file in a repository.

## Steps

1. For each of the five file names, use `test -f` to find out if the
   file exists at the project root. Create only the files that do not
   exist. Do not overwrite a file that exists, even if its header is
   wrong.

2. If `PLAN.org` does not exist, create it with this text:
   ```
   #+TITLE: Plan
   #+TODO: TODO(t) IN-PROGRESS(i) HELD(h) | DONE(d) CANCELED(c)

   # This file is the backlog. It contains only epics, features, and
   # tasks. Requirements go in REQUIREMENTS.org. Problems in code that
   # exists go in ISSUES.org. Design discussion does not go in this
   # file.
   #
   # Depth 1 is an epic, depth 2 is a feature, and depth 3 is a task.
   # Each heading has a TODO keyword, a title, optional tags, and a
   # :PROPERTIES: drawer with a :CUSTOM_ID: (epic-, feature-, or
   # task-<slug>). Each epic and each feature has a description after
   # its drawer. A task has a description only when its title is not
   # sufficient. A description states what the item delivers and when
   # the item is complete, in 5 lines or fewer. A description can link
   # to REQUIREMENTS.org. The node that closes an issue has a :CLOSES:
   # link to that issue. See the org-plan-add and org-issue-plan skills.
   ```

3. If `ISSUES.org` does not exist, create it with this text:
   ```
   #+TITLE: Issues
   #+TODO: TRIAGE(t) PLANNED(p) | RESOLVED(r) CANCELED(c)

   # This file contains problems in code, builds, or tests that exist.
   # A design question, a missing feature, or work that is not started
   # is not an issue. Put it in PLAN.org.
   #
   # Each issue is a depth-1 heading "* TRIAGE <n> Title" with
   # :CUSTOM_ID: issue-<n>. A number is unique across ISSUES.org and
   # RESOLVED.org. Do not use a number again. Each issue has a
   # "** Description" heading. An issue can have a "** Proposed
   # Solutions" heading with "*** Solution <k> Title" children. The
   # accepted solution has the suffix " (Accepted)". An issue goes from
   # TRIAGE to PLANNED when org-issue-plan accepts a solution. It goes
   # from PLANNED to RESOLVED when org-plan-sync finds its closing
   # PLAN.org node DONE. If nobody will repair it, it goes to CANCELED.
   ```

4. If `RESOLVED.org` does not exist, create it with this text:
   ```
   #+TITLE: Resolved issues
   #+TODO: TRIAGE(t) PLANNED(p) | RESOLVED(r) CANCELED(c)

   # This file is the archive of RESOLVED issues from ISSUES.org. Each
   # issue keeps its Description and Proposed Solutions. Numbers stay
   # unique across this file and ISSUES.org. Add text at the end of this file only.
   # See the org-plan-archive skill.
   ```

5. If `COMPLETED.org` does not exist, create it with this text:
   ```
   #+TITLE: Completed work
   #+TODO: TODO(t) IN-PROGRESS(i) HELD(h) | DONE(d) CANCELED(c)

   # This file is the archive of DONE epics from PLAN.org. Add text at the end of
   # this file only. See the org-plan-archive skill.
   ```

6. If `REQUIREMENTS.org` does not exist, create it with this text:
   ```
   #+TITLE: Requirements

   # This file contains the requirements of the project, as prose.
   # Headings have no TODO keywords. Each heading has a :CUSTOM_ID:
   # (req-<slug>), so that PLAN.org descriptions can link to it with
   # [[file:REQUIREMENTS.org::#req-<slug>][title]].
   ```

7. Report which files you created and which files existed.
