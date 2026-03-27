# GEMINI.md - org files for work

## Overview

This is a personal workspace and knowledge base for me (Kun Zhang) as
a Technical Lead Manager of Google's Prod RPC team, which works on
Stubby (Google's internal RPC library) and google internal users of
gRPC (Google's open-source RPC library).  The content is structured
using Org mode, a major mode for Emacs, designed for notes, planning,
and authoring. The files contain detailed work logs, project tracking,
meeting notes, and personal reminders.

## General formatting conventions

*   Every period (`.`) at the end of a sentence in the middle of a
    paragraph is followed by two space characters instead of one.
    
*   Unnumbered lists in the text body uses `-` or `+` as the bullet
    character.  `*` is reserved for entry headings only.

## File naming convention

*   **`notesYYYYqN.org` (e.g., `notes2026q1.org`)**: These files serve
    as a chronological journal for each quarter. They contain dated
    entries with meeting notes, progress updates, technical
    discussions, and thoughts on various work-related topics.

*   **`+<topic>.org` (e.g., `+interview-....org`)**: Files prefixed
    with a `+` is dedicated to specific events that are not indexed or
    referenced by the chronological journals.

## Tag naming convention

*  **`#...` (e.g., `#stubby_java`)** is a tag associated with a
   project or topic tags

*  The tag **`project`** is the parent tag of all `#...` tags   

*  **`@...` (e.g., `@roth`)** is a tag for a person.  It's the user
   name of the person, so usually it's related to but different from
   the name of the person.

*  The tag **`tbs`** indicates this entry is a meeting notes entry
   that's yet to be sorted.

*  The tag **`document`** indicates this entry contains or refers to a
   documentation that's worth keeping for long-term.
   
*  The tag **`managerial`** indicates this entry is about managerial
   work.  It's the parent tag of all `m#...` tags (e.g., `m#grad`).

*  The tag **`okr_planning`** indicates this entry is about planning
   OKRs.

*  The tag **`tbdsc`** indicates a meeting agenda entry, indicating
   the parent entry will be back referenced in the notes of the
   meeting with the person whose tag is along with the `tbdsc` tag.
   
*  No space is allowed within a tag.

*  Tags must be at the end of the heading line.

## Meeting notes

*  A meeting notes entry is tagged with the tag for the persons
   involved.

*  A meeting notes entry with a `tbs` tag means it's raw notes that
   are yet to be sorted.
   
### Back references

*  A back reference (or back link) is a line that starts with `RE:`,
   followed by the heading of the linked entry, ended by a single
   caret (`^`, without any parenthesis) which holds the link.  For example:
   ```
   RE: Promise-based HTTP2 (PH2) [[file:notes2025q3.org::#notes2025q3_promise_based_http2_ph2][^]]`.
   ```

*  A back reference means the current entry is in response to, a
   follow up for, or related to the back referenced entry.
   
*  Within an entry, back references may all appear at the beginning,
   or spred out so that each back reference is followed by the
   relevant discussion.
   
### Multi-section meeting notes

*  A meeting notes may consist of multiple sections.  Each section is
   presented by one or more back references grouped as one paragraph,
   followed by the discussion content.

*  A section is mapped to a topic, e.g., a project, or an ongoing
   effort.  Multiple sections can be mapped to the same topic.

*  A meeting notes entry may discuss multiple topics unrelated to each
   other.
