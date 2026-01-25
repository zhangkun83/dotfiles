# GEMINI.md - org files for work

## Overview

This is a personal workspace and knowledge base for me (Kun Zhang) as
a Technical Lead Manager of Google's Prod RPC team, which works on
Stubby (Google's internal RPC library) and google internal users of
gRPC (Google's open-source RPC library).  The content is structured
using Org mode, a major mode for Emacs, designed for notes, planning,
and authoring. The files contain detailed work logs, project tracking,
meeting notes, and personal reminders.

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

*  The tag **`tbdsc`** indicates a meeting agenda entry, indicating
   the parent entry will be back referenced in the notes of the
   meeting with the person whose tag is along with the `tbdsc` tag.
   
*  No space is allowed within a tag.

## Meeting notes

*  A meeting notes entry usually has a heading like "Meeting with XXX"
   where XXX is the name of the person, and is tagged with the tag for
   that person.

*  A meeting notes entry with a `tbs` tag means it's raw notes that
   are yet to be sorted.
   
### Back references

*  A back reference (or back link) is a line that starts with `RE:`,
   followed by the heading of the linked entry, ended by a single
   caret (`^`) which holds the link.  For example:
   ```
   RE: Promise-based HTTP2 (PH2) [[file:notes2025q3.org::#notes2025q3_promise_based_http2_ph2][^]]`.
   ```

*  A back reference means the current entry is in response to, a
   follow up for, or related to the back referenced entry.
   
*  Within an entry, back references may all appear at the beginning,
   or spred out so that each back reference is followed by the
   relevant discussion.

### Sorting meeting notes

Sorting a meeting notes entry includes the following work:

1. Add relevant back references for discussions.
2. Fix typos and grammar errors.
3. Identify action items that I need to take, and prepend them using
   `*AI*`.
4. Change the heading text to be a summary of the whole notes entry.

### Adding TODO entries for action items

When asked to turn action items, find every `*AI*` in the meeting
notes, create a TODO entry to capture that action item.
