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

*  The tag **`okr_planning`** indicates this entry is about planning
   OKRs.

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

### Sorting meeting notes

Sorting a meeting notes entry includes **all** of the following steps:

1. If a paragraph lacks back reference, add relevant back references
   when applicable.
2. Fix typos and grammar errors.
3. Identify the topic of each section.  When necessary, reorder
   sections so that sections about the same topic are together.  If
   the meeting notes entry discusses multiple topics, create a new
   heading entry for each topic.
4. For each heading entry:
    1. Change the heading text to be a summary of the whole entry.
       Try to include concrete events, updates, ideas, conclusions,
       instead of generalization in the summary.
    2. Update the tags to reflect the people and projects relevant to
       this meeting.  Use preexisting tags only.
    3. Add a TL;DR paragraph at the beginning for main take-aways,
       with the first line being "TL;DR:", followed by bullet points
       that summarize key conclusions, agreements, consensus, and
       action items.  An action item is prepended with `*AI*`.

### Generating TODO entries for action items

When asked to generate TODO entries for a meeting notes, find every
`*AI*` in the meeting notes, create a TODO entry to capture that
action item.  The TODO entry shall:

1. Have a short (less than 80 characters) heading summarizing the action
2. Have a text body that includes the full text of the original action
   item, and related context information (on why we need to take this
   action) from the meeting notes.
3. Be placed after, and at the same level (starting with the same
   number of asterisks) as the meeting notes entry
4. Be tagged according to the project and people associated with this
   TODO entry.
