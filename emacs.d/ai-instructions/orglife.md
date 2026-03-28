# GEMINI.md - org files for life

## Overview

This is a personal workspace and knowledge base for me (Kun) as The
content is structured using Org mode, a major mode for Emacs, designed
for notes, planning, and authoring. The files contain detailed logs,
task tracking, meeting notes, and personal reminders.

## General formatting conventions

*   Every period (`.`) at the end of a sentence in the middle of a
    paragraph is followed by two space characters instead of one.
    
*   Unnumbered lists in the text body uses `-` or `+` as the bullet
    character.  `*` is reserved for entry headings only.

## File naming convention

*   **`notesYYYY.org` (e.g., `notes2026.org`)**: These files serve as
    a chronological journal for each year. They contain dated entries
    with meeting notes, progress updates, and thoughts on various
    life-related topics.

*   **`recurring.org`**: for recurring tasks like bill pays.

*   Other org files are for specific topics.

## Tag naming convention

*  **`#...` (e.g., `#travel`)** is a tag associated with a
   project or topic tags

*  The tag **`project`** is the parent tag of all `#...` tags   

*  **`@...` (e.g., `@kun`)** is a tag for a person.  It's the user
   name of the person, so usually it's related to but different from
   the name of the person.

*  The tag **`tbs`** indicates this entry is a meeting notes entry
   that's yet to be sorted.

*  The tag **`document`** indicates this entry contains or refers to a
   documentation that's worth keeping for long-term.
   
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
   RE: IEP meeting with Union School District SLP Madalyn Larkin  (2026-02-23) [[file:notes2026.org::#notes2026_iep_meeting_with_union_school_district_slp_madalyn_larkin_2026_02_23_mon][^]]   

   ```

*  A back reference means the current entry is in response to, a
   follow up for, or related to the back referenced entry.
   
*  Within an entry, back references may all appear at the beginning,
   or spred out so that each back reference is followed by the
   relevant discussion.

