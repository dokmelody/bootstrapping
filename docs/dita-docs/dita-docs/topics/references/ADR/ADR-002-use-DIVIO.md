<!---
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 2023 Massimo Zaniboni - mzan@dokmelody.org
-->

# ADR-002: Use DIVIO documentation system

## Status 

Active

## Context

According [link 1][1], documentation can have different and often conflicting needs.

## Decision

Follow the guidelines of [DIVIO][1], adding a new type of document ``Design``. In particular, decide the type of a document according this table:

|             | Tutorials                          | How-to guides                        | Reference                         | Explanation                           | Design                                                                    |
|-------------|------------------------------------|--------------------------------------|-----------------------------------|---------------------------------------|---------------------------------------------------------------------------|
| oriented to | learning                           | a goal                               | information                       | understanding                         | solving a problem                                                         |
| must        | allow the newcomer to get started  | show how to solve a specific problem | describe the machinery            | explain                               | understand a problem, design a solution and evaluate it |
| its form    | a lesson                           | a series of steps                    | dry description                   | discursive explanation                | analysis and problem solving                                              |
| analogy     | teaching a small child how to cook | a recipe in a cookery book           | a reference encyclopaedia article | an article on culinary social history | scientific paper                                                          |

Make sure that the type of a document (i.e. `tutorial`, `how-to guide`, `reference` or `explanation`) is obvious for the reader.

The documentation is used also during the design of Dokmelody, so an additional type of documentation will be added: `design`.

## Consequences

Writers can focus on the type of document to write, according its scope. 

Easier navigation and comprehensions of documentation for readers.

[1]: https://documentation.divio.com/ "DIVIO documentation system"

