<!---
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 2023 Massimo Zaniboni - mzan@dokmelody.org
-->

# ADR-004: Favors branching and forking of Dokmelody

## Status

Active

## Context

There are two divergent forces: the need to customize Dokmelody for different usage scenario; the need to have a unique federation of servers sharing data and services.

## Decision

Like the Dok language is based on the paradigm of stepwise refinements, where each transformation pass can be branched or forked, then also Dokmelody tools, libraries and communities MUST be able to freely branch or fork, according the needs of different groups of users.

Every branch and/or fork of Dokmelody tools and libraries is called a Dokmelody distribution, and like a Linux distribution, it can: have a distinct mission and/or requirements; customize certain things; send common improvements to upstream project; maintain patches towards the upstream project; document and maintains all differences.

Every group of Dokmelody users with specific needs MUST be able to create their distinct community, and to decide what to share with other Dokmelody communities. 

Whenever possible, a unique communication protocol between federated servers of different distributions will be used. If custom protocols had to be created, then the standard protocols can be still used for sharing some relevant data.

## Consequences

The upstream Dokmelody project will mainly favor generic tools, and flexibility, while specific needs are fullfilled by Dokmelody distributions.

Every Dokmelody distribution will be distinct project, with:

* a specific usage-scenario/domain 
* a potentially distinct community of developers
* a distinct name and marketing strategy
* an optional reference to the upstream Dokmelody distributions from which it is forking

Pros of this approach:

* avoiding pointless discussions on things that must be managed in different ways
* possibility to explore different technical solutions
* use the stepwise refinement paradigm of Dok, also for managing Dokmelody distributions



