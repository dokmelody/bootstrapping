<!---
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 2023 Massimo Zaniboni - mzan@dokmelody.org
-->

# ADR-003: Use MIT and CC-BY-4.0 licenses

## Status

Active

## Context

In Dok programming language, libraries can be managed like code templates, i.e. their source code can be specialized and then injected inside the code of the combined-work. If these libraries are under a copyleft license like GPL and LGPL then also the license of the derived work could be affected.

The preferred solution is a way to enforce programmers to send the improvements of Dokmelody source code back to upstream, but giving them the freedom to license their own code under the license they prefer. 

Another requirement, should be to enforce an LGPL-like clause, where users of a derived products, developed using Dokmelody, have the rights to update the derived product with new versions of Dokmelody libraries. This is a sort of "right to repair", and it reduces the lock-in of an user towards a product for which he has a valid license.

Another requirement, should be to enforce an Affero like LGPL license, because many vendors of derived works can publish their work only in the cloud, as services to end users, and so they can avoid to send improvements to upstream, in case a simple LGPL license is used.

Another requirement is the possibility to generate code that can be called by projects using (L)GPLv2 licenses. There can be problems doing this, because libraries under LGPLv3 cannot be called from code under (L)GPLv2, and there are many projects using (L)GPLv2-only license, instead of (L)GPLv2+ license.

Another requirement is the possibility to include code under Apache License 2.0, in Dok. GPLv2 code cannot include Apache License 2.0 code, but if Dok injects this code in the combined-work, there can be problems.

## Decision

For simplifying the use of Dokmelody tools and libraries with projects using different licenses, the permissive [MIT license](https://spdx.org/licenses/MIT.html) (SPDX-License-Identifier: MIT) will be used for all Dokmelody source code.

MIT was chosen because it is one of the most used permissive licenses.

Despite being released under MIT, Dok programming language can use the source code of other libraries released under different licenses, and in some case it can specialize and inject it, in the final product source code. So Dokmelody MUST include a licensing management tool for analyzing the license and usage of libraries, and for each distinct project, it MUST infer/suggest:

* the final license of the derived work
* if the derived work MUST be re-linkable with newer versions of used LGPL libraries
* the credits to give to used libraries in source-code and in binary distribution
* other licensing aspects

In future, according the experience with the above tool, Dokmelody project CAN consider to switch to a different copyleft license, according the requirements described in the `Context`. Up to date, these requirements are ignored, for favouring the application of Dokmelody source code and tools to projects with different licenses.

Dokmelody documentation that is not part of the source code will be released under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) (SPDX-License-Identifier: CC-BY-4.0). It is a license, specialized for documentation, data and media, used from many big projects like Wikipedia. 

## Discarded decisions

Using of AGPL-1.0-or-later license only for Dok compiler, Tuttology UI and other tools of Dokmelody, while using MIT license for libraries, was discarded, because this approach will complicate the licensing of code. Moreover, in case the Dok compiler source code is included in a derived-work for supporting run-time generation and execution of code, there can be the same problems described in the `Context` section.

GPL and LGPL licenses with macro-expansion exceptions, create a proliferation of incompatible licenses.

Apache License 2.0 is a good permissive license, because it considers also patents, but source code under this license cannot be combined with code under GPLv2-only. MIT is more permissive.

Using CC0 (i.e. public domain) license for documentation was discarded, because the majority of documentation of other projects is released under CC-BY-4.0. 

## Consequences

Every (also minor) source code file must start with an header comment like

```
SPDX-License-Identifier: MIT 
Copyright (C) 20XX Author Name \<author-email@example.net\>
```

Authors and contributors must add them self to the list of copyright holders, when they change a file.

Contributors of minor bug-fixes and minor patches can also not add them self to the list of copyright owners of the file. In this case the project repository history will give them credit.

Some source files can contain source-code under different licenses. They will use the proper [SPDX identifier](https://spdx.org).

Every documentation file must start with an header like

```
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 20XX Author Name \<author-email@example.net\>
```

Ownership and copyright of source files is distributed between all contributors.

