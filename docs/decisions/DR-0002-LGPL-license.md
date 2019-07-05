<!---
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 2019 Massimo Zaniboni - mzan@dokmelody.org
-->

# Status

* DISCARDED in favour of DR-0004-Boost-license 

# Context

DokMelody is a project composed of an user interface, various programming languages compilers, and libraries.

The IDE can use different widgets and components, and it can run on different end-user systems.

In Dok many libraries are managed like templates: they are transformed/specialized and then injected inside the code of the combined-work,
and they are not anymore shipped as distinct libraries.

DokMelody can be used for producing commercial and propietary code, and not only OSS software.

The metaprogramming nature of DokMelody favours the import and reuse of a lot of code 
of different projects, under different licenses.

# Decision

DokMelody code for user interface, compilers and libraries will be licensed under the LGPLv3+ license, 
following the spirit of [Collective Code Construction Contract (C4)](https://rfc.zeromq.org/spec:42/C4/)
(i.e. "The copyrights in the project SHALL be owned collectively by all its Contributors").

Patches to external projects and libraries used from DokMelody will be released using the original upstream license, 
for avoiding unucessary forks.

Documentation that is not part of the source code will be released under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)
for maximizing reuse.

DokMelody will include a tool for checking the license of used libraries and components, and suggesting 
the legal implications for the license of the combined-work. 

# Consequences

## Short story

The majority of Dok applications can use commercial licenses but they had to be released with some (obfuscated) source-code, for allowing the rebuild/relink with newer version of Dok and related libraries.

Documentation can be freely reused and licensed under different licenses.

## Long story

In this section we mean:

* "use": for calling a library in the combined-work, without relicensing its source-code inside the combined-work as part of it
* "import the source code": for including and maybe modify the source code of the library
inside the combined-work, and relicense it as part of the combined-work, and under its license 
* "combined-work": the code and shipped product "using" the LGPLv3+ library


LGPLv3+ can "import the source code" from LGPLv2+, LGPLv3+, APLv2 and other permissive licenses, relicensing it under LGPLv3+.

LGPLv3+ can "use" but not "import the source code" licensed under LGPLv2, and LGPLv3, and other propietary licenses

Propietary code can not "import the source code" licensed under LGPLv3+, without relicensing its code to LGPLv3+.

Propietary code can "use" LGPLv3+ source code, maintaining its propietary license in case it gives rights to the end-user
to use different versions of the "used" LGPLv3+ libraries instead of the shipped one. 
If the combined-work is "using" dynamic LGPL libraries, the end-user can substitute the LGPL libraries
with different versions, and the derived-work will use them automatically at run-time.

In case of DokMelody, many libraries are not distinct from the combined-work, 
but they are instantied and injected inside the combined-work at compile-time.
So the vendor had to include (at moment of the shipping or under request of its end-users)
its source-code (also obfuscated) or its object-code, 
with the building instructions for creating a new version of its combined-work,
using newer/different versions of the "used" LGPL libraries and/or 
of the DokMelody compiler.
The end-user must be informed of this possibility.

This affects only end-users with the right to use the original combined-work, i.e. users without a valid license 
can never rebuild and use the new version of the combined-work.
Accordingly end-users inherit no rights to modify the vendor code vendor released under propietary license, 
but they have rights only for the LGPL parts.

It is not clear if LGPLv3+ code can be "used" on combined-work shipped on locked devices, where
the user has no technical way to install new versions of it, and if the responsability is of the 
vendor of the derived-work or of the locked device.

## Rationale

A more permissive license like APLv2 seems simpler to use, but:

* there will be similar problems in case LGPL code is "used" or "imported", and so the problem is only postponed, not completely avoided
* the right to update a combined-work seem fair, when this involve free libraries: i.e. repairing a software should be legal and normal as repairing a mechanical device, also if the software is under a propietry license
* security in IT is very important, and end-users with DokMelody can always rebuild a combined-work using more secure versions of the compiler and shipped libraries
* JVM or .NET code is already a form of inspectable and specializable high-level object-code on which the end-user can update the called libraries and also the executing virtual machine, so the same right is enforced also for compiled DokMelody code
