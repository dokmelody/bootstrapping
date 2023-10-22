# Dokmelody

Dokmelody is a federated system for programming and sharing information between individual users, groups and communities. 

Dokmelody will be composed of:

- the Dok programming language;
- the Doknil knowledge-base language, for linking the various parts of a project;
- the Tuttology IDE
- the Dokmelody services for running the federation of servers

## Bootstrapping

This repo is an attempt to bootstrap a first usable version of Dokmelody, using Common Lisp as implementation and target programming language.

## Status

Early alpha: nothing of useful.

## Contacts

See [https://www.dokmelody.org](https://www.dokmelody.org) for more information.

## Quick start

The source code is in `pass-XY` directories.

For entering in a development environment, following the NixOS Flakes approach

```shell
cd pass-01
nix develop
```

For the documentation

``` shell
cd docs
nix develop
```

## Licenses

Dokmelody source code is released under the [MIT license](https://spdx.org/licenses/MIT.html) (`SPDX-License-Identifier: MIT`). 

Documentation is released under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/) (SPDX-License-Identifier: CC-BY-4.0). 

### MIT License

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Contributing

All submitted patches and commits are owned by their authors.

Every source code file MUST report in the header the copyright of all main contributors, following the [SPDX](https://spdx.org/) standard. So something like

```
SPDX-License-Identifier: MIT
Copyright (C) YYYY-YYYY Some Name <some.emai@example.net>
Copyright (C) ... 
```

or

```
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) YYYY-YYYY Some Name <some.emai@example.net>
Copyright (C) ... 
```

Updating the copyright header is optional in case of minor contributions, like typos and small bug-fixes. In these cases, it is sufficient the commit history.
