<!-- 
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>
-->

# About

Compile a first version of Dok to Common Lisp CLOS code.

# Running tests

Add `pass-01` directory to QuickLisp or ASDF  load path (for example setting `.sbcl` config file, but there are many other methods), then execute 

```
(asdf:test-system :dok-01)
```

# Loading 

```
(asdf:load-system :dok-01)
```

