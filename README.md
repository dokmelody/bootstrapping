# Bootstrapping DokMelody

Implement the first useable version of DokMelody, using an iterative bootstrapping process. 

## Getting started

```
nix-shell
```

for loading dependencies.

TODO continue

## Bootstrapping process

* ``prod`` contains the last online/working version of DokMelody project: documentation, code, etc...
* ``dev`` contains the currently developed version of DokMelody with some demo/test data
* ``test`` is the upgrading of ``prod`` according the changes in ``dev``

After ``test`` is upgraded, it became the new ``prod``, and the iterative bootstrapping process can continue.

This is a big mono-repo, that is not meant to be elegant or compact, because it is used only for designing and bootstrapping DokMelody. 

## Issues

Use ``lentil-all`` for showing open issues.

## Decision process

Every decision is documented inside ``kb`` directories.

Every decision is taken following different possible methods, e.g. six hats, RFC, pros vs cons and so on.

