<!-- 
SPDX-License-Identifier: CC-BY-4.0
Copyright (C) 2021-2022 Massimo Zaniboni <mzan@dokmelody.org>
-->

# About

A first implementation of Doknil.

# Status

Nothing to see: the code is completely in alpha-state, and everything can change.

# Installation under NixOS

In `/etc/nixos/configuration.nix` set something like

```
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
    enableTCPIP = false;
    extraPlugins = with pkgs.postgresql_14.pkgs; [
      pg_repack
      postgis
      periods
    ];
    ensureDatabases = [ "dokmelody" ];
    ensureUsers = [
    {  name = "<your-dev-or-dokmelody-user>";
       ensurePermissions = {
         "DATABASE dokmelody" = "ALL PRIVILEGES";
       };
    }
    ];
  };
```

# Running tests

Add `pass-01` directory to ASDF load path, then execute 

```
(asdf:test-system :doknil-01)
```

# Loading 

Add `pass-01` directory to ASDF load path, then execute 

```
(asdf:load-system :doknil-01)
```

