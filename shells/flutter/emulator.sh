#!/usr/bin/env bash

if [ ! -d "result"]; then
    nix-build emulator.nix
fi

./result/bin/run-test-emulator
```