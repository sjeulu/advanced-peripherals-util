# advanced-peripherals-util

A maintenance script for the [AdvancedPeripherals](https://github.com/IntelligenceModding/AdvancedPeripherals/) Minecraft mod.

## Usage

### Generating the procedural textures

[Install Nix](https://nixos.org/download/) and run the following command in the root directory of the main repository:
```
nix --extra-experimental-features "nix-command flake" run github:sjeulu/advanced-peripherals-util -- generate-textures
```

`--extra-experimental-features` is not needed if you have them enabled already.

See `... generate-textures --help` for more.
