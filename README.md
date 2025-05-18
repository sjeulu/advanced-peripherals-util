# advanced-peripherals-util

A maintenance script for the [AdvancedPeripherals](https://github.com/IntelligenceModding/AdvancedPeripherals/) Minecraft mod.

## Usage

### Generating the procedural textures

[Install Nix](https://nixos.org/download/) and run the following command in the root directory of the main repository:
```
nix --extra-experimental-features "nix-command flake" run github:sjeulu/advanced-peripherals-util -- generate-textures
```

`--extra-experimental-features` is not needed if you have them enabled already.

See `... generate-textures --help` for more:
```
Usage: advanced-peripherals-util generate-textures 
         [-a|--assets-path ARG] [-o|--output-path ARG] [-s|--seed ARG] 
         [-i|--inject ARG] [-g|--gif-only] [-u|--upscale]

  Generates the procedurally animated textures for blocks.

Available options:
  -a,--assets-path ARG     Path to the directory where the original block
                           textures are stored
  -o,--output-path ARG     Where to output the generated textures
  -s,--seed ARG            The seed to use for the procedural textures. Defaults
                           to a random one every time the script is run.
  -i,--inject ARG          Injects the generated textures into the specified
                           .jar file, replacing existing ones with the same
                           names
  -g,--gif-only            Exports only animated block faces as GIF files.
                           Useful for previewing the animations
  -u,--upscale             Makes the exported textures 100 times bigger which
                           makes them easier to view in some scenarios
  -h,--help                Show this help text
```
