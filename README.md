
# [Mattel Intellivision](https://en.wikipedia.org/wiki/Intellivision) for [MiSTer Platform](https://github.com/MiSTer-devel/Main_MiSTer/wiki)
Written by [Grabulosaure](https://github.com/Grabulosaure)

This core uses raw INT roms, not ROM/INTV headered roms.
 * Place .int files in Intellivision folder

## Features
 * Entertainment Computer System (ECS)
 * Intellivoice
 
## Bios
This core needs copies of original ROMs in the Intellivision folder

**Either use boot0..3.rom files**

Name      | Original   | Content
----------|------------|--------------------
boot0.rom | exec.bin   | System ROM (8kB)
boot1.rom | grom.bin   | Character generator ROM (2kB)
boot2.rom | sp0256-012.bin | Intellivoice ROM (2kB)
boot3.rom | ecs.bin    | ECS extension ROM (24kB)

**Or merge all 4 ROMs into a single boot.rom file**
```
cp exec.bin boot.rom
cat grom.bin >>boot.rom
cat sp0256-012.bin >>boot.rom
cat ecs.bin >>boot.rom
```

## Download precompiled binaries
Go to [releases](https://github.com/MiSTer-devel/Intv_MiSTer/tree/master/releases) folder. 
