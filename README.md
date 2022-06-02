
# Intv_MiSTer

Mattel Intellivision

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

Unless playing ECS games, the ECS toggle is recommended to be turned off by default as it can create some graphical issues during transition screens as well as making the following games unplayable:
-Centipede
-Congo Bongo
-Dig Dug
-Pac-Man

The disc button has not been implemented or mapped as a button yet. There's a ticket open regarding this: https://github.com/MiSTer-devel/Intv_MiSTer/issues/16
