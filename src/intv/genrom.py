#!/usr/bin/python
import os

if not os.path.isfile("exec.bin") or not os.path.isfile("grom.bin"):
    print("\n\n  Missing 'exec.bin' or 'grom.bin'\n\n")
    exit()

   
fou = open("rom_pack.vhd","w")
fou.write("LIBRARY IEEE;\nUSE IEEE.std_logic_1164.ALL;\nUSE IEEE.numeric_std.ALL;\n");
fou.write("PACKAGE rom_pack IS\n");
fou.write("  TYPE arr16 IS ARRAY(natural RANGE<>) OF unsigned(15 DOWNTO 0);\n");
fou.write("  TYPE arr8  IS ARRAY(natural RANGE<>) OF unsigned(7 DOWNTO 0);\n");

i=0
fou.write("  CONSTANT INIT_EXEC : arr16 := (\n    ");

with open("exec.bin","rb") as fin:
    b = fin.read(1)
    while b:
        if i!=0:
            fou.write(",")
            if i& 7==0:
                fou.write("\n    ")
        fou.write("x\"{:02X}".format(ord(b)))
        b = fin.read(1)
        fou.write("{:02X}\"".format(ord(b)))
        b = fin.read(1)
        i = i +1
    fou.write(");\n");

i=0
fou.write("  CONSTANT INIT_GROM : arr8 := (\n    ");

with open("grom.bin","rb") as fin:
    b = fin.read(1)
    while b:
        if i!=0:
            fou.write(",")
            if i& 15==0:
                fou.write("\n    ")
        fou.write("x\"{:02X}\"".format(ord(b)))
        b = fin.read(1)
        i = i +1
    fou.write(");\n");

fou.write("END PACKAGE;");
fou.close

print("Created 'rom_pack.vhd'")
