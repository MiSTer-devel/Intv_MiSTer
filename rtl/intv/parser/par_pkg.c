/*
 Generate VDHL constant with CFG file parser binary

 gcc par_pkg.c -o par_pkg

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define WORDS_PER_LINE 8

int main(int argc, char *argv[])
{
    const char *in    = "par.bin";
    const char *out   = "parser_pack.vhd";
    const char *pkg   = "parser_pack";
    const char *rom   = "ROM_PARSER";
    const int size = 2048; // * 16bits

    FILE *fin = fopen(in, "rb");

    if (fseek(fin, 0, SEEK_END) != 0) {
        fclose(fin);
        return -1;
    }
    long file_size = ftell(fin);
    rewind(fin);

    size_t word_count = (size_t)(file_size / 2);

    if (word_count > size) {
        printf ("TOO LARGE !\n");
        return -1;
    }
    unsigned char *buf = malloc((size_t)4096);

    size_t read_bytes = fread(buf, 1, (size_t)file_size, fin);
    fclose(fin);

    FILE *fout = fopen(out, "w");

    /* --- Write VHDL header --- */

    fprintf(fout,"--------------------------------------------------------------------------------\n");
    fprintf(fout,"-- Intellivision\n");
    fprintf(fout,"--------------------------------------------------------------------------------\n");
    fprintf(fout,"-- CFG FILE PARSER ROM\n");
    fprintf(fout,"--------------------------------------------------------------------------------\n");
    fprintf(fout,"-- DO 8/2026\n");
    fprintf(fout,"--------------------------------------------------------------------------------\n");
    
    fprintf(fout, "LIBRARY IEEE;\n");
    fprintf(fout, "USE IEEE.std_logic_1164.ALL;\n\n");

    fprintf(fout,"LIBRARY work;\n");
    fprintf(fout,"USE work.base_pack.ALL;\n\n");

    fprintf(fout, "PACKAGE %s IS\n\n", pkg);

    fprintf(fout, "  CONSTANT %s : arr_uv16 := (\n", rom);

    fprintf(fout, "    ");

    for (size_t i = 0; i < size; i++) {
        uint16_t msb = buf[2 * i];
        uint16_t lsb = buf[2 * i + 1];
        uint16_t value = (uint16_t)((msb << 8) | lsb);

        fprintf(fout, "x\"%04X\"", value);
        if (i != size - 1)
            fprintf(fout, ",");
        else
            fprintf(fout, " ");

        /* Newline every WORDS_PER_LINE words (and re-indent), except after the last word */
        //if (i != size - 1) {
            if ((i + 1) % WORDS_PER_LINE == 0) {
                fprintf(fout, " -- %4X\n    ",i-WORDS_PER_LINE + 1);
            }
        //}
    }
    fprintf(fout, ");\n\n");
    fprintf(fout, "END PACKAGE %s;\n", pkg);

    fclose(fout);
    free(buf);

    return EXIT_SUCCESS;
}
