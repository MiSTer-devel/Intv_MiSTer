;------------------------------------------------------------------------------
; CFG File Parser
;
; ./as1600 par.asm -o par.bin -l par.lst -s par.syn -m
;
;------------------------------------------------------------------------------
; Supported Syntax :
;
; ; <comment>
;
; $<hexadecimal>
; 
; [mapping]
; <src_start> - <src_end> = <dest_start> [PAGE <hexnum>]  => Mapping
;
; [ecsbank]
; <hexnum> : <src_start> - <src_end> = <dest_start>       => Mapping
;
; [memattr]
; <dest_start> - <dest_end> = RAM [8|16]    => Define RAM area
;
; [vars]
; ecs = [0|1]                      => Enable / Disable ECS
; jlp = [0|1]                      => Enable / Disable JLP
; ecs_compat = [0|1|2|3]           => Enable (>=2) / Disable ECS (<=1)
; jlp_compat = [0|1|2|3]           => Enable (>=2) / Disable JLP (<=1)
;
; Any other CFG file features are ignored!
;
;------------------------------------------------------------------------------
; CONSTANTS

MAPPING     EQU     1
ECSBANK     EQU     2
MEMATTR     EQU     3
VARS        EQU     4

ATTR_NOMAP  EQU     0
ATTR_ROM    EQU     1
ATTR_RAM8   EQU     2
ATTR_RAM16  EQU     3

REG_ECS_SET EQU     1
REG_ECS_CLR EQU     2
REG_JLP_SET EQU     4
REG_JLP_CLR EQU     8
REG_RESTART EQU     16

PCFG        EQU     $5000
PSRC        EQU     $6000
PDEST       EQU     $D000

REG_SRC_ZONE   EQU   $0060
REG_SRC_HIZONE EQU   $0061
REG_DEST_ZONE  EQU   $0062
REG_DEST_PAGE  EQU   $0063
REG_MEMATTR    EQU   $0064
REG_VARS       EQU   $0065

CARD_BLANK     EQU   $0007 ; xx00 0000 0000 0111 = VIDE
CARD_MINUS     EQU   $006F ; xx00 0000 0110 1111 = -
CARD_PLUS      EQU   $005F ; xx00 0000 0101 1111 = +
CARD_STAR      EQU   $0057 ; xx00 0000 0101 0111 = *

BACKTAB        EQU   $0200

;------------------------------------------------------------------------------
; DATA RAM

STACK           EQU     $2F0

                ORG     $0300
; mapping(), mapping_page()
MAP_SEND_LO     RMB     1
MAP_SEND_HI     RMB     1

; vars()
VARS_VAR        RMB     1
VARS_VAL        RMB     1

; parse_mapping_line()
PML_P           RMB     1
PML_SSTART_LO   RMB     1
PML_SSTART_HI   RMB     1
PML_SEND_LO     RMB     1
PML_SEND_HI     RMB     1
PML_DEST        RMB     1

; parse_vars_line()
PVL_Q           RMB     1

; parse_memattr_line()
PMA_P           RMB     1
PMA_DSTART      RMB     1
PMA_DEND        RMB     1
PMA_WIDTH       RMB     1

; parse_buffer()
PB_LINE         RMB     1
PB_NLINE        RMB     1
PB_SECTION      RMB     1
PB_EOL          RMB     1

DPOS            RMB     1

;------------------------------------------------------------------------------
; CODE

;------------------------------------------------------------------------------
        ORG     $1000

;------------------------------------------------------------------------------
; RESET = $1000
; IRQ   = $1004
@@RESET:
        B       @@ENTRY ; 1000 1001
        B       @@ENTRY ; 1002 1003 : Filler
        
@@IRQ:  
        PULR    R7      ; 1004

@@ENTRY
        MVII    #STACK, R6
        MVII    #REG_ECS_CLR + REG_JLP_CLR, R3
        MVO     R3, REG_VARS
        MVII    #PCFG, R0
        JSR     R5, @@PARSE_BUFFER
        CLRR    R0
        MVII    #REG_RESTART, R3
        MVO     R3, REG_VARS    ; Triger RESET, leave Mapping mode
@@WAIT:
        B       @@WAIT

;------------------------------------------------------------------------------
; Read-only string constants, one character per word, NUL terminated

STR_ECS         DECLE   "ecs",0
STR_JLP         DECLE   "jlp",0
STR_ECS_COMPAT  DECLE   "ecs_compat",0
STR_JLP_COMPAT  DECLE   "jlp_compat",0

STR_PAGE        DECLE   "page",0
STR_RAM         DECLE   "ram",0
STR_MAPPING     DECLE   "mapping",0
STR_ECSBANK     DECLE   "ecsbank",0
STR_MEMATTR     DECLE   "memattr",0
STR_VARS        DECLE   "vars",0

;------------------------------------------------------------------------------
; static const char *skip_space(const char *p)
;   R0 = p (in/out)

@@SKIP_SPACE:
        MOVR    R0, R3
        MVI@    R3, R3
        TSTR    R3
        BEQ     @@SS_DONE
        CMPI    #' ', R3
        BEQ     @@SS_ADV
        CMPI    #9, R3     ; '\t'
        BNEQ    @@SS_DONE
@@SS_ADV:
        INCR    R0
        B       @@SKIP_SPACE
@@SS_DONE:
        JR      R5

;------------------------------------------------------------------------------
; static const char *token_end(const char *p)
;   R0 = p (in/out)

@@TOKEN_END:
        MOVR    R0, R3
        MVI@    R3, R3
        TSTR    R3
        BEQ     @@TE_DONE
        CMPI    #' ', R3
        BEQ     @@TE_DONE
        CMPI    #9, R3
        BEQ     @@TE_DONE
        CMPI    #'=', R3
        BEQ     @@TE_DONE
        CMPI    #'-', R3
        BEQ     @@TE_DONE
        CMPI    #'[', R3
        BEQ     @@TE_DONE
        INCR    R0
        B       @@TOKEN_END
@@TE_DONE:
        JR      R5

;------------------------------------------------------------------------------
; static uint16_t parse_cmplow(const char *p, const char *s, uint16_t len)
;   R0 = p, R1 = s, R2 = len   ->  R0 = result (0 match / 1 no-match)

@@PARSE_CMPLOW:
        MOVR    R0, R3          ; R3 = p   (R1 already usable indirectly = s)
@@PCL_LOOP:
        MVI@    R3, R4         ; R4 = *p
        TSTR    R2
        BEQ     @@PCL_LAST
        TSTR    R4
        BEQ     @@PCL_NOMATCH
        MVI@    R1, R0         ; R0 = *s
        TSTR    R0
        BEQ     @@PCL_NOMATCH

        CMPI    #'A', R4
        BNC     @@PCL_CMP       ; <'A'
        CMPI    #'Z'+1, R4
        BC      @@PCL_CMP       ; >'Z' : >='Z'+1
        ADDI    #32, R4         ; 'a' - 'A'
@@PCL_CMP:
        CMPR    R4, R0          ; R0 - R4
        BNEQ    @@PCL_NOMATCH
        INCR    R3              ; p++
        INCR    R1              ; s++
        DECR    R2              ; len--
        B       @@PCL_LOOP

@@PCL_NOMATCH:
        MVII    #1, R0
        JR      R5

@@PCL_LAST:
        ; Check that the next character isn't a letter or underscore
        CMPI    #'A', R4
        BNC     @@PCL_EX1       ; <'A'
        CMPI    #'Z'+1, R4
        BNC     @@PCL_NOMATCH   ; <='Z'
@@PCL_EX1:
        CMPI    #'a', R4
        BNC     @@PCL_EX2       ; <'a'
        CMPI    #'z'+1, R4
        BNC     @@PCL_NOMATCH   ; <='z'
@@PCL_EX2:
        CMPI    #'_', R4
        BEQ     @@PCL_NOMATCH
        CLRR    R0
        JR      R5

;------------------------------------------------------------------------------
; static uint32_t parse_number(const char *s)
;   R0 = s   ->   R0 = result_lo, R1 = result_hi

@@PARSE_NUMBER:
        MOVR    R0, R2
        CLRR    R0
        CLRR    R1
        MVI@    R2, R3
        CMPI    #'$', R3
        BNEQ    @@PN_DEC
        INCR    R2             ; t = s+1

@@PN_HEX_LOOP:
        MVI@    R2, R3         ; R3 = *s
        CMPI    #'0', R3
        BNC     @@PN_DONE      ; < '0'
        CMPI    #'9'+1, R3
        BC      @@PN_HEX_ALPHAU ; > '9'
        SUBI    #'0', R3
        B       @@PN_HEX_DIGIT
@@PN_HEX_ALPHAU:
        CMPI    #'A', R3
        BNC     @@PN_DONE ; < 'A' => DONE
        CMPI    #'F'+1, R3
        BC      @@PN_HEX_LOWER ; > 'F'
        SUBI    #'A'-10, R3
        B       @@PN_HEX_DIGIT
@@PN_HEX_LOWER:
        CMPI    #'a', R3
        BNC     @@PN_DONE ; < 'a' => DONE
        CMPI    #'f'+1, R3
        BC      @@PN_DONE ; > 'f' => DONE
        SUBI    #'a'-10, R3
@@PN_HEX_DIGIT:
        ; v = v*16 + digit   (4 x shift-left-1 of the 32-bit value in R1:R0)
        CLRC
        RLC     R0, 1
        RLC     R1, 1
        RLC     R0, 1
        RLC     R1, 1
        RLC     R0, 1
        RLC     R1, 1
        RLC     R0, 1
        RLC     R1, 1
        ADDR    R3, R0
        ADCR    R1
        INCR    R2
        B       @@PN_HEX_LOOP

@@PN_DONE:
        JR      R5

@@PN_DEC:
        ; *s
        MVI@    R2, R3
        CMPI    #'0', R3
        BNC     @@PN_DONE ; < '0' => DONE
        CMPI    #'9'+1, R3
        BC      @@PN_DONE ; > '9' => DONE
        SUBI    #'0', R3
        PSHR    R3
        ; (R3:R4) = v *2
        CLRC
        RLC     R0, 1
        RLC     R1, 1
        MOVR    R0, R3
        MOVR    R1, R4
        ; (R0:R1) = (v*2)*4
        RLC     R0, 1
        RLC     R1, 1
        RLC     R0, 1
        RLC     R1, 1
        ; (R0:R1) = v*8 + v*2
        ADDR    R3, R0
        ADCR    R1
        ADDR    R4, R1
        ; v += digit
        PULR    R3
        ADDR    R3, R0
        ADCR    R1
        INCR    R2
        B       @@PN_DEC

;------------------------------------------------------------------------------
; static uint16_t parse_hexdigit(const char *s) => Reuse parse_number
;   R0 = s   ->  R0 = result

@@PARSE_HEXDIGIT:
          MOVR    R0, R2
          B       @@PN_HEX_LOOP

;------------------------------------------------------------------------------
; void mapping(uint32_t src_start, uint32_t src_end, uint16_t dest)
;   R0=src_start_lo R1=src_start_hi
;   R2=src_end_lo   R3=src_end_hi
;   R4=dest

@@MAPPING:
        PSHR    R5
        MVO     R3, MAP_SEND_HI
        MVO     R2, MAP_SEND_LO
        MVII    #ATTR_ROM, R2
        MVO     R2, REG_MEMATTR
        MOVR    R4, R5

@@MAP_BODY:
        ; slow = src_lo & 0xFFF
        MOVR    R0, R2
        MVO     R0, REG_SRC_ZONE
        MVO     R1, REG_SRC_HIZONE
        ANDI    #$0FFF, R2
        ADDI    #PSRC , R2        ; R2 = &psrc[slow]
        ; R2 = psrc[slow]
        MVI@    R2, R2
        
        ; dlow = ddd & 0xFFF ; dzone = ddd >> 12
        MVO     R5, REG_DEST_ZONE
        MOVR    R5, R4
        ANDI    #$0FFF, R4
        ADDI    #PDEST, R4
        ; for (i=0;i<16;i++) { *reg_dest_page=i; pdest[dlow]=v; }
        CLRR    R3

@@MAP_INNER:
        MVO     R3, REG_DEST_PAGE
        MVO@    R2, R4
        DECR    R4
        INCR    R3
        CMPI    #16, R3
        BLT     @@MAP_INNER

        ; R1:R0 = src
        MVI     MAP_SEND_LO, R2
        CMPR    R2,R0
        BNE     @@MAP_INCR
        MVI     MAP_SEND_HI, R2
        CMPR    R2, R1
        BZE     @@MAP_DONE
        
        ; ddd++ ; src++
@@MAP_INCR:
        INCR    R5
        ADDI    #1, R0
        ADCR    R1
        B       @@MAP_BODY
@@MAP_DONE:
        PULR    PC

;------------------------------------------------------------------------------
; void mapping_page(uint32_t src_start, uint32_t src_end,
;                   uint16_t dest, uint16_t page)
;   R0=src_start_lo R1=src_start_hi
;   R2=src_end_lo   R3=src_end_hi
;   R4=dest
;   R1:R0 = SRC POINTER
;   R2    = V : Copied value
;   R4    = DEST POINTER
;   R5    = Temp

@@MAPPING_PAGE:
        PSHR    R5
        MVO     R3, MAP_SEND_HI
        MVO     R2, MAP_SEND_LO
        MVII    #ATTR_ROM, R2
        MVO     R2, REG_MEMATTR

@@MPG_BODY:
        ; slow = src_lo & 0xFFF
        MOVR    R0, R2           
        MVO     R0, REG_SRC_ZONE
        MVO     R1, REG_SRC_HIZONE
        ANDI    #$0FFF, R2
        ADDI    #PSRC , R2        ; R2 = &psrc[slow]
        ; R2 = psrc[slow]
        MVI@    R2, R2
        
        ; dlow = ddd & 0xFFF ; dzone = ddd
        MVO     R4, REG_DEST_ZONE
        MOVR    R4, R5
        ANDI    #$0FFF, R5
        ADDI    #PDEST, R5
        MVO@    R2, R5
        
        ; R1:R0 = src
        MVI     MAP_SEND_LO, R2
        CMPR    R2,R0
        BNE     @@MPG_INC
        MVI     MAP_SEND_HI, R2
        CMPR    R2, R1
        BZE     @@MPG_DONE
        
@@MPG_INC:
        ; ddd++ ; src++
        INCR    R4
        ADDI    #1, R0
        ADCR    R1
        B       @@MPG_BODY
@@MPG_DONE:
        PULR    PC

;------------------------------------------------------------------------------
; void vars(const char *var, uint16_t value)
;   R0 = var, R1 = value

@@VARS_FN:
        PSHR    R5
        ; ECS. Diable if ECS=0
        MVO     R0, VARS_VAR
        MVO     R1, VARS_VAL
        MVII    #STR_ECS, R1
        MVII    #3, R2
        JSR     R5, @@PARSE_CMPLOW
        MVII    #REG_ECS_CLR, R3
        TSTR    R0
        BNEQ    @@VARS_SKIP1
        MVI     VARS_VAL, R2
        TSTR    R2
        BEQ     @@VARS_ECS0
        ADDI    #REG_ECS_SET-REG_ECS_CLR, R3
@@VARS_ECS0:
        MVO     R3, REG_VARS
        PULR    PC

@@VARS_SKIP1:
        ; JLP. Disable if JLP=0. <TODO> JLP accel/flash options
        MVI     VARS_VAR, R0
        MVII    #STR_JLP, R1
        MVII    #3, R2
        JSR     R5, @@PARSE_CMPLOW
        MVII    #REG_JLP_CLR, R3
        TSTR    R0
        BNEQ    @@VARS_SKIP2
        MVI     VARS_VAL, R2
        TSTR    R2
        BEQ     @@VARS_JLP0
        ADDI    #REG_JLP_SET-REG_JLP_CLR, R3
@@VARS_JLP0:
        MVO     R3, REG_VARS
        PULR    PC
        
@@VARS_SKIP2:
        ; ECS_COMPAT : Enables ECS if ecs_compat>=2
        MVI     VARS_VAR, R0
        MVII    #STR_ECS_COMPAT, R1
        MVII    #10, R2
        JSR     R5, @@PARSE_CMPLOW
        MVII    #REG_ECS_CLR, R3
        TSTR    R0
        BNEQ    @@VARS_SKIP3
        MVI     VARS_VAL, R2
        TSTR    R2
        CMPI    #2, R2 ; <2 => Set ECS = OFF
        BNC     @@VARS_ECS_COMPAT0
        ADDI    #REG_ECS_SET-REG_ECS_CLR, R3
@@VARS_ECS_COMPAT0:
        MVO     R3, REG_VARS
        PULR    PC

@@VARS_SKIP3:
        ; JLP_COMPAT : Enables JLP if jlp_compat>=2
        MVI     VARS_VAR, R0
        MVII    #STR_JLP_COMPAT, R1
        MVII    #10, R2
        JSR     R5, @@PARSE_CMPLOW
        MVII    #REG_JLP_CLR, R3
        TSTR    R0
        BNEQ    @@VARS_SKIP4
        MVI     VARS_VAL, R2
        TSTR    R2
        CMPI    #2, R2 ; <2 => Set JLP = OFF
        BNC     @@VARS_JLP_COMPAT0
        ADDI    #REG_JLP_SET-REG_JLP_CLR, R3
@@VARS_JLP_COMPAT0:
        MVO     R3, REG_VARS
        PULR    PC

@@VARS_SKIP4:
        PULR    PC

;------------------------------------------------------------------------------
; void attr_ram(uint16_t dest_start, uint16_t dest_end,
;               uint16_t ram, uint16_t width)
;   R0=dest_start R1=dest_end R2=Width

;   R0 = dest pointer
;   R1 = dest_end
;   R2 = dlow
;   R3 = i
;   R4 = 0

@@ATTR_RAM:
        CMPI    #8, R2
        BEQ     @@ATTR_W8
        CMPI    #16, R2
        BEQ     @@ATTR_W16
        JR      R5                      ; invalid width -> return
@@ATTR_W8:
        MVII    #ATTR_RAM8, R2
        B       @@ATTR_SETATTR
@@ATTR_W16:
        MVII    #ATTR_RAM16, R2
@@ATTR_SETATTR:
        MVO     R2, REG_MEMATTR
        CLRR    R4
                
@@ATTR_BODY:
        MOVR    R0, R2
        MVO     R0, REG_DEST_ZONE
        ANDI    #$0FFF, R2
        ADDI    #PDEST, R2
        CLRR    R3

@@ATTR_INNER:
        MVO     R3, REG_DEST_PAGE
        MVO@    R4, R2                 ; pdest[dlow] = 0
        INCR    R3
        CMPI    #16, R3
        BLT     @@ATTR_INNER

@@ATTR_LOOP:
        CMPR    R0, R1
        BNE     @@ATTR_INCR
        JR      R5 ; RETURN

@@ATTR_INCR:
        INCR    R0                      ; ddd++
        B       @@ATTR_BODY

;------------------------------------------------------------------------------
; static void parse_mapping_line(const char *line)
;   R0 = line

@@PARSE_MAPPING_LINE:
        PSHR    R5
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PML_SSTART_LO
        MVO     R1, PML_SSTART_HI
        MVI     PML_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        MOVR    R0, R3
        MVI@    R3, R3
        CMPI    #'-', R3
        BNE     @@PML_RET

        INCR    R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PML_SEND_LO
        MVO     R1, PML_SEND_HI

        MVI     PML_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        MOVR    R0, R3
        MVI@    R3, R3
        CMPI    #'=', R3
        BNE     @@PML_RET

        INCR    R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PML_DEST            ; dest is 16-bit: keep low word only
        MVI     PML_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        MVII    #STR_PAGE, R1
        MVII    #4, R2
        JSR     R5, @@PARSE_CMPLOW
        TSTR    R0
        BNEQ    @@PML_ELSE

        MVI     PML_P, R0
        ADDI    #4, R0
        JSR     R5, @@SKIP_SPACE
        JSR     R5, @@PARSE_HEXDIGIT
        MVO     R0, REG_DEST_PAGE ; Set current page

        ; mapping_page(src_start,src_end,dest)
        MVI     PML_SSTART_LO, R0
        MVI     PML_SSTART_HI, R1
        MVI     PML_SEND_LO, R2
        MVI     PML_SEND_HI, R3
        MVI     PML_DEST, R4
        JSR     R5, @@MAPPING_PAGE
        B       @@PML_RET
@@PML_ELSE:
        ; mapping(src_start,src_end,dest)
        MVI      PML_SSTART_LO, R0
        MVI      PML_SSTART_HI, R1
        MVI      PML_SEND_LO, R2
        MVI      PML_SEND_HI, R3
        MVI      PML_DEST, R4
        JSR      R5, @@MAPPING
@@PML_RET:
        PULR     PC

;------------------------------------------------------------------------------
; static void parse_ecsbank_line(const char *line)
;   R0 = line

@@PARSE_ECSBANK_LINE:
        PSHR    R5
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        JSR     R5, @@PARSE_HEXDIGIT
        MVO     R0, REG_DEST_PAGE ; Set current page
        
        MVI     PML_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PML_SSTART_LO
        MVO     R1, PML_SSTART_HI

        MVI     PML_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P

        MOVR    R0, R2
        MVI@    R2, R3
        CMPI    #'-', R3
        BNE     @@PEL_RET

        INCR    R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PML_SEND_LO
        MVO     R1, PML_SEND_HI
        
        MVI     PML_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PML_P
        MOVR    R0, R2
        MVI@    R2, R3
        CMPI    #'=', R3
        BNE     @@PEL_RET
        INCR    R0
        JSR     R5, @@SKIP_SPACE
        JSR     R5, @@PARSE_NUMBER
        MOVR    R0, R4 ; DEST
        MVI     PML_SSTART_LO, R0
        MVI     PML_SSTART_HI, R1
        MVI     PML_SEND_LO, R2
        MVI     PML_SEND_HI, R3
        JSR     R5, @@MAPPING_PAGE
@@PEL_RET:
        PULR    PC

;------------------------------------------------------------------------------
; static void parse_vars_line(const char *line)
;   R0 = line

@@PARSE_VARS_LINE:
        PSHR    R5
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PVL_Q
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MOVR    R0, R3
        MVI@    R3, R3
        CMPI    #'=', R3
        BNE     @@PVL_RET

        INCR    R0
        JSR     R5, @@SKIP_SPACE
        JSR     R5, @@PARSE_NUMBER
        MOVR    R0, R1
        MVI     PVL_Q, R0
        JSR     R5, @@VARS_FN
@@PVL_RET:
        PULR    PC

;------------------------------------------------------------------------------
; static void parse_memattr_line(const char *line)
;   R0 = line

@@PARSE_MEMATTR_LINE:
        PSHR    R5
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PMA_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PMA_DSTART
        MVI     PMA_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PMA_P
        MOVR    R0, R2
        MVI@    R2, R3
        CMPI    #'-', R3
        BNE     @@PMA_RET

        INCR    R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PMA_P
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PMA_DEND
        MVI     PMA_P, R0
        JSR     R5, @@TOKEN_END
        JSR     R5, @@SKIP_SPACE
        MOVR    R0, R2
        MVI@    R2, R3
        CMPI    #'=', R3
        BNE     @@PMA_RET

        INCR    R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PMA_P


        MVII    #STR_RAM, R1
        MVII    #3, R2
        JSR     R5, @@PARSE_CMPLOW
        TSTR    R0
        BNEQ    @@PMA_RET

        MVI     PMA_P, R0
        ADDI    #3, R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PMA_P

@@PMA_WIDTHP:
        MVI     PMA_P, R0
        JSR     R5, @@PARSE_NUMBER
        MVO     R0, PMA_WIDTH
        CMPI    #8, R0
        BEQ     @@PMA_CALL
        CMPI    #16, R0
        BEQ     @@PMA_CALL
        B       @@PMA_RET

@@PMA_CALL:
        MVI     PMA_DSTART, R0
        MVI     PMA_DEND, R1
        MVI     PMA_WIDTH, R2
        JSR     R5, @@ATTR_RAM
@@PMA_RET:
        PULR    PC

;------------------------------------------------------------------------------
; void parse_buffer(char *buf)
;   R0 = buf

@@PARSE_BUFFER:
        PSHR    R5

        ; -----------------------------
        ; Strip comments: turn everything from ';' to EOL into space
        MVII    #BACKTAB, R3
        MVO     R3, DPOS
        MOVR    R0, R2                  ; R2 = p
        MVO     R0, PB_LINE             ; LINE = BUF
        CLRR    R1                      ; R1 = ISCO
        MVO     R1, PB_SECTION          ; section = 0
        
        ; R0 = Start pointer
        ; R1 = IsComment flag
        ; R2 = Current pointer
        ; R3 = Temp
        ; R4 = Temp

@@PB_CCLOOP:
        
        MVI@    R2, R4                 ; R4 = *p
        TSTR    R4
        BEQ     @@PB_DOLOOPP
        CMPI    #';', R4
        BNEQ    @@PB_CC2
        MVII    #1, R1
@@PB_CC2:
        CMPI    #10, R4                 ; '\n'
        BEQ     @@PB_CC4

        CMPI    #13, R4                 ; '\r'
        BNEQ    @@PB_CC5
@@PB_CC4:
        CLRR    R1              ; IsComment = 0 after CR or LF
        MVI     DPOS, R3
        MVII    #CARD_MINUS,R4
        MVO@    R4,R3           ; Write "-" into BACKTAB RAM
        INCR    R3
        MVO     R3, DPOS
@@PB_CC5:
        TSTR    R1
        BEQ     @@PB_CCNEXT

        MVII    #' ', R3
        MVO@    R3, R2
@@PB_CCNEXT:
        INCR    R2
        B       @@PB_CCLOOP

        ; -----------------------------
        ; Main line loop
@@PB_DOLOOPP:
        MVII    #BACKTAB, R3
        MVO     R3, DPOS

        MVI     PB_LINE, R2
@@PB_DOLOOP:        
        MVI@    R2, R3
        TSTR    R3
        BEQ     @@PB_END                ; !*line -> break
        ; Look for end of line
@@PB_EOLLOOP:
        MVI@    R2, R3
        TSTR    R3
        BEQ     @@PB_EOLDONE
        CMPI    #10, R3
        BEQ     @@PB_EOLDONE
        CMPI    #13, R3
        BEQ     @@PB_EOLDONE
        INCR    R2
        B       @@PB_EOLLOOP

@@PB_EOLDONE:
        MVI     DPOS, R4
        MVII    #CARD_PLUS,R0
        MVO@    R0,R4           ; R4++ Write "+" into BACKTAB RAM
        MVO     R4, DPOS

        MOVR    R2, R4  ; R4 = EOL = position CR en fin de ligne
        INCR    R2
        MVO     R2, PB_NLINE            ; nline = eol+1

        ; Skip spaces at start of line
        MVI     PB_LINE, R0
        JSR     R5, @@SKIP_SPACE
        MVO     R0, PB_LINE
        
        ; Test Empty line
        CMPR    R4, R0
        BNE     @@PB_HEADER
        ;MVI     DPOS, R4
        ;MVII    #CARD_PLUS,R0
        ;MVO@    R0,R4           ; R4++ Write "+" into BACKTAB RAM
        ;MVO     R4, DPOS        
        B       @@PB_ADVANCE

@@PB_HEADER
        MOVR    R0, R2
        MVI@    R2, R3
        CMPI    #'[', R3
        BNEQ    @@PB_DISPATCH
        INCR    R0
        MVO     R0, PB_LINE

        ; Section header
        MVII    #STR_MAPPING, R1
        MVII    #7, R2
        JSR     R5, @@PARSE_CMPLOW
        TSTR    R0
        BNEQ    @@PB_SEC2
        MVII    #MAPPING, R3
        MVO     R3, PB_SECTION
        B       @@PB_ADVANCE
@@PB_SEC2:
        MVI     PB_LINE, R0
        MVII    #STR_ECSBANK, R1
        MVII    #7, R2
        JSR     R5, @@PARSE_CMPLOW
        TSTR    R0
        BNEQ    @@PB_SEC3
        MVII    #ECSBANK, R3
        MVO     R3, PB_SECTION
        B       @@PB_ADVANCE
@@PB_SEC3:
        MVI     PB_LINE, R0
        MVII    #STR_MEMATTR, R1
        MVII    #7, R2
        JSR     R5, @@PARSE_CMPLOW
        TSTR    R0
        BNEQ    @@PB_SEC4
        MVII    #MEMATTR, R3
        MVO     R3, PB_SECTION
        B       @@PB_ADVANCE
@@PB_SEC4:
        MVI     PB_LINE, R0
        MVII    #STR_VARS, R1
        MVII    #4, R2
        JSR     R5, @@PARSE_CMPLOW
        TSTR    R0
        BNEQ    @@PB_SEC0
        MVII    #VARS, R3
        MVO     R3, PB_SECTION
        B       @@PB_ADVANCE
@@PB_SEC0:
        CLRR    R3
        MVO     R3, PB_SECTION
        B       @@PB_ADVANCE

        ; Dispatch to the section-specific line parser
@@PB_DISPATCH:
        MVI     PB_LINE, R0
        MVI     PB_SECTION, R3

        CMPI    #MAPPING, R3
        BNEQ    @@PB_D2
        JSR     R5, @@PARSE_MAPPING_LINE
        B       @@PB_ADVANCE
@@PB_D2:
        CMPI    #VARS, R3
        BNEQ    @@PB_D3
        JSR     R5, @@PARSE_VARS_LINE
        B       @@PB_ADVANCE
@@PB_D3:
        CMPI    #MEMATTR, R3
        BNEQ    @@PB_D4
        JSR     R5, @@PARSE_MEMATTR_LINE
        B       @@PB_ADVANCE
@@PB_D4:
        CMPI    #ECSBANK, R3
        BNEQ    @@PB_ADVANCE    ; Skip unmatched lines
        JSR     R5, @@PARSE_ECSBANK_LINE
        B       @@PB_ADVANCE

@@PB_ADVANCE:
        MVI     PB_NLINE, R2
        MVO     R2, PB_LINE
        B       @@PB_DOLOOP
@@PB_END:
        PULR    PC

