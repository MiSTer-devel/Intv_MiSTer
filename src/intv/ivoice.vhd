--------------------------------------------------------------------------------
-- Intellivision Voice
--------------------------------------------------------------------------------
-- DO 9/2019
--------------------------------------------------------------------------------
-- VHDL-1993
--------------------------------------------------------------------------------
-- Intellivoice

-- Developed with the help of the JZINTV emulator
--------------------------------------------------------------------------------
-- CLKSYS NTSC : 3.579545MHz * 12
-- CLKSYS PAL  : 4MHz * 12

-- TICK = CLKSYS / 12
-- DIVI NTSC : 358 => 9.9987kHz
-- DIVI PAL  : 400 => 10kHz

--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY work;
USE work.base_pack.ALL;
USE work.rom_pack.ALL;

ENTITY ivoice IS
  PORT (
    --------------------------
    ad        : IN  uv16;
    dw        : IN  uv16;
    dr        : OUT uv16;
    wr        : IN  std_logic;
    tick_cpu  : IN  std_logic;
    
    --------------------------
    tick      : IN  std_logic; -- 3.58MHz ... 4MHz
    divi      : in  natural;   -- 358 ... 400
    
    sound     : OUT sv16;
    --------------------------
    clksys    : IN  std_logic; --- 43MHz ... 48MHz
    reset_na  : IN std_logic
    );
END ENTITY ivoice;

ARCHITECTURE rtl OF ivoice IS

  CONSTANT AM : natural :=0;  -- Amplitude
  CONSTANT PR : natural :=1;  -- Pitch period
  CONSTANT B0 : natural :=2;  -- B0 filter coeff.
  CONSTANT F0 : natural :=3;  -- F0 filter coeff.
  CONSTANT B1 : natural :=4;
  CONSTANT F1 : natural :=5;
  CONSTANT B2 : natural :=6;
  CONSTANT F2 : natural :=7;
  CONSTANT B3 : natural :=8;
  CONSTANT F3 : natural :=9;
  CONSTANT B4 : natural :=10;
  CONSTANT F4 : natural :=11;
  CONSTANT B5 : natural :=12;
  CONSTANT F5 : natural :=13;
  CONSTANT IA : natural :=14; -- Interpolation amplitude
  CONSTANT IP : natural :=15; -- Interpolation pitch

  TYPE arr_string8 IS ARRAY(natural RANGE <>) OF string(1 TO 8);
  CONSTANT OP_TXT : arr_string8(0 TO 15):= (
    "RTS_PAGE","LOADALL ","LOAD_2  ","SETMSB_3",
    "LOAD_4  ","SETMSB_5","SETMSB_6","JMP     ",
    "SETMODE ","DELTA_9 ","SETMSB_A","JSR     ",
    "LOAD_C  ","DELTA_D ","LOAD_E  ","PAUSE   ");
  SIGNAL optxt : string(1 TO 8);
  
  TYPE enum_state IS (
    sIDLE,sDECODE1,sDECODE2,sDECODE3,sMICROCODE,
    sGENE1,sGENE2,sGENE3,sGENE4,
    sCALC01,sCALC02,sCALC11,sCALC12,sCALC21,sCALC22,
    sCALC31,sCALC32,sCALC41,sCALC42,sCALC51,sCALC52,
    sSOUND);
  SIGNAL state,state2 : enum_state;
  
  TYPE enum_op  IS (RTS,SETMODE,JSR,JMP,
                    LREG,DELTA,FIELD,CLR);
  TYPE type_micro IS RECORD
    op   : enum_op;
    len  : uint4; -- Length
    shi  : uint3; -- Left shift amount
    reg  : uint4; -- Updated parameter
    last : uint1; -- End of instruction
  END RECORD;
  TYPE arr_micro IS ARRAY(natural range <>) of type_micro;
  SIGNAL micro: type_micro;
  
  FUNCTION bswap(v : unsigned) RETURN unsigned IS
    VARIABLE u,x: unsigned(0 TO v'length-1) :=v;
  BEGIN
    FOR i IN 0 TO v'length-1 LOOP
      x(v'length-1-i):=u(i);
    END LOOP;
    return x;
  END FUNCTION;
  
  CONSTANT MICROCODE : arr_micro := (
    -- --------------------------------------------------------------------
    --  OPCODE 0000: RTS / SETPAGE. No parameter
    -- --------------------------------------------------------------------
    -- ($00 $01 $02 $03)
    (RTS  , 0, 0, AM, 1), -- @ RTS
    
    -- --------------------------------------------------------------------
    --  Opcode 0001: LOADALL
    -- --------------------------------------------------------------------
    -- MODE 00 & 10 ($10 $12)
    (LREG , 8, 0, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 8, 0, B0, 0), -- @ B0
    (LREG , 8, 0, F0, 0), -- @ F0
    (LREG , 8, 0, B1, 0), -- @ B1
    (LREG , 8, 0, F1, 0), -- @ F1
    (LREG , 8, 0, B2, 0), -- @ B2
    (LREG , 8, 0, F2, 0), -- @ F2
    (LREG , 8, 0, B3, 0), -- @ B3
    (LREG , 8, 0, F3, 0), -- @ F3
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- MODE 01 and 11 ($11 $13)
    (LREG , 8, 0, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 8, 0, B0, 0), -- @ B0
    (LREG , 8, 0, F0, 0), -- @ F0
    (LREG , 8, 0, B1, 0), -- @ B1
    (LREG , 8, 0, F1, 0), -- @ F1
    (LREG , 8, 0, B2, 0), -- @ B2
    (LREG , 8, 0, F2, 0), -- @ F2
    (LREG , 8, 0, B3, 0), -- @ B3
    (LREG , 8, 0, F3, 0), -- @ F3
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (LREG , 8, 0, IA, 0), -- @ Amp Interp
    (LREG , 8, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  Opcode 0010: LOAD_2  Mode 00 and 10
    -- --------------------------------------------------------------------
    -- MODE 00 ($20)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 3, 4, B0, 0), -- @ B0[6:4] (S=0)
    (LREG , 5, 3, F0, 0), -- @ F0[7:3]
    (LREG , 3, 4, B1, 0), -- @ B1[6:4] (S=0)
    (LREG , 5, 3, F1, 0), -- @ F1[7:3]
    (LREG , 3, 4, B2, 0), -- @ B2[6:4] (S=0)
    (LREG , 5, 3, F2, 0), -- @ F2[7:3]
    (LREG , 4, 3, B3, 0), -- @ B3[6:3] (S=0)
    (LREG , 6, 2, F3, 0), -- @ F3[7:2]
    (LREG , 7, 1, B4, 0), -- @ B4[7:1]
    (LREG , 6, 2, F4, 0), -- @ F4[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- MODE 01 ($21)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 3, 4, B0, 0), -- @ B0[6:4] (S=0)
    (LREG , 5, 3, F0, 0), -- @ F0[7:3]
    (LREG , 3, 4, B1, 0), -- @ B1[6:4] (S=0)
    (LREG , 5, 3, F1, 0), -- @ F1[7:3]
    (LREG , 3, 4, B2, 0), -- @ B2[6:4] (S=0)
    (LREG , 5, 3, F2, 0), -- @ F2[7:3]
    (LREG , 4, 3, B3, 0), -- @ B3[6:3] (S=0)
    (LREG , 6, 2, F3, 0), -- @ F3[7:2]
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 6, 2, F4, 0), -- @ F4[7:2]
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- MODE 10 ($22)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 6, 1, B0, 0), -- @ B0[6:1] (S=0)
    (LREG , 6, 2, F0, 0), -- @ F0[7:2]
    (LREG , 6, 1, B1, 0), -- @ B1[6:1] (S=0)
    (LREG , 6, 2, F1, 0), -- @ F1[7:2]
    (LREG , 6, 1, B2, 0), -- @ B2[6:1] (S=0)
    (LREG , 6, 2, F2, 0), -- @ F2[7:2]
    (LREG , 6, 1, B3, 0), -- @ B3[6:1] (S=0)
    (LREG , 7, 1, F3, 0), -- @ F3[7:1]
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- MODE 11 ($23)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 6, 1, B0, 0), -- @ B0[6:1] (S=0)
    (LREG , 6, 2, F0, 0), -- @ F0[7:2]
    (LREG , 6, 1, B1, 0), -- @ B1[6:1] (S=0)
    (LREG , 6, 2, F1, 0), -- @ F1[7:2]
    (LREG , 6, 1, B2, 0), -- @ B2[6:1] (S=0)
    (LREG , 6, 2, F2, 0), -- @ F2[7:2]
    (LREG , 6, 1, B3, 0), -- @ B3[6:1] (S=0)
    (LREG , 7, 1, F3, 0), -- @ F3[7:1]
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    	
    -- --------------------------------------------------------------------
    --  Opcode 0011: SETMSB_3
    -- --------------------------------------------------------------------
    -- MODE 00 ($30)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 5, 3, F0, 0), -- @ F0[7:3]
    (FIELD, 5, 3, F1, 0), -- @ F1[7:3]
    (FIELD, 5, 3, F2, 0), -- @ F2[7:3]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- MODE 01 ($31)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 5, 3, F0, 0), -- @ F0[7:3]
    (FIELD, 5, 3, F1, 0), -- @ F1[7:3]
    (FIELD, 5, 3, F2, 0), -- @ F2[7:3]
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- MODE 10 ($32)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 6, 2, F0, 0), -- @ F0[7:2]
    (FIELD, 6, 2, F1, 0), -- @ F1[7:2]
    (FIELD, 6, 2, F2, 0), -- @ F2[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- MODE 11 ($33)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 6, 2, F0, 0), -- @ F0[7:2]
    (FIELD, 6, 2, F1, 0), -- @ F1[7:2]
    (FIELD, 6, 2, F2, 0), -- @ F2[7:2]
    (LREG , 5, 0, IA, 0), -- @ Ampl. Intr.
    (LREG , 5, 0, IP, 1), -- @ Per. Intr.
    
    -- --------------------------------------------------------------------
    --  Opcode 0100: LOAD_4
    -- --------------------------------------------------------------------
    -- MODE 00 ($40)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (CLR  , 0, 0, B0, 0), -- @ B0=0
    (CLR  , 0, 0, F0, 0), -- @ F0=0
    (CLR  , 0, 0, B1, 0), -- @ B1=0
    (CLR  , 0, 0, F1, 0), -- @ F1=0
    (CLR  , 0, 0, B2, 0), -- @ B2=0
    (CLR  , 0, 0, F2, 0), -- @ F2=0
    (LREG , 4, 3, B3, 0), -- @ B3 (S=0)
    (LREG , 6, 2, F3, 0), -- @ F3[7:2]
    (LREG , 7, 1, B4, 0), -- @ B4[7:1]
    (LREG , 6, 2, F4, 0), -- @ F4[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp

    -- MODE 01 ($41)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (CLR  , 0, 0, B0, 0), -- @ B0=0
    (CLR  , 0, 0, F0, 0), -- @ F0=0
    (CLR  , 0, 0, B1, 0), -- @ B1=0
    (CLR  , 0, 0, F1, 0), -- @ F1=0
    (CLR  , 0, 0, B2, 0), -- @ B2=0
    (CLR  , 0, 0, F2, 0), -- @ F2=0
    (LREG , 4, 3, B3, 0), -- @ B3 (S=0)
    (LREG , 6, 2, F3, 0), -- @ F3[7:2]
    (LREG , 7, 1, B4, 0), -- @ B4[7:1]
    (LREG , 6, 2, F4, 0), -- @ F4[7:2]
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- MODE 10 ($42)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (CLR  , 0, 0, B0, 0), -- @ B0=0
    (CLR  , 0, 0, F0, 0), -- @ F0=0
    (CLR  , 0, 0, B1, 0), -- @ B1=0
    (CLR  , 0, 0, F1, 0), -- @ F1=0
    (CLR  , 0, 0, B2, 0), -- @ B2=0
    (CLR  , 0, 0, F2, 0), -- @ F2=0
    (LREG , 6, 1, B3, 0), -- @ B3 (S=0)
    (LREG , 7, 1, F3, 0), -- @ F3[7:1]
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- MODE 11 ($43)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (CLR  , 0, 0, B0, 0), -- @ B0=0
    (CLR  , 0, 0, F0, 0), -- @ F0=0
    (CLR  , 0, 0, B1, 0), -- @ B1=0
    (CLR  , 0, 0, F1, 0), -- @ F1=0
    (CLR  , 0, 0, B2, 0), -- @ B2=0
    (CLR  , 0, 0, F2, 0), -- @ F2=0
    (LREG , 6, 1, B3, 0), -- @ B3 (S=0)
    (LREG , 7, 1, F3, 0), -- @ F3[7:1]
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  Opcode 0101: SETMSB_5
    -- --------------------------------------------------------------------
    -- MODE 00 ($50)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 5, 3, F0, 0), -- @ F0[7:3]
    (FIELD, 5, 3, F1, 0), -- @ F1[7:3]
    (FIELD, 5, 3, F2, 0), -- @ F2[7:3]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- MODE 01 ($51)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 5, 3, F0, 0), -- @ F0[7:3]
    (FIELD, 5, 3, F1, 0), -- @ F1[7:3]
    (FIELD, 5, 3, F2, 0), -- @ F2[7:3]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- MODE 10 ($52)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 6, 2, F0, 0), -- @ F0[7:2]
    (FIELD, 6, 2, F1, 0), -- @ F1[7:2]
    (FIELD, 6, 2, F2, 0), -- @ F2[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- MODE 11 ($53)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (FIELD, 6, 2, F0, 0), -- @ F0[7:2]
    (FIELD, 6, 2, F1, 0), -- @ F1[7:2]
    (FIELD, 6, 2, F2, 0), -- @ F2[7:2]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
	
    -- --------------------------------------------------------------------
    --  Opcode 0110: SETMSB_6
    -- --------------------------------------------------------------------
    -- MODE 00 ($60)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (FIELD, 6, 2, F3, 0), -- @ F3[7:2]
    (FIELD, 6, 2, F4, 0), -- @ F4[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 01 ($61)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (FIELD, 6, 2, F3, 0), -- @ F3[7:2]
    (FIELD, 6, 2, F4, 0), -- @ F4[7:2]
    (FIELD, 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 10 ($62)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (FIELD, 7, 1, F3, 0), -- @ F3[7:2]
    (FIELD, 8, 0, F4, 0), -- @ F4
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 11 ($63)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (FIELD, 7, 1, F3, 0), -- @ F3[7:1]
    (FIELD, 8, 0, F4, 0), -- @ F4
    (FIELD, 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  OPCODE 0111: JMP  LLLLLLLL . Jump to 12-bit/16-bit Abs Addr
    -- --------------------------------------------------------------------
    -- ($70 $71 $72 $73)
    (JMP  , 8, 0, AM,  1),   -- @
    -- --------------------------------------------------------------------
    --  OPCODE 1000: SETMODE . Set the Mode and Repeat MSBs. No parameter
    -- --------------------------------------------------------------------
    -- ($80 $81 $82 $83)
    (SETMODE, 0, 0, AM, 1), -- @
    
    -- --------------------------------------------------------------------
    --  Opcode 1001: DELTA_9
    -- --------------------------------------------------------------------
    -- Mode 00 ($90)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude
    (DELTA, 5, 0, PR, 0), -- @ Period
    (DELTA, 3, 4, B0, 0), -- @ B0[6:4]
    (DELTA, 3, 3, F0, 0), -- @ F0[5:3]
    (DELTA, 3, 4, B1, 0), -- @ B1[6:4]
    (DELTA, 3, 3, F1, 0), -- @ F1[5:3]
    (DELTA, 3, 4, B2, 0), -- @ B2[6:4]
    (DELTA, 3, 3, F2, 0), -- @ F2[5:3]
    (DELTA, 3, 3, B3, 0), -- @ B3[5:3]
    (DELTA, 4, 2, F3, 0), -- @ F3[5:2]
    (DELTA, 4, 1, B4, 0), -- @ B4[4:1]
    (DELTA, 4, 2, F4, 0), -- @ F4[5:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
	
    -- Mode 01 ($91)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude
    (DELTA, 5, 0, PR, 0), -- @ Period
    (DELTA, 3, 4, B0, 0), -- @ B0[6:4]
    (DELTA, 3, 3, F0, 0), -- @ F0[5:3]
    (DELTA, 3, 4, B1, 0), -- @ B1[6:4]
    (DELTA, 3, 3, F1, 0), -- @ F1[5:3]
    (DELTA, 3, 4, B2, 0), -- @ B2[6:4]
    (DELTA, 3, 3, F2, 0), -- @ F2[5:3]
    (DELTA, 3, 3, B3, 0), -- @ B3[5:3]
    (DELTA, 4, 2, F3, 0), -- @ F3[5:2]
    (DELTA, 4, 1, B4, 0), -- @ B4[4:1]
    (DELTA, 4, 2, F4, 0), -- @ F4[5:2]
    (DELTA, 5, 0, B5, 0), -- @ B5[4:0]
    (DELTA, 5, 0, F5, 0), -- @ F5[4:0]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
	
    -- Mode 10 ($92)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude
    (DELTA, 5, 0, PR, 0), -- @ Period
    (DELTA, 4, 1, B0, 0), -- @ B0[4:1]
    (DELTA, 4, 2, F0, 0), -- @ F0[5:2]
    (DELTA, 4, 1, B1, 0), -- @ B1[4:1]
    (DELTA, 4, 2, F1, 0), -- @ F1[5:2]
    (DELTA, 4, 1, B2, 0), -- @ B2[4:1]
    (DELTA, 4, 2, F2, 0), -- @ F2[5:2]
    (DELTA, 4, 1, B3, 0), -- @ B3[4:1]
    (DELTA, 5, 1, F3, 0), -- @ F3[5:1]
    (DELTA, 5, 0, B4, 0), -- @ B4[4:0]
    (DELTA, 5, 0, F4, 0), -- @ F4[4:0]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 11 ($93)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude
    (DELTA, 5, 0, PR, 0), -- @ Period
    (DELTA, 4, 1, B0, 0), -- @ B0[4:1]
    (DELTA, 4, 2, F0, 0), -- @ F0[5:2]
    (DELTA, 4, 1, B1, 0), -- @ B1[4:1]
    (DELTA, 4, 2, F1, 0), -- @ F1[5:2]
    (DELTA, 4, 1, B2, 0), -- @ B2[4:1]
    (DELTA, 4, 2, F2, 0), -- @ F2[5:2]
    (DELTA, 4, 1, B3, 0), -- @ B3[4:1]
    (DELTA, 5, 1, F3, 0), -- @ F3[5:1]
    (DELTA, 5, 0, B4, 0), -- @ B4[4:0]
    (DELTA, 5, 0, F4, 0), -- @ F4[4:0]
    (DELTA, 5, 0, B5, 0), -- @ B5[4:0]
    (DELTA, 5, 0, F5, 0), -- @ F5[4:0]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
	
    -- --------------------------------------------------------------------
    --  Opcode 1010: SETMSB_A
    -- --------------------------------------------------------------------
    -- Mode 00 ($A0)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (FIELD, 5, 3, F0, 0), -- @ F0[7:3]
    (FIELD, 5, 3, F1, 0), -- @ F1[7:3]
    (FIELD, 5, 3, F2, 0), -- @ F2[7:3]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 01 ($A1)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (FIELD, 5, 3, F0, 0), -- @ F0[7:3]
    (FIELD, 5, 3, F1, 0), -- @ F1[7:3]
    (FIELD, 5, 3, F2, 0), -- @ F2[7:3]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 10 ($A2)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (FIELD, 6, 2, F0, 0), -- @ F0[7:2]
    (FIELD, 6, 2, F1, 0), -- @ F1[7:2]
    (FIELD, 6, 2, F2, 0), -- @ F2[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 11 ($A3)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (FIELD, 6, 2, F0, 0), -- @ F0[7:2]
    (FIELD, 6, 2, F1, 0), -- @ F1[7:2]
    (FIELD, 6, 2, F2, 0), -- @ F2[7:2]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  OPCODE 1011: JSR  LLLLLLLL . Jump to Subroutine
    -- --------------------------------------------------------------------
    -- ($B0 $B1 $B2 $B3)
    (JSR  , 8, 0, AM, 1), -- @
    
    -- --------------------------------------------------------------------
    --  Opcode 1100: LOAD_C  Mode 00 and 10
    -- --------------------------------------------------------------------
    --  Mode 00 ($C0)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 3, 4, B0, 0), -- @ B0[6:4] (S=0)
    (LREG , 5, 3, F0, 0), -- @ F0[7:3]
    (LREG , 3, 4, B1, 0), -- @ B1[6:4] (S=0)
    (LREG , 5, 3, F1, 0), -- @ F1[7:3]
    (LREG , 3, 4, B2, 0), -- @ B2[6:4] (S=0)
    (LREG , 5, 3, F2, 0), -- @ F2[7:3]
    (LREG , 4, 3, B3, 0), -- @ B3[6:3] (S=0)
    (LREG , 6, 2, F3, 0), -- @ F3[7:2]
    (LREG , 7, 1, B4, 0), -- @ B4[7:1]
    (LREG , 6, 2, F4, 0), -- @ F4[7:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    --  Mode 01 ($C1)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 3, 4, B0, 0), -- @ B0[6:4] (S=0)
    (LREG , 5, 3, F0, 0), -- @ F0[7:3]
    (LREG , 3, 4, B1, 0), -- @ B1[6:4] (S=0)
    (LREG , 5, 3, F1, 0), -- @ F1[7:3]
    (LREG , 3, 4, B2, 0), -- @ B2[6:4] (S=0)
    (LREG , 5, 3, F2, 0), -- @ F2[7:3]
    (LREG , 4, 3, B3, 0), -- @ B3[6:3] (S=0)
    (LREG , 6, 2, F3, 0), -- @ F3[7:2]
    (LREG , 7, 1, B4, 0), -- @ B4[7:1]
    (LREG , 6, 2, F4, 0), -- @ F4[7:2]
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 10 ($C2)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 6, 1, B0, 0), -- @ B0[6:1] (S=0)
    (LREG , 6, 2, F0, 0), -- @ F0[7:2]
    (LREG , 6, 1, B1, 0), -- @ B1[6:1] (S=0)
    (LREG , 6, 2, F1, 0), -- @ F1[7:2]
    (LREG , 6, 1, B2, 0), -- @ B2[6:1] (S=0)
    (LREG , 6, 2, F2, 0), -- @ F2[7:2]
    (LREG , 6, 1, B3, 0), -- @ B3[6:1] (S=0)
    (LREG , 7, 1, F3, 0), -- @ F3[7:1]
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 11 ($C3)
    (LREG , 6, 2, AM, 0), -- @ Amplitude
    (LREG , 8, 0, PR, 0), -- @ Period
    (LREG , 6, 1, B0, 0), -- @ B0[6:1] (S=0)
    (LREG , 6, 2, F0, 0), -- @ F0[7:2]
    (LREG , 6, 1, B1, 0), -- @ B1[6:1] (S=0)
    (LREG , 6, 2, F1, 0), -- @ F1[7:2]
    (LREG , 6, 1, B2, 0), -- @ B2[6:1] (S=0)
    (LREG , 6, 2, F2, 0), -- @ F2[7:2]
    (LREG , 6, 1, B3, 0), -- @ B3[6:1] (S=0)
    (LREG , 7, 1, F3, 0), -- @ F3[7:1]
    (LREG , 8, 0, B4, 0), -- @ B4
    (LREG , 8, 0, F4, 0), -- @ F4
    (LREG , 8, 0, B5, 0), -- @ B5
    (LREG , 8, 0, F5, 0), -- @ F5
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  OPCODE 1101: DELTA_D
    -- --------------------------------------------------------------------
    -- Mode 00 ($D0)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude [5:2]
    (DELTA, 5, 0, PR, 0), -- @ Period [4:0]
    (DELTA, 3, 3, B3, 0), -- @ B3[5:3]
    (DELTA, 4, 2, F3, 0), -- @ F3[5:2]
    (DELTA, 4, 1, B4, 0), -- @ B4[4:1]
    (DELTA, 4, 2, F4, 0), -- @ F4[5:2]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 01 ($D1)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude [5:2]
    (DELTA, 5, 0, PR, 0), -- @ Period [4:0]
    (DELTA, 3, 3, B3, 0), -- @ B3[5:3]
    (DELTA, 4, 2, F3, 0), -- @ F3[5:2]
    (DELTA, 4, 1, B4, 0), -- @ B4[5:1]
    (DELTA, 4, 2, F4, 0), -- @ F4[5:2]
    (DELTA, 5, 0, B5, 0), -- @ B5[4:0]
    (DELTA, 5, 0, F5, 0), -- @ F5[4:0]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
	
    -- Mode 10 ($D2)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude [5:2]
    (DELTA, 5, 0, PR, 0), -- @ Period [4:0]
    (DELTA, 4, 1, B3, 0), -- @ B3[5:1]
    (DELTA, 5, 1, F3, 0), -- @ F3[6:1]
    (DELTA, 5, 0, B4, 0), -- @ B4[4:0]
    (DELTA, 5, 0, F4, 0), -- @ F4[4:0]
    (CLR  , 0, 0, B5, 0), -- @ B5=0
    (CLR  , 0, 0, F5, 0), -- @ F5=0
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- Mode 11 ($D3)
    (DELTA, 4, 2, AM, 0), -- @ Amplitude [5:2]
    (DELTA, 5, 0, PR, 0), -- @ Period [4:0]
    (DELTA, 4, 1, B3, 0), -- @ B3[5:1]
    (DELTA, 5, 1, F3, 0), -- @ F3[6:1]
    (DELTA, 5, 0, B4, 0), -- @ B4[4:0]
    (DELTA, 5, 0, F4, 0), -- @ F4[4:0]
    (DELTA, 5, 0, B5, 0), -- @ B5[4:0]
    (DELTA, 5, 0, F5, 0), -- @ F5[4:0]
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  OPCODE 1110: LOAD_E
    -- --------------------------------------------------------------------
    -- ($E0 $E1 $E2 $E3)
    (LREG , 6, 2, AM, 0), -- @ Amplitude [7:2]
    (LREG , 8, 0, PR, 0), -- @ Period
    (CLR  , 0, 0, IA, 0), -- @ Amp Interp
    (CLR  , 0, 0, IP, 1), -- @ Pit Interp
    
    -- --------------------------------------------------------------------
    --  OPCODE 1111: PAUSE
    -- --------------------------------------------------------------------
    -- ($F0 $F1 $F2 $F3)
    (CLR  , 0, 0, AM, 1), -- @
    -- --------------------------------------------------------------------
    (CLR  , 0, 0, AM, 1) -- @
    );
  
  TYPE arr_uint9 IS ARRAY(natural RANGE <>) OF uint9;
  CONSTANT CODE_INDEX : arr_uint9(0 TO 16*4-1) := (
    0   ,0   ,0   ,0   ,1   ,17  ,1   ,17  ,
    33  ,49  ,66  ,82  ,98  ,107 ,114 ,123 ,
    130 ,146 ,162 ,178 ,194 ,203 ,210 ,219 ,
    226 ,233 ,239 ,246 ,252 ,252 ,252 ,252 ,
    253 ,253 ,253 ,253 ,254 ,270 ,286 ,302 ,
    318 ,326 ,332 ,340 ,346 ,346 ,346 ,346 ,
    347 ,363 ,379 ,395 ,411 ,421 ,431 ,441 ,
    451 ,451 ,451 ,451 ,455 ,455 ,455 ,455 );
  
  CONSTANT ROMVOICE : arr8 := INIT_VOICE; -- ROM
  
  TYPE arr_uv10 IS ARRAY(natural RANGE <>) OF uv10;
  SIGNAL fifo : arr_uv10(0 TO 63);
  SIGNAL fifo_lev : uint6;
  SIGNAL fifo_v,fifo_v2,push,pop,pop2 : std_logic;
  SIGNAL fifo_rd,fifo_rd2,fifo_wd : uv10;
  SIGNAL repeat : uv6;
  SIGNAL regs : arr_uv8(0 TO 15);
  SIGNAL index,istart : uint9;

  SIGNAL pc,ret_pc : uv19;
  SIGNAL ret_val,nexti : boolean;
  SIGNAL reset : std_logic;
  
  -- Coefficient Quantization Table.
  CONSTANT QTABLE : arr_uint9 := (
    0,   9,   17,  25,  33,  41,  49,  57,
    65,  73,  81,  89,  97,  105, 113, 121,
    129, 137, 145, 153, 161, 169, 177, 185,
    193, 201, 209, 217, 225, 233, 241, 249,
    257, 265, 273, 281, 289, 297, 301, 305,
    309, 313, 317, 321, 325, 329, 333, 337,
    341, 345, 349, 353, 357, 361, 365, 369,
    373, 377, 381, 385, 389, 393, 397, 401,
    405, 409, 413, 417, 421, 425, 427, 429,
    431, 433, 435, 437, 439, 441, 443, 445,
    447, 449, 451, 453, 455, 457, 459, 461,
    463, 465, 467, 469, 471, 473, 475, 477,
    479, 481, 482, 483, 484, 485, 486, 487,
    488, 489, 490, 491, 492, 493, 494, 495,
    496, 497, 498, 499, 500, 501, 502, 503,
    504, 505, 506, 507, 508, 509, 510, 511);

  FUNCTION amp_decode(a : uv8) RETURN integer IS
    VARIABLE ampl : uv12 :=(OTHERS =>'0');
    VARIABLE exp  : uint3;
  BEGIN
    exp:=to_integer(a(7 DOWNTO 5));
    ampl(exp+4 DOWNTO exp):=a(4 DOWNTO 0);
    RETURN to_integer(ampl);
  END FUNCTION;

  FUNCTION calc(samp  : int16;
                coef  : int10;
                zdata : int16;
                div   : int16) RETURN integer IS
    VARIABLE mul : signed(25 DOWNTO 0);
  BEGIN
    mul:=to_signed(coef,10) * to_signed(zdata,16);
    IF div=256 THEN
      mul := mul + (to_signed(samp,16) & "00000000");
      RETURN to_integer(mul(23 DOWNTO 8));
    ELSE -- div=512
      mul := mul + (to_signed(samp,16) & "000000000");
      RETURN to_integer(mul(24 DOWNTO 9));
    END IF;
  END FUNCTION calc;
  FUNCTION sat(i : integer) RETURN integer IS
  BEGIN
    IF i>32767 THEN RETURN i-65536; END IF;
    IF i<-32768 THEN RETURN i+65536; END IF;
    RETURN i;
  END FUNCTION;

  SIGNAL qreg : uv7;
  SIGNAL reg_coef : uint9;
  SIGNAL coef : int10;
  SIGNAL samp,samp2 : int16;
  SIGNAL zdata00,zdata01,zdata10,zdata11,zdata20,zdata21 : int16;
  SIGNAL zdata30,zdata31,zdata40,zdata41,zdata50,zdata51 : int16;
  SIGNAL lfsr : uv15;
  SIGNAL diff : uv8;
  SIGNAL count : uv8;
  SIGNAL tick2,tick3,tick4 : std_logic;
  SIGNAL divcpt : uint9;
  SIGNAL tickper : std_logic;
  SIGNAL noise : boolean;
  SIGNAL ald,ald_clr : std_logic;
  SIGNAL ald_ad : uv8;

  SIGNAL reg_dr,reg_dw : uv8;
  SIGNAL reg_wr : std_logic;
  SIGNAL reg_a : uint4;
  SIGNAL index_a : uint6;
  SIGNAL sig,silent,act : std_logic;
  SIGNAL mode : uv2;
  SIGNAL INST : uv8;
  SIGNAL page : uv4;
  SIGNAL fifoptr : uint5;
  SIGNAL romd : uv16;
  SIGNAL fifod : uv20;
  SIGNAL rom_a : uint16;
  SIGNAL rom_dr : uv8;
  SIGNAL fifomode : boolean;
  SIGNAL xxx_code : uv8;
  SIGNAL xxx_imm : uv8;
  
BEGIN
  
  ------------------------------------------------------------------------------
  -- Reg access
  Reg:PROCESS (clksys,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      fifo_v<='0';
      fifo_lev<=0;
      fifo_v2<='0';
      ald<='0';
    ELSIF rising_edge(clksys) THEN
      ------------------------------------------------------
      dr<=x"0000";
      IF ad=x"0080" THEN
        dr(15)<=NOT ald;
        IF wr='1' AND tick_cpu='1' AND ald='0' THEN
          -- Address Load
          ald_ad<=dw(7 DOWNTO 0);
          ald<='1';
        END IF;
      END IF;
      IF ald_clr='1' THEN
        ald<='0';
      END IF;
      
      ------------------------------------------------------
      push<='0';
      reset<='0';
      IF ad=x"0081" THEN 
        fifo_wd<=dw(9 DOWNTO 0);
        -- Speech FIFO. Write=Push data. Read = Get FIFO full flag
        push<=wr AND tick_cpu AND to_std_logic(fifo_lev<62);
        IF dw(10)='1' and wr='1' THEN
          reset<='1';
        END IF;
        dr(15)<=to_std_logic(fifo_lev>=62);
      END IF;
      
      ------------------------------------------------------
      pop<='0';
      
      IF push='1' THEN
        fifo<=fifo_wd & fifo(0 TO fifo'high-1);
      END IF;
      IF push='1' AND pop='0' THEN
        fifo_v<='1';
        IF fifo_v='1' THEN
          fifo_lev<=fifo_lev+1;
        END IF;
      ELSIF push='0' AND pop='1' THEN
        IF fifo_lev>0 THEN
          fifo_lev<=fifo_lev-1;
        ELSE
          fifo_v<='0';
        END IF;
      END IF;
      
      ------------------------------------------------------
      IF fifo_v='1' AND fifo_v2='0' THEN
        pop<='1';
        fifo_rd2<=fifo_rd;
        fifo_v2<='1';
      END IF;
      IF pop2='1' AND fifo_v='1' THEN
        pop<='1';
        fifo_rd2<=fifo_rd;
      END IF;
      IF pop2='1' AND fifo_v='0' THEN
        fifo_v2<='0';
      END IF;
      
      IF reset='1' THEN
        fifo_v<='0';
        fifo_v2<='0';
        fifo_lev<=0;
        ald<='0';
      END IF;
      
      ------------------------------------------------------
    END IF;
  END PROCESS;
  
  ------------------------------------------------------------------------------
  fifo_rd<=fifo(fifo_lev);
  fifod<=fifo_rd & fifo_rd2;
  
  -- Mem access
  rom_a<=to_integer(pc(18 DOWNTO 3)) WHEN tick2='1' ELSE
         to_integer(pc(18 DOWNTO 3))+1;
  
  rom_dr<=ROMVOICE(rom_a MOD 2048) WHEN rising_edge(clksys);

  istart<=CODE_INDEX(index_a) WHEN rising_edge(clksys);

  micro<=MICROCODE(index) WHEN rising_edge(clksys);

  reg_dr<=regs(reg_a) WHEN rising_edge(clksys);
  
  qreg<=reg_dr(6 DOWNTO 0) WHEN reg_dr(7)='0' ELSE
         ("0000000"-reg_dr(6 DOWNTO 0));
  
  reg_coef<=QTABLE(to_integer(qreg)) WHEN rising_edge(clksys);
  coef<=reg_coef WHEN reg_dr(7)='1' ELSE -reg_coef;

  PROCESS (clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF reg_wr='1' AND tick2='1' THEN
        regs(reg_a)<=reg_dw;
      END IF;
    END IF;
  END PROCESS;

  ------------------------------------------------------------------------------
  PROCESS (state2,micro) IS
  BEGIN
    reg_a<=micro.reg;
    CASE state2 IS
      WHEN sGENE1  => reg_a<=IP;
      WHEN sGENE2  => reg_a<=PR;
      WHEN sGENE3  => reg_a<=IA;
      WHEN sGENE4  => reg_a<=AM;
      WHEN sCALC01 => reg_a<=B0;
      WHEN sCALC02 => reg_a<=F0;
      WHEN sCALC11 => reg_a<=B1;
      WHEN sCALC12 => reg_a<=F1;
      WHEN sCALC21 => reg_a<=B2;
      WHEN sCALC22 => reg_a<=F2;
      WHEN sCALC31 => reg_a<=B3;
      WHEN sCALC32 => reg_a<=F3;
      WHEN sCALC41 => reg_a<=B4;
      WHEN sCALC42 => reg_a<=F4;
      WHEN sCALC51 => reg_a<=B5;
      WHEN sCALC52 => reg_a<=F5;
      WHEN OTHERS  => reg_a<=micro.reg;
    END CASE;
  END PROCESS;
  
  ------------------------------------------------------------------------------
  -- Sequencer
  Machine:PROCESS(clksys,reset_na) IS
    VARIABLE romd_v,fifod_v,imm_v,inst_v,code_v : uv8;
    VARIABLE tmp_v : uv16;
    VARIABLE len_v : uint4;
    VARIABLE rpc_v : uv19;
  BEGIN
    IF reset_na='0' THEN
      state<=sIDLE;
      pc<=(OTHERS =>'0');
      act<='0';
      repeat<="000000";
      mode<="00";
      page<=x"1";
      
    ELSIF rising_edge(clksys) THEN
      ------------------------------------------------------
      IF reset='1' THEN
        pc<=(OTHERS =>'0');
        act<='0';
        repeat<="000000";
        mode<="00";
        page<=x"1";
        lfsr<=to_unsigned(1,15);
      END IF;
      ------------------------------------------------------
      tick2<=tick;
      tick3<=tick2;
      tick4<=tick3;
      state2<=state;
      
      len_v:=0;
      pop2<='0';
      
      IF tick3='1' THEN
        romd(7 DOWNTO 0)<=rom_dr;
      ELSIF tick4='1' THEN
        romd(15 DOWNTO 8)<=rom_dr;
      END IF;
      
      romd_v:=romd(7+to_integer(pc(2 DOWNTO 0)) DOWNTO
                     to_integer(pc(2 DOWNTO 0)));
      fifod_v:=fifod(7+fifoptr DOWNTO fifoptr);
      
      code_v:=mux(fifomode,fifod_v,romd_v);
      xxx_code<=code_v;
      
      ------------------------------------------------------
      ald_clr<='0';
      
      IF tick='1' THEN
        IF divcpt+1=divi-1 THEN
          divcpt<=0;
          tickper<='1';
        ELSE
          divcpt<=divcpt+1;
          tickper<='0';
        END IF;
      END IF;
      
      -----------------------------------------------------
      IF tick='1' THEN -- 1MHz
        len_v:=0;
        reg_wr<='0';
        
        CASE state IS
          -------------------------------------------------
          WHEN sIDLE =>
            IF tickper='1' THEN -- 10KHz sampling rate
              IF act='1' AND nexti THEN
                state<=sDECODE1;
              ELSIF (repeat/="000000" AND repeat/="000001") AND act='1' THEN
                -- Continue current sound
                state<=sGENE1;
                repeat<=repeat-1;
              ELSIF act='1' THEN
                -- New sound, execute new instruction
                state<=sDECODE1;
              ELSE
                -- Waiting for command
                IF ald='1' THEN
                  -- Loaded address
                  ald_clr<='1';
                  act<='1';
                  state<=sDECODE1;
                  pc<="0000000" & ald_ad & "0000";
                  fifomode<=false;
                END IF;
              END IF;
            END IF;
            
            -------------------------------------------------  
          WHEN sDECODE1 =>
            len_v:=8;
            pc<=pc+len_v;
            inst_v:=bswap(code_v);
            index_a <=to_integer(inst_v(3 DOWNTO 0) & mode);
            optxt<=OP_TXT(to_integer(inst_v(3 DOWNTO 0)));
            inst<=inst_v;
            state<=sDECODE2;
            IF inst_v(7 DOWNTO 4)="0000" AND 
               inst_v(3 DOWNTO 0)/="0000" AND -- SETPAGE
               inst_v(3 DOWNTO 0)/="0111" AND -- JMP
               inst_v(3 DOWNTO 0)/="1000" AND -- SETMODE
               inst_v(3 DOWNTO 0)/="1011" AND -- JSR
               repeat(5 DOWNTO 4)="00" THEN
              state<=sGENE1; -- If Zero repeat, skip instruction
            END IF;
            IF inst_v(3 DOWNTO 0)/="0111" AND  -- JMP
               inst_v(3 DOWNTO 0)/="1011" AND  -- JSR
               inst_v(3 DOWNTO 0)/="0000" AND  -- SETPAGE
               inst_v(3 DOWNTO 0)/="1000" THEN -- SETMODE
              repeat(3 DOWNTO 0)<=bswap(inst_v(7 DOWNTO 4));
            END IF;
            
          WHEN sDECODE2 =>
            state<=sDECODE3;
            index<=istart;
            
          WHEN sDECODE3 =>
            -- Read index memory
            state<=sMICROCODE;
            
          WHEN sMICROCODE =>
            -- Execute Microcode
            len_v:=micro.len;
            pc<=pc+len_v;
            index<=index+1;
            
            CASE micro.op IS
              WHEN SETMODE =>
                nexti<=true;
                mode<=inst(5 DOWNTO 4);
                repeat(5 DOWNTO 4)<=inst(7 DOWNTO 6);
                
              WHEN JMP =>
                nexti<=true;
                pc<=page & inst(7 DOWNTO 4) & bswap(code_v) & "000";
                fifomode<=(page & inst(7 DOWNTO 4) & bswap(code_v)=x"1800");
                fifoptr<=0;
                
              WHEN JSR =>
                nexti<=true;
                pc<=page & inst(7 DOWNTO 4) & bswap(code_v) & "000";
                fifomode<=(page & inst(7 DOWNTO 4) & bswap(code_v)=x"1800");
                fifoptr<=0;
                ret_pc<=pc + len_v; -- Return address
                
                rpc_v:=pc + len_v + 7;
                ret_pc<=rpc_v(18 DOWNTO 3) & "000";
                
                ret_val <=true; -- return execution valid
                
              WHEN RTS =>
                nexti<=true;
                IF inst(7 DOWNTO 4)="0000" THEN -- RTS
                  IF NOT ret_val THEN
                    act<='0';
                  ELSE
                    fifomode<=(ret_pc(18 DOWNTO 3)=x"1800");
                    fifoptr<=0;
                    pc<=ret_pc;
                    ret_val<=false;
                  END IF;
                ELSE -- SETPAGE
                  page<=inst(7 DOWNTO 4);
                END IF;
                
              WHEN LREG | DELTA | FIELD =>
                nexti<=false;
                
                tmp_v:=x"0000";
                tmp_v(7+micro.shi DOWNTO micro.shi):=code_v;
                
                IF micro.op=LREG THEN
                  FOR i IN 0 TO 7 LOOP
                    IF i<micro.shi THEN
                      imm_v(i):='0';
                    ELSIF i<micro.shi+micro.len THEN
                      imm_v(i):=tmp_v(i);
                    ELSE
                      imm_v(i):='0';
                    END IF;
                  END LOOP;
                  reg_dw<=imm_v;
                  xxx_imm<=imm_v;
                  
                ELSIF micro.op=DELTA THEN
                  FOR i IN 0 TO 7 LOOP
                    IF i<micro.shi THEN
                      imm_v(i):='0';
                    ELSIF i<micro.shi+micro.len THEN
                      imm_v(i):=tmp_v(i);
                    ELSE
                      imm_v(i):=code_v(micro.len-1);
                    END IF;
                  END LOOP;
                  reg_dw<=reg_dr+imm_v;
                  xxx_imm<=imm_v;
                  
                ELSIF micro.op=FIELD THEN
                  FOR i IN 0 TO 7 LOOP
                    IF i<micro.shi THEN
                      imm_v(i):=reg_dr(i);
                    ELSIF i<micro.shi+micro.len THEN
                      imm_v(i):=tmp_v(i);
                    ELSE
                      imm_v(i):=reg_dr(i);
                    END IF;
                  END LOOP;
                  reg_dw<=imm_v;
                  xxx_imm<=imm_v;
                  
                END IF;
                reg_wr<='1';
                silent<='0';
                
              WHEN CLR =>
                nexti<=false;
                reg_dw<=x"00";
                reg_wr<='1';
                silent<='0';
                IF micro.reg=AM THEN
                  silent<='1';
                END IF;
                
            END CASE;
            
            IF micro.last=1 THEN
              state<=sGENE1;
            END IF;
            count<=x"00";
            
          -----------------------------------------------
          -- Sound Generator
          WHEN sGENE1 =>
            -- <IP>
            diff<=reg_dr;
            state<=sGENE2;
            
          WHEN sGENE2 =>
            -- <PR>
            lfsr<=('0' & lfsr(14 DOWNTO 1)) XOR
                   (lfsr(0) & "0000000000000" & lfsr(0));
            
            IF reg_dr=x"00" THEN
              -- Period = 0 : Noise generator
              noise<=true;
              IF count>0 THEN
                count<=count-1;
              ELSE
                count<=x"3F";
              END IF;
              sig<=lfsr(0);
            ELSE
              -- Pulse
              noise<=false;
              IF count>0 THEN
                count<=count-1;
                sig<='0';
              ELSE
                count<=reg_dr-1;
                sig<='1';
              END IF;
            END IF;
            reg_dw<=reg_dr+diff;
            reg_wr<=to_std_logic(diff/=0);
            state<=sGENE3;
            
          -----------------------------------------------
          WHEN sGENE3 =>
            -- <IA>
            diff<=reg_dr;
            state<=sGENE4;
            
          WHEN sGENE4 =>
            -- <AM>
            reg_dw<=reg_dr+diff;
            reg_wr<=to_std_logic(diff/=0);
            IF noise THEN
              samp<=mux(sig='0',-amp_decode(reg_dr),amp_decode(reg_dr));
            ELSE
              samp<=mux(sig='0',0,amp_decode(reg_dr));
            END IF;
            state<=sCALC01;
            
          -----------------------------------------------
          -- Filter Stage 0
          WHEN sCALC01 =>
            -- <B0>
            samp2<=samp;
            state<=sCALC02;
            samp   <=calc(samp,coef,zdata01,512);
            zdata01<=zdata00;
            
          WHEN sCALC02 =>
            -- <F0>
            state<=sCALC11;
            samp   <=calc(samp,coef,zdata00,256);
            zdata00<=calc(samp,coef,zdata00,256);
            
          -----------------------------------------------
          -- Filter Stage 1
          WHEN sCALC11 =>
            -- <B1>
            state<=sCALC12;
            samp   <=calc(samp,coef,zdata11,512);
            zdata11<=zdata10;
            
          WHEN sCALC12 =>
            -- <F1>
            state<=sCALC21;
            samp   <=calc(samp,coef,zdata10,256);
            zdata10<=calc(samp,coef,zdata10,256);
            
          -----------------------------------------------
          -- Filter Stage 2
          WHEN sCALC21 =>
            -- <B2>
            state<=sCALC22;
            samp   <=calc(samp,coef,zdata21,512);
            zdata21<=zdata20;
            
          WHEN sCALC22 =>
            -- <F2>
            state<=sCALC31;
            samp   <=calc(samp,coef,zdata20,256);
            zdata20<=calc(samp,coef,zdata20,256);
            
          -----------------------------------------------
          -- Filter Stage 3
          WHEN sCALC31 =>
            -- <B3>
            state<=sCALC32;
            samp   <=calc(samp,coef,zdata31,512);
            zdata31<=zdata30;
            
          WHEN sCALC32 =>
            -- <F3>
            state<=sCALC41;
            samp   <=calc(samp,coef,zdata30,256);
            zdata30<=calc(samp,coef,zdata30,256);
            
          -----------------------------------------------
          -- Filter Stage 4
          WHEN sCALC41 =>
            -- <B4>
            state<=sCALC42;
            samp   <=calc(samp,coef,zdata41,512);
            zdata41<=zdata40;
            
          WHEN sCALC42 =>
            -- <F4>
            state<=sCALC51;
            samp   <=calc(samp,coef,zdata40,256);
            zdata40<=calc(samp,coef,zdata40,256);
            
          -----------------------------------------------
          -- Filter Stage 5
          WHEN sCALC51 =>
            -- <B5>
            state<=sCALC52;
            samp   <=calc(samp,coef,zdata51,512);
            zdata51<=zdata50;
            
          WHEN sCALC52 =>
            -- <F5>
            state<=sSOUND;
            samp   <=calc(samp,coef,zdata50,256);
            zdata50<=calc(samp,coef,zdata50,256);
            
          -----------------------------------------------
          -- Sound output.
          WHEN sSOUND =>
            IF divcpt=320 THEN
              sound<=to_signed(samp*8,16);
              IF silent='1' THEN
                sound<=x"0000";
              END IF;
            END IF;
            IF divcpt+1=divi-1 THEN
              IF count>0 THEN
                state<=sGENE1;
              ELSE
                state<=sIDLE;
              END IF;
            END IF;
            
          -----------------------------------------------
        END CASE;
        
        IF fifomode THEN
          fifoptr<=fifoptr+len_v;
          IF fifoptr+len_v>=10 THEN
            fifoptr<=fifoptr+len_v-10;
            pop2<='1';
          END IF;
        END IF;
      END IF; -- tick='1'
      
      ---------------------------------------------------
    END IF;
  END PROCESS;

END ARCHITECTURE rtl;
