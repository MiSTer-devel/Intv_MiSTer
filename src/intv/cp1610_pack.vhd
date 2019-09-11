--------------------------------------------------------------------------------
-- General Instruments Corporation CP1610 CPU model
--------------------------------------------------------------------------------
-- DO 3/2019
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY work;
USE work.base_pack.ALL;

PACKAGE cp1610_pack IS
  --------------------------------------
  -- Bus phases      BDIR / BC2 / BC1 
  CONSTANT B_BAR   : uv3 :="001"; -- Bus to address register
  CONSTANT B_DWS   : uv3 :="011"; -- Data write strobe
  CONSTANT B_DW    : uv3 :="101"; -- Data write
  CONSTANT B_INTAK : uv3 :="111"; -- Interrupt acknowledge
  CONSTANT B_IAB   : uv3 :="010"; -- Interrupt to address bus
  CONSTANT B_ADAR  : uv3 :="100"; -- Addressed data to address register
  CONSTANT B_DTB   : uv3 :="110"; -- Data to bus
  CONSTANT B_NACT  : uv3 :="000"; -- No action

  --------------------------------------
  TYPE type_szoc IS RECORD
    s : std_logic; -- Sign
    z : std_logic; -- Zero
    o : std_logic; -- Overflow
    c : std_logic; -- Carry
  END RECORD;
  
  --------------------------------------
  PROCEDURE alu(
    op    : IN  uv10;
    vi1   : IN  uv16;
    vi2   : IN  uv16;
    vo    : OUT uv16;
    szoci : IN  type_szoc;
    szoco : OUT type_szoc;
    reg   : IN  boolean);
  
  PROCEDURE unary(
    op    : IN uv10;
    vi    : IN  uv16;
    vo    : OUT uv16;
    szoci : IN  type_szoc;
    szoco : OUT type_szoc;
    sin   : OUT std_logic);
  
  PROCEDURE shift(
    op     : IN  uv10; -- Opcode
    vi     : IN  uv16;
    vo     : OUT uv16;
    szoci  : IN  type_szoc;
    szoco  : OUT type_szoc;
    double : OUT boolean);
  
  FUNCTION bcond(op   : IN uv10;
                 szoc : IN type_szoc;
                 ebc  : IN uv16 -- External condition
                 ) RETURN boolean;
  
  --------------------------------------

END PACKAGE;

--##############################################################################
PACKAGE BODY cp1610_pack IS

  --------------------------------------
  PROCEDURE unary(
    op : IN uv10;
    vi : IN  uv16;
    vo : OUT uv16;
    szoci : IN  type_szoc;
    szoco : OUT type_szoc;
    sin   : OUT std_logic) IS
    VARIABLE vt : uv17;
  BEGIN
    szoco:=szoci;
    vo:=vi;
    sin:='0';
    
    CASE op(5 DOWNTO 3) IS
      WHEN "001" => -- INCR
        vt:=('0' & vi)+1;
        vo:=vt(15 DOWNTO 0);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "010" => -- DECR
        vt:=('0' & vi)-1;
        vo:=vt(15 DOWNTO 0);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "011" => -- COMR
        vt:='0' & NOT vi;
        vo:=vt(15 DOWNTO 0);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "100" => -- NEGR
        vt:=('0' & x"0000") + ('0' & NOT vi) + 1;
        vo:=vt(15 DOWNTO 0);
        szoco.c:=vt(16);
        szoco.o:=vt(15) AND vi(15);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "101" => -- ADCR
        vt:=('0' & vi) + ("0" & szoci.c);
        vo:=vt(15 DOWNTO 0);
        szoco.c:=vt(16);
        szoco.o:=(vi(15) XOR vt(15)) AND NOT vi(15);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "110" =>
        CASE op(2 DOWNTO 0) IS
          WHEN "000" | "001" | "010" | "011" => -- GSWD
            vo:=szoci.s & szoci.z & szoci.o & szoci.c & x"0" &
                szoci.s & szoci.z & szoci.o & szoci.c & x"0";
            
          WHEN "100" | "101" => -- NOP
            NULL;
          WHEN OTHERS => -- SIN Software Interrupt
            sin:='1';
        END CASE;
            
      WHEN "111" => -- RSWD
        vo:=vi;
        szoco.c:=vi(4);
        szoco.o:=vi(5);
        szoco.z:=vi(6);
        szoco.s:=vi(7);

      WHEN OTHERS => -- Somewhere else
        NULL;
    END CASE;
  END PROCEDURE;
  --------------------------------------
  
  PROCEDURE alu(
    op    : IN uv10; -- Opcode
    vi1   : IN uv16; -- SSS
    vi2   : IN uv16; -- DDD
    vo    : OUT uv16; -- DDD
    szoci : IN  type_szoc;
    szoco : OUT type_szoc;
    reg   : IN  boolean) IS
    VARIABLE vt : uv17;
  BEGIN
    szoco:=szoci;
    
    CASE op(8 DOWNTO 6) IS
      WHEN "000" => -- Special
        vo:=vi1;
      WHEN "001" => -- MVO
        vo:=vi1;
        IF reg THEN
          szoco.z:=to_std_logic(vi1(15 DOWNTO 0)=x"0000");
          szoco.s:=vi1(15);
        END IF;
        
      WHEN "010" => -- MVI
        vo:=vi1;
        IF reg THEN
          szoco.z:=to_std_logic(vi1(15 DOWNTO 0)=x"0000");
          szoco.s:=vi1(15);
        END IF;
        
      WHEN "011" => -- ADD
        vt:=('0' & vi2) + ('0' & vi1);
        vo:=vt(15 DOWNTO 0);
        szoco.c:=vt(16);
        szoco.o:=(vi2(15) XOR vt(15)) AND NOT (vi1(15) XOR vi2(15));
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "100" => -- SUB
        vt:=('0' & vi2) + ('0' & NOT vi1) + 1;
        vo:=vt(15 DOWNTO 0);
        szoco.c:=vt(16);
        szoco.o:=(vi2(15) XOR vt(15)) AND (vi1(15) XOR vi2(15));
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "101" => -- CMP
        vt:=('0' & vi2) + ('0' & NOT vi1) + 1;
        vo:=vi2;
        szoco.c:=vt(16);
        szoco.o:=(vi2(15) XOR vt(15)) AND (vi1(15) XOR vi2(15));
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN "110" => -- AND
        vt:='0' & (vi1 AND vi2);
        vo:=vt(15 DOWNTO 0);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
      WHEN OTHERS => -- XOR
        vt:='0' & (vi1 XOR vi2);
        vo:=vt(15 DOWNTO 0);
        szoco.z:=to_std_logic(vt(15 DOWNTO 0)=x"0000");
        szoco.s:=vt(15);
        
    END CASE;
    
  END PROCEDURE;
  
  --------------------------------------
  PROCEDURE shift(
    op    : IN  uv10; -- Opcode
    vi    : IN  uv16;
    vo    : OUT uv16;
    szoci : IN  type_szoc;
    szoco : OUT type_szoc;
    double : OUT boolean
    ) IS
    VARIABLE vt : uv16;
  BEGIN
    szoco:=szoci;
    vt:=vi;
    double:=op(2)='1'; -- Two exec. cycles instruction.
    
    CASE op(5 DOWNTO 2) IS
      WHEN "0000" => -- SWAP
        vt:=vi(7 DOWNTO 0) & vi(15 DOWNTO 8);
        szoco.s:=vt(7);
      WHEN "0001" => -- Double SWAP : Copy low byte
        vt:=vi(7 DOWNTO 0) & vi(7 DOWNTO 0);
        szoco.s:=vt(7);
      WHEN "0010" => -- Shift Logical Left one bit
        vt:=vi(14 DOWNTO 0) & '0';
        szoco.s:=vt(15);
      WHEN "0011" => -- Shift Logical Left two bits
        vt:=vi(13 DOWNTO 0) & "00";
        szoco.s:=vt(15);
      WHEN "0100" => -- Rotate left one bit using Carry
        vt:=vi(14 DOWNTO 0) & szoci.c;
        szoco.c:=vi(15);
        szoco.s:=vt(15);
      WHEN "0101" => -- Rotate left two bits using Carry and OV
        vt:=vi(13 DOWNTO 0) & szoci.c & szoci.o;
        szoco.c:=vi(15);
        szoco.o:=vi(14);
        szoco.s:=vt(15);
      WHEN "0110" => -- Shift Logical Left one bit using Carry
        vt:=vi(14 DOWNTO 0) & '0';
        szoco.c:=vi(15);
        szoco.s:=vt(15);
      WHEN "0111" => -- Shift Logical Left two bits using Carry and OV
        vt:=vi(13 DOWNTO 0) & "00";
        szoco.c:=vi(15);
        szoco.o:=vi(14);
        szoco.s:=vt(15);
      WHEN "1000" => -- Shift Logical Right one bit
        vt:='0' & vi(15 DOWNTO 1);
        szoco.s:=vt(7);
      WHEN "1001" => -- Shift Logical Right two bits
        vt:="00" & vi(15 DOWNTO 2);
        szoco.s:=vt(7);
      WHEN "1010" => -- Shift Arithmetic Right one bit. Sign bit copied
        vt:=vi(15) & vi(15 DOWNTO 1);
        szoco.s:=vt(7);
      WHEN "1011" => -- Shift Arithmetic Right two bits. Sign bit copied
        vt:=vi(15) & vi(15) & vi(15 DOWNTO 2);
        szoco.s:=vt(7);
      WHEN "1100" => -- Rotate Right one bit using Carry
        vt:=szoci.c & vi(15 DOWNTO 1);
        szoco.c:=vi(0);
        szoco.s:=vt(7);
      WHEN "1101" => -- Rotate Right two bits using Carry and OV
        vt:=szoci.o & szoci.c & vi(15 DOWNTO 2);
        szoco.o:=vi(1);
        szoco.c:=vi(0);
        szoco.s:=vt(7);
      WHEN "1110" => -- Shift Arithmetic Right one bit thru Carry
        vt:=vi(15) & vi(15 DOWNTO 1);
        szoco.c:=vi(0);
        szoco.s:=vt(7);
      WHEN OTHERS => -- Shift Arithmetic Right two bits thru Carry and OV
        vt:=vi(15) & vi(15) & vi(15 DOWNTO 2);
        szoco.c:=vi(0);
        szoco.o:=vi(1);
        szoco.s:=vt(7);
    END CASE;
    
    vo:=vt;
    szoco.z:=to_std_logic(vt=x"0000");
    
  END PROCEDURE;
  
  --------------------------------------
  FUNCTION bcond(op   : IN uv10;
                 szoc : IN type_szoc; -- Sign / Zero / Overflow / Carry
                 ebc  : IN uv16 -- External condition
                 ) RETURN boolean IS
  BEGIN
    CASE op(4 DOWNTO 0) IS
      WHEN "00000" => -- Branch Always
        RETURN true;
      WHEN "01000" => -- Branch Never
        RETURN false;
      WHEN "00001" => -- Branch on Carry. C=1
        RETURN szoc.c='1';
      WHEN "01001" => -- Branch on No Carry. C=0
        RETURN szoc.c='0';
      WHEN "00010" => -- Branch on Overflow. OV=1
        RETURN szoc.o='1';
      WHEN "01010" => -- Branch on No Overflow. OV=0
        RETURN szoc.o='0';
      WHEN "00011" => -- Branch on Plus. S=0
        RETURN szoc.s='0';
      WHEN "01011" => -- Branch on Minus. S=1
        RETURN szoc.s='1';
      WHEN "00100" => -- Branch on Zero. Z=1
        RETURN szoc.z='1';
      WHEN "01100" => -- Branch on No Zero. Z=0
        RETURN szoc.z='0';
      WHEN "00101" => -- Branch if Less than. S^OV=1
        RETURN (szoc.s XOR szoc.o)='1';
      WHEN "01101" => -- Branch if Greater than or Equal. S^OV=0
        RETURN (szoc.s XOR szoc.o)='0';
      WHEN "00110" => -- Branch if Less than or Equal. Z | (S^OV) =1
        RETURN (szoc.z OR (szoc.s XOR szoc.o))='1';
      WHEN "01110" => -- Branch if Greater than. Z | (S^OV) =0
        RETURN (szoc.z OR (szoc.s XOR szoc.o))='0';
      WHEN "00111" => -- Branch if unequal sign and carry. C^S=1
        RETURN (szoc.c XOR szoc.s)='1';
      WHEN "01111" => -- Branch if equal sign and carry. C^S=0
        RETURN (szoc.c XOR szoc.s)='0';
      WHEN OTHERS  => -- Branch on external condition true
        RETURN ebc(to_integer(op(3 DOWNTO 0)))='1';
    END CASE;
  END FUNCTION bcond;


END PACKAGE BODY;
