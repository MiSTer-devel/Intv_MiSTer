--------------------------------------------------------------------------------
-- General Instruments Corporation CP1610 CPU model
--------------------------------------------------------------------------------
-- DO 3/2019
--------------------------------------------------------------------------------
-- Intellivision CPU.

--------------------------------------------------------------------------------
-- VHDL-2002
--------------------------------------------------------------------------------
-- Keeping original multiplexed bus interfaces.
-- All signals on positive logic. (/=original)

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;
USE work.cp1610_pack.ALL;

ENTITY cp1610 IS
  PORT (
    dr     : IN  uv16; -- Data Read
    dw     : OUT uv16; -- Data Write / Address
    bdic   : OUT uv3;  -- BDIR / BC2 / BC1 Bus phase
    ebci   : IN  uv16; -- Demultiplexed EBCA inputs (unused)

    msync  : IN  std_logic; -- Master sync
    bdrdy  : IN  std_logic; -- Bus data ready
    intr   : IN  std_logic; -- Interrupt request (unused)
    intrm  : IN  std_logic; -- Interrupt request maskable
    tci    : OUT std_logic; -- Terminate current interrupt <Unused>
    pci    : IN  std_logic; -- Program counter increment inhibit <unused>
    pct    : OUT std_logic; -- Software interrupt instruction <unused>

    busrq  : IN  std_logic; -- Bus request
    busak  : OUT std_logic; -- Bus acknowledge
    stpst  : IN  std_logic; -- Stop Start : <Unused>
    halt   : OUT std_logic; -- CPU stopped <Unused>

    phi      : IN std_logic; -- PHI clock enable
    phip     : IN std_logic; -- PHI advance
    clk      : IN std_logic;
    reset_na : IN std_logic
    );
END ENTITY;

ARCHITECTURE rtl OF cp1610 IS

  TYPE enum_state IS
    (sINIT1,sINIT2,
     sFETCH1,sFETCH2,sFETCH3,sFETCH4,sWAIT_FETCH,
     sEXEC_UNARY1,sEXEC_UNARY2,
     sEXEC_ALUR1,sEXEC_ALUR2,sEXEC_ALUR3,
     sEXEC_SHIFT1,sEXEC_SHIFT2,sEXEC_SHIFT3,sEXEC_SHIFT4,
     sDIRECT1,sDIRECT2,sDIRECT3,sDIRECT4,sDIRECT5,sDIRECT6,
     sDIRECT_WRITE6,
     sINDIRECT1,sINDIRECT2,sINDIRECT3,sINDIRECT4,sINDIRECT_WRITE4,
     sCBRANCH1,sCBRANCH2,sCBRANCH3,sCBRANCH4,sCBRANCH5,
     sJUMP1,sJUMP2,sJUMP3,sJUMP4,sJUMP5,sJUMP6,sJUMP7,sJUMP8,
     sINTR1,sINTR2,sINTR3,sINTR4,sINTR5,sINTR6,sINTR7,sINTR8,sINTR9,sINTR10);
  SIGNAL state,state_r : enum_state;
     
  SIGNAL regs : arr_uv16(0 TO 7) :=(OTHERS =>x"0000");
  SIGNAL inter : std_logic;
  
  SIGNAL nri1,nri1_r,nri2,nri2_r,nro : uv3; -- Register select
  SIGNAL ri1_r,ri2_r,ro : uv16; -- Register read/write
  SIGNAL wreg :std_logic;
  SIGNAL szoc,szoc_r : type_szoc; -- Flags
  SIGNAL op,op_r,op2,op3,op2_r,op3_r : uv10;
  SIGNAL ro_r : uv16;
  SIGNAL intm,intm_r : std_logic; -- Interrupt mask
  SIGNAL sdbd,sdbd_r,sdbd2,sdbd2_r : std_logic; -- Double Byte Data
  SIGNAL disp,disp_r : uv16;
  SIGNAL dw_r,dw_i : uv16;
  
  FILE fil : text OPEN write_mode IS "trace.log";
  SIGNAL ccnt : natural;
  
  SIGNAL bdic_i : uv3;
  SUBTYPE uv33 IS unsigned(32 DOWNTO 0);
  TYPE arr_uv33 IS ARRAY(natural RANGE <>) OF uv33;
  SIGNAL acc : arr_uv33(0 TO 4);
  SIGNAL xxx_bdic : string(1 TO 4);
  
BEGIN
  -- Interrupt requests
  inter<=intr OR (intrm AND NOT intm_r);
  
  bdic<=bdic_i;
  dw<=dw_i;
  
  ------------------------------------------------------------------------------
  Comb:PROCESS(intm_r,dw_r,state_r,op_r,nri1_r,nri2_r,szoc_r,disp_r,ri1_r,ri2_r,
               sdbd_r,sdbd2_r,inter,op2_r,op3_r,dr,bdrdy,
               busrq,ebci) IS
    VARIABLE szoc_v : type_szoc;
    VARIABLE ro_v : uv16;
    VARIABLE double_v : boolean;
    VARIABLE sin_v : std_logic;
  BEGIN
    ------------------------------------
    intm<=intm_r;
    dw_i<=dw_r;
    state<=state_r;
    bdic_i<=B_NACT;
    op<=op_r;
    op2<=op2_r;
    op3<=op3_r;
    wreg<='0';
    ro<=ro_r;
    nro <=op_r(2 DOWNTO 0);
    nri1<=nri1_r;
    nri2<=nri2_r;
    szoc<=szoc_r;
    disp<=disp_r;
    sdbd<=sdbd_r;
    sdbd2<=sdbd2_r;
    busak<='0';
    tci<='0';
    pct<='0'; -- Software interrupt output (unused)
    halt<='0';
    
    ------------------------------------
    CASE state_r IS
      WHEN sINIT1 =>
        state<=sINIT2;
        
      WHEN sINIT2 =>
        state<=sFETCH1;
        bdic_i<=B_IAB;
        nro<="111";
        wreg<='1';
        ro<=dr;
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        nri1<="111";
        
        --------------------------------
      WHEN sWAIT_FETCH =>
        IF busrq='1' THEN
          state<=sWAIT_FETCH;
          busak<='1';
        ELSIF inter='1' THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        nri1<="111";
        
        --------------------------------
      WHEN sFETCH1 =>
        nri1<="111";
        dw_i<=ri1_r;
        bdic_i<=B_BAR;
        state<=sFETCH2;
        nro<="111";
        ro<=ri1_r+1; -- Increment PC
        wreg<='1';
        
      WHEN sFETCH2 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sFETCH3;
        END IF;
        
      WHEN sFETCH3 =>
        bdic_i<=B_DTB;
        op<=dr(9 DOWNTO 0);
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        state<=sFETCH4;
        nri1<="111";
        
      WHEN sFETCH4 =>
        bdic_i<=B_NACT;
        nri1<="111";
        
        -- Decode
        IF op_r(9 DOWNTO 0)="0000000100" THEN -- JUMP
          state<=sJUMP1;
        ELSE
          CASE op_r(9 DOWNTO 6) IS
            WHEN "0000" => -- Unary
              IF op_r(5 DOWNTO 3)="000" THEN
                state<=sFETCH1;
                CASE op_r(2 DOWNTO 0) IS
                  WHEN "000" => -- HLT Halt.
                    -- Not handled. Not used on Intellivision
                    halt<='1';
                  WHEN "001" => -- SDBD Set Double Byte Data
                    sdbd<='1';
                  WHEN "010" => -- EIS Enable Interrupt
                    intm<='0';
                  WHEN "011" => -- DIS Disable Interrupt
                    intm<='1';
                  WHEN "100" => -- JUMP Jump / Jump and save
                    NULL; -- Processed elsewhere
                  WHEN "101" => -- TCI Terminate Current Interrupt
                    tci<='1';
                  WHEN "110" => -- CLRC Clear Carry
                    szoc.c<='0';
                  WHEN "111" => -- SEC Set Carry
                    szoc.c<='1';
                  WHEN OTHERS => NULL;
                END CASE;
              ELSE
                state<=sEXEC_UNARY1;
              END IF;
              
            WHEN "0001" => -- Shift
              state<=sEXEC_SHIFT1;
              
            WHEN "0010" | "0011" | "0100" |   -- MOVR ADDR SUBR
                 "0101" | "0110" | "0111" =>  -- CMPR ANDR XORR
              state<=sEXEC_ALUR1;
              
            WHEN "1000" => -- Conditional branch
              state<=sCBRANCH1;
              
            WHEN "1001" | "1010" | "1011" |  -- MVO MVI ADD
                 "1100" | "1101" | "1110" |  -- SUB CMP AND
                 "1111" =>                   -- XOR
              CASE op_r(5 DOWNTO 3) IS
                WHEN "000" => -- Direct address
                  state<=sDIRECT1;
                WHEN OTHERS => -- Indirect / Immediate
                  state<=sINDIRECT1;
              END CASE;
            WHEN OTHERS => NULL;
          END CASE;
        END IF;
        
        --------------------------------
      WHEN sEXEC_UNARY1 =>
        state<=sEXEC_UNARY2;
        
      WHEN sEXEC_UNARY2 =>
        -- INCR DECR COMR NEGR ADCR GSWD RSWD NOP SIN
        nri2<=op_r(2 DOWNTO 0);
        unary(op_r,ri2_r,ro_v,szoc_r,szoc_v,sin_v);
        pct<=sin_v;
        szoc<=szoc_v;
        ro<=ro_v;
        nro<=op_r(2 DOWNTO 0);
        wreg<='1';
        IF busrq='1' THEN
          state<=sWAIT_FETCH;
        ELSIF inter='1' THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        
        --------------------------------
      WHEN sEXEC_ALUR1 =>
        -- MOVR ADDR SUBR CMPR ANDR XORR
        state<=sEXEC_ALUR2;
        
      WHEN sEXEC_ALUR2 =>
        --<données registres à stabiliser>
        nri2<=op_r(2 DOWNTO 0); -- DDD
        nri1<=op_r(5 DOWNTO 3); -- SSS
        alu(op_r,ri1_r,ri2_r,ro_v,szoc_r,szoc_v,true);
        ro<=ro_v;
        nro<=op_r(2 DOWNTO 0);
        szoc<=szoc_v;
        wreg<='1';
        IF op_r(2 DOWNTO 1)="11" THEN
          state<=sEXEC_ALUR3;
        ELSIF busrq='1' THEN
          state<=sWAIT_FETCH;
        ELSIF inter='1' THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        
      WHEN sEXEC_ALUR3 =>
        -- IF DDD = 6 or 7, add 1 cycle : 6 -> 7 cycles
        IF busrq='1' THEN
          state<=sWAIT_FETCH;
        ELSIF inter='1' THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        
        --------------------------------
      WHEN sEXEC_SHIFT1 =>
        nri1<='0' & op_r(1 DOWNTO 0);
        nri2<='0' & op_r(1 DOWNTO 0);
        state<=sEXEC_SHIFT2;
        
      WHEN sEXEC_SHIFT2 =>
        shift(op_r,ri1_r,ro_v,szoc_r,szoc_v,double_v);
        ro<=ro_v;
        nro<='0' & op_r(1 DOWNTO 0);
        szoc<=szoc_v;
        wreg<='1';
        IF double_v THEN
          state<=sEXEC_SHIFT3;
        ELSE
          state<=sFETCH1; -- Shifts are uniterruptible
        END IF;
          
      WHEN sEXEC_SHIFT3 =>
        -- Double shifts : +2 cycles
        state<=sEXEC_SHIFT4;
        
      WHEN sEXEC_SHIFT4 =>
        state<=sFETCH1; -- Shifts are uniterruptible
        
        --------------------------------
      WHEN sDIRECT1 => -- 10 cycles total
        -- MVO MVI ADD SUB CMP
        nri1<="111";
        bdic_i<=B_BAR;
        dw_i<=ri1_r;
        state<=sDIRECT2;
        nro<="111";
        ro<=ri1_r+1; -- Increment PC
        wreg<='1';
        IF sdbd2_r='1' THEN
          state<=sDIRECT3;
        END IF;
        
      WHEN sDIRECT2 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sDIRECT3;
        END IF;
        
      WHEN sDIRECT3 =>
        bdic_i<=B_ADAR;
        dw_i<=dr;
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        op2<=dr(9 DOWNTO 0);
        state<=sDIRECT4;
        
      WHEN sDIRECT4 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sDIRECT5;
        END IF;
        
      WHEN sDIRECT5 =>
        nri2<=op_r(2 DOWNTO 0);
        nro<=op_r(2 DOWNTO 0);
        IF op_r(8 DOWNTO 6)="001" THEN -- MVO
          bdic_i<=B_DW;
          IF sdbd_r='1' THEN
            dw_i<=x"00" & ri2_r(7 DOWNTO 0);
          ELSIF sdbd2_r='1' THEN
            dw_i<=x"00" & ri2_r(15 DOWNTO 8);
          ELSE
            dw_i<=ri2_r;
          END IF;
          state<=sDIRECT_WRITE6;
        ELSE
          bdic_i<=B_DTB;
          IF sdbd_r='1' THEN
            op2<=dr(9 DOWNTO 0);
            sdbd<='0';
            sdbd2<=sdbd;
            state<=sDIRECT1;
          ELSIF sdbd2_r='1' THEN
            ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
            alu(op_r,dr(7 DOWNTO 0) & op2_r(7 DOWNTO 0),ri2_r,ro_v,szoc_r,szoc_v,false);
            ro<=ro_v;
            szoc<=szoc_v;
            wreg<='1';
            state<=sDIRECT6;
          ELSE
            ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
            alu(op_r,dr,ri2_r,ro_v,szoc_r,szoc_v,false);
            ro<=ro_v;
            szoc<=szoc_v;
            wreg<='1';
            state<=sDIRECT6;
          END IF;
        END IF;
        
      WHEN sDIRECT6 =>
        bdic_i<=B_NACT;
        IF busrq='1' THEN
          state<=sWAIT_FETCH;
        ELSIF inter='1' THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        sdbd2<=sdbd_r;
        sdbd <='0';
        -- <MVO not interruptible ?>
        
      WHEN sDIRECT_WRITE6 =>
        state<=sDIRECT6;
        bdic_i<=B_DWS;
        IF sdbd_r='1' THEN
          dw_i<=x"00" & ri2_r(7 DOWNTO 0);
        ELSIF sdbd2_r='1' THEN
          dw_i<=x"00" & ri2_r(15 DOWNTO 8);
        ELSE
          dw_i<=ri2_r;
        END IF;
        
        --------------------------------
      WHEN sINDIRECT1 =>
        -- MVO MVI ADD SUB CMP AND XOR
        dw_i<=ri1_r;
        bdic_i<=B_BAR;
        state<=sINDIRECT2;
        nri1<=op_r(5 DOWNTO 3);
        nro<=op_r(5 DOWNTO 3);
        CASE op_r(5 DOWNTO 3) IS
          WHEN "001" | "010" | "011" => -- Register index
            NULL;
          WHEN "100" | "101" | "111" => -- Register index postincrement
            ro<=ri1_r+1;
            wreg<='1';
          WHEN "110" =>
            IF op_r(8 DOWNTO 6)/="001" THEN -- Not MVO : Pre decrement
              dw_i<=ri1_r-1;
              ro<=ri1_r-1;
              wreg<='1';
            END IF;
          WHEN OTHERS => NULL;
        END CASE;
        IF sdbd2_r='1' THEN
          state<=sINDIRECT3;
        END IF;
        
      WHEN sINDIRECT2 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sINDIRECT3;
        END IF;
        nri2<=op_r(2 DOWNTO 0);
        
      WHEN sINDIRECT3 =>
        IF op_r(8 DOWNTO 6)="001" THEN -- MVO
          bdic_i<=B_DW;
          IF sdbd_r='1' THEN
            dw_i<=x"00" & ri2_r(7 DOWNTO 0);
          ELSIF sdbd2_r='1' THEN
            dw_i<=x"00" & ri2_r(15 DOWNTO 8);
          ELSE
            dw_i<=ri2_r;
          END IF;
          state<=sINDIRECT_WRITE4;
        ELSE
          bdic_i<=B_DTB;
          IF sdbd_r='1' THEN
            ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
            op2<=dr(9 DOWNTO 0);
            state<=sINDIRECT1;
            sdbd<='0';
            sdbd2<=sdbd_r;
          ELSIF sdbd2_r='1' THEN
            ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
            alu(op_r,dr(7 DOWNTO 0) & op2_r(7 DOWNTO 0),ri2_r,ro_v,szoc_r,szoc_v,false);
            ro<=ro_v;
            nro<=op_r(2 DOWNTO 0);
            szoc<=szoc_v;
            wreg<='1';
            state<=sINDIRECT4;
          ELSE
            ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
            alu(op_r,dr,ri2_r,ro_v,szoc_r,szoc_v,false);
            ro<=ro_v;
            nro<=op_r(2 DOWNTO 0);
            szoc<=szoc_v;
            wreg<='1';
            state<=sINDIRECT4;
          END IF;
        END IF;
        -- <No NACT after double SDBD>
        
      WHEN sINDIRECT4 =>
        bdic_i<=B_NACT;
        nri1<="110";
        nro <="110";
        ro<=ri1_r+1; -- Post increment if stack
        IF op_r(8 DOWNTO 3)="001110" THEN -- MVO R6 (stack push)
          wreg<='1';
        END IF;
        state<=sFETCH1;
        sdbd2<=sdbd_r;
        sdbd <='0';
        
      WHEN sINDIRECT_WRITE4 =>
        bdic_i<=B_DWS;
        IF sdbd_r='1' THEN
          dw_i<=x"00" & ri1_r(7 DOWNTO 0);
        ELSIF sdbd2_r='1' THEN
          dw_i<=x"00" & ri1_r(15 DOWNTO 8);
        ELSE
          dw_i<=ri1_r;
        END IF;
        nri1<=op_r(2 DOWNTO 0);
        state<=sINDIRECT4;

        --------------------------------
      WHEN sCBRANCH1 =>
        nri1<="111";
        dw_i<=ri1_r;
        bdic_i<=B_BAR;
        nro<="111";
        ro<=ri1_r+1; -- Increment PC
        wreg<='1';
        state<=sCBRANCH2;
        
      WHEN sCBRANCH2 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sCBRANCH3;
        END IF;
        
      WHEN sCBRANCH3 =>
        bdic_i<=B_DTB;
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        disp<=dr;
        IF bcond(op_r,szoc_r,ebci) THEN
          state<=sCBRANCH4;
        ELSE
          state<=sFETCH1;
        END IF;
        
      WHEN sCBRANCH4 =>
        bdic_i<=B_NACT;
        nri1<="111";
        nro<="111";
        IF op_r(5)='0' THEN
          ro<=ri1_r + disp_r;
        ELSE
          ro<=ri1_r + NOT disp_r;
        END IF;
        state<=sCBRANCH5;
        wreg<='1';
        
      WHEN sCBRANCH5 =>
        bdic_i<=B_NACT;
        IF busrq='1' THEN
          state<=sWAIT_FETCH;
        ELSIF inter='1' THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        
        --------------------------------
      WHEN sJUMP1 =>
        nri1<="111";
        dw_i<=ri1_r;
        bdic_i<=B_BAR;
        state<=sJUMP2;
        
      WHEN sJUMP2 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sJUMP3;
        END IF;
        
      WHEN sJUMP3 =>
        bdic_i<=B_DTB;
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        op2<=dr(9 DOWNTO 0);
        state<=sJUMP4;
        
      WHEN sJUMP4 =>
        bdic_i<=B_NACT;
        nro<="111";
        nri1<="111";
        ro<=ri1_r+1; -- Increment PC
        wreg<='1';
        state<=sJUMP5;
        
      WHEN sJUMP5 =>
        nri1<="111";
        dw_i<=ri1_r;
        bdic_i<=B_BAR;
        state<=sJUMP6;
        
      WHEN sJUMP6 =>
        bdic_i<=B_NACT;
        IF bdrdy='1' THEN
          state<=sJUMP7;
        END IF;
        
      WHEN sJUMP7 =>
        bdic_i<=B_DTB;
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        op3<=dr(9 DOWNTO 0);
        state<=sJUMP8;
        nri1<="111";
        nro<='1' & op2_r(9 DOWNTO 8);
        ro<=ri1_r+1;
        IF op_r(9 DOWNTO 8)/="11" THEN
          -- JSR, save previous value of PC
          wreg<='1';
        END IF;
        
      WHEN sJUMP8 =>
        bdic_i<=B_NACT;
        wreg<='1';
        nro<="111";
        ro<=op2_r(7 DOWNTO 2) & op3_r(9 DOWNTO 0);
        
        IF op2_r(1 DOWNTO 0)="01" THEN
          intm<='0'; -- JE, JSRE
        ELSIF op2_r(1 DOWNTO 0)="10" THEN
          intm<='1'; -- JD, JSRD
        END IF;
        IF busrq='1' THEN
          state<=sWAIT_FETCH;
        ELSIF inter='1' AND op2_r(1 DOWNTO 0)/="10" THEN
          state<=sINTR1;
        ELSE
          state<=sFETCH1;
        END IF;
        
        -- <JMP 13 cycles ???>
        
        --------------------------------
      WHEN sINTR1 =>
        bdic_i<=B_INTAK;
        state<=sINTR2;
        nri1<="110";
        nro<="110"; -- Stack pointer on bus, increment
        dw_i<=ri1_r;
        ro<=ri1_r+1;
        wreg<='1';
        
      WHEN sINTR2 =>
        bdic_i<=B_NACT;
        state<=sINTR3;
        
      WHEN sINTR3 =>
        bdic_i<=B_DW;
        state<=sINTR4;
        nri1<="111";
        dw_i<=ri1_r;
        
      WHEN sINTR4 =>
        bdic_i<=B_DWS;
        state<=sINTR5;
        dw_i<=ri1_r;
        
      WHEN sINTR5 =>
        bdic_i<=B_NACT;
        state<=sINTR6;
        
      WHEN sINTR6 =>
        bdic_i<=B_IAB;
        nro<="111";
        ASSERT dr(0)/='U' AND dr(0)/='X' REPORT "XXXX" SEVERITY error;
        ro<=dr;
        wreg<='1';
        state<=sINTR7;
        
      WHEN sINTR7 =>
        bdic_i<=B_NACT;
        state<=sINTR8;
        
      WHEN sINTR8 =>
        bdic_i<=B_NACT;
        state<=sINTR9;
        
      WHEN sINTR9 =>
        bdic_i<=B_NACT;
        state<=sINTR10;
        
      WHEN sINTR10 =>
        bdic_i<=B_NACT;
        state<=sFETCH1;
        
    END CASE;
    
  END PROCESS;
  
  ------------------------------------------------------------------------------
  Seq:PROCESS(clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      state_r<=sINIT1;
      
      sdbd_r <='0';
      sdbd2_r<='0';

      szoc_r<=('0','0','0','0');
      intm_r<='1';

      ccnt<=0;
      
    ELSIF rising_edge(clk) THEN
      IF phi='1' THEN
        
        disp_r<=disp;
        intm_r<=intm;
        sdbd_r<=sdbd;
        sdbd2_r<=sdbd2;
                 
        szoc_r<=szoc;
        state_r<=state;
        op_r<=op;
        op2_r<=op2;
        op3_r<=op3;

        ro_r<=ro;
        
        dw_r<=dw_i;
        nri1_r<=nri1;
        nri2_r<=nri2;
        
        ccnt<=ccnt+1;
        
      END IF;
      ri1_r<=regs(to_integer(nri1));
      ri2_r<=regs(to_integer(nri2));
      
      IF wreg='1' AND phip='1' THEN
        regs(to_integer(nro))<=ro;
      END IF;

      
    END IF;
  END PROCESS;
  
  --pragma synthesis_off
  --############################################################################
  -- Instruction trace

  Datatrace:PROCESS IS
    VARIABLE ad : uv16;
  BEGIN
    WAIT UNTIL reset_na='1';
    LOOP
      LOOP
        wure(clk);
        EXIT WHEN phi='1';
      END LOOP;
      
      IF bdic_i=B_BAR OR bdic_i=B_ADAR THEN
        ad:=dw_i;
      END IF;
      IF bdic_i=B_DW THEN
        acc<=(ad & dw_i & '1') & acc(0 TO 3);
      ELSIF bdic_i=B_DTB THEN
        acc<=(ad & dr & '0') & acc(0 TO 3);
      END IF;
    END LOOP;
  END PROCESS;
  
  Trace:PROCESS IS
    VARIABLE lout : line;
    -----------------------------------------------
    TYPE arr_string4 IS ARRAY(natural RANGE <>) OF string(1 TO 4);
    TYPE arr_string5 IS ARRAY(natural RANGE <>) OF string(1 TO 5);
    TYPE arr_string7 IS ARRAY(natural RANGE <>) OF string(1 TO 7);
    TYPE arr_string9 IS ARRAY(natural RANGE <>) OF string(1 TO 9);
    CONSTANT ph_txt : arr_string4(0 TO 3):=("CODE","DATA","INDI","IO  ");
    -----------------------------------------------
    CONSTANT STR_IMP    : arr_string7(0 TO 7) := (
      "HLT    ","SDBD   ","EIS    ","DIS    ","JUMP   ","TCI    ","CLRC   ","SETC   ");
    CONSTANT STR_UNARY  : arr_string7(0 TO 7) := (
      "---    ","INCR   ","DECR   ","COMR   ","NEGR   ","ADCR   ","GSWD   ","RSWD   ");
    CONSTANT STR_ALUOPR : arr_string7(0 TO 7) := (
      "---    ","---    ","MOVR   ","ADDR   ","SUBR   ","CMPR   ","ANDR   ","XORR   ");
    CONSTANT STR_ALUOP  : arr_string7(0 TO 7) := (
      "---    ","MVO    ","MVI    ","ADD    ","SUB    ","CMP    ","AND    ","XOR    ");
    CONSTANT STR_BCOND  : arr_string7(0 TO 31) := (
      "B      ","BC     ","BOV    ","BPL    ","BZE    ","BLT    ","BLE    ","BUSC   ",
      "NOPP   ","BNC    ","BNOV   ","BMI    ","BNZE   ","BGE    ","BGT    ","BESC   ",
      "BEXT  0","BEXT  1","BEXT  2","BEXT  3","BEXT  4","BEXT  5","BEXT  6","BEXT  7",
      "BEXT  8","BEXT  9","BEXT 10","BEXT 11","BEXT 12","BEXT 13","BEXT 14","BEXT 15");
    CONSTANT STR_SHIFT  : arr_string7(0 TO 15) := (
      "SWAP   ","CLOW   ","SLL   1","SLL   2","ROL C 1","ROL C 2","SLL C 1","SLL C 2",
      "SLR   1","SLR   2","SAR   1","SAR   2","ROR C 1","ROR C 2","SAR C 1","SAR C 2");
    CONSTANT STR_JUMP   : arr_string9(0 TO 15) := (
      "JSR    R4","JSR    R5","JSR    R6","J        ","JSRE   R4","JSRE   R5","JSRE   R6","JE       ",
      "JSRD   R4","JSRD   R5","JSRD   R6","JD       ","????     ","????     ","????     ","????     ");
      
    CONSTANT STR_MODE   : arr_string4(0 TO 7) := (
      "IMM ","R1  ","R2  ","R3  ","R4++","R5++","R6++","   #");
    CONSTANT STR_REG    : arr_string4(0 TO 7) := (
      "R0  ","R1  ","R2  ","R3  ","R4  ","R5  ","R6  ","R7  ");
    CONSTANT STR_ISZOC : arr_string5(0 TO 31) := (
      ".....","....C","...O.","...OC","..Z..","..Z.C","..ZO.","..ZOC",
      ".S...",".S..C",".S.O.",".S.OC",".SZ..",".SZ.C",".SZO.",".SZOC",
      "I....","I...C","I..O.","I..OC","I.Z..","I.Z.C","I.ZO.","I.ZOC",
      "IS...","IS..C","IS.O.","IS.OC","ISZ..","ISZ.C","ISZO.","ISZOC");
    VARIABLE pc_v : uv16;
    
    VARIABLE csa,csb : string(1 TO 100) :=(OTHERS =>NUL);
    -----------------------------------------------
    PROCEDURE write(cs: INOUT string;
                    s : IN string) IS
      VARIABLE j,k : integer;
    BEGIN
      j:=-1;
      FOR i IN 1 TO cs'length LOOP
        IF cs(i)=nul THEN j:=i; EXIT; END IF;
      END LOOP;
      
      k:=s'length;
      FOR i IN 1 TO s'length LOOP
        IF s(i)=nul THEN k:=i; EXIT; END IF;
      END LOOP;
      
      IF j>0 THEN
        cs(j TO j+k-1):=s(1 TO k);
      END IF;
    END PROCEDURE write;

    FUNCTION strip(s : IN string) RETURN string IS
    BEGIN
      FOR i IN 1 TO s'length LOOP
        IF s(i)=nul THEN RETURN s(1 TO i-1); END IF;
      END LOOP;
      RETURN s;
    END FUNCTION;
    
    PROCEDURE pad(s : INOUT string; l : natural) IS
      VARIABLE j : integer;
    BEGIN
      j:=-1;
      FOR i IN 1 TO s'length LOOP
        IF s(i)=nul THEN j:=i; EXIT; END IF;
      END LOOP;
      IF j>0 THEN
        s(j TO l):=(OTHERS =>' ');
      END IF;
    END PROCEDURE;
    -----------------------------------------------
    VARIABLE dpop : natural;
    VARIABLE cccop : natural;
  BEGIN
    WAIT UNTIL reset_na='1';
    LOOP
      wure(clk);
      csa:=(OTHERS =>NUL);
      csb:=(OTHERS =>NUL);
      IF state_r=sFETCH4 THEN
        pc_v:=dw_i;
        dpop:=0;
        cccop:=ccnt-5;
        IF op_r(9 DOWNTO 0)="0000000100" THEN -- JUMP
          LOOP
            wure(clk);
            EXIT WHEN state_r=sJUMP8;
          END LOOP;
          wure(clk);
          write(csa,STR_JUMP(to_integer(op2_r(1 DOWNTO 0) & op2_r(9 DOWNTO 8))));
          write(csa,"  ," & to_hstring(op2_r(7 DOWNTO 2) & op3_r(9 DOWNTO 0)));
          write(csb,to_hstring("00" & op_r) & " " & to_hstring("00" & op2_r) & " " &
                to_hstring("00" & op3_r));
          
        ELSE
          CASE op_r(9 DOWNTO 6) IS
            WHEN "0000" =>
              CASE op_r(5 DOWNTO 3) IS
                WHEN "000" => -- Implicit : HLT, SDBD, EIS, DIS, ....
                  write(csa,STR_IMP(to_integer(op_r(2 DOWNTO 0))));
                  
                WHEN "001" | "010" | "011" | "100" | "101" | "111"=> -- INCR, DECR, ...
                  write(csa,STR_UNARY(to_integer(op_r(5 DOWNTO 3))) &
                            STR_REG(to_integer(op_r(2 DOWNTO 0))));
                  
                WHEN "110" =>
                  IF op_r(2)='0' THEN
                    write(csa,"GSWD    " & STR_REG(to_integer(op_r(1 DOWNTO 0))));
                  ELSIF op(1)='0' THEN
                    write(csa,"NOP     ");
                  ELSE
                    write(csa,"SWI     ");
                  END IF;
                WHEN OTHERS => NULL;
              END CASE;
              write(csb,to_hstring("00" & op_r) & "        ");
              
            WHEN "0001" => -- Shift/Rol
              write(csa,STR_SHIFT(to_integer(op_r(5 DOWNTO 2))) & "," &
                    STR_REG(to_integer(op_r(1 DOWNTO 0))));
              write(csb,to_hstring("00" & op_r) & "        ");
              
            WHEN "0010" | "0011" | "0100" | "0101" |
              "0110" | "0111" => -- MOVR, ADDR, SUBR, CMPR, ANDR, XORR
              write(csa,STR_ALUOPR(to_integer(op_r(8 DOWNTO 6))));
              write(csa,STR_REG(to_integer(op_r(5 DOWNTO 3))) & ", " &
                    STR_REG(to_integer(op_r(2 DOWNTO 0))));
              write(csb,to_hstring("00" & op_r) & "        ");
              
            WHEN "1000" => -- Conditional branch
              LOOP
                wure(clk);
                EXIT WHEN state_r=sFETCH1;
              END LOOP;
              write(csa,STR_BCOND(to_integer(op_r(4 DOWNTO 0))));
              IF op_r(5)='0' THEN
                write(csa,"+" & to_hstring(pc_v + 2 + ("000000" & disp_r(9 DOWNTO 0))));
              ELSE
                write(csa,"-" & to_hstring(pc_v + 2 + ("111111" & NOT disp_r(9 DOWNTO 0))));
              END IF;
              write(csb,to_hstring("00" & op_r)  & " " &
                        to_hstring(disp_r(11 DOWNTO 0)) & "    ");
              
            WHEN "1001" | "1010" | "1011" | "1100" | "1101" | "1110" | "1111" =>
              write(csa,STR_ALUOP(to_integer(op_r(8 DOWNTO 6))));
              
              CASE op_r(5 DOWNTO 3) IS
                WHEN "000" => -- Direct address
                  IF sdbd='0' THEN
                    LOOP
                      wure(clk);
                      EXIT WHEN state_r=sDIRECT5;
                    END LOOP;
                    write(csa,"@" & to_hstring("00" & op2_r) & ", ");
                    write(csa,STR_REG(to_integer(op_r(2 DOWNTO 0))));
                    write(csb,to_hstring("00" & op_r) & " " &
                          to_hstring("00" & op2_r));
                  ELSE
                    LOOP
                      wure(clk);
                      EXIT WHEN state_r=sDIRECT5;
                    END LOOP;
                    write(csa,"@" & to_hstring(dr(7 DOWNTO 0) &
                                               op2_r(7 DOWNTO 0)) & ", ");
                    write(csa,STR_REG(to_integer(op_r(2 DOWNTO 0))));
                    write(csb,to_hstring("00" & op_r) & " " &
                          to_hstring("00" & op2_r(9 DOWNTO 0)) & " " &
                          to_hstring("00" & dr(9 DOWNTO 0)));
                  END IF;
                  dpop:=1;
                WHEN "110" => -- STACK
                  IF op_r(8 DOWNTO 6)="001" THEN
                    write(csa,"@R6++, "); -- Post increment if output
                  ELSE
                    write(csa,"@--R6, ");
                  END IF;
                  write(csa,STR_REG(to_integer(op_r(2 DOWNTO 0))));
                  write(csb,to_hstring("00" & op_r));
                  dpop:=1;
                  
                WHEN "111" => -- Immediate
                  IF sdbd='0' THEN
                    LOOP
                      wure(clk);
                      EXIT WHEN state_r=sINDIRECT4;
                    END LOOP;
                    write(csa,"#" & to_hstring("00" & dr(9 DOWNTO 0)) & ", ");
                    write(csa,STR_REG(to_integer(op_r(2 DOWNTO 0))));
                    write(csb,to_hstring("00" & op_r) & " " &
                          to_hstring("00" & dr(9 DOWNTO 0)));
                  ELSE
                    LOOP
                      wure(clk);
                      EXIT WHEN state_r=sINDIRECT4;
                    END LOOP;
                    write(csa,"#" & to_hstring(dr(7 DOWNTO 0) &
                                               op2_r(7 DOWNTO 0)) & ", ");
                    write(csa,STR_REG(to_integer(op_r(2 DOWNTO 0))));
                    write(csb,to_hstring("00" & op_r) & " " &
                          to_hstring("00" & op2_r(9 DOWNTO 0)) & " " &
                          to_hstring("00" & dr(9 DOWNTO 0)));
                  END IF;
                  dpop:=1;
                  
                WHEN OTHERS => -- Indirect
                  write(csa,"@" & STR_MODE(to_integer(op_r(5 DOWNTO 3))) & ", ");
                  write(csa,STR_REG(to_integer(op_r(2 DOWNTO 0))));
                  write(csb,to_hstring("00" & op_r));
                  dpop:=1;
                  
              END CASE;
              
            WHEN OTHERS => NULL;
          END CASE;
        END IF;
        LOOP
          wure(clk);
          EXIT WHEN state_r=sFETCH1;
        END LOOP;
        pad(csa,20);
        pad(csb,16);
        write(lout,to_hstring(pc_v) & string'(" : "));
        write(lout,strip(csb) & strip(csa));
        write(lout,string'("  |"));
        write(lout,to_hstring(regs(0)) & ",");
        write(lout,to_hstring(regs(1)) & ",");
        write(lout,to_hstring(regs(2)) & ",");
        write(lout,to_hstring(regs(3)) & ",");
        write(lout,to_hstring(regs(4)) & ",");
        write(lout,to_hstring(regs(5)) & ",");
        write(lout,to_hstring(regs(6)) & ",");
        write(lout,to_hstring(regs(7)) & "|");
        write(lout,STR_ISZOC(to_integer(
          unsigned'(NOT intm & szoc_r.s & szoc_r.z & szoc_r.o & szoc_r.c))));
        write(lout,string'("|  "));
        write(lout,cccop,LEFT,7); write(lout,string'("|"));
        IF dpop>0 THEN
          FOR i IN dpop-1 DOWNTO 0 LOOP
            IF acc(i)(0)='0' THEN
              write(lout,string'("R[") & to_hstring(acc(i)(32 DOWNTO 17)) &
                    "]=" & to_hstring(acc(i)(16 DOWNTO 1)) & " ",LEFT,20);
            ELSE
              write(lout,string'("W[") & to_hstring(acc(i)(32 DOWNTO 17)) &
                    "]=" & to_hstring(acc(i)(16 DOWNTO 1)) & " ",LEFT,20);
            END IF;
            write(lout,string'("  "));
          END LOOP;
        ELSE
          write(lout,string'(" "),LEFT,22);
        END IF;
        write(lout,now);
        writeline(fil,lout);
        
      END IF;
      
    END LOOP;
  END PROCESS Trace;

  xxx_bdic<="BAR " WHEN bdic_i=B_BAR ELSE
            "DWS " WHEN bdic_i=B_DWS ELSE
            "DW  " WHEN bdic_i=B_DW  ELSE
            "INTA" WHEN bdic_i=B_INTAK ELSE
            "IAB " WHEN bdic_i=B_IAB ELSE
            "ADAR" WHEN bdic_i=B_ADAR ELSE
            "DTB " WHEN bdic_i=B_DTB ELSE
            "NACT" WHEN bdic_i=B_NACT ELSE
            "xxxx";
--pragma synthesis_on
  
  
END ARCHITECTURE rtl;

