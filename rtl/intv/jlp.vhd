--------------------------------------------------------------------------------
-- Intellivision
--------------------------------------------------------------------------------
-- JLP Extensions 
--------------------------------------------------------------------------------
-- DO 10/2024
--------------------------------------------------------------------------------

-- Additional RAM
-- Additional ROM
-- Hardware acceleration for MUL, DIV, CRC, Random numbers
-- Flash storage

-- 8000 8035  JLP FLASH Commands
-- 8023 : RO : First Valid Flash Row Number
-- 8024 : RO : Last  Valid Flash Row Number
-- 8025 : WO : Address in RAM to operate on
-- 8026 : WO : Flash Row to operate on
-- 802D : WO : Copy RAM to FLASH.  Write xC0DE
-- 802E : WO : Copy FLASH to RAM.  Write xDEC0
-- 802F : WO : Erase FLASH sector. Write xBEEF

-- 8040 9F7F  JLP RAM

-- 9F80 9FFF  JLP Accelerator
-- 9F80       RW : MPY_SS1
-- 9F81       RW : MPY_SS2
-- 9F82       RW : MPY_SU1
-- 9F83       RW : MPY_SU2
-- 9F84       RW : MPY_US1
-- 9F85       RW : MPY_US2
-- 9F86       RW : MPY_UU1
-- 9F87       RW : MPY_UU2
-- 9F88       RW : DIV_SS1 dividend
-- 9F89       RW : DIV_SS2 divisor
-- 9F8A       RW : DIV_UU1
-- 9F8B       RW : DIV_UU2

-- 9F8E       RO : Result L / Quotient
-- 9F8F       RO : Result H / Remainder

-- 9FFC       WO : CRC Write
-- 9FFD       RW : CRC Read

-- 9FFE       RO : Random number

-- 8000 9FFF  ROM Page Switch
-- A000 DFFF  ROM
-- E000 EFFF  ROM Pages
-- F000 FFFF  ROM


LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY jlp IS
  PORT (
    --------------------------
    -- CPU BUS
    ad       : IN  uv16;
    dw       : IN  uv16;
    dr       : OUT uv16;
    wr       : IN  std_logic;
    
    --------------------------
    -- FLASH
    img_mounted  : IN std_logic;
    img_size     : IN unsigned(63 DOWNTO 0);
    img_readonly : IN std_logic;

    sd_lba       : OUT unsigned(31 DOWNTO 0);
    sd_rd        : OUT std_logic;
    sd_wr        : OUT std_logic;
    sd_ack       : IN  std_logic;
    sd_buff_addr : IN  unsigned(8 DOWNTO 0);
    sd_buff_dout : IN  std_logic_vector(7 DOWNTO 0);
    sd_buff_din  : OUT std_logic_vector(7 DOWNTO 0);
    sd_buff_wr   : IN  std_logic;

    --------------------------
    clk      : IN  std_logic;
    reset_na : IN  std_logic
    );
END ENTITY jlp;


ARCHITECTURE rtl OF jlp IS
  
  ------------------------------------------------
  SIGNAL op1,      op2      : uv16;
  SIGNAL op1_sign, op2_sign : std_logic;
  SIGNAL recalc : std_logic;

  SIGNAL mul1a : uv17;
  SIGNAL mul2a : uv32;
  
  SIGNAL res_lo, res_hi : uv16;
  SIGNAL count : natural RANGE 0 TO 31;

  SIGNAL op : std_logic;
  CONSTANT OPMUL : std_logic := '0';
  CONSTANT OPDIV : std_logic := '1';

  CONSTANT ZERO17 : unsigned(16 DOWNTO 0) := (OTHERS =>'0');
  CONSTANT ZERO15 : unsigned(14 DOWNTO 0) := (OTHERS =>'0');

  TYPE enum_mulstate IS (sINIT,sCALC,sSIGN,sDONE);
  SIGNAL mulstate : enum_mulstate;

  SIGNAL drr, drm : uv16;
  SIGNAL dr_force : std_logic;

  SIGNAL op1a_sign, op2a_sign : std_logic;

  SIGNAL mul, mul2 : uv32;
  SIGNAL rmd, rmd2, quo, quo2 : uv16;

  SIGNAL div1a, div2a : unsigned(32 DOWNTO 0);
  
  SIGNAL crc_wr, crc_wr2, crc_set : std_logic;
  SIGNAL crc, crc_dw : uv16;
  SIGNAL crccount : natural RANGE 0 TO 16;

  SIGNAL lfsr : uv32;
  
  SHARED VARIABLE jlpram : arr_uv16(0 TO 8191); -- JLP RAM
  ATTRIBUTE ramstyle : string;
  ATTRIBUTE ramstyle OF jlpram : VARIABLE IS "no_rw_check";

  CONSTANT FIRST_FLASH_ROW : uv16 := x"00C0";
  --CONSTANT LAST_FLASH_ROW  : uv16 := x"03FF";
  SIGNAL last_row : uv16;

  SHARED VARIABLE secbuf_lo, secbuf_hi : arr_uv8(0 TO 255);
  ATTRIBUTE ramstyle OF secbuf_lo : VARIABLE IS "no_rw_check";
  ATTRIBUTE ramstyle OF secbuf_hi : VARIABLE IS "no_rw_check";
  SIGNAL sd_buff_addr2 : unsigned(8 DOWNTO 0);
  
  SIGNAL flash_ad  : unsigned(12 DOWNTO 0);
  SIGNAL flash_row : uv16;
  SIGNAL flash_read, flash_write, flash_erase : std_logic;
  SIGNAL flash_cpt : natural RANGE 0 TO 127;
  SIGNAL flash_tog : std_logic;

  SIGNAL buf_wr : std_logic;
  SIGNAL buf_a : unsigned(7 DOWNTO 0);
  SIGNAL buf_dw, buf_dr : uv16;
  SIGNAL fdw, fdr : uv16;
  SIGNAL fad : unsigned(12 DOWNTO 0);
  SIGNAL fwr : std_logic;

  SIGNAL sd_buff_din_t : std_logic_vector(15 DOWNTO 0);
  TYPE   enum_state IS (sIDLE,
                        sWRITE,sWRITE2,sWRITE3,sWRITE4,sWRITE5,
                        sREAD,sREAD2,sREAD3,
                        sERASE,sERASE2,sERASE3);
  SIGNAL state : enum_state;
  SIGNAL mounted : std_logic;

BEGIN

  -----------------------------------------------------------------------------
  -- RAM
  RAM:PROCESS(clk,reset_na) IS
  BEGIN
    IF rising_edge(clk) THEN
      -----------------------------------------------------
      fdr <= jlpram(to_integer(fad));
      IF fwr='1' THEN
        jlpram(to_integer(fad)) := fdw;
      END IF;
      -----------------------------------------------------
    END IF;
  END PROCESS RAM;

  RAM2:PROCESS(clk,reset_na) IS
  BEGIN
    IF rising_edge(clk) THEN
      -----------------------------------------------------
      drm <= jlpram(to_integer(ad) MOD 8192);
      IF wr='1' THEN --ad >= x"8000" AND ad < x"A000" AND wr='1' THEN
        jlpram(to_integer(ad) MOD 8192) := dw;
      END IF;
      -----------------------------------------------------
    END IF;
  END PROCESS RAM2;

  dr <= drr WHEN dr_force='1' ELSE drm;

  -----------------------------------------------------------------------------
  -- Registers
  Reg:PROCESS(clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      recalc <= '0';

    ELSIF rising_edge(clk) THEN

      recalc  <= '0';

      crc_wr  <= '0';
      crc_set <= '0';
      flash_read <= '0';
      flash_write <= '0';
      flash_erase <= '0';

      dr_force <= '0';

      last_row <= FIRST_FLASH_ROW + (img_size(19 DOWNTO 11) & "000") - 1;
      
      -----------------------------------------------------
      IF ad < x"8040" THEN
        drr <= x"FFFF";
        dr_force <= '1';
      ELSIF ad >= x"9F90" AND ad < x"9FFC" THEN
        drr <= x"FFFF";
        dr_force <= '1';
      END IF;
      
      CASE ad IS
        WHEN x"8023" => -- RO : First Valid Flash Row Number
          drr <= FIRST_FLASH_ROW;
          dr_force <= '1';

        WHEN x"8024" => -- RO : Last  Valid Flash Row Number
          drr <= last_row;
          dr_force <= '1';

        WHEN x"8025" => -- WO : Address in RAM to operate on
          IF wr='1' THEN
            flash_ad <= dw(12 DOWNTO 0);
          END IF;
          drr <= "000" & flash_ad;
          dr_force <= '1';

        WHEN x"8026" => -- WO : Flash Row to operate on
          IF wr='1' THEN
            flash_row <= dw;
          END IF;
          drr <= flash_row;
          dr_force <= '1';

        WHEN x"802D" => -- WO : Copy RAM to FLASH.  Write xC0DE
          IF wr='1' AND dw=x"C0DE" THEN
            flash_write <= mounted;
          END IF;
          drr <= x"0000";
          dr_force <= '1';

        WHEN x"802E" => -- WO : Copy FLASH to RAM.  Write xDEC0
          IF wr='1' AND dw=x"DEC0" THEN
            flash_read <= mounted;
          END IF;
          drr <= x"0000";
          dr_force <= '1';

        WHEN x"802F" => -- WO : Erase FLASH sector. Write xBEEF
          IF wr='1' AND dw=x"BEEF" THEN
            flash_erase <= mounted;
          END IF;
          drr <= x"0000";
          dr_force <= '1';

        WHEN x"9F80" => -- MPY_SS1
          IF wr='1' THEN
            op1      <= dw;
            op1_sign <= '1';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F81" => -- MPY_SS2
          IF wr='1' THEN
            op2      <= dw;
            op2_sign <= '1';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F82" => -- MPY_SU1
          IF wr='1' THEN
            op1      <= dw;
            op1_sign <= '1';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F83" => -- MPY_SU2
          IF wr='1' THEN
            op2      <= dw;
            op2_sign <= '0';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F84" => -- MPY_US1
          IF wr='1' THEN
            op1      <= dw;
            op1_sign <= '0';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F85" => -- MPY_US2
          IF wr='1' THEN
            op2      <= dw;
            op2_sign <= '1';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F86" => -- MPY_UU1
          IF wr='1' THEN
            op1      <= dw;
            op1_sign <= '0';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F87" => -- MPY_UU2
          IF wr='1' THEN
            op2      <= dw;
            op2_sign <= '0';
            op       <= OPMUL;
            recalc   <= '1';
          END IF;

        WHEN x"9F88" => -- DIV_SS1
          IF wr='1' THEN
            op1      <= dw;
            op1_sign <= '1';
            op2_sign <= '1';
            op       <= OPDIV;
            recalc   <= '1';
          END IF;

        WHEN x"9F89" => -- DIV_SS2
          IF wr='1' THEN
            op2      <= dw;
            op1_sign <= '1';
            op2_sign <= '1';
            op       <= OPDIV;
            recalc   <= '1';
          END IF;

        WHEN x"9F8A" => -- DIV_UU1
          IF wr='1' THEN
            op1      <= dw;
            op1_sign <= '0';
            op2_sign <= '0';
            op       <= OPDIV;
            recalc   <= '1';
          END IF;

        WHEN x"9F8B" => -- DIV_UU2
          IF wr='1' THEN
            op2      <= dw;
            op1_sign <= '0';
            op2_sign <= '0';
            op       <= OPDIV;
            recalc   <= '1';
          END IF;

        WHEN x"9F8E" => -- Result Low  / Quotient
          drr <= res_lo;
          dr_force <= '1';

        WHEN x"9F8F" => -- Result High / Remainder
          drr <= res_hi;
          dr_force <= '1';
        
        WHEN x"9FFC" => -- CRC Write
          drr      <= crc;
          dr_force <= '1';
          crc_dw  <= dw;
          crc_wr  <= wr;

        WHEN x"9FFD" => -- CRC Read
          drr      <= crc;
          dr_force <= '1';
          crc_dw  <= dw;
          crc_set <= wr;

        WHEN x"9FFE" => -- Random num read
          drr      <= lfsr(15 DOWNTO 0);
          dr_force <= '1';

        WHEN x"9FFF" => -- Zero
          drr      <= x"0000";
          dr_force <= '1';

        WHEN OTHERS =>
          
      END CASE;
      
      ---------------------------------------------------
      IF state /=sIDLE THEN
        dr_force <= '1';
        drr <= x"FFFF";
      END IF;

      ---------------------------------------------------
    END IF;
  END PROCESS Reg;

  -----------------------------------------------------------------------------
  Muldiv:PROCESS(clk,reset_na) IS
    VARIABLE div_v : unsigned(32 DOWNTO 0);
  BEGIN
    IF reset_na='0' THEN
      mulstate <=sINIT;

    ELSIF rising_edge(clk) THEN

      IF recalc ='1' THEN
        mulstate <= sINIT;
      ELSE
        CASE mulstate IS
          -------------------------------
          WHEN sINIT =>
            mulstate <=sCALC;
            -- MUL
            mul1a <= '0' & op1;
            IF op1_sign='1' AND op1(15)='1' THEN
              mul1a <= ZERO17 - ('1' & op1);
            END IF;

            mul2a <= x"0000" & op2;
            IF op2_sign='1' AND op2(15)='1' THEN
              mul2a <= x"0000_0000" - (x"FFFF" & op2);
            END IF;

            -- DIV
            div1a <= '0' & x"0000" & op1; -- Dividend
            IF op1_sign='1' AND op1(15)='1' THEN
              div1a <= (ZERO17 & x"0000") - ('1' & x"FFFF" & op1);
            END IF;

            div2a <= "00" & op2 & ZERO15; -- Divider
            IF op2_sign='1' AND op2(15)='1' THEN
              div2a <= (ZERO17 & x"0000") - ("11" & op2 & ZERO15);
            END IF;

            op1a_sign <= op1_sign AND op1(15);
            op2a_sign <= op2_sign AND op2(15);

            count <= 0;
            mul <= x"0000_0000";

            -- 1234 0000   F49D FFFC
            -- 1234 0000   07F9 0009

          -------------------------------
          WHEN sCALC =>
            count <= count + 1;
            IF count = 15 THEN
              mulstate <= sSIGN;
            END IF;
            
            -- MUL
            IF mul1a(count)='1' THEN
              mul <= mul + mul2a;
            END IF;
            mul2a<=mul2a(30 DOWNTO 0) & '0';

            -- DIV
            div_v := div1a - div2a;
            quo(15-count)<=NOT div_v(32);
            IF div_v(32)='1' THEN
              div_v:=div1a;
            END IF;
            div1a <= div_v(31 DOWNTO 0) & '0';
            
          -------------------------------
          WHEN sSIGN =>
            mul2 <= mul;
            quo2 <= quo;
            rmd2 <= div1a(31 DOWNTO 16);
            IF (op1a_sign='1' OR  op2a_sign='1') AND NOT
               (op1a_sign='1' AND op2a_sign='1') THEN
              -- Negative result
              mul2 <= x"0000_0000" - mul;
              quo2 <= x"0000" - quo;
            END IF;
            IF op1a_sign='1' THEN
              rmd2 <= x"0000" - div1a(31 DOWNTO 16);
            END IF;
            mulstate <= sDONE;
          
          -------------------------------
          WHEN sDONE =>
            IF op=OPMUL THEN
              res_lo <= mul2(15 DOWNTO 0);
              res_hi <= mul2(31 DOWNTO 16);
            ELSE
              res_lo <= quo2;
              res_hi <= rmd2;
            END IF;

          -------------------------------
        END CASE;
      END IF;
    END IF;
  END PROCESS Muldiv;

  -----------------------------------------------------------------------------
  Cyclic:PROCESS(clk,reset_na) IS
    CONSTANT POLY  : uv16 := x"AD52";

  BEGIN
    IF reset_na='0' THEN
      crc <= x"0000";
      lfsr <= x"0000_0001";
      crccount <= 16;

    ELSIF rising_edge(clk) THEN
      ---------------------------------
      -- CRC
      IF crc_set='1' THEN
        crc <= crc_dw;
      END IF;

      crc_wr2 <= crc_wr;
      IF crc_wr='1' AND crc_wr2='0' THEN
        crccount <= 0;
        crc <= crc XOR crc_dw;
      END  IF;

      IF crccount < 16 THEN
        crccount <= crccount + 1;
        IF crc(0)='1' THEN
          crc <= ('0' & crc(15 DOWNTO 1)) XOR POLY;
        ELSE
          crc <= ('0' & crc(15 DOWNTO 1));
        END IF;
      END IF;
      
      ---------------------------------
      -- Random generator. For now just a LFSR
      lfsr <= lfsr(30 DOWNTO 0) &
        (lfsr(31) XOR lfsr(30) XOR lfsr(29) XOR lfsr(9));

    END IF;
  END PROCESS Cyclic;
  
  -----------------------------------------------------------------------------
  SectorBuf:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      IF sd_buff_wr='1' AND sd_buff_addr(0)='0' THEN
        secbuf_lo(to_integer(sd_buff_addr(8 DOWNTO 1))) := unsigned(sd_buff_dout);
      END IF;
      IF sd_buff_wr='1' AND sd_buff_addr(0)='1' THEN
        secbuf_hi(to_integer(sd_buff_addr(8 DOWNTO 1))) := unsigned(sd_buff_dout);
      END IF;
      
      sd_buff_din_t(7 DOWNTO 0)  <= std_logic_vector(secbuf_lo(to_integer(sd_buff_addr(8 DOWNTO 1))));
      sd_buff_din_t(15 DOWNTO 8) <= std_logic_vector(secbuf_hi(to_integer(sd_buff_addr(8 DOWNTO 1))));
      
    END IF;
  END PROCESS SectorBuf;
    
  SectorBuf2:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      IF buf_wr='1' THEN
        secbuf_lo(to_integer(buf_a)) := buf_dw(7 DOWNTO 0);
        secbuf_hi(to_integer(buf_a)) := buf_dw(15 DOWNTO 8);
      END IF;
      buf_dr(7 DOWNTO 0) <=secbuf_lo(to_integer(buf_a));
      buf_dr(15 DOWNTO 8)<=secbuf_hi(to_integer(buf_a));
    END IF;
  END PROCESS SectorBuf2;

  -----------------------------------------------------------------------------
  buf_dw <= fdr;
  buf_a  <= flash_row(0) & to_unsigned(flash_cpt,7);
  fdw    <= buf_dr;
  fad    <= flash_ad + flash_cpt; -- Adresse RAM

  sd_buff_din <= sd_buff_din_t(7 DOWNTO 0)  WHEN state/=sERASE2 AND sd_buff_addr2(0)='0' ELSE
                 sd_buff_din_t(15 DOWNTO 8) WHEN state/=sERASE2 AND sd_buff_addr2(0)='1' ELSE
                 x"FF";
  
  FlashOps:PROCESS(clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      sd_rd <= '0';
      sd_wr <= '0';
      mounted <= '0';

    ELSIF rising_edge(clk) THEN

      sd_buff_addr2 <= sd_buff_addr;
      fwr <= '0';
      buf_wr <= '0';

      CASE state IS
        -------------------------------
        WHEN sIDLE =>
          IF flash_write='1' THEN
            state <= sWRITE;
          ELSIF flash_read='1' THEN
            state <= sREAD;
          ELSIF flash_erase='1' THEN
            state <= sERASE;
          END IF;
          flash_cpt <= 0;
          flash_tog <= '0';

        -------------------------------
        -- 802D : WO : Copy RAM to FLASH
        --   Read disc to buffer
        --   Read ram, copy to buffer
        --   Write disc from buffer
        WHEN sWRITE =>
          sd_lba<=x"0000_0000";
          sd_lba(14 DOWNTO 0) <= flash_row(15 DOWNTO 1) - FIRST_FLASH_ROW(15 DOWNTO 1);
          sd_rd<='1';
          IF sd_ack='1' THEN
            state <= sWRITE2;
            sd_rd <= '0';
          END IF;
        
        WHEN sWRITE2 =>
          IF sd_ack='0' THEN
            state <= sWRITE3;
          END IF;

        WHEN sWRITE3 =>
          buf_wr <= NOT flash_tog;
          flash_tog <= NOT flash_tog;
          IF flash_tog='1' THEN
            flash_cpt <= flash_cpt+1;
            IF flash_cpt=95 THEN
              state <= sWRITE4;
              buf_wr <= '0';
            END IF;
          END IF;

        WHEN sWRITE4 =>
          sd_wr <= '1';
          IF sd_ack='1' THEN
            state <= sWRITE5;
            sd_wr <= '0';
          END IF;

        WHEN sWRITE5 =>
          IF sd_ack='0' THEN
            state <= sIDLE;
          END IF;
          
        -------------------------------
        -- 802E : WO : Copy FLASH to RAM
        --   Read disc to buffer
        --   copy from buffer to ram
        WHEN sREAD =>
          sd_lba<=x"0000_0000";
          sd_lba(14 DOWNTO 0) <= flash_row(15 DOWNTO 1) - FIRST_FLASH_ROW(15 DOWNTO 1);
          sd_rd<='1';
          IF sd_ack='1' THEN
            state <= sREAD2;
            sd_rd<='0';
          END IF;

        WHEN sREAD2 =>
          IF sd_ack='0' THEN
            state <= sREAD3;
          END IF;

        WHEN sREAD3 =>
          fwr <= NOT flash_tog;
          flash_tog<=NOT flash_tog;
          IF flash_tog='1' THEN
            flash_cpt <= flash_cpt+1;
            IF flash_cpt=95 THEN
              state <= sIDLE;
              fwr <= '0';
            END IF;
          END IF;

        -------------------------------
        -- 802F : WO : Erase FLASH sector
        WHEN sERASE =>
          sd_lba<=x"0000_0000";
          sd_lba(14 DOWNTO 0) <= (flash_row(15 DOWNTO 3) & "00") + flash_cpt - FIRST_FLASH_ROW(15 DOWNTO 1);
          sd_wr<='1';
          IF sd_ack='1' THEN
            state <= sERASE2;
            sd_wr<='0';
          END IF;

        WHEN sERASE2 =>
          IF sd_ack='0' THEN
            state <= sERASE3;
          END IF;

        WHEN sERASE3 =>
          flash_cpt <= flash_cpt+1;
          IF flash_cpt=3 THEN
            state <= sIDLE;
          ELSE
            state <= sERASE;
          END IF;

      END CASE;
      ---------------------------------
      IF img_size > 0 THEN
        mounted <= '1';
      END IF;

      ---------------------------------
    END IF;
  END PROCESS FlashOps;




  
END ARCHITECTURE rtl;
