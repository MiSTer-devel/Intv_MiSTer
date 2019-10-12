--------------------------------------------------------------------------------
-- Intellivision Video
--------------------------------------------------------------------------------
-- DO 4/2019
--------------------------------------------------------------------------------
-- VHDL-1993
--------------------------------------------------------------------------------
-- STIC + SYSRAM + GRAM + GROM + Decode addresses

--------------------------------------------------------------------------------

-- STICREG  0000 ... 003F      16bits
-- Scratch  0100 ... 01EF       8bits
-- Sound    01F0 ... 01FF       8bits
-- SYSRAM   0200 ... 035F      16bits
-- EXECROM  1000 ... 1FFF   4k 16bits
-- GROM     3000 ... 37FF   2k  8bits
-- GRAM     3800 ... 39FF       8bits
-- CART     5000 ... 6FFF   8k 16bits 
-- CART     8000 ... FFFF  32k 16bits

--------------------------------------------------------------------------------
--  167 x 105 pixels field (NTSC)
--  168 x 104 pixels field (PAL)

--  159 x 96 displayed area

-- 20 x 12 matrix. 8x8 background = 160 x 96

-- object field : -8 pix 

--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;
USE work.cp1610_pack.ALL;
USE work.rom_pack.ALL;

ENTITY stic IS
  PORT (
    ------------------------------------
    -- CPU signals
    dw     : IN  uv16;
    dr     : OUT uv16;
    bdic   : IN  uv3;  -- BDIR / BC2 / BC1 Bus phase
    bdrdy  : OUT std_logic; -- Bus Data Ready
    busrq  : OUT std_logic; -- Bus Request
    busak  : IN  std_logic; -- Bus Acknowledge
    intrm  : OUT std_logic; -- Interrupt request maskable
    phi    : IN std_logic; -- PHI clock enable
    
    pal    : IN  std_logic;
    ecs    : IN  std_logic;
    ivoice : IN  std_logic;
    
    ------------------------------------
    ad     : OUT uv16;
    
    -- Audio chip
    snd_dr : IN  uv8;
    snd_dw : OUT uv8;
    snd_wr : OUT std_logic;

    -- Audio chip ECS
    snd2_dr : IN  uv8;
    snd2_dw : OUT uv8;
    snd2_wr : OUT std_logic;
    
    -- IVoice
    ivoice_dr : IN  uv16;
    ivoice_dw : OUT uv16;
    ivoice_wr : OUT std_logic;
    
    -- Cartridge
    cart_acc : IN std_logic;
    cart_dr  : IN  uv16;
    cart_dw  : OUT uv16;
    cart_wr  : OUT std_logic;
    
    -- Intellicart Registers
    icart_dw : OUT uv16;
    icart_wr : OUT std_logic;
    
    hits  : OUT uv64;
    hitbg : OUT uv8;
    hitbo : OUT uv8;
    
    ------------------------------------
    -- Video out
    vid_r  : OUT uv8;
    vid_g  : OUT uv8;
    vid_b  : OUT uv8;
    vid_hs : OUT std_logic;
    vid_vs : OUT std_logic;
    vid_de : OUT std_logic;
    vid_ce : OUT std_logic;
    
    ------------------------------------
    clk      : IN std_logic; -- 12x Pixel Clock
    reset_na : IN std_logic
    );
END ENTITY stic;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF stic IS

  TYPE arr_uv14 IS ARRAY (natural RANGE <>) OF uv14;
  TYPE arr_uv24 IS ARRAY (natural RANGE <>) OF uv24;
  
  SIGNAL hpos,hpos2,hlen,hsync,hsyncend,hdisp : uint9;
  SIGNAL vpos,vpos2,vlen,vsync,vsyncend,vdisp : uint9;
  CONSTANT HSTART : uint9 :=6; --20;
  CONSTANT VSTART : uint9 :=12;
  SIGNAL cyc : natural RANGE 0 TO 11;
  
  ------------------------------------------------
  TYPE type_col IS RECORD
    a : std_logic; -- 0=Transparent
    col : uv4;
  END RECORD type_col;
  
  ------------------------------------------------
  -- Character Memory address for object
  FUNCTION objadrs(vpos  : uint9;
                   delay_v : uv3;
                   ry,ra : uv14) RETURN uint11 IS
    VARIABLE v  : uint4;
    VARIABLE vu : integer RANGE -16384 TO 16383;
  BEGIN
    vu:=vpos - VSTART - 2*to_integer(delay_v) - 2*to_integer(ry(6 DOWNTO 0));
    CASE ry(9 DOWNTO 7) IS -- Ysiz4 / Ysiz2 / Yres
      WHEN "000"  => v:=(vu + 16384) MOD 8;      -- MOB 8x8 -> 8 lines
      WHEN "001"  => v:=(vu + 16384) MOD 16;     -- MOB 8x16-> 16 lines
      WHEN "010"  => v:=((vu + 16384)/2) MOD 8;  -- MOB 8x8 -> 16 lines
      WHEN "011"  => v:=((vu + 16384)/2) MOD 16; -- MOB 8x16-> 32 lines
      WHEN "100"  => v:=((vu + 16384)/4) MOD 8;  -- MOB 8x8 -> 32 lines
      WHEN "101"  => v:=((vu + 16384)/4) MOD 16; -- MOB 8x16-> 64 lines
      WHEN "110"  => v:=((vu + 16384)/8) MOD 8;  -- MOB 8x8 -> 64 lines
      WHEN OTHERS => v:=((vu + 16384)/8) MOD 16; -- MOB 8x16-> 128lines
    END CASE;
    IF ry(11)='1' THEN -- YFLIP
      IF ry(7)='0' THEN v:=7-v; ELSE v:=15-v; END IF;
    END IF;
    RETURN to_integer(ra(10 DOWNTO 3))*8 + v;
  END FUNCTION objadrs;
  
  -- Generate object pixel from GRAM/GROM character memory
  FUNCTION objpix(hpos      : uint9;
                  delay_h   : uv3;
                  gram,grom : uv8;   -- Read memory GRAM/GROM
                  rx,ry,ra  : uv14; -- MOB Registers
                  csmode    : std_logic) -- 1=Color Stack Mode, 0=FGBG mode
    RETURN type_col IS
    VARIABLE m : uv8;
    VARIABLE ch : integer RANGE -1023 TO 1024;
  BEGIN
    -- Selection GRAM / GROM
    IF ra(11)='0' THEN m:=grom; ELSE m:=gram; END IF;
    
    ch:=((hpos - HSTART - to_integer(delay_h) - to_integer(rx(7 DOWNTO 0)))
         +1024) MOD 1024;
    
    IF rx(10)='0' THEN
      ch:=ch MOD 8;
    ELSE
      ch:=(ch/2) MOD 8;
    END IF;
    IF ry(10)='0' THEN ch:=7-ch; END IF; -- FLIP X
    
    RETURN (col=>ra(12) & ra(2 DOWNTO 0),a=>m(ch));
  END FUNCTION;

  FUNCTION objhit(hpos    : uint9;
                  vpos    : uint9;
                  delay_h : uv3;
                  delay_v : uv3;
                  rx,ry   : uv14) RETURN boolean IS
    VARIABLE h,v : boolean;
    VARIABLE ch,cv : integer RANGE -1024 TO 1023;
  BEGIN
    ch:=hpos - HSTART - to_integer(delay_h) - to_integer(rx(7 DOWNTO 0));
    IF rx(10)='0' THEN
      h:=(ch>=0 AND ch<8);
    ELSE
      h:=(ch>=0 AND ch<16);
    END IF;
    IF rx(7 DOWNTO 0)=x"00" THEN h:=false; END IF;
    
    cv:=vpos - VSTART - 2*to_integer(delay_v) - 2*to_integer(ry(6 DOWNTO 0));
    CASE ry(9 DOWNTO 7) IS -- Ysiz4 / Ysiz2 / Yres
      WHEN "000"  => v:=(cv>=0 AND cv<8);
      WHEN "001"  => v:=(cv>=0 AND cv<16);
      WHEN "010"  => v:=(cv>=0 AND cv<16);
      WHEN "011"  => v:=(cv>=0 AND cv<32);
      WHEN "100"  => v:=(cv>=0 AND cv<32);
      WHEN "101"  => v:=(cv>=0 AND cv<64);
      WHEN "110"  => v:=(cv>=0 AND cv<64);
      WHEN OTHERS => v:=(cv>=0 AND cv<128);
    END CASE;
    RETURN h AND v;
  END FUNCTION;
  
  ------------------------------------------------
  -- Background access in SYSRAM
  FUNCTION cartadrs(hpos,vpos : uint9;
                    delay_h,delay_v : uv3) RETURN uint12 IS
    VARIABLE i : integer RANGE -511 TO 512;
  BEGIN
    i:=((hpos - 8 - HSTART  - to_integer(delay_h))/8 +
      20 * (((vpos - VSTART)/2 - 8 - to_integer(delay_v))/8) + 512) MOD 512;
     RETURN i;
  END FUNCTION;
  
  FUNCTION bgadrs(vpos : uint9;
                  delay_v : uv3;
                  csmode : std_logic;
                  rd : uv16) RETURN uint11 IS
  BEGIN
    IF csmode='1' THEN
      -- 2*256 cards in FG/BG mode
      RETURN to_integer(rd(10 DOWNTO 3))*8 +
        (((vpos - VSTART)/2 - to_integer(delay_v)+32) MOD 8);
    ELSE
      -- 2*64 cards in FG/BG mode
      RETURN to_integer(rd(8 DOWNTO 3))*8 +
        (((vpos - VSTART)/2 - to_integer(delay_v)+32) MOD 8);
    END IF;
  END FUNCTION;
  
  -- Generate background pixel from SYSRAM memory
  FUNCTION bgpix (
    hpos,vpos  : uint9;
    delay_h,delay_v : uv3;
    csmode  : std_logic; -- 1=Color Stack Mode, 0=FGBG mode
    gram,grom : uv8;
    cstack : uv4;
    dr : uv16) RETURN type_col IS
    VARIABLE col : uv4;
    VARIABLE v : type_col;
    VARIABLE m : uv8;
    VARIABLE ch,cv : integer RANGE -1024 TO 1023;
  BEGIN
    IF dr(11)='0' THEN m:=grom; ELSE m:=gram; END IF;
    
    ch:=(hpos - HSTART - 8 - to_integer(delay_h) + 32) MOD 8;
    cv:=((vpos - VSTART)/2 - 8 - to_integer(delay_v) + 32) MOD 8;
    
    IF dr(12 DOWNTO 11)="10" AND csmode='1' THEN
      -- Coloured Squares Mode
      IF ch<4 AND cv<4 THEN
        col:='0' & dr(2 DOWNTO 0);
      ELSIF ch>3 AND cv<4 THEN
        col:='0' & dr(5 DOWNTO 3);
      ELSIF ch<4 AND cv>3 THEN
        col:='0' & dr(8 DOWNTO 6);
      ELSE
        col:='0' & dr(13) & dr(10 DOWNTO 9);
      END IF;
      IF col="111" THEN
        v:=(col=>cstack,a=>'0');
      ELSE
        v:=(col=>col,a=>'1');
      END IF;
    ELSE
      IF csmode='1' THEN
        -- Color Stack Mode
        col:=cstack;
        IF m(7-ch)='1' THEN
          col:=dr(12) & dr(2 DOWNTO 0);
        END IF;
      ELSE
        -- FGBG Mode
        col:=dr(12) & dr(13) & dr(10) & dr(9);
        IF m(7-ch)='1' THEN
          col:='0' & dr(2 DOWNTO 0);
        END IF;
      END IF;
      v:=(col=>col,a=>m(7-ch));
    END IF;
    RETURN v;
  END FUNCTION;
  
  FUNCTION visible(hpos : uint9;
                   vpos : uint9;
                   bext_l,bext_t : std_logic) RETURN boolean IS
    VARIABLE h,v : boolean;
  BEGIN
    IF bext_l='0' THEN
      h:=(hpos >= 8 + HSTART AND hpos <  7 + HSTART + 8*20);
    ELSE
      h:=(hpos >= 16 + HSTART AND hpos <  7 + HSTART + 8*20);
    END IF;
    IF bext_t='0' THEN
      v:=(vpos >= 16 + VSTART AND vpos < 15 + VSTART + 16*12);
    ELSE
      v:=(vpos >= 32 + VSTART AND vpos < 15 + VSTART + 16*12);
    END IF;
    RETURN h AND v;
  END FUNCTION;

 ---- MOB-to-MOB collisions are evaluated within the visible screen and the
 --   1 pixel border surrounding the screen.  

 --    -- This 1 pixel border is 1 double-res pixel tall on top and bottom. 
 --       It's the same border as is used for border-collision detect.
  FUNCTION coll_mob(hpos : uint9;
                    vpos : uint9;
                    bext_l,bext_t : std_logic) RETURN boolean IS
    VARIABLE h,v : boolean;
  BEGIN
    IF bext_l='0' THEN
      h:=(hpos >=  8 + HSTART-1 AND hpos < 8 + HSTART + 8*20);
    ELSE
      h:=(hpos >= 16 + HSTART-1 AND hpos < 8 + HSTART + 8*20);
    END IF;
    IF bext_t='0' THEN
      v:=(vpos >= 16 + VSTART-1 AND vpos < 16 + VSTART + 16*12 +1);
    ELSE
      v:=(vpos >= 32 + VSTART-1 AND vpos < 16 + VSTART + 16*12 +1);
    END IF;
    RETURN h AND v;
  END FUNCTION;

 ---- MOB-to-BACKTAB collisions are evaluated within the visible screen and
 --   one column to the right of the visible screen.  That's it.  

 --    -- No MOB-to-BACKTAB collisions are calculated in the bottom edge.

 --    -- Horizontal and vertical delay are taken into account as with
 --       everything else.
  FUNCTION coll_bg(hpos,vpos : uint9;
                   bext_l,bext_t : std_logic) RETURN boolean IS
    VARIABLE h,v : boolean;
  BEGIN
    IF bext_l='0' THEN
      h:=(hpos >   8 + HSTART AND hpos < 7 + HSTART + 8*20+1);
    ELSE
      h:=(hpos >  16 + HSTART AND hpos < 7 + HSTART + 8*20+1);
    END IF;
    IF bext_t='0' THEN
      v:=(vpos >  16 + VSTART AND vpos < 15 + VSTART + 16*12-1);
    ELSE
      v:=(vpos >  32 + VSTART AND vpos < 15 + VSTART + 16*12-1);
    END IF;
    RETURN h AND v;
  END FUNCTION;

 ---- MOB-to-Border collisions are evaluated on a 1-pixel-wide border around
 --   the visible screen.
 --     -- Horizontal and vertical delay are taken into account.

 --     -- The top and bottom edges are 1 *double-res* row thick.  This only
 --        matters for MOBs that have double vertical resolution.

 --     -- MOBs w/ X==0 never interact with anything, including the borders.
  
  FUNCTION coll_border(hpos : uint9;
                       vpos : uint9;
                       bext_l,bext_t : std_logic) RETURN boolean IS
    VARIABLE h,v : boolean;
  BEGIN
    
    IF bext_l='0' THEN
      h:=(hpos =  8 + HSTART - 1 OR  hpos =  7 + HSTART + 8*20);
      v:=(hpos >= 8 + HSTART - 1 AND hpos <  8 + HSTART + 8*20);
    ELSE
      h:=(hpos = 16 + HSTART - 1 OR  hpos =  7 + HSTART + 8*20);
      v:=(hpos >=16 + HSTART - 1 AND hpos <  8 + HSTART + 8*20);
    END IF;
    
    IF bext_t='0' THEN
      h:=h AND (vpos>= 16 + VSTART - 1 AND vpos < 16 + VSTART + 16*12 + 1);
      v:=v AND (vpos = 16 + VSTART - 1 OR  vpos = 15 + VSTART + 16*12 + 1);
    ELSE
      h:=h AND (vpos>= 32 + VSTART - 1 AND vpos < 16 + VSTART + 16*12 + 1);
      v:=v AND (vpos = 32 + VSTART - 1 OR  vpos = 15 + VSTART + 16*12 + 1);
    END IF;
    RETURN h OR v;
  END FUNCTION;
  
  ------------------------------------------------
  SIGNAL pwr_x,pwr_y,pwr_a,pwr_c : std_logic;
  SIGNAL pwr_ecsram,pwr_sysram,pwr_gram,pwr_scram : std_logic;
  SIGNAL pr_x,pr_y,pr_a,pr_c : uv14;
  SIGNAL pr_sysram : uv16;
  SIGNAL pr_gram,pr_grom,pr_scram,pr_ecsram : uv8;
  SIGNAL pr_execrom,pr_ecsrom : uv16;
  
  SIGNAL prd,pwr : std_logic;
  SIGNAL padrs  : uint16;
  SIGNAL csmode : std_logic;
  SIGNAL cscpt : uint2;

  SIGNAL cstack : arr_uv4(0 TO 3);
  SIGNAL cstack_cpt,cstack_cpt_mem : uint2;
  
  SIGNAL coll,collsetbg,collsetborder : uv8;
  SIGNAL collset : arr_uv8(0 TO 7);
  SIGNAL delay_h,delay_v : uv3;
  SIGNAL de,bext_l,bext_t : std_logic;
  SIGNAL border : uv4;
  SIGNAL intrm_i : std_logic;

  SIGNAL a_mob : uint4;
  SIGNAL a_gmem : uint11;
  SIGNAL a_sysram : uint9;
  SIGNAL bg,fg,col : type_col;
  SIGNAL over,under : type_col;
  
  ------------------------------------------------
  CONSTANT Z14 : uv14 :=(OTHERS =>'0');
  SIGNAL mobx,moby,moba,mobc : arr_uv14(0 TO 7) :=(OTHERS =>Z14);
  SIGNAL r_x,r_x2,r_y,r_y2,r_a,r_a2 : uv14;
  SIGNAL r_gram,r_grom : uv8;
  SIGNAL r_sysram : uv16;
  
  CONSTANT GROM : arr8(0 TO 2047) := INIT_GROM;
  
  SIGNAL   gram : arr_uv8(0 TO 511) :=(OTHERS =>x"00"); -- 512 * 8bits
  SIGNAL sysram : arr_uv16(0 TO 511) :=(OTHERS =>x"0000"); -- 160h = 352 * 16bits real
  SIGNAL scram  : arr_uv8(0 TO 255) :=(OTHERS =>x"00"); -- 256 * 8bits

  SIGNAL ecsram : arr_uv8(0 TO 2047):=(OTHERS =>x"00"); -- 2k * 8bits

  CONSTANT EXECROM : arr16(0 TO 4095) := INIT_EXEC; -- Executive ROM
  
  CONSTANT ECSROM  : arr16(0 TO 12287) := INIT_ECS; -- ECS ROM
  SIGNAL bank : arr_uv4(0 TO 15);
  
  CONSTANT PALETTE : arr_uv24(0 TO 15) := -- RRGGBB
    (x"0C0005",x"002DFF",x"FF3E00",x"C9D464", -- 8 primAry
     x"00780F",x"00A720",x"FAEA27",x"FFFCFF",
     x"A7A8A8",x"5ACBFF",x"FFA600",x"3C5800", -- 8 pastels
     x"FF3276",x"BD95FF",x"6CCD30",x"C81A7D");

  SIGNAL xxx_hit : boolean;
  SIGNAL xxx_pix : type_col;
BEGIN
  
  ------------------------------------------------------------------------------
  RegAcc:PROCESS (clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      delay_v<="000";
      delay_h<="000";
      border<="0000";
      bext_t<='0';
      bext_l<='0';
      csmode<='0';
      bank<=(OTHERS =>x"0");
      
    ELSIF rising_edge(clk) THEN
      ----------------------------------
      pwr<='0';
      prd<='0';
      IF bdic=B_BAR OR bdic=B_ADAR OR bdic=B_INTAK THEN
        IF phi='1' THEN
          padrs<=to_integer(dw);
        END IF;
      ELSIF bdic=B_DWS THEN
        pwr<='1';
      ELSIF bdic=B_DTB THEN
        prd<='1';
      END IF;
      
      ----------------------------------
      pwr_x<='0';
      pwr_y<='0';
      pwr_a<='0';
      pwr_c<='0';
      pwr_sysram<='0';
      pwr_gram<='0';

      snd_wr<='0';
      snd2_wr<='0';
      ivoice_wr<='0';
      cart_wr<='0';
      snd_dw  <=dw(7 DOWNTO 0);
      snd2_dw <=dw(7 DOWNTO 0);
      ivoice_dw<=dw;
      icart_dw<=dw;
      
      -- 14 bits registers
      -- 0000-0007 MOB X position regs ? ? ? Xsize VISB INTR X[7:0]
      -- 0008-000F MOB Y position regs ? ? Yflip Xflip Ysz4 Ysz2 Yres Y[6:0]
      -- 0010-0017 MOB attribute  regs PRIO FG3 GRAMROM Cart[7:0] FGcol[2:0]
      -- 0018-001F MOB collision  regs ? ? ? Cbord Cbg Cmob[7:0]
      -- 0020      Display Enable
      -- 0021      Color Stack Mode select
      -- 0022-0027 <reserved>
      -- 0028-002B Color stack 0...3
      -- 002C      Display border colour
      -- 002D-002F <reserved>
      -- 0030      Horizontal delay register
      -- 0031      Vertical   delay register
      -- 0032      Border extension (0=left 1=top)
      -- 0033-007F <reserved>

      -- STIC registers ----------------
        
      --IF padrs>=16#D000# AND padrs<=16#DFFF# THEN -- 4kw
      --  dr<=cart_dr;
        
      --ELSIF padrs>=16#F000# AND padrs<=16#FFFF# THEN -- 4kw
      --  dr<=cart_dr;
      
      --  ELS
      IF padrs MOD 16384<8 THEN
        dr<="00111" & pr_x(10 DOWNTO 0);
        pwr_x<=pwr;
        
      ELSIF padrs MOD 16384<16 THEN
        dr<="0011" & pr_y(11 DOWNTO 0);
        pwr_y<=pwr;
        
      ELSIF padrs MOD 16384<24 THEN
        dr<="00" & pr_a;
        pwr_a<=pwr;
        
      ELSIF padrs MOD 16384<32 THEN
        dr<="001111" & pr_c(9 DOWNTO 0);
        pwr_c<=pwr;
        
      ELSIF padrs MOD 16384=32 THEN
        dr<=x"0000"; -- Display Enable
        IF prd='1' THEN
          de<='0';
        END IF;
        IF pwr='1' THEN
          de<='1';
        END IF;
        
      ELSIF padrs MOD 16384=33 THEN
        dr<=x"0000"; -- Mode Select
        IF prd='1' THEN
          csmode<='1'; -- Color Stack Mode
        END IF;
        IF pwr='1' THEN
          csmode<='0'; -- FG / BG Mode
        END IF;
        
      ELSIF padrs MOD 16384=16#28# THEN
        dr<=x"000" & cstack(0);
        IF pwr='1' THEN
          cstack(0)<=dw(3 DOWNTO 0);
        END IF;

      ELSIF padrs MOD 16384=16#29# THEN
        dr<=x"000" & cstack(1);
        IF pwr='1' THEN
          cstack(1)<=dw(3 DOWNTO 0);
        END IF;
          
      ELSIF padrs MOD 16384=16#2A# THEN
        dr<=x"000" & cstack(2);
        IF pwr='1' THEN
          cstack(2)<=dw(3 DOWNTO 0);
        END IF;
          
      ELSIF padrs MOD 16384=16#2B# THEN
        dr<=x"000" & cstack(3);
        IF pwr='1' THEN
          cstack(3)<=dw(3 DOWNTO 0);
        END IF;

      ELSIF padrs MOD 16384=16#2C# THEN
        dr<=x"000" & border;
        IF pwr='1' THEN
          border<=dw(3 DOWNTO 0);
        END IF;
        
      ELSIF padrs MOD 16384=16#30# THEN
        dr<="0000000000000" & delay_h;
        dr(15 DOWNTO 3)<=(OTHERS =>'0');
        IF pwr='1' THEN
          delay_h<=dw(2 DOWNTO 0);
        END IF;
          
      ELSIF padrs MOD 16384=16#31# THEN
        dr<="0000000000000" & delay_v;
        IF pwr='1' THEN
          delay_v<=dw(2 DOWNTO 0);
        END IF;

      ELSIF padrs MOD 16384=16#32# THEN
        dr<=x"000" & "00" & bext_t & bext_l;
        IF pwr='1' THEN
          -- Border Extension bits
          bext_l<=dw(0);
          bext_t<=dw(1);
        END IF;
        
      -- SYSRAM ------------------------
      ELSIF padrs>=16#200# AND padrs<=16#3FF# THEN
        dr<=pr_sysram;
        pwr_sysram<=pwr;
        
      -- GROM --------------------------
      ELSIF padrs>=16#3000# AND padrs<=16#37FF# THEN
        dr<=x"00" & pr_grom;
        
      -- GRAM --------------------------
      ELSIF (padrs >=16#3800# AND padrs <=16#3FFF#) THEN
--      ELSIF padrs MOD 16384>=16#3800# AND padrs MOD 16384<=16#39FF# THEN
        dr<=x"00" & pr_gram;
        pwr_gram<=pwr;
        
      -- EXEC --------------------------
      ELSIF padrs>=16#1000# AND padrs<=16#1FFF# THEN
        dr<=pr_execrom;
        
      -- Scratch RAM -------------------
      ELSIF padrs>=16#0100# AND padrs<=16#01EF# THEN
        dr<=x"00" & pr_scram;
        pwr_scram<=pwr;
        
      -- Sound chip --------------------
      ELSIF padrs>=16#01F0# AND padrs<=16#01FF# THEN
        dr<=x"00" & snd_dr;
        snd_wr<=pwr;
        
      -- IntelliVoice ------------------
      ELSIF padrs>=16#0080# AND padrs<=16#0081# AND ivoice='1' THEN
        dr<=ivoice_dr;
        ivoice_wr<=pwr;
        
      -- Sound chip ECS ----------------
      ELSIF padrs>=16#00F0# AND padrs<=16#00FF# AND ecs='1' THEN
        dr<=x"00" & snd2_dr;
        snd2_wr<=pwr;
        
      -- RAM ECS -----------------------
      ELSIF padrs>=16#4000# AND padrs<=16#47FF# AND ecs='1' THEN
        dr<=x"00" & pr_ecsram;
        pwr_ecsram<=pwr;
        
      -- ROM ECS -----------------------
      ELSIF padrs  >=16#2000# AND padrs<=16#2FFF# AND ecs='1' AND bank(2)=x"1" THEN
        dr<=pr_ecsrom;
        
      ELSIF padrs>=16#7000# AND padrs<=16#7FFF# AND ecs='1' AND bank(7)=x"0" THEN
        dr<=pr_ecsrom;
        
      ELSIF padrs>=16#E000# AND padrs<=16#EFFF# AND ecs='1' AND bank(14)=x"1" THEN
        dr<=pr_ecsrom;
        
      -- Cartridges --------------------
      ELSE
        dr<=cart_dr;
        cart_wr<=pwr;
        
      END IF;
      
      -- BANK SWITCH REG ---------------
      IF padrs=16#2FFF# AND dw(15 DOWNTO 4)=x"2A5" AND pwr='1' AND ecs='1' THEN
        bank(2)<=dw(3 DOWNTO 0);
      END IF;
      IF padrs=16#7FFF# AND dw(15 DOWNTO 4)=x"7A5" AND pwr='1' AND ecs='1' THEN
        bank(7)<=dw(3 DOWNTO 0);
      END IF;
      IF padrs=16#EFFF# AND dw(15 DOWNTO 4)=x"EA5" AND pwr='1' AND ecs='1' THEN
        bank(14)<=dw(3 DOWNTO 0);
      END IF;
      
      -- INTELLICART REGS --------------
      icart_wr<='0';
      IF padrs>=16#40# AND padrs<=16#5F# THEN
        icart_wr<=pwr;
      END IF;
      
      ----------------------------------
      IF bdic=B_IAB THEN -- Interrupt to Address Bus
        IF intrm_i='1' THEN
          dr<=x"1004"; -- Interrupt vector
        ELSE
          dr<=x"1000"; -- RESET vector
        END IF;
      END IF;
      
      ----------------------------------
      bdrdy<='1';
      
    END IF;
  END PROCESS RegAcc;

  ------------------------------------------------------------------------------
  Mem:PROCESS (clk,reset_na) IS
    VARIABLE ad_v : uint16;
  BEGIN
    IF reset_na='0' THEN
      mobx<=(OTHERS =>"00000000000000");
      moby<=(OTHERS =>"00000000000000");
      moba<=(OTHERS =>"00000000000000");
      mobc<=(OTHERS =>"00000000000000");
      
    ELSIF rising_edge(clk) THEN

      --------------------------
      -- CPU Access
      pr_x<=mobx(padrs MOD 8);
      pr_y<=moby(padrs MOD 8);
      pr_a<=moba(padrs MOD 8);
      pr_c<=mobc(padrs MOD 8);
      pr_sysram<=sysram(padrs MOD 512);
      pr_ecsram<=ecsram(padrs MOD 2048);
      pr_grom  <=GROM(padrs MOD 2048);
      pr_gram  <=gram(padrs MOD 512);
      pr_scram <=scram(padrs MOD 256);
      
      pr_execrom  <=EXECROM(padrs MOD 4096);
      
      IF padrs>=16#2000# AND padrs<=16#2FFF# THEN
        ad_v:=padrs - 16#2000#;
      ELSIF padrs>=16#7000# AND padrs<=16#7FFF# THEN
        ad_v:=padrs - 16#6000#;
      ELSIF padrs>=16#E000# AND padrs<=16#EFFF# THEN
        ad_v:=padrs - 16#C000#;
      ELSE
        ad_v:=padrs MOD 4096;
      END IF;
      pr_ecsrom<=ECSROM(ad_v);
      
      -- Set collision bits
      FOR i IN 0 TO 7 LOOP
        -- Objects collisions
        mobc(i)(7 DOWNTO 0)<=mobc(i)(7 DOWNTO 0) OR collset(i);
        -- Background collision
        mobc(i)(8)<=mobc(i)(8) OR collsetbg(i);
        -- Border collision
        mobc(i)(9)<=mobc(i)(9) OR collsetborder(i);
      END LOOP;
      
      IF pwr_x='1' THEN mobx(padrs MOD 8)<=dw(13 DOWNTO 0); END IF;
      IF pwr_y='1' THEN moby(padrs MOD 8)<=dw(13 DOWNTO 0); END IF;
      IF pwr_a='1' THEN moba(padrs MOD 8)<=dw(13 DOWNTO 0); END IF;
      IF pwr_c='1' THEN mobc(padrs MOD 8)<=dw(13 DOWNTO 0); END IF;
      
      IF pwr_sysram='1' THEN sysram(padrs MOD 512)<=dw(15 DOWNTO 0); END IF;
      IF pwr_ecsram='1' THEN ecsram(padrs MOD 2048)<=dw(7 DOWNTO 0); END IF;
      IF pwr_gram='1'   THEN gram(padrs MOD 512)<=dw(7 DOWNTO 0); END IF;
      IF pwr_scram='1'  THEN scram(padrs MOD 256)<=dw(7 DOWNTO 0); END IF;

      -- Clear self-collision bit
      FOR i IN 0 TO 7 LOOP
        mobc(i)(i)<='0';
      END LOOP;
      
      --------------------------
      -- STIC access
      r_x<=mobx(a_mob);
      r_y<=moby(a_mob);
      r_a<=moba(a_mob);
      r_sysram<=sysram(a_sysram);

      
      r_x2<=r_x;
      r_y2<=r_y;
      r_a2<=r_a;
      
      r_gram <=gram(a_gmem MOD 512);
      r_grom <=GROM(a_gmem MOD 2048);
      
      --------------------------
      
    END IF;
  END PROCESS Mem;

  ad<=to_unsigned(padrs,16);
  
  ------------------------------------------------------------------------------
  -- Memory blocks address selection
  MemAdrs:PROCESS(cyc,hpos,vpos,delay_h,delay_v,
                  r_y,r_a,r_sysram) IS
  BEGIN
    a_sysram<=cartadrs(hpos,vpos,delay_h,delay_v);
    a_mob<=0;
    a_gmem<=objadrs(vpos,delay_v,r_y,r_a);
    
    CASE cyc IS
      WHEN 0 =>
        a_mob<=0; -- Select Obj 0
        
      WHEN 1 =>
        a_mob<=1; -- Select Obj 1
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 0
        
      WHEN 2 =>
        a_mob<=2; -- Select Obj 2
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 1
        
      WHEN 3 =>
        a_mob<=3; -- Select Obj 3
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 2
        
      WHEN 4 =>
        a_mob<=4; -- Select Obj 4
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 3
        
      WHEN 5 =>
        a_mob<=5; -- Select Obj 5
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 4
        
      WHEN 6 =>
        a_mob<=6; -- Select Obj 6
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 5
        
      WHEN 7 =>
        a_mob<=7; -- Select Obj 7
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 6
        
      WHEN 8 =>
        -- Select background
        a_gmem<=objadrs(vpos,delay_v,r_y,r_a); -- Reg obj 7
        
      WHEN 9 =>
        a_gmem<=bgadrs(vpos,delay_v,csmode,r_sysram); -- Background
        
      WHEN OTHERS =>
        NULL;
        
    END CASE;
  END PROCESS MemAdrs;

  ----------------------------------
  hdisp<=180    WHEN pal='0' ELSE 180;
  hsync<=185    WHEN pal='0' ELSE 208;
  hsyncend<=202 WHEN pal='0' ELSE 227;
  hlen <=228    WHEN pal='0' ELSE 256;

  vdisp<=240    WHEN pal='0' ELSE 288;
  vsync<=242    WHEN pal='0' ELSE 290;
  vsyncend<=245 WHEN pal='0' ELSE 293;
  vlen <=262    WHEN pal='0' ELSE 312;
  
  ------------------------------------------------------------------------------
  Sync:PROCESS(clk,reset_na) IS
    VARIABLE p_v : type_col;
    VARIABLE cpt_v : uint2;
  BEGIN
    IF reset_na='0' THEN
      intrm_i<='0';
      busrq<='0';
      collset<=(x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00");
      collsetbg<=x"00";
      collsetborder<=x"00";
      
    ELSIF rising_edge(clk) THEN
      
      ----------------------------------
      hpos2<=hpos;
      vpos2<=vpos;
      collset<=(x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00");
      collsetbg<=x"00";
      collsetborder<=x"00";
      
      ----------------------------------
      IF cyc<11 THEN cyc<=cyc+1; ELSE cyc<=0; END IF;
      
      ----------------------------------
      CASE cyc IS
        WHEN 0 => -- CLEAR, video sweep
          IF hpos<hlen-1 THEN
            hpos<=hpos+1;
          ELSE
            hpos<=0;
            IF vpos<vlen-1 THEN
              vpos<=vpos+1;
            ELSE
              vpos<=0;
              cstack_cpt<=0;
              cstack_cpt_mem<=0;
            END IF;
          END IF;
          over.a <='0';
          under.a<='0';
          IF hpos=0 AND vpos = 16 + VSTART + 8*12*2 THEN
            intrm_i<='1';
          END IF;
          IF hpos=0 AND vpos = 9 THEN
            intrm_i<='0';
          END IF;
          
        WHEN 1 => NULL;
          
        WHEN 2 TO 9 => -- Objects
          p_v:=objpix(hpos,delay_h,r_gram,r_grom,r_x2,r_y2,r_a2,csmode);
          xxx_pix<=p_v;
          p_v.a:=p_v.a AND
                  to_std_logic(objhit(hpos,vpos,delay_h,delay_v,r_x2,r_y2));
          xxx_hit<=objhit(hpos,vpos,delay_h,delay_v,r_x2,r_y2);
          IF r_a2(13)='0' THEN -- 0=Before background 1=Behind background
            IF p_v.a='1' AND r_x2(9)='1' AND over.a='0' THEN -- If visible and non transparent
              over <=p_v;
            END IF;
          ELSE
            IF p_v.a='1' AND r_x2(9)='1' AND under.a='0' THEN
              under<=p_v;
            END IF;
          END IF;
          coll(cyc-2)<=p_v.a AND r_x2(8);
          
        WHEN 10 => -- Background
          cpt_v:=cstack_cpt;
          IF r_sysram(13)='1' AND csmode='1' AND
            r_sysram(12 DOWNTO 11)/="10" AND
            (hpos - HSTART - to_integer(delay_h)) MOD 8=0 AND
            visible(hpos,vpos,bext_l,bext_t)
          THEN -- Advance colour stack counter
            cpt_v:=(cstack_cpt+1) MOD 4;
            cstack_cpt<=cpt_v;
          END IF;

          IF hpos=0 AND 
            (((vpos - VSTART) - 2*to_integer(delay_v)+64) MOD 16)=0 THEN
            cstack_cpt_mem<=cstack_cpt;
          END IF;
          IF hpos=0 AND 
            (((vpos - VSTART) - 2*to_integer(delay_v)+64) MOD 16)/=0 THEN
            cstack_cpt<=cstack_cpt_mem;
          END IF;
                    
          bg<=bgpix(hpos,vpos,delay_h,delay_v,csmode,r_gram,r_grom,
                    cstack(cpt_v),r_sysram);
          
        WHEN 11 =>
          IF NOT visible(hpos,vpos,bext_l,bext_t) THEN
            col<=('0',border);
          ELSIF over.a='1' THEN
            col<=over;
          ELSIF bg.a='1' THEN
            col<=bg;
          ELSIF under.a='1' THEN
            col<=under;
          ELSE
            col<=bg;
          END IF;
          
          FOR i IN 0 TO 7 LOOP
            IF coll(i)='1' AND coll_border(hpos,vpos,bext_l,bext_t) THEN
              collsetborder(i)<='1';
            END IF;
            FOR j IN 0 TO 7 LOOP
              IF coll(i)='1' AND coll(j)='1' AND
              coll_mob(hpos,vpos,bext_l,bext_t) THEN
                collset(i)(j)<='1';
              END IF;
            END LOOP;
            IF coll(i)='1' AND bg.a='1' AND
              coll_bg(hpos,vpos,bext_l,bext_t) THEN
              collsetbg(i)<='1';
            END IF;
          END LOOP;
          
        WHEN OTHERS =>
          NULL;
      END CASE;
      
      ----------------------------------
      -- Video output
      vid_r<=PALETTE(to_integer(col.col))(23 DOWNTO 16);
      vid_g<=PALETTE(to_integer(col.col))(15 DOWNTO 8);
      vid_b<=PALETTE(to_integer(col.col))( 7 DOWNTO 0);
      
      vid_hs<=to_std_logic(hpos>=hsync AND hpos<hsyncend);
      vid_vs<=to_std_logic((vpos=vsync AND hpos>=hsync) OR
                           (vpos>vsync AND vpos<vsyncend) OR
                           (vpos=vsyncend AND hpos<hsyncend)) ;
      
      vid_de<=to_std_logic(hpos<hdisp AND vpos<vdisp);
      vid_ce<=to_std_logic(cyc=11);
      
      ----------------------------------
      -- Interrupt acknowledge
      IF bdic=B_IAB AND phi='1' THEN
        intrm_i<='0';
      END IF;
      
      ----------------------------------
      -- BUSRQ (just for slowing down the CPU)
      IF (vpos=VSTART-1                             AND hpos=0) OR
         (vpos=VSTART+1       + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16    + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*2  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*3  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*4  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*5  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*6  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*7  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*8  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*9  + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*10 + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*11 + to_integer(delay_v) AND hpos=0) OR
         (vpos=VSTART+1+16*12 + to_integer(delay_v) AND hpos=0) THEN
        busrq<='1';
      ELSIF (vpos=VSTART AND hpos=0) OR
         (vpos=VSTART+2       + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16    + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*2  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*3  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*4  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*5  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*6  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*7  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*8  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*9  + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*10 + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+2+16*11 + to_integer(delay_v) AND hpos=212) OR
         (vpos=VSTART+1+16*12 + to_integer(delay_v) AND hpos=176) THEN
        busrq<='0';
      END IF;
    END IF;
  END PROCESS Sync;

  intrm<=intrm_i;
  
  hits<=mobc(7)(7 DOWNTO 0) & mobc(6)(7 DOWNTO 0) &
        mobc(5)(7 DOWNTO 0) & mobc(4)(7 DOWNTO 0) &
        mobc(3)(7 DOWNTO 0) & mobc(2)(7 DOWNTO 0) &
        mobc(1)(7 DOWNTO 0) & mobc(0)(7 DOWNTO 0);
  hitbg<=mobc(7)(8) & mobc(6)(8) & mobc(5)(8) & mobc(4)(8) &
         mobc(3)(8) & mobc(2)(8) & mobc(1)(8) & mobc(0)(8);
  hitbo <=mobx(7)(9) & mobx(6)(9) & mobx(5)(9) & mobx(4)(9) &
         mobx(3)(9) & mobx(2)(9) & mobx(1)(9) & mobx(0)(9);
  
END ARCHITECTURE rtl;

