--------------------------------------------------------------------------------
-- Intellivision
--------------------------------------------------------------------------------
-- Developed with the help of the JZINTV emulator
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;

--USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY intv_core IS
  PORT (
    clksys           : IN    std_logic;
    pll_locked       : IN    std_logic;
    
    pal              : IN    std_logic;
    swap             : IN    std_logic;
    ecs              : IN    std_logic;
    ivoice           : IN    std_logic;
    jlp              : IN    std_logic;
    mapp             : IN    std_logic_vector(3 DOWNTO 0);
    format           : IN    std_logic_vector(1 DOWNTO 0);
    reset            : IN    std_logic;

    sdram_dq          : INOUT std_logic_vector(15 DOWNTO 0);
    sdram_a           : OUT   std_logic_vector(12 DOWNTO 0);
    sdram_dqml        : OUT   std_logic;
    sdram_dqmh        : OUT   std_logic;
    sdram_ba          : OUT   std_logic_vector(1 DOWNTO 0);
    sdram_ncs         : OUT   std_logic;
    sdram_nwe         : OUT   std_logic;
    sdram_nras        : OUT   std_logic;
    sdram_ncas        : OUT   std_logic;
    sdram_cke         : OUT   std_logic;
    sdram_clk         : OUT   std_logic;

    -- VGA
    vga_clk          : OUT   std_logic;
    vga_ce           : OUT   std_logic;
    vga_r            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_g            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_b            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_hs           : OUT   std_logic; -- positive pulse!
    vga_vs           : OUT   std_logic; -- positive pulse!
    vga_de           : OUT   std_logic; -- = not (VBlank or HBlank)
    vga_hb           : OUT   std_logic;
    vga_vb           : OUT   std_logic;
    
    -- HPS IO
    joystick_0        : IN  unsigned(31 DOWNTO 0);
    joystick_1        : IN  unsigned(31 DOWNTO 0);
    joystick_analog_0 : IN  unsigned(15 DOWNTO 0);
    joystick_analog_1 : IN  unsigned(15 DOWNTO 0);
    ps2_key           : IN  std_logic_vector(10 DOWNTO 0);

    ioctl_download    : IN  std_logic;
    ioctl_index       : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wr          : IN  std_logic;
    ioctl_addr        : IN  std_logic_vector(24 DOWNTO 0);
    ioctl_dout        : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wait        : OUT std_logic;

    -- JLP RW FLASH
    img_mounted       : IN std_logic;
    img_size          : IN unsigned(63 DOWNTO 0);
    img_readonly      : IN std_logic;
    sd_lba            : OUT unsigned(31 DOWNTO 0);
    sd_rd             : OUT std_logic;
    sd_wr             : OUT std_logic;
    sd_ack            : IN  std_logic;
    sd_buff_addr      : IN  unsigned(8 DOWNTO 0);
    sd_buff_dout      : IN  std_logic_vector(7 DOWNTO 0);
    sd_buff_din       : OUT std_logic_vector(7 DOWNTO 0);
    sd_buff_wr        : IN  std_logic;

    -- AUDIO
    audio_l          : OUT   std_logic_vector(15 DOWNTO 0);
    audio_r          : OUT   std_logic_vector(15 DOWNTO 0)
    );
END intv_core;

ARCHITECTURE struct OF intv_core IS

  CONSTANT CDIV : natural := 12 * 8;
  
  SIGNAL inclk : std_logic_vector(3 DOWNTO 0);
  SIGNAL clkselect : std_logic_vector(1 DOWNTO 0);
  
  SIGNAL ps2_key_delay,ps2_key_mem : std_logic_vector(10 DOWNTO 0);
  
  SIGNAL key_0,key_1,key_2,key_3,key_4,key_5,key_6,key_7 : std_logic;
  SIGNAL key_8,key_9,key_a,key_b,key_c,key_d,key_e,key_f : std_logic;
  SIGNAL key_g,key_h,key_i,key_j,key_k,key_l,key_m,key_n : std_logic;
  SIGNAL key_o,key_p,key_q,key_r,key_s,key_t,key_u,key_v : std_logic;
  SIGNAL key_w,key_x,key_y,key_z : std_logic;
  SIGNAL key_space,key_colon,key_period,key_comma : std_logic;
  SIGNAL key_up,key_down,key_right,key_left : std_logic;
  SIGNAL key_enter,key_esc,key_lshift,key_rshift,key_lctrl,key_rctrl : std_logic;
  
  SIGNAL key_rc   ,key_wc  ,key_bp ,key_pc  : std_logic;
  SIGNAL key_minus,key_plus,key_reg,key_mem : std_logic;
  
  ----------------------------------------
  SIGNAL reset_na : std_logic;
  
  SIGNAL clkdiv,clkdivsnd,clkdivivoice : uint6 :=0;
  SIGNAL tick_cpu,tick_cpup,tick_snd,tick_ivoice : std_logic;
    
  SIGNAL dr,dw,ad,cart_dr,cart_dw : uv16;
  SIGNAL snd_dr,snd_dw,snd2_dr,snd2_dw : uv8;
  SIGNAL snd_wr,snd2_wr,cart_wr,cart_rd, cart_rdy : std_logic;
  SIGNAL jlp_dw, jlp_dr : uv16;
  SIGNAL jlp_wr : std_logic;
  SIGNAL ivoice_dr,ivoice_dw : uv16;
  SIGNAL ivoice_wr : std_logic;
  SIGNAL ivoice_divi : uint9;
  SIGNAL sound,sound2 : uv12;
  SIGNAL sound_iv : sv16;
  SIGNAL bdic : uv3;
  SIGNAL bdrdy,busrq,busak,halt,intrm : std_logic;
  SIGNAL pa_i,pb_i,pa_o,pb_o : uv8;
  SIGNAL pa2_i,pb2_i,pa2_o,pb2_o : uv8;
  SIGNAL pa_en,pb_en,pa2_en,pb2_en : std_logic;
  SIGNAL clear : std_logic;
  SIGNAL rom_dw : uv8;
  SIGNAL rom_aw : uv16;
  SIGNAL rom_voice_wr,rom_grom_wr,rom_exec_wr,rom_ecs_wr : std_logic;
  
  SIGNAL vga_r_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_g_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_b_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_de_u,vga_de_v : std_logic;
  SIGNAL vga_ce_l,vga_ce2,vga_ce3,vga_ce4,vga_ce5  : std_logic;

  SIGNAL hwreset_n : std_logic;
  SIGNAL map_reset : std_logic;
  SIGNAL icart_dw : uv16;
  SIGNAL icart_wr : std_logic;

  -------------------------------------
  -- SDRAM
  COMPONENT sdram IS
    PORT (
      init       : IN    std_logic;
      clk        : IN    std_logic;
      SDRAM_DQ   : INOUT std_logic_vector(15 DOWNTO 0);
      SDRAM_A    : OUT   std_logic_vector(12 DOWNTO 0);
      SDRAM_DQML : OUT   std_logic;
      SDRAM_DQMH : OUT   std_logic;
      SDRAM_BA   : OUT   std_logic_vector(1 DOWNTO 0);
      SDRAM_nCS  : OUT   std_logic;
      SDRAM_nWE  : OUT   std_logic;
      SDRAM_nRAS : OUT   std_logic;
      SDRAM_nCAS : OUT   std_logic;
      SDRAM_CKE  : OUT   std_logic;
      SDRAM_CLK  : OUT   std_logic;
      wtbt       : IN    std_logic_vector(1 DOWNTO 0);
      addr       : IN    std_logic_vector(24 DOWNTO 0);
      dout       : OUT   std_logic_vector(15 DOWNTO 0);
      din        : IN    std_logic_vector(15 DOWNTO 0);
      wr         : IN    std_logic;
      rd         : IN    std_logic;
      ready      : OUT   std_logic
    );
  END COMPONENT;

  SIGNAL sdram_init  : std_logic;
  SIGNAL sdram_req   : std_logic;
  SIGNAL sdram_wtbt  : std_logic_vector(1 DOWNTO 0);
  SIGNAL sdram_addr  : std_logic_vector(24 DOWNTO 0);
  SIGNAL sdram_dout  : std_logic_vector(15 DOWNTO 0);
  SIGNAL sdram_din   : std_logic_vector(15 DOWNTO 0);
  SIGNAL sdram_wr    : std_logic;
  SIGNAL sdram_rd    : std_logic;
  SIGNAL sdram_ready : std_logic;
  SIGNAL sdram_dr    : uv8;
  SIGNAL sdram_hilo  : std_logic;
  TYPE enum_sdram_state IS (sIDLE,sDOWN,sCPU,sVRAM);
  SIGNAL sdram_state  : enum_sdram_state;
  SIGNAL sdram_state2 : enum_sdram_state;
  SIGNAL sdram_state3 : enum_sdram_state;

BEGIN 

  ----------------------------------------------------------
  
  Clepsydre:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      tick_cpup<='0';
      IF clkdiv/=12*4-1 THEN
        clkdiv<=clkdiv+1;
      ELSE
        clkdiv<=0;
        tick_cpup<='1';
      END IF;
      tick_cpu<=tick_cpup;
      
      tick_snd<='0';
      IF tick_cpu='1' THEN
        clkdivsnd<=(clkdivsnd+1) MOD 4;
        IF clkdivsnd=0 THEN
          tick_snd<='1';
        END IF;
      END IF;
      
      IF clkdivivoice=11 THEN
        tick_ivoice<='1';
        clkdivivoice<=0;
      ELSE
        tick_ivoice<='0';
        clkdivivoice<=clkdivivoice+1;
      END IF;
    END IF;
  END PROCESS Clepsydre;
  
  ----------------------------------------------------------
  -- CPU
  i_cp1610: ENTITY work.cp1610
    PORT MAP (
      dr       => dr,
      dw       => dw,
      bdic     => bdic,
      ebci     => x"0000",
      msync    => '0',
      bdrdy    => bdrdy,
      intr     => '0',
      intrm    => intrm,
      tci      => OPEN,
      pci      => '0',
      pct      => OPEN,
      busrq    => busrq,
      busak    => busak,
      stpst    => '0',
      halt     => halt,
      phi      => tick_cpu,
      phip     => tick_cpup,
      clk      => clksys,
      reset_na => reset_na);
  
  -- STIC + SYSRAM + GRAM + GROM + Decoder
  i_stic: ENTITY work.stic
    PORT MAP (
      dw          => dw,
      dr          => dr,
      bdic        => bdic,
      bdrdy       => bdrdy,
      busrq       => busrq,
      busak       => busak,
      intrm       => intrm,
      phi         => tick_cpu,
      pal         => pal,
      ecs         => ecs,
      ivoice      => ivoice,
      jlp         => jlp,
      clear       => clear,
      ad          => ad,
      snd_dr      => snd_dr,
      snd_dw      => snd_dw,
      snd_wr      => snd_wr,
      snd2_dr     => snd2_dr,
      snd2_dw     => snd2_dw,
      snd2_wr     => snd2_wr,
      ivoice_dr   => ivoice_dr,
      ivoice_dw   => ivoice_dw,
      ivoice_wr   => ivoice_wr,
      jlp_dr      => jlp_dr,
      jlp_dw      => jlp_dw,
      jlp_wr      => jlp_wr,
      cart_dr     => cart_dr,
      cart_dw     => cart_dw,
      cart_rd     => cart_rd,
      cart_wr     => cart_wr,
      cart_rdy    => cart_rdy,
      icart_dw    => icart_dw,
      icart_wr    => icart_wr,
      rom_grom_wr => rom_grom_wr,
      rom_exec_wr => rom_exec_wr,
      rom_ecs_wr  => rom_ecs_wr,
      rom_aw      => rom_aw,
      rom_dw      => rom_dw,
      vid_r       => vga_r_u,
      vid_g       => vga_g_u,
      vid_b       => vga_b_u,
      vid_de      => vga_de_u,
      vid_hs      => vga_hs,
      vid_vs      => vga_vs,
      vid_hb      => vga_hb,
      vid_vb      => vga_vb,
      vid_ce      => vga_ce_l,
      clk         => clksys,
      reset_na    => reset_na);

  -- CARTRIDGE
  i_cart: ENTITY work.cart
    PORT MAP (
      mapp           => mapp,
      ecs            => ecs,
      format         => format,
      ad             => ad,
      cart_dr        => cart_dr,
      cart_dw        => cart_dw,
      cart_rd        => cart_rd,
      cart_wr        => cart_wr,
      cart_rdy       => cart_rdy,
      icart_dw       => icart_dw,
      icart_wr       => icart_wr,
      map_reset      => map_reset,
      clear          => clear,
      sdram_init     => sdram_init,
      sdram_wtbt     => sdram_wtbt,
      sdram_addr     => sdram_addr,
      sdram_dout     => sdram_dout,
      sdram_din      => sdram_din,
      sdram_wr       => sdram_wr,
      sdram_rd       => sdram_rd,
      sdram_ready    => sdram_ready,
      ioctl_download => ioctl_download,
      ioctl_index    => ioctl_index,
      ioctl_wr       => ioctl_wr,
      ioctl_addr     => ioctl_addr,
      ioctl_dout     => ioctl_dout,
      ioctl_wait     => ioctl_wait,
      rom_aw         => rom_aw,
      rom_dw         => rom_dw,
      rom_grom_wr    => rom_grom_wr,
      rom_exec_wr    => rom_exec_wr,
      rom_ecs_wr     => rom_ecs_wr,
      rom_voice_wr   => rom_voice_wr,
      phi            => tick_cpu,
      clksys         => clksys,
      reset          => reset,
      reset_na       => reset_na,
      hwreset_n      => hwreset_n);

  -- SDRAM CTRL
  i_sdram : sdram
    PORT MAP (
      init       => sdram_init,
      clk        => clksys,
      SDRAM_DQ   => sdram_dq,
      SDRAM_A    => sdram_a,
      SDRAM_DQML => sdram_dqml,
      SDRAM_DQMH => sdram_dqmh,
      SDRAM_BA   => sdram_ba,
      SDRAM_nCS  => sdram_ncs,
      SDRAM_nWE  => sdram_nwe,
      SDRAM_nRAS => sdram_nras,
      SDRAM_nCAS => sdram_ncas,
      SDRAM_CKE  => sdram_cke,
      SDRAM_CLK  => sdram_clk,
      wtbt       => sdram_wtbt,
      addr       => sdram_addr,
      dout       => sdram_dout,
      din        => sdram_din,
      wr         => sdram_wr,
      rd         => sdram_rd,
      ready      => sdram_ready);
    
  -- AUDIO+IO AY-3-8914
  i_snd: ENTITY work.snd
    PORT MAP (
      ad       => ad,
      dw       => snd_dw,
      dr       => snd_dr,
      wr       => snd_wr,
      sound    => sound,
      pa_i     => pa_i,
      pa_o     => pa_o,
      pa_en    => pa_en,
      pb_i     => pb_i,
      pb_o     => pb_o,
      pb_en    => pb_en,
      tick     => tick_snd,
      clk      => clksys,
      reset_na => reset_na);

  -- Second audio ECS
  i_snd2: ENTITY work.snd
    PORT MAP (
      ad       => ad,
      dw       => snd2_dw,
      dr       => snd2_dr,
      wr       => snd2_wr,
      sound    => sound2,
      pa_i     => pa2_i,
      pa_o     => pa2_o,
      pa_en    => pa2_en,
      pb_i     => pb2_i,
      pb_o     => pb2_o,
      pb_en    => pb2_en,
      tick     => tick_snd,
      clk      => clksys,
      reset_na => reset_na);

  -- JLP Features
  i_jlp: ENTITY work.jlp
    PORT MAP (
      ad           => ad,
      dw           => jlp_dw,
      dr           => jlp_dr,
      wr           => jlp_wr,
      img_mounted  => img_mounted,
      img_size     => img_size,
      img_readonly => img_readonly,
      sd_lba       => sd_lba,
      sd_rd        => sd_rd,
      sd_wr        => sd_wr,
      sd_ack       => sd_ack,
      sd_buff_addr => sd_buff_addr,
      sd_buff_dout => sd_buff_dout,
      sd_buff_din  => sd_buff_din,
      sd_buff_wr   => sd_buff_wr,
      clk          => clksys,
      reset_na     => reset_na);
  
  -- Intellivoice
  i_ivoice: ENTITY work.ivoice
    PORT MAP (
      ad       => ad,
      dw       => ivoice_dw,
      dr       => ivoice_dr,
      wr       => ivoice_wr,
      tick_cpu => tick_cpu,
      tick     => tick_ivoice,
      divi     => ivoice_divi,
      sound    => sound_iv,
      rom_voice_wr => rom_voice_wr,
      rom_aw   => rom_aw,
      rom_dw   => rom_dw,
      clksys   => clksys,
      reset_na => reset_na);
  
  ivoice_divi<=358 WHEN pal='0' ELSE 400;
  
  audio_l<=std_logic_vector(
    ('0' & signed(sound + mux(ecs,sound2,x"000")) & "000") +
    (signed(mux(ivoice,unsigned(sound_iv(15 DOWNTO 8)),x"00")) & "000"));
  
  audio_r<=std_logic_vector(
    ('0' & signed(sound + mux(ecs,sound2,x"000")) & "000") +
    (signed(mux(ivoice,unsigned(sound_iv(15 DOWNTO 8)),x"00")) & "000"));
  
  ----------------------------------------------------------
  -- IO MAPPING
  
  PROCESS (key_1,key_2,key_3,key_4,key_5,key_6,key_7,key_8,key_9,
           key_0,key_r,key_w,key_space,key_enter,swap,
           joystick_0,joystick_1,joystick_analog_0,joystick_analog_1) IS
    CONSTANT dirtable : arr_uv8(0 TO 15):= (-- NSWE
      x"00", -- 0000 : no press
      x"02", -- 0001 : E
      x"08", -- 0010 : W
      x"00", -- 0011 : WE = no press
      x"01", -- 0100 : S
      x"13", -- 0101 : SE
      x"19", -- 0110 : SW
      x"01", -- 0111 : SWE = S
      x"04", -- 1000 : N
      x"16", -- 1001 : NE
      x"1C", -- 1010 : NW
      x"04", -- 1011 : NWE = N
      x"00", -- 1100 : NS = no press
      x"02", -- 1101 : NSE = E
      x"08", -- 1110 : NSW = W
      x"00"); -- 1111 : NSWE = no press
    
    CONSTANT dir16 : arr_uv8(0 TO 255) := (
      x"1C",x"1C",x"1C",x"18",x"18",x"08",x"08",x"08",x"08",x"08",x"08",x"09",x"09",x"19",x"19",x"19",
      x"1C",x"1C",x"1C",x"18",x"18",x"18",x"08",x"08",x"08",x"08",x"09",x"09",x"09",x"19",x"19",x"19",
      x"1C",x"1C",x"1C",x"1C",x"18",x"18",x"08",x"08",x"08",x"08",x"09",x"09",x"19",x"19",x"19",x"19",
      x"0C",x"0C",x"1C",x"1C",x"1C",x"18",x"18",x"08",x"08",x"09",x"09",x"19",x"19",x"19",x"11",x"11",
      x"0C",x"0C",x"0C",x"1C",x"1C",x"00",x"00",x"00",x"00",x"00",x"00",x"19",x"19",x"11",x"11",x"11",
      x"04",x"0C",x"0C",x"0C",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"11",x"11",x"11",x"01",
      x"04",x"04",x"04",x"0C",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"11",x"01",x"01",x"01",
      x"04",x"04",x"04",x"04",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"01",x"01",x"01",x"01",
      x"04",x"04",x"04",x"04",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"01",x"01",x"01",x"01",
      x"04",x"04",x"04",x"14",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"03",x"01",x"01",x"01",
      x"04",x"14",x"14",x"14",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"03",x"03",x"03",x"01",
      x"14",x"14",x"14",x"16",x"16",x"00",x"00",x"00",x"00",x"00",x"00",x"13",x"13",x"03",x"03",x"03",
      x"14",x"14",x"16",x"16",x"16",x"06",x"06",x"02",x"02",x"12",x"12",x"13",x"13",x"13",x"03",x"03",
      x"16",x"16",x"16",x"16",x"06",x"06",x"02",x"02",x"02",x"02",x"12",x"12",x"13",x"13",x"13",x"13",
      x"16",x"16",x"16",x"06",x"06",x"06",x"02",x"02",x"02",x"02",x"12",x"12",x"12",x"13",x"13",x"13",
      x"16",x"16",x"16",x"06",x"06",x"02",x"02",x"02",x"02",x"02",x"02",x"12",x"12",x"13",x"13",x"13");
    
    VARIABLE io_v,io2_v : uv8;
    VARIABLE t_v : std_logic_vector(3 DOWNTO 0);
  BEGIN
    -- PORT A
    io_v:=dirtable(to_integer(unsigned(joystick_0(3 DOWNTO 0)))); -- Direction cross
    io_v:=io_v OR dir16(to_integer((unsigned(joystick_analog_0( 7 DOWNTO 4)) + x"8") &
                                   (unsigned(joystick_analog_0(15 DOWNTO 12))  + x"8")));
    io_v:=io_v OR ("10100000" AND sext(joystick_0( 4),8)); -- Action UP
    io_v:=io_v OR ("01100000" AND sext(joystick_0( 5),8)); -- Action BL
    io_v:=io_v OR ("11000000" AND sext(joystick_0( 6),8)); -- Action BR
    io_v:=io_v OR ("10001000" AND sext(joystick_0( 7),8)); -- Clear
    io_v:=io_v OR ("00101000" AND sext(joystick_0( 8),8)); -- Enter 
    io_v:=io_v OR ("01001000" AND sext(joystick_0( 9),8)); -- 0
    io_v:=io_v OR ("10000001" AND sext(joystick_0(10),8)); -- 1
    io_v:=io_v OR ("01000001" AND sext(joystick_0(11),8)); -- 2
    io_v:=io_v OR ("00100001" AND sext(joystick_0(12),8)); -- 3
    io_v:=io_v OR ("10000010" AND sext(joystick_0(13),8)); -- 4
    io_v:=io_v OR ("01000010" AND sext(joystick_0(14),8)); -- 5
    io_v:=io_v OR ("00100010" AND sext(joystick_0(15),8)); -- 6
    io_v:=io_v OR ("10000100" AND sext(joystick_0(16),8)); -- 7
    io_v:=io_v OR ("01000100" AND sext(joystick_0(17),8)); -- 8
    io_v:=io_v OR ("00100100" AND sext(joystick_0(18),8)); -- 9

    t_v:=key_up & key_down & key_left & key_right;
    IF ecs='0' THEN
      io_v:=io_v OR dirtable(to_integer(unsigned(t_v)));
      io_v:=io_v OR ("10100000" AND sext(key_lctrl,8)); -- Action UP
      io_v:=io_v OR ("10100000" AND sext(key_rctrl,8)); -- Action UP
      io_v:=io_v OR ("01100000" AND sext(key_lshift,8)); -- Action BL
      io_v:=io_v OR ("11000000" AND sext(key_rshift,8)); -- Action BR
      io_v:=io_v OR ("10001000" AND sext(key_space,8)); -- Clear
      io_v:=io_v OR ("00101000" AND sext(key_enter,8)); -- Enter 
      io_v:=io_v OR ("01001000" AND sext(key_0,8)); -- 0
      io_v:=io_v OR ("10000001" AND sext(key_1,8)); -- 1
      io_v:=io_v OR ("01000001" AND sext(key_2,8)); -- 2
      io_v:=io_v OR ("00100001" AND sext(key_3,8)); -- 3
      io_v:=io_v OR ("10000010" AND sext(key_4,8)); -- 4
      io_v:=io_v OR ("01000010" AND sext(key_5,8)); -- 5
      io_v:=io_v OR ("00100010" AND sext(key_6,8)); -- 6
      io_v:=io_v OR ("10000100" AND sext(key_7,8)); -- 7
      io_v:=io_v OR ("01000100" AND sext(key_8,8)); -- 8
      io_v:=io_v OR ("00100100" AND sext(key_9,8)); -- 9
    END IF;
    
    ---------------------------------
    -- PORT B
    io2_v:=dirtable(to_integer(unsigned(joystick_1(3 DOWNTO 0))));
    io2_v:=io2_v OR dir16(to_integer((unsigned(joystick_analog_1( 7 DOWNTO 4)) + x"8") &
                                     (unsigned(joystick_analog_1(15 DOWNTO 12))  + x"8")));
    io2_v:=io2_v OR ("10100000" AND sext(joystick_1( 4),8)); -- Action UP
    io2_v:=io2_v OR ("01100000" AND sext(joystick_1( 5),8)); -- Action BL
    io2_v:=io2_v OR ("11000000" AND sext(joystick_1( 6),8)); -- Action BR
    io2_v:=io2_v OR ("10001000" AND sext(joystick_1( 7),8)); -- Clear
    io2_v:=io2_v OR ("00101000" AND sext(joystick_1( 8),8)); -- Enter 
    io2_v:=io2_v OR ("01001000" AND sext(joystick_1( 9),8)); -- 0
    io2_v:=io2_v OR ("10000001" AND sext(joystick_1(10),8)); -- 1
    io2_v:=io2_v OR ("01000001" AND sext(joystick_1(11),8)); -- 2
    io2_v:=io2_v OR ("00100001" AND sext(joystick_1(12),8)); -- 3
    io2_v:=io2_v OR ("10000010" AND sext(joystick_1(13),8)); -- 4
    io2_v:=io2_v OR ("01000010" AND sext(joystick_1(14),8)); -- 5
    io2_v:=io2_v OR ("00100010" AND sext(joystick_1(15),8)); -- 6
    io2_v:=io2_v OR ("10000100" AND sext(joystick_1(16),8)); -- 7
    io2_v:=io2_v OR ("01000100" AND sext(joystick_1(17),8)); -- 8
    io2_v:=io2_v OR ("00100100" AND sext(joystick_1(18),8)); -- 9
    
    pa_i<=NOT mux(swap,io_v,io2_v);
    pb_i<=NOT mux(swap,io2_v,io_v);
    
  END PROCESS;
  
  ----------------------------------------------------------
  -- ECS Keyboard
  
  --bits | 0     1     2     3     4     5      6      7
  -------+----------------------------------------------------
  --  7  | n/a   n/a   n/a   n/a   n/a   n/a    n/a    n/a
  --  6  | shift n/a   n/a   n/a   n/a   n/a    n/a    n/a
  --  5  | a     ctrl  right 1     q     up     down   space
  --  4  | d     e     2     3     w     s      z      x
  --  3  | g     t     4     5     r     f      c      v
  --  2  | j     u     6     7     y     h      b      n
  --  1  | l     o     8     9     i     k      m      comma
  --  0  | n/a   enter 0     esc   p     scolon period left
  -------+----------------------------------------------------
    
  PROCESS(key_0,key_1,key_2,key_3,key_4,key_5,key_6,key_7,key_8,key_9,
          key_a,key_b,key_c,key_d,key_e,key_f,key_g,key_h,key_i,key_j,
          key_k,key_l,key_m,key_n,key_o,key_p,key_q,key_r,key_s,key_t,
          key_u,key_v,key_w,key_x,key_y,key_z,
          key_space,key_colon,key_period,key_comma,
          key_up,key_down,key_right,key_left,
          key_enter,key_esc,key_lshift,key_rshift,key_lctrl,key_rctrl,pa2_o,pb2_o,pa2_en,pb2_en) IS
    VARIABLE dr : uv8;
  BEGIN
    IF pa2_en='1' AND pb2_en='0' THEN
      dr:=x"00";
      dr:=dr OR mux(NOT pa2_o(7),
                    "00000000",x"00");
      dr:=dr OR mux(NOT pa2_o(6),
                    (key_rshift OR key_lshift) & "0000000",x"00");
      dr:=dr OR mux(NOT pa2_o(5),
                    key_a & (key_rctrl OR key_lctrl) & key_right & key_1 & key_q & key_up & key_down & key_space,x"00");
      dr:=dr OR mux(NOT pa2_o(4),
                    key_d & key_e & key_2 & key_3 & key_w & key_s & key_z & key_x,x"00");
      dr:=dr OR mux(NOT pa2_o(3),
                    key_g & key_t & key_4 & key_5 & key_r & key_f & key_c & key_v,x"00");
      dr:=dr OR mux(NOT pa2_o(2),
                    key_j & key_u & key_6 & key_7 & key_y  & key_h & key_b & key_n,x"00");
      dr:=dr OR mux(NOT pa2_o(1),
                    key_l & key_o & key_8 & key_9 & key_i & key_k & key_m & key_comma,x"00");
      dr:=dr OR mux(NOT pa2_o(0),
                    '0' & key_enter & key_0 & key_esc & key_p & key_colon & key_period & key_left,x"00");
      dr:=NOT dr;
    ELSIF pa2_en='0' AND pb2_en='1' THEN
      dr:=x"FF";
      -- <TODO>
    ELSE
      dr:=x"FF";
    END IF;
    pb2_i<=dr;
    pa2_i<=dr;
      
  END PROCESS;
  
  ----------------------------------------------------------
  KeyCodes:PROCESS (clksys,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
         key_0<='0';  key_1<='0';  key_2<='0';  key_3<='0';  key_4<='0';
         key_5<='0';  key_6<='0';  key_7<='0';  key_8<='0';  key_9<='0';
         key_a<='0';  key_b<='0';  key_c<='0';  key_d<='0';  key_e<='0';  key_f<='0';
         key_g<='0';  key_h<='0';  key_i<='0';  key_j<='0';  key_k<='0';  key_l<='0';
         key_m<='0';  key_n<='0';  key_o<='0';  key_p<='0';  key_q<='0';  key_r<='0';
         key_s<='0';  key_t<='0';  key_u<='0';  key_v<='0';  key_w<='0';  key_x<='0';
         key_y<='0';  key_z <='0';
         key_space<='0'; key_colon<='0'; key_period<='0'; key_comma <='0';
         key_up<='0';    key_down<='0';  key_right<='0';  key_left <='0';
         key_enter<='0'; key_esc<='0';   key_lshift<='0'; key_rshift<='0';
         key_lctrl<='0'; key_rctrl<='0';
         
    ELSIF rising_edge(clksys) THEN
      ps2_key_delay<=ps2_key;
      ps2_key_mem<=ps2_key;
      IF ps2_key_delay(10)/=ps2_key(10) THEN
        CASE ps2_key(7 DOWNTO 0) IS
          WHEN x"45" => key_0<=ps2_key(9);
          WHEN x"16" => key_1<=ps2_key(9);
          WHEN x"1E" => key_2<=ps2_key(9);
          WHEN x"26" => key_3<=ps2_key(9);
          WHEN x"25" => key_4<=ps2_key(9);
          WHEN x"2E" => key_5<=ps2_key(9);
          WHEN x"36" => key_6<=ps2_key(9);
          WHEN x"3D" => key_7<=ps2_key(9);
          WHEN x"3E" => key_8<=ps2_key(9);
          WHEN x"46" => key_9<=ps2_key(9);
          WHEN x"1C" => key_a<=ps2_key(9);
          WHEN x"32" => key_b<=ps2_key(9);
          WHEN x"21" => key_c<=ps2_key(9);
          WHEN x"23" => key_d<=ps2_key(9);
          WHEN x"24" => key_e<=ps2_key(9);
          WHEN x"2B" => key_f<=ps2_key(9);
          WHEN x"34" => key_g<=ps2_key(9);
          WHEN x"33" => key_h<=ps2_key(9);
          WHEN x"43" => key_i<=ps2_key(9);
          WHEN x"3B" => key_j<=ps2_key(9);
          WHEN x"42" => key_k<=ps2_key(9);
          WHEN x"4B" => key_l<=ps2_key(9);
          WHEN x"3A" => key_m<=ps2_key(9);
          WHEN x"31" => key_n<=ps2_key(9);
          WHEN x"44" => key_o<=ps2_key(9);
          WHEN x"4D" => key_p<=ps2_key(9);
          WHEN x"15" => key_q<=ps2_key(9);
          WHEN x"2D" => key_r<=ps2_key(9);
          WHEN x"1B" => key_s<=ps2_key(9);
          WHEN x"2C" => key_t<=ps2_key(9);
          WHEN x"3C" => key_u<=ps2_key(9);
          WHEN x"2A" => key_v<=ps2_key(9);
          WHEN x"1D" => key_w<=ps2_key(9);
          WHEN x"22" => key_x<=ps2_key(9);
          WHEN x"35" => key_y<=ps2_key(9);
          WHEN x"1A" => key_z<=ps2_key(9);
          WHEN x"29" => key_space <=ps2_key(9);
          WHEN x"5A" => key_enter <=ps2_key(9);
          WHEN x"27" => key_colon<=ps2_key(9);
          WHEN x"49" => key_period<=ps2_key(9);
          WHEN x"41" => key_comma <=ps2_key(9);
          WHEN x"63" => key_up<=ps2_key(9);
          WHEN x"60" => key_down<=ps2_key(9);
          WHEN x"6A" => key_right<=ps2_key(9);
          WHEN x"61" => key_left <=ps2_key(9);
          WHEN x"08" => key_esc<=ps2_key(9);
          WHEN x"12" => key_lshift<=ps2_key(9);
          WHEN x"59" => key_rshift<=ps2_key(9);
          WHEN x"11" => key_lctrl<=ps2_key(9);
          WHEN x"58" => key_rctrl<=ps2_key(9);
          WHEN OTHERS => NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS KeyCodes;
  
  ----------------------------------------------------------
  PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF vga_ce_l='1' THEN
        vga_r<=std_logic_vector(vga_r_u);
        vga_g<=std_logic_vector(vga_g_u);
        vga_b<=std_logic_vector(vga_b_u);
        
        vga_de_v<=vga_de_u;
        vga_de<=vga_de_v;
        
      END IF;
      vga_ce2<=vga_ce_l;
      vga_ce3<=vga_ce2;
      vga_ce4<=vga_ce3;
      vga_ce5<=vga_ce4;
      
    END IF;
  END PROCESS;

  vga_clk<=clksys;
  vga_ce <=vga_ce_l;
  
  ----------------------------------------------------------
  reset_na<=NOT reset AND pll_locked AND NOT ioctl_download AND NOT map_reset;

  hwreset_n<=NOT reset AND pll_locked;
  
END struct;
