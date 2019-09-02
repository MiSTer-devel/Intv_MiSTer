---------------------------------------------------------------------------------
-- Intellivision

---------------------------------------------------------------------------------
-- Developed with the help of the JZINTV emulator
---------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;

--USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY emu IS
  PORT (
    -- Master input clock
    clk_50m          : IN    std_logic;

    -- Async reset from top-level module. Can be used as initial reset.
    reset            : IN    std_logic;

    -- Must be passed to hps_io module
    hps_bus          : INOUT std_logic_vector(45 DOWNTO 0);
    
    -- Base video clock. Usually equals to CLK_SYS.
    clk_video        : OUT   std_logic;
    
    -- Multiple resolutions are supported using different CE_PIXEL rates.
    -- Must be based on CLK_VIDEO
    ce_pixel         : OUT   std_logic;

    -- VGA
    vga_r            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_g            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_b            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_hs           : OUT   std_logic; -- positive pulse!
    vga_vs           : OUT   std_logic; -- positive pulse!
    vga_de           : OUT   std_logic; -- = not (VBlank or HBlank)
    vga_f1           : OUT   std_logic;
    vga_sl           : OUT   std_logic_vector(1 DOWNTO 0);
    
    -- LED
    led_user         : OUT   std_logic; -- 1 - ON, 0 - OFF.
    
    -- b[1]: 0 - LED status is system status ORed with b[0]
    --       1 - LED status is controled solely by b[0]
    -- hint: supply 2'b00 to let the system control the LED.
    led_power        : OUT   std_logic_vector(1 DOWNTO 0);
    led_disk         : OUT   std_logic_vector(1 DOWNTO 0);
    
    -- Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
    video_arx        : OUT   std_logic_vector(7 DOWNTO 0);
    video_ary        : OUT   std_logic_vector(7 DOWNTO 0);


    -- AUDIO
    audio_l          : OUT   std_logic_vector(15 DOWNTO 0);
    audio_r          : OUT   std_logic_vector(15 DOWNTO 0);
    audio_s          : OUT   std_logic; -- 1 = signed audio, 0 = unsigned
    -- 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)
    audio_mix        : OUT   std_logic_vector(1 DOWNTO 0);
    
    adc_bus          : INOUT std_logic_vector(3 DOWNTO 0);
    
    -- SD-SPI
    sd_sck           : OUT   std_logic := 'Z';
    sd_mosi          : OUT   std_logic := 'Z';
    sd_miso          : IN    std_logic;
    sd_cs            : OUT   std_logic := 'Z';
    sd_cd            : IN    std_logic;

    -- High latency DDR3 RAM interface
    -- Use for non-critical time purposes
    ddram_clk        : OUT   std_logic;
    ddram_busy       : IN    std_logic;
    ddram_burstcnt   : OUT   std_logic_vector(7 DOWNTO 0);
    ddram_addr       : OUT   std_logic_vector(28 DOWNTO 0);
    ddram_dout       : IN    std_logic_vector(63 DOWNTO 0);
    ddram_dout_ready : IN    std_logic;
    ddram_rd         : OUT   std_logic;
    ddram_din        : OUT   std_logic_vector(63 DOWNTO 0);
    ddram_be         : OUT   std_logic_vector(7 DOWNTO 0);
    ddram_we         : OUT   std_logic;
    
    -- SDRAM interface with lower latency
    sdram_clk        : OUT   std_logic;
    sdram_cke        : OUT   std_logic;
    sdram_a          : OUT   std_logic_vector(12 DOWNTO 0);
    sdram_ba         : OUT   std_logic_vector(1 DOWNTO 0);
    sdram_dq         : INOUT std_logic_vector(15 DOWNTO 0);
    sdram_dqml       : OUT   std_logic;
    sdram_dqmh       : OUT   std_logic;
    sdram_ncs        : OUT   std_logic;
    sdram_ncas       : OUT   std_logic;
    sdram_nras       : OUT   std_logic;
    sdram_nwe        : OUT   std_logic;
    
    uart_cts         : IN    std_logic;
    uart_rts         : OUT   std_logic;
    uart_rxd         : IN    std_logic;
    uart_txd         : OUT   std_logic;
    uart_dtr         : OUT   std_logic;
    uart_dsr         : IN    std_logic;
    
    user_in          : IN    std_logic_vector(5 DOWNTO 0);
    user_out         : OUT   std_logic_vector(5 DOWNTO 0);
    
    osd_status       : IN    std_logic);
END emu;

ARCHITECTURE struct OF emu IS

  CONSTANT CDIV : natural := 12 * 8;
  
  COMPONENT pll IS
    PORT (
      refclk   : in  std_logic; -- clk
      rst      : in  std_logic; -- reset
      outclk_0 : out std_logic; -- clk
      outclk_1 : out std_logic; -- clk
      locked   : out std_logic  -- export
      );
  END COMPONENT pll;
  
  CONSTANT CONF_STR : string := 
    "Intellivision;;" &
    "-;" &
    "FS,ROMINT;" &
    "O47,MAP,0,1,2,3,4,5,6,7,8,9;" &
--  "O0,Video standard,PAL,NTSC;" &
    "O3,Aspect ratio,4:3,16:9;" &
--  "O46,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;" &
    "O1,Swap Joystick,Off,On;" &
    "O2,Overlay,Off,On;" &
    "J1,Action Up,Action Left,Action Right,Clear,Enter,0,1,2,3,4,5,6,7,8,9;" &
    "V1.0";
  
  FUNCTION to_slv(s: string) return std_logic_vector is 
    CONSTANT ss : string(1 to s'length) := s; 
    VARIABLE rval : std_logic_vector(1 to 8 * s'length); 
    VARIABLE p : integer; 
    VARIABLE c : integer; 
  BEGIN
    FOR i in ss'range LOOP
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    END LOOP;
    RETURN rval;
  END FUNCTION; 

  COMPONENT hps_io
    GENERIC (
      STRLEN : integer;
      PS2DIV : integer := 1000;
      WIDE   : integer := 0; --  8bits download bus
      VDNUM  : integer := 1;
      PS2WE  : integer := 0);
    PORT (
      clk_sys           : IN  std_logic;
      hps_bus           : INOUT std_logic_vector(45 DOWNTO 0);

      conf_str          : IN  std_logic_vector(8*STRLEN-1 DOWNTO 0);
      joystick_0        : OUT std_logic_vector(31 DOWNTO 0);
      joystick_1        : OUT std_logic_vector(31 DOWNTO 0);
      joystick_2        : OUT std_logic_vector(31 DOWNTO 0);
      joystick_3        : OUT std_logic_vector(31 DOWNTO 0);
      joystick_4        : OUT std_logic_vector(31 DOWNTO 0);
      joystick_5        : OUT std_logic_vector(31 DOWNTO 0);
      joystick_analog_0 : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_1 : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_2 : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_3 : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_4 : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_5 : OUT std_logic_vector(15 DOWNTO 0);
      buttons           : OUT std_logic_vector(1 DOWNTO 0);
      forced_scandoubler: OUT std_logic;
      status            : OUT std_logic_vector(31 DOWNTO 0);
      status_in         : IN  std_logic_vector(31 DOWNTO 0);
      status_set        : IN  std_logic;
      status_menumask   : IN  std_logic_vector(15 DOWNTO 0);
      new_vmode         : IN  std_logic;
      img_mounted       : OUT std_logic;
      img_readonly      : OUT std_logic;
      img_size          : OUT std_logic_vector(63 DOWNTO 0);
      sd_lba            : IN  std_logic_vector(31 DOWNTO 0);
      sd_rd             : IN  std_logic;
      sd_wr             : IN  std_logic;
      sd_ack            : OUT std_logic;
      sd_conf           : IN  std_logic;
      sd_ack_conf       : OUT std_logic;
      sd_buff_addr      : OUT std_logic_vector(8 DOWNTO 0);
      sd_buff_dout      : OUT std_logic_vector(7 DOWNTO 0);
      sd_buff_din       : IN  std_logic_vector(7 DOWNTO 0);
      sd_buff_wr        : OUT std_logic;
      ioctl_download    : OUT std_logic;
      ioctl_index       : OUT std_logic_vector(7 DOWNTO 0);
      ioctl_wr          : OUT std_logic;
      ioctl_addr        : OUT std_logic_vector(24 DOWNTO 0);
      ioctl_dout        : OUT std_logic_vector(7 DOWNTO 0);
      ioctl_wait        : IN  std_logic;
      rtc               : OUT std_logic_vector(64 DOWNTO 0);
      timestamp         : OUT std_logic_vector(32 DOWNTO 0);
      uart_mode         : IN  std_logic_vector(15 DOWNTO 0);
      ps2_kbd_clk_out   : OUT std_logic;
      ps2_kbd_data_out  : OUT std_logic;
      ps2_kbd_clk_in    : IN  std_logic;
      ps2_kbd_data_in   : IN  std_logic;
      ps2_kbd_led_use   : IN  std_logic_vector(2 DOWNTO 0);
      ps2_kbd_led_status: IN  std_logic_vector(2 DOWNTO 0);
      ps2_mouse_clk_out : OUT std_logic;
      ps2_mouse_data_out: OUT std_logic;
      ps2_mouse_clk_in  : IN  std_logic;
      ps2_mouse_data_in : IN  std_logic;
      ps2_key           : OUT std_logic_vector(10 DOWNTO 0);
      ps2_mouse         : OUT std_logic_vector(24 DOWNTO 0);
      ps2_mouse_ext     : OUT std_logic_vector(15 DOWNTO 0));
  END COMPONENT hps_io;

  SIGNAL joystick_0      : std_logic_vector(31 DOWNTO 0);
  SIGNAL joystick_1      : std_logic_vector(31 DOWNTO 0);
  SIGNAL joystick_analog_0 : std_logic_vector(15 DOWNTO 0);
  SIGNAL joystick_analog_1 : std_logic_vector(15 DOWNTO 0);
  SIGNAL buttons         : std_logic_vector(1 DOWNTO 0);
  SIGNAL status          : std_logic_vector(31 DOWNTO 0);
  SIGNAL status_in       : std_logic_vector(31 DOWNTO 0):=x"00000000";
  SIGNAL status_set      : std_logic :='0';
  SIGNAL status_menumask : std_logic_vector(15 DOWNTO 0):=x"0000";
  SIGNAL new_vmode       : std_logic :='0';
  
  SIGNAL ioctl_download : std_logic;
  SIGNAL ioctl_index    : std_logic_vector(7 DOWNTO 0);
  SIGNAL ioctl_wr       : std_logic;
  SIGNAL ioctl_addr     : std_logic_vector(24 DOWNTO 0);
  SIGNAL ioctl_dout     : std_logic_vector(7 DOWNTO 0);
  SIGNAL ioctl_wait     : std_logic :='0';

  SIGNAL w_wrl,w_wrh    : std_logic;
  SIGNAL w_d : uv8;
  SIGNAL w_a : uv15;
  
  SIGNAL ps2_key,ps2_key_delay,ps2_key_mem : std_logic_vector(10 DOWNTO 0);
  
  SIGNAL key_0, key_1 , key_2 , key_3  : std_logic;
  SIGNAL key_4, key_5 , key_6 , key_7  : std_logic;
  SIGNAL key_8, key_9 , key_a , key_b  : std_logic;
  SIGNAL key_r, key_w : std_logic;
  
  SIGNAL key_rc   ,key_wc  ,key_bp ,key_pc  : std_logic;
  SIGNAL key_minus,key_plus,key_reg,key_mem : std_logic;
  SIGNAL key_return,key_space               : std_logic;
  
  ----------------------------------------
  SIGNAL reset_na : std_logic;
  SIGNAL clksys,clksys_ntsc,clksys_pal,pll_locked : std_logic;
  
  SIGNAL clkdiv,clkdivsnd : uint6 :=0;
  SIGNAL tick_cpu,tick_cpup,tick_snd : std_logic;

  SIGNAL ram : arr_uv16(0 TO 2047);
  
  SIGNAL carth,cartl : arr_uv8(0 TO 32767);
  ATTRIBUTE ramstyle : string;
  ATTRIBUTE ramstyle OF carth : SIGNAL IS "no_rw_check";
  ATTRIBUTE ramstyle OF cartl : SIGNAL IS "no_rw_check";
  SIGNAL cad : uv16;
  SIGNAL selram : std_logic;
  
  SIGNAL ntsc_pal,swap : std_logic;

  SIGNAL dr,dw,ad,cart_dr,cart_dw,rom_dr,ram_dr : uv16;
  SIGNAL snd_dr,snd_dw : uv8;
  SIGNAL snd_wr,cart_wr : std_logic;
  SIGNAL sound : sv8;
  SIGNAL bdic : uv3;
  SIGNAL bdrdy,busrq,busak,halt,intrm : std_logic;
  SIGNAL pa_i,pb_i,pa_o,pb_o : uv8;
  SIGNAL pa_en,pb_en : std_logic;
  
  -- OVO -----------------------------------------
  FUNCTION CC(i : character) RETURN unsigned IS
  BEGIN
    CASE i IS
      WHEN '0' => RETURN "00000";
      WHEN '1' => RETURN "00001";
      WHEN '2' => RETURN "00010";
      WHEN '3' => RETURN "00011";
      WHEN '4' => RETURN "00100";
      WHEN '5' => RETURN "00101";
      WHEN '6' => RETURN "00110";
      WHEN '7' => RETURN "00111";
      WHEN '8' => RETURN "01000";
      WHEN '9' => RETURN "01001";
      WHEN 'A' => RETURN "01010";
      WHEN 'B' => RETURN "01011";
      WHEN 'C' => RETURN "01100";
      WHEN 'D' => RETURN "01101";
      WHEN 'E' => RETURN "01110";
      WHEN 'F' => RETURN "01111";
      WHEN ' ' => RETURN "10000";
      WHEN '=' => RETURN "10001";
      WHEN '+' => RETURN "10010";
      WHEN '-' => RETURN "10011";
      WHEN '<' => RETURN "10100";
      WHEN '>' => RETURN "10101";
      WHEN '^' => RETURN "10110";
      WHEN 'v' => RETURN "10111";
      WHEN '(' => RETURN "11000";
      WHEN ')' => RETURN "11001";
      WHEN ':' => RETURN "11010";
      WHEN '.' => RETURN "11011";
      WHEN ',' => RETURN "11100";
      WHEN '?' => RETURN "11101";
      WHEN '|' => RETURN "11110";
      WHEN '#' => RETURN "11111";
      WHEN OTHERS => RETURN "10000";
    END CASE;
  END FUNCTION CC;
  FUNCTION CS(s : string) RETURN unsigned IS
    VARIABLE r : unsigned(0 TO s'length*5-1);
    VARIABLE j : natural :=0;
  BEGIN
    FOR i IN s'RANGE LOOP
      r(j TO j+4) :=CC(s(i));
      j:=j+5;
    END LOOP;
    RETURN r;
  END FUNCTION CS;
  
  SIGNAL vga_r_i,vga_r_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_g_i,vga_g_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_b_i,vga_b_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_de_u,vga_de_v : std_logic;
  SIGNAL vga_hs_i,vga_vs_i : std_logic;
  SIGNAL vga_de_i,vga_ce,vga_ce2,vga_ce3,vga_ce4,vga_ce5  : std_logic;
  
  SIGNAL ovo_ena  : std_logic;
  SIGNAL ovo_in0  : unsigned(0 TO 32*5-1) :=(OTHERS =>'0');
  SIGNAL ovo_in1  : unsigned(0 TO 32*5-1) :=(OTHERS =>'0');
  
  SIGNAL hits : uv64;
  SIGNAL hitbg,hitbo : uv8;

BEGIN

  hps : hps_io
    GENERIC MAP (
      STRLEN => CONF_STR'length)
    PORT MAP (
      clk_sys            => clksys,
      hps_bus            => hps_bus,
      conf_str           => to_slv(CONF_STR),
      joystick_0         => joystick_0,
      joystick_1         => joystick_1,
      joystick_2         => OPEN,
      joystick_3         => OPEN,
      joystick_4         => OPEN,
      joystick_5         => OPEN,
      joystick_analog_0  => joystick_analog_0,
      joystick_analog_1  => joystick_analog_1,
      joystick_analog_2  => OPEN,
      joystick_analog_3  => OPEN,
      joystick_analog_4  => OPEN,
      joystick_analog_5  => OPEN,
      buttons            => buttons,
      forced_scandoubler => OPEN,
      status             => status,
      status_in          => status_in,
      status_set         => status_set,
      status_menumask    => status_menumask,
      new_vmode          => new_vmode,
      img_mounted        => OPEN,
      img_readonly       => OPEN,
      img_size           => OPEN,
      sd_lba             => std_logic_vector'(x"0000_0000"),
      sd_rd              => '0',
      sd_wr              => '0',
      sd_ack             => OPEN,
      sd_conf            => '0',
      sd_ack_conf        => OPEN,
      sd_buff_addr       => OPEN,
      sd_buff_dout       => OPEN,
      sd_buff_din        => std_logic_vector'(x"00"),
      sd_buff_wr         => OPEN,
      ioctl_download     => ioctl_download,
      ioctl_index        => ioctl_index,
      ioctl_wr           => ioctl_wr,
      ioctl_addr         => ioctl_addr,
      ioctl_dout         => ioctl_dout,
      ioctl_wait         => ioctl_wait,
      rtc                => OPEN,
      timestamp          => OPEN,
      uart_mode          => std_logic_vector'(x"0000"),
      ps2_kbd_clk_out    => OPEN,
      ps2_kbd_data_out   => OPEN,
      ps2_kbd_clk_in     => '1',
      ps2_kbd_data_in    => '1',
      ps2_kbd_led_use    => std_logic_vector'("000"),
      ps2_kbd_led_status => std_logic_vector'("000"),
      ps2_mouse_clk_out  => OPEN,
      ps2_mouse_data_out => OPEN,
      ps2_mouse_clk_in   => '1',
      ps2_mouse_data_in  => '1',
      ps2_key            => ps2_key,
      ps2_mouse          => OPEN,
      ps2_mouse_ext      => OPEN);

  ntsc_pal<=status(0);
  swap<=status(1);
  

  ----------------------------------------------------------
  ipll : pll
    PORT MAP (
      refclk   => clk_50m,
      rst      => '0',
      outclk_0 => clksys_ntsc, -- 3.579545MHz * 12 = 42.95454MHz
      outclk_1 => clksys_pal,  -- 4MHz * 12 = 48MHz
      locked   => pll_locked
      );
  
  clksys<=clksys_ntsc;
  
  -- NTSC : 3.579545MHz
  -- PAL  : 4MHz

  -- STIC : CLK * 12
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
      dw       => dw,
      dr       => dr,
      bdic     => bdic,
      bdrdy    => bdrdy,
      busrq    => busrq,
      busak    => busak,
      intrm    => intrm,
      phi      => tick_cpu,
      ad       => ad,
      snd_dr   => snd_dr,
      snd_dw   => snd_dw,
      snd_wr   => snd_wr,
      cart_dr  => cart_dr,
      cart_dw  => cart_dw,
      cart_wr  => cart_wr,
      hits => hits,
      hitbg => hitbg,
      hitbo => hitbo,
      vid_r    => vga_r_i,
      vid_g    => vga_g_i,
      vid_b    => vga_b_i,
      vid_de   => vga_de_i,
      vid_hs   => vga_hs_i,
      vid_vs   => vga_vs_i,
      vid_ce   => vga_ce,
      clk      => clksys,
      reset_na => reset_na);

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

  audio_l<=std_logic_vector(sound) & x"00";
  audio_r<=std_logic_vector(sound) & x"00";
  audio_s<='1';
  audio_mix<="11";
  
  led_user<='0';
  led_power<="00";
  led_disk<="00";
  
  ----------------------------------------------------------
  -- MAPPINGS
  -- MAP 0
  --   $0000 - $1FFF = $5000   ;  8K to $5000 - $6FFF
  --   $2000 - $2FFF = $D000   ;  4K to $D000 - $DFFF
  --   $3000 - $3FFF = $F000   ;  4K to $F000 - $FFFF
 
  -- MAP 1
  --   $0000 - $1FFF = $5000   ;  8K to $5000 - $6FFF
  --   $2000 - $4FFF = $D000   ; 12K to $D000 - $FFFF

  -- MAP 2
  --   $0000 - $1FFF = $5000   ;  8K to $5000 - $6FFF
  --   $2000 - $4FFF = $9000   ; 12K to $9000 - $BFFF
  --   $5000 - $5FFF = $D000   ;  4K to $D000 - $DFFF

  -- MAP 3
  --   $0000 - $1FFF = $5000   ;  8K to $5000 - $6FFF
  --   $2000 - $3FFF = $9000   ;  8K to $9000 - $AFFF
  --   $4000 - $4FFF = $D000   ;  4K to $D000 - $DFFF
  --   $5000 - $5FFF = $F000   ;  4K to $F000 - $FFFF

  -- MAP 4
  --   $0000 - $1FFF = $5000   ;  8K to $5000 - $6FFF
  --   RAM $D000 - $D3FF = RAM 8

  -- MAP 5
  --   $0000 - $2FFF = $5000   ; 12K to $5000 - $7FFF
  --   $3000 - $5FFF = $9000   ; 12K to $9000 - $BFFF

  -- MAP 6
  --   $0000 - $1FFF = $6000   ;  8K to $6000 - $7FFF

  -- MAP 7
  --   $0000 - $1FFF = $4800   ;  8K to $4800 - $67FF

  -- MAP 8
  --   $0000 - $0FFF = $5000   ;  4K to $5000 - $6000
  --   $1000 - $1FFF = $7000   ;  4K to $7000 - $7FFF

  -- MAP 9
  --   $0000 - $1FFF = $5000   ;  8K to $5000 - $6FFF
  --   $2000 - $3FFF = $9000   ;  8K to $9000 - $AFFF
  --   $4000 - $4FFF = $D000   ;  4K to $D000 - $DFFF
  --   $5000 - $5FFF = $F000   ;  4K to $F000 - $FFFF
  --   RAM $8800 - $8FFF = RAM 8

  PROCESS(ad,status) IS
    VARIABLE aad : uv12;
    VARIABLE mmap : uint4;
    FUNCTION sel(ad : uv15;
                 c  : boolean) RETURN unsigned IS
    BEGIN
      IF c THEN RETURN '1' & ad;
           ELSE RETURN x"0000";
           END IF;
    END FUNCTION;
  BEGIN
    aad:=ad(11 DOWNTO 0);
    selram<='0';
    mmap:=to_integer(unsigned(status(7 DOWNTO 4)));
    
    CASE mmap IS
      WHEN 0 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("010" & aad,ad(15 DOWNTO 12)=x"D") OR
             sel("011" & aad,ad(15 DOWNTO 12)=x"F");
        
      WHEN 1 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
            sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
            sel("010" & aad,ad(15 DOWNTO 12)=x"D") OR
            sel("011" & aad,ad(15 DOWNTO 12)=x"E") OR
            sel("100" & aad,ad(15 DOWNTO 12)=x"F");
        
      WHEN 2 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("010" & aad,ad(15 DOWNTO 12)=x"9") OR
             sel("011" & aad,ad(15 DOWNTO 12)=x"A") OR
             sel("100" & aad,ad(15 DOWNTO 12)=x"B") OR
             sel("101" & aad,ad(15 DOWNTO 12)=x"D");
        
      WHEN 3 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("010" & aad,ad(15 DOWNTO 12)=x"9") OR
             sel("011" & aad,ad(15 DOWNTO 12)=x"A") OR
             sel("100" & aad,ad(15 DOWNTO 12)=x"D") OR
             sel("101" & aad,ad(15 DOWNTO 12)=x"F");
        
      WHEN 4 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6");
        
        selram<=to_std_logic(ad(15 DOWNTO 10)="110100");
        
      WHEN 5 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("010" & aad,ad(15 DOWNTO 12)=x"7") OR
             sel("011" & aad,ad(15 DOWNTO 12)=x"9") OR
             sel("100" & aad,ad(15 DOWNTO 12)=x"A") OR
             sel("101" & aad,ad(15 DOWNTO 12)=x"B");
        
      WHEN 6 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"7");
        
      WHEN 7 =>
        cad<=sel("0000" & aad(10 DOWNTO 0),ad(15 DOWNTO 11)="01001") OR -- 48
             sel("0001" & aad(10 DOWNTO 0),ad(15 DOWNTO 11)="01010") OR -- 50
             sel("0010" & aad(10 DOWNTO 0),ad(15 DOWNTO 11)="01011") OR -- 58
             sel("0011" & aad(10 DOWNTO 0),ad(15 DOWNTO 11)="01100");   -- 60
        
      WHEN 8 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"7");

      WHEN 9 =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("010" & aad,ad(15 DOWNTO 12)=x"9") OR
             sel("011" & aad,ad(15 DOWNTO 12)=x"A") OR
             sel("100" & aad,ad(15 DOWNTO 12)=x"D") OR
             sel("101" & aad,ad(15 DOWNTO 12)=x"F");
        selram<=to_std_logic(ad(15 DOWNTO 10)="110100");
        
      WHEN OTHERS =>
        cad<=sel("000" & aad,ad(15 DOWNTO 12)=x"5") OR
             sel("001" & aad,ad(15 DOWNTO 12)=x"6") OR
             sel("010" & aad,ad(15 DOWNTO 12)=x"D") OR
             sel("011" & aad,ad(15 DOWNTO 12)=x"F");
        
    END CASE;
  END PROCESS;

  ----------------------------------------------------------
  rom_dr<=carth(to_integer(cad(14 DOWNTO 0))) &
          cartl(to_integer(cad(14 DOWNTO 0))) WHEN rising_edge(clksys);
  
  IRAM:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      ram_dr<=ram(to_integer(cad(10 DOWNTO 0)));
      IF cart_wr='1' AND selram='1' THEN
        ram(to_integer(cad(10 DOWNTO 0)))<=cart_dw;
      END IF;
    END IF;
  END PROCESS IRAM;
  
  cart_dr<=rom_dr WHEN cad(15)='1' ELSE
           ram_dr WHEN selram='1' ELSE
           x"FFFF";
  
  icart:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      -- Download
      IF w_wrl='1' THEN
        cartl(to_integer(w_a))<=w_d;
      END IF;
      IF w_wrh='1' THEN
        carth(to_integer(w_a))<=w_d;
      END IF;
    END IF;
  END PROCESS icart;
  
  PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      w_wrl<=ioctl_download AND ioctl_wr AND     ioctl_addr(0);
      w_wrh<=ioctl_download AND ioctl_wr AND NOT ioctl_addr(0);
      w_d <=unsigned(ioctl_dout);
      w_a <=unsigned(ioctl_addr(15 DOWNTO 1));
    END IF;
  END PROCESS;
  
  ioctl_wait<='0';
  
  ----------------------------------------------------------
  -- IO MAPPING
  
  PROCESS (key_1,key_2,key_3,key_4,key_5,key_6,key_7,key_8,key_9,
           key_0,key_r,key_w,key_space,key_return,swap,
           joystick_0,joystick_1,joystick_analog_0,joystick_analog_1
           ) IS
    
    CONSTANT dirtable : arr_uv8(0 TO 15):= (-- NDLR
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
  BEGIN
    -- PORT A
    io_v:=dirtable(to_integer(unsigned(joystick_0(3 DOWNTO 0)))); -- Direction cross
    io_v:=io_v OR dir16(to_integer((unsigned(joystick_analog_0( 7 DOWNTO 4)) + x"8") &
                                   (unsigned(joystick_analog_0(15 DOWNTO 12))  + x"8")));
    io_v:=io_v OR ("10100000" AND sext((key_r      OR joystick_0( 4)),8)); -- Action UP
    io_v:=io_v OR ("01100000" AND sext((key_a      OR joystick_0( 5)),8)); -- Action BL
    io_v:=io_v OR ("11000000" AND sext((key_b      OR joystick_0( 6)),8)); -- Action BR
    io_v:=io_v OR ("10001000" AND sext((key_space  OR joystick_0( 7)),8)); -- Clear
    io_v:=io_v OR ("00101000" AND sext((key_return OR joystick_0( 8)),8)); -- Enter 
    io_v:=io_v OR ("01001000" AND sext((key_0      OR joystick_0( 9)),8)); -- 0
    io_v:=io_v OR ("10000001" AND sext((key_1      OR joystick_0(10)),8));
    io_v:=io_v OR ("01000001" AND sext((key_2      OR joystick_0(11)),8));
    io_v:=io_v OR ("00100001" AND sext((key_3      OR joystick_0(12)),8));
    io_v:=io_v OR ("10000010" AND sext((key_4      OR joystick_0(13)),8));
    io_v:=io_v OR ("01000010" AND sext((key_5      OR joystick_0(14)),8));
    io_v:=io_v OR ("00100010" AND sext((key_6      OR joystick_0(15)),8));
    io_v:=io_v OR ("10000100" AND sext((key_7      OR joystick_0(16)),8));
    io_v:=io_v OR ("01000100" AND sext((key_8      OR joystick_0(17)),8));
    io_v:=io_v OR ("00100100" AND sext((key_9      OR joystick_0(18)),8));
    
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
    io2_v:=io2_v OR ("10000001" AND sext(joystick_1(10),8));
    io2_v:=io2_v OR ("01000001" AND sext(joystick_1(11),8));
    io2_v:=io2_v OR ("00100001" AND sext(joystick_1(12),8));
    io2_v:=io2_v OR ("10000010" AND sext(joystick_1(13),8));
    io2_v:=io2_v OR ("01000010" AND sext(joystick_1(14),8));
    io2_v:=io2_v OR ("00100010" AND sext(joystick_1(15),8));
    io2_v:=io2_v OR ("10000100" AND sext(joystick_1(16),8));
    io2_v:=io2_v OR ("01000100" AND sext(joystick_1(17),8));
    io2_v:=io2_v OR ("00100100" AND sext(joystick_1(18),8));
    
    pa_i<=NOT mux(swap,io_v,io2_v);
    pb_i<=NOT mux(swap,io2_v,io_v);
    
  END PROCESS;

  
  ----------------------------------------------------------
  KeyCodes:PROCESS (clksys,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
         key_0<='0';
         key_1<='0';
         key_2<='0';
         key_3<='0';
         key_4<='0';
         key_5<='0';
         key_6<='0';
         key_7<='0';
         key_8<='0';
         key_9<='0';
         key_a<='0';
         key_b<='0';
         key_r<='0';
         key_w<='0';
         key_space <='0'; -- SPACE
         key_return<='0'; -- RETURN
         
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
          WHEN x"2D" => key_r<=ps2_key(9); -- R
          WHEN x"1D" => key_w<=ps2_key(9); -- W
          WHEN x"29" => key_space <=ps2_key(9); -- SPACE
          WHEN x"5A" => key_return<=ps2_key(9); -- RETURN
          WHEN OTHERS => NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS KeyCodes;
  
  ----------------------------------------------------------
  
  vga_sl<="00";
  vga_f1<='0';
  
  audio_l<=std_logic_vector(sound) & x"00";
  audio_r<=std_logic_vector(sound) & x"00";
  audio_s<='1';
  audio_mix<="11";

  video_arx <= x"04" WHEN status(3)='0' ELSE x"16";
  video_ary <= x"03" WHEN status(3)='0' ELSE x"09";
  
  
  ----------------------------------------------------------
  sd_sck<='0';
  sd_mosi<='0';
  sd_cs<='0';
  ddram_clk<='0';
  ddram_burstcnt<=(OTHERS =>'0');
  ddram_addr   <=(OTHERS =>'0');
  ddram_rd     <='0';
  ddram_din    <=(OTHERS =>'0');
  ddram_be     <=(OTHERS =>'0');
  ddram_we     <='0';
  
  -- SDRAM interface with lower latency
  sdram_clk    <='0';
  sdram_cke    <='0';
  sdram_a      <=(OTHERS =>'0');
  sdram_ba     <=(OTHERS =>'0');
  sdram_dq     <=(OTHERS =>'0');
  sdram_dqml   <='0';
  sdram_dqmh   <='0';
  sdram_ncs    <='0';
  sdram_ncas   <='0';
  sdram_nras   <='0';
  sdram_nwe    <='0';
  uart_rts     <='0';
  uart_txd     <='0';
  uart_dtr     <='0';
  user_out     <=(OTHERS =>'Z');
  
  ----------------------------------------------------------
  i_ovo: ENTITY work.ovo
    PORT MAP (
      i_r     => vga_r_i,
      i_g     => vga_g_i,
      i_b     => vga_b_i,
      i_hs    => vga_hs_i,
      i_vs    => vga_vs_i,
      i_de    => vga_de_i,
      i_en    => vga_ce,
      i_clk   => clksys,
      o_r     => vga_r_u,
      o_g     => vga_g_u,
      o_b     => vga_b_u,
      o_hs    => vga_hs,
      o_vs    => vga_vs,
      o_de    => vga_de_u,
      ena     => ovo_ena,
      in0     => ovo_in0,
      in1     => ovo_in1);

  PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF vga_ce='1' THEN
        vga_r<=std_logic_vector(vga_r_u);
        vga_g<=std_logic_vector(vga_g_u);
        vga_b<=std_logic_vector(vga_b_u);
        
        vga_de_v<=vga_de_u;
        vga_de<=vga_de_v;
        
      END IF;
      vga_ce2<=vga_ce;
      vga_ce3<=vga_ce2;
      vga_ce4<=vga_ce3;
      vga_ce5<=vga_ce4;

      ce_pixel<=vga_ce;
      
      --IF status(4)='1' THEN
      --  ce_pixel<=vga_ce2;
      --END IF;
      
      --IF status(5)='1' THEN
      --  ce_pixel<=vga_ce3;
      --END IF;
      --IF status(5)='1' THEN
      --  ce_pixel<=vga_ce4;
      --END IF;
      
    END IF;
  END PROCESS;

  clk_video<=clksys;
  --ce_pixel<=vga_ce5;
  
  ovo_in0<='0' & pa_i(7 DOWNTO 4) &
            '0' & pa_i(3 DOWNTO 0) &
            CC(' ') &
            '0' & pb_i(7 DOWNTO 4) &
            '0' & pb_i(3 DOWNTO 0) &
            CC(' ') &
            "0000" & intrm &
            CC(' ') &
            '0' & ad(15 DOWNTO 12) &
            '0' & ad(11 DOWNTO 8) &
            '0' & ad(7 DOWNTO 4) &
            '0' & ad(3 DOWNTO 0) &
            CC(' ') &
            "0000" & bdic(2) &
            "0000" & bdic(1) &
            "0000" & bdic(0) &

            CC(' ') &
            '0' & unsigned(joystick_analog_0(15 DOWNTO 12)) &
            '0' & unsigned(joystick_analog_0(11 DOWNTO 8)) &
            '0' & unsigned(joystick_analog_0(7 DOWNTO 4)) &
            '0' & unsigned(joystick_analog_0(3 DOWNTO 0)) &

            CS("   ") &
            "00" & unsigned(ps2_key_mem(10 DOWNTO 8)) &
            '0' & unsigned(ps2_key_mem(7 DOWNTO 4)) &
            '0' & unsigned(ps2_key_mem(3 DOWNTO 0)) &
            CC(' ') &
            "0000" & bdrdy &
            "0000" & busrq &
            "0000" & busak &
            CS(" ");

  ovo_in1<=
    '0' & hits(32+31 DOWNTO 28+32) &
    '0' & hits(32+27 DOWNTO 24+32) &
    '0' & hits(32+23 DOWNTO 20+32) &
    '0' & hits(32+19 DOWNTO 16+32) &
    '0' & hits(32+15 DOWNTO 12+32) &
    '0' & hits(32+11 DOWNTO 8+32) &
    '0' & hits(32+7 DOWNTO 4+32) &
    '0' & hits(32+3 DOWNTO 0+32) &
    '0' & hits(31 DOWNTO 28) &
    '0' & hits(27 DOWNTO 24) &
    '0' & hits(23 DOWNTO 20) &
    '0' & hits(19 DOWNTO 16) &
    '0' & hits(15 DOWNTO 12) &
    '0' & hits(11 DOWNTO 8) &
    '0' & hits(7 DOWNTO 4) &
    '0' & hits(3 DOWNTO 0) &
    CC(' ') &
    '0' & hitbg(7 DOWNTO 4) &
    '0' & hitbg(3 DOWNTO 0) &
    CC(' ') &
    '0' & hitbo(7 DOWNTO 4) &
    '0' & hitbo(3 DOWNTO 0) &
    CS("          ");

            
  ovo_ena<=status(2);
  
  ----------------------------------------------------------
  reset_na<=NOT reset AND pll_locked AND NOT ioctl_download;
  
END struct;
