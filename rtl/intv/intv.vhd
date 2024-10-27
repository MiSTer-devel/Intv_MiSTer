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
  
  SIGNAL ioctl_wait_l,ioctl_download2,ioctl_wr2 : std_logic;

  SIGNAL adrs : uv17;
  TYPE enum_state IS (sIDLE,sDOWN,sDOWN_BIN,sCLR,sROM,
                      sDOWN_ROM,sDOWN_ROM2,sDOWN_ROM3,sDOWN_ROM4,
                      sDOWN_LOOP,sDOWN_CRC,sDOWN_CRC2,
                      sDOWN_RANGE,sDOWN_RANGE2,sDOWN_RANGE3,sWAIT);
  SIGNAL state : enum_state;
  SIGNAL w_wrl,w_wrh : std_logic;
  SIGNAL w_d : uv8;
  SIGNAL w_a : uv16;
  
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
  
  SHARED VARIABLE carth,cartl : arr_uv8(0 TO 65535);
  ATTRIBUTE ramstyle : string;
  ATTRIBUTE ramstyle OF carth : VARIABLE IS "no_rw_check";
  ATTRIBUTE ramstyle OF cartl : VARIABLE IS "no_rw_check";
  SIGNAL cad : uv16;
  
  SIGNAL ecs2 : std_logic;
  
  SIGNAL dr,dw,ad,cart_dr,cart_dw : uv16;
  SIGNAL cart_drl,cart_drh : uv8;
  SIGNAL cart_acc : std_logic;
  SIGNAL snd_dr,snd_dw,snd2_dr,snd2_dw : uv8;
  SIGNAL snd_wr,snd2_wr,cart_wr : std_logic;
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
  SIGNAL map_reset : std_logic;
  SIGNAL map_cpt : uint4;
  SIGNAL clear : std_logic;
  SIGNAL rom_dw : uv8;
  SIGNAL rom_aw : uv16;
  SIGNAL rom_voice_wr,rom_grom_wr,rom_exec_wr,rom_ecs_wr : std_logic;
  SIGNAL rom_voice_up,rom_grom_up,rom_exec_up,rom_ecs_up : std_logic;
  
  SIGNAL vga_r_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_g_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_b_u  : unsigned(7 DOWNTO 0);
  SIGNAL vga_de_u,vga_de_v : std_logic;
  SIGNAL vga_ce_l,vga_ce2,vga_ce3,vga_ce4,vga_ce5  : std_logic;
  
  TYPE type_jmap IS RECORD
    crc : uv32;
    m   : uint4;
  END RECORD;
  TYPE arr_jmap IS ARRAY (natural RANGE <>) OF type_jmap;
  CONSTANT MAPS : arr_jmap := (
    (x"4CC46A04",1),(x"D5F038B6",1),(x"A3ACD160",1),(x"4422868E",1),
    (x"C2063C08",1),(x"A12C27E1",1),
    (x"515E1D7E",2),(x"0BF464C6",2),(x"3289C8BA",2),(x"16BFB8EB",2),
    (x"6802B191",2),(x"13EE56F1",2),(x"FF83FF80",2),(x"2C5FD5FA",2),
    (x"632F6ADF",2),(x"B745C1CA",2),(x"BB939881",2),(x"800B572F",2),
    (x"32076E9D",2),(x"A95021FC",2),(x"3825C25B",2),(x"23DC808D",2),
    (x"D1D352A0",3),
    (x"4B23A757",5),(x"D8F99AA2",5),(x"159AF7F7",5),(x"A21C31C3",5),
    (x"6E4E8EB4",5),
    (x"D5363B8C",6),
    (x"13FF363C",7),(x"C047D487",7),(x"5E6A8CD8",7),(x"E806AD91",7),
    (x"C83EEA4C",8),
    (x"CE8FC699",9),(x"095638C0",9));

  SIGNAL crc,xcrc : uv32;
  SIGNAL search,found : std_logic;
  SIGNAL mapcpt : natural RANGE 0 TO MAPS'length+2;
  SIGNAL smap,mmap,mmap2 : uint4;
  
  SIGNAL imap : uv8;
  SIGNAL iacc : uint4;
  SIGNAL ifine : uv8;
  SIGNAL idx,cidx  : uint9;
  SIGNAL rden,wren,bsen,byen : std_logic;
  SIGNAL icart_dw : uv16;
  SIGNAL icart_wr : std_logic;
  SIGNAL fine : std_logic;
  SIGNAL icart_acc_dwr,icart_map_dwr : std_logic;
  SIGNAL icart_acc_ddw : uint4;
  SIGNAL icart_map_ddw : uv8;
  SIGNAL icart_map_da,icart_acc_da : uint9;
  SIGNAL icart_fine_dwr : std_logic;
  SIGNAL icart_fine_ddw : uv8;
  SIGNAL icart_fine_da : uint9;
  
  SIGNAL icart : std_logic;
  SIGNAL icart_pwr,cart_wrm : std_logic;
  SIGNAL zone_min,zone_max : uv8;
  SIGNAL numrange,numzone : natural RANGE 0 TO 31;
  
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
  
  ----------------------------------------------------------
  
  SIGNAL icart_map : arr_uv8(0 TO 32*16-1) := (
    --00    08    10    18    20    28    30    38    40    48    50    58    60    68    70    78    80    88    90    98    A0    A8    B0    B8    C0    C8    D0    D8    E0    E8    F0    F8
    -- MAP 0
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"20",x"28",x"00",x"00",x"30",x"38",
    -- MAP 1
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"20",x"28",x"30",x"38",x"40",x"48",
    -- MAP 2
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"20",x"28",x"30",x"38",x"40",x"48",x"00",x"00",x"50",x"58",x"00",x"00",x"00",x"00",
    -- MAP 3
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"20",x"28",x"30",x"38",x"00",x"00",x"00",x"00",x"40",x"48",x"00",x"00",x"50",x"58",
    -- MAP 4
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"20",x"00",x"00",x"00",x"00",x"00",  
    -- MAP 5
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"20",x"28",x"00",x"00",x"30",x"38",x"40",x"48",x"50",x"58",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    -- MAP 6
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    -- MAP 7
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    -- MAP 8
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"00",x"00",x"10",x"18",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    -- MAP 9
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"08",x"10",x"18",x"00",x"00",x"00",x"60",x"20",x"28",x"30",x"38",x"00",x"00",x"00",x"00",x"40",x"48",x"00",x"00",x"50",x"58",  
    -- MAP 10,11,12,13,14 : Unused. 15 : Programmable
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",  
    x"00",x"08",x"10",x"18",x"20",x"28",x"30",x"38",x"40",x"48",x"50",x"58",x"60",x"68",x"70",x"78",x"80",x"88",x"90",x"98",x"A0",x"A8",x"B0",x"B8",x"C0",x"C8",x"D0",x"D8",x"E0",x"E8",x"F0",x"F8");
  
  SIGNAL icart_fine : arr_uv8(0 TO 32*16-1) := (
    --00    08    10    18    20    28    30    38    40    48    50    58    60    68    70    78    80    88    90    98    A0    A8    B0    B8    C0    C8    D0    D8    E0    E8    F0    F8
    -- MAP 0
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 1
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 2
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 3
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 4
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"03",x"07",x"07",x"07",x"07",x"07",
    -- MAP 5
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 6
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 7
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 8
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 9
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    -- MAP 10,11,12,13,14 : Unused. 15 : Programmable
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",
    x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07",x"07");
  
  -- Access table 0:RD 1:WR 3:Remap
  TYPE arr_uint4 IS ARRAY(natural RANGE <>) OF uint4;
  SIGNAL icart_acc : arr_uint4(0 TO 32*16-1) := (
    --  10  20  30  40  50  60  70  80  90  A0  B0  C0  D0  E0  F0
    -- MAP 0
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,
    -- MAP 1
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
    -- MAP 2
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,1,1,0,0,0,0,
    -- MAP 3
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,
    -- MAP 4. RAM D000:D3FF
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,
    -- MAP 5
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
    -- MAP 6
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    -- MAP 7
    0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    -- MAP 8
    0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    -- MAP 9. RAM 8800:8FFF
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,3,1,1,1,1,0,0,0,0,1,1,0,0,1,1,
    -- MAP 10,11,12,13,14 : Unused. 15 : Programmable
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  
  ----------------------------------------------------------
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
      cart_acc    => cart_acc,
      cart_dr     => cart_dr,
      cart_dw     => cart_dw,
      cart_wr     => cart_wr,
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
  
  Seq:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      ------------------------------------------------------
      idx<=to_integer(ad(15 DOWNTO 11)) + 32 * mmap;
      
      cad<=(imap + ad(10 DOWNTO 8)) & ad(7 DOWNTO 0);
      
      rden<=to_unsigned(iacc,4)(0);
      wren<=to_unsigned(iacc,4)(1);
      byen<=to_unsigned(iacc,4)(2);
      bsen<=to_unsigned(iacc,4)(3);
      
      -- [0xxx 0yyy]  : Enable between X and Y
      fine<=to_std_logic(to_integer(ifine(6 DOWNTO 4))
                         <=to_integer(ad(10 DOWNTO 8)) AND
                         to_integer(ifine(2 DOWNTO 0))
                         >=to_integer(ad(10 DOWNTO 8)));
      
      ------------------------------------------------------
      IF icart='1' THEN
        mmap<=15;
      ELSIF mapp="0000" THEN
        mmap<=mux(found='1',smap,0);
      ELSE
        mmap<=to_integer(unsigned(mapp))-1;
      END IF;
      
      mmap2<=mmap;
      ecs2<=ecs;
      
      IF mmap2/=mmap OR ecs2/=ecs THEN
        map_cpt<=0;
      END IF;
      IF map_cpt<15 THEN
        map_cpt<=map_cpt+1;
        map_reset<='1';        
      ELSE
        map_reset<='0';
      END IF;
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------
  CRCCalc:PROCESS(clksys) IS
    FUNCTION crc8 (
      CONSTANT d   : IN unsigned(7 DOWNTO 0);
      CONSTANT crc : IN unsigned(31 DOWNTO 0)) RETURN unsigned IS
      VARIABLE co : unsigned(31 DOWNTO 0);
      VARIABLE h  : unsigned(7 DOWNTO 0);
    BEGIN
      h(0):=d(0) XOR crc(31);
      h(1):=d(1) XOR crc(30);
      h(2):=d(2) XOR crc(29);
      h(3):=d(3) XOR crc(28);
      h(4):=d(4) XOR crc(27);
      h(5):=d(5) XOR crc(26);
      h(6):=d(6) XOR crc(25) XOR h(0);
      h(7):=d(7) XOR crc(24) XOR h(1);
      co(0) :=h(7);
      co(1) :=h(6) XOR h(7);
      co(2) :=h(5) XOR h(6) XOR h(7);
      co(3) :=h(4) XOR h(5) XOR h(6);
      co(4) :=h(3) XOR h(4) XOR h(5) XOR h(7);
      co(5) :=h(2) XOR h(3) XOR h(4) XOR h(6) XOR h(7);
      co(6) :=h(1) XOR h(2) XOR h(3) XOR h(5) XOR h(6);
      co(7) :=h(0) XOR h(1) XOR h(2) XOR h(4) XOR h(5) XOR h(7);
      co(8) := crc(0) XOR h(0) XOR h(1) XOR h(3) XOR h(4) XOR h(6) XOR h(7);
      co(9) := crc(1) XOR h(0) XOR h(2) XOR h(3) XOR h(5) XOR h(6);
      co(10):= crc(2) XOR h(1) XOR h(2) XOR h(4) XOR h(5) XOR h(7);
      co(11):= crc(3) XOR h(0) XOR h(1) XOR h(3) XOR h(4) XOR h(6) XOR h(7);
      co(12):= crc(4) XOR h(0) XOR h(2) XOR h(3) XOR h(5) XOR h(6) XOR h(7);
      co(13):= crc(5) XOR h(1) XOR h(2) XOR h(4) XOR h(5) XOR h(6);
      co(14):= crc(6) XOR h(0) XOR h(1) XOR h(3) XOR h(4) XOR h(5);
      co(15):= crc(7) XOR h(0) XOR h(2) XOR h(3) XOR h(4);
      co(16):= crc(8) XOR h(1) XOR h(2) XOR h(3) XOR h(7);
      co(17):= crc(9) XOR h(0) XOR h(1) XOR h(2) XOR h(6);
      co(18):=crc(10) XOR h(0) XOR h(1) XOR h(5);
      co(19):=crc(11) XOR h(0) XOR h(4);
      co(20):=crc(12) XOR h(3);
      co(21):=crc(13) XOR h(2);
      co(22):=crc(14) XOR h(1) XOR h(7);
      co(23):=crc(15) XOR h(0) XOR h(6) XOR h(7);
      co(24):=crc(16) XOR h(5) XOR h(6);
      co(25):=crc(17) XOR h(4) XOR h(5);
      co(26):=crc(18) XOR h(3) XOR h(4) XOR h(7);
      co(27):=crc(19) XOR h(2) XOR h(3) XOR h(6);
      co(28):=crc(20) XOR h(1) XOR h(2) XOR h(5);
      co(29):=crc(21) XOR h(0) XOR h(1) XOR h(4);
      co(30):=crc(22) XOR h(0) XOR h(3);
      co(31):=crc(23) XOR h(2);
      RETURN co;
    END crc8;
  BEGIN
    IF rising_edge(clksys) THEN
      IF ioctl_wr='1' THEN
        crc<=crc8(unsigned(ioctl_dout),
                  mux(to_integer(unsigned(ioctl_addr))=0,x"FFFFFFFF",crc));
      END IF;
      
      FOR i IN 0 TO 31 LOOP
        xcrc(i)<=NOT crc(31-i);
      END LOOP;

      ioctl_download2<=ioctl_download;
      
      IF search='0' THEN
        IF ioctl_download='0' AND ioctl_download2='1' THEN
          search<='1';
          found<='0';
        END IF;
        mapcpt<=0;
      ELSE
        mapcpt<=mapcpt+1;
        IF xcrc=MAPS(mapcpt).crc THEN
          smap<=MAPS(mapcpt).m;
          found<='1';
        END IF;
        IF mapcpt=MAPS'length-1 THEN
          search<='0';
        END IF;
      END IF;
    END IF;
  END PROCESS CRCCalc;
  
  ----------------------------------------------------------
  
  ReadRom:PROCESS(clksys) IS
    VARIABLE wr_v : std_logic;
  BEGIN
    IF rising_edge(clksys) THEN
      ioctl_wr2<=ioctl_wr;
      wr_v:=ioctl_wr AND NOT ioctl_wait_l;
      
      IF ioctl_download='0' AND state/=sROM THEN
        state<=sIDLE;
      END IF;
      
      w_wrl<='0';
      w_wrh<='0';
      w_d <=unsigned(ioctl_dout);
      icart_acc_dwr<='0';
      icart_map_dwr<='0';
      icart_fine_dwr<='0';

      rom_dw<=unsigned(ioctl_dout);
      rom_exec_wr<='0';
      rom_grom_wr<='0';
      rom_ecs_wr <='0';
      rom_voice_wr<='0';
      
      ioctl_wait_l<=ioctl_wr;
      clear<='0';
      
      ------------------------------------------------------
      CASE state IS
        ----------------------------------------------------
        WHEN sIDLE =>
          w_a<=(OTHERS =>'0');
          IF ioctl_download='1' THEN
            ioctl_wait_l<='1';
            state<=sCLR;
          END IF;
          rom_exec_up<='0';
          rom_grom_up<='0';
          rom_voice_up<='0';
          rom_ecs_up<='0';
          
        WHEN sCLR =>
          clear<='1';
          ioctl_wait_l<='1';
          w_d<=x"FF";
          w_wrl<='1';
          w_wrh<='1';
          w_a<=w_a+1;
          IF w_a=x"FFFF" THEN
            state<=sDOWN;
          END IF;
          
        WHEN sDOWN =>
          w_wrl<=wr_v AND     ioctl_addr(0);
          w_wrh<=wr_v AND NOT ioctl_addr(0);
          w_a <=unsigned(ioctl_addr(16 DOWNTO 1));
          IF wr_v='1' THEN
            IF unsigned(ioctl_index)=0 THEN
              state<=sROM;
              w_wrl<='0';
              w_wrh<='0';
            ELSIF (ioctl_dout=x"A8" OR ((ioctl_dout AND x"DF")=x"41") OR format="10") AND format/="01" THEN
              state<=sDOWN_ROM;
              icart<='1';
            ELSE
              state<=sDOWN_BIN;
              icart<='0';
            END IF;
          END IF;
          
          ----------------------------------------------------
          -- Internal ROMs : EXEC,GROM,VOICE,ECS
        WHEN sROM =>
          rom_aw<=unsigned(ioctl_addr(15 DOWNTO 0));
          w_wrl<='0';
          w_wrh<='0';
          IF unsigned(ioctl_addr)<16#2000# AND unsigned(ioctl_index)=0 THEN
            rom_exec_wr<=wr_v;
            rom_exec_up<=rom_exec_up OR wr_v;
          ELSIF (unsigned(ioctl_addr)<16#2800# AND unsigned(ioctl_index)=0) OR
            unsigned(ioctl_index)=16#40# THEN
            rom_grom_wr<=wr_v;
            rom_grom_up<=rom_grom_up OR wr_v;
          ELSIF (unsigned(ioctl_addr)<16#3000# AND unsigned(ioctl_index)=0) OR
            unsigned(ioctl_index)=16#80# THEN
            rom_voice_wr<=wr_v;
            rom_voice_up<=rom_voice_up OR wr_v;
          ELSIF unsigned(ioctl_index)=0 THEN
            rom_ecs_wr<=wr_v;
            rom_ecs_up<=rom_ecs_up OR wr_v;
            rom_aw<=unsigned(ioctl_addr(15 DOWNTO 0)) - x"3000";
          ELSIF unsigned(ioctl_index)=16#C0# THEN
            rom_ecs_wr<=wr_v;
            rom_ecs_up<=rom_ecs_up OR wr_v;
          END IF;
          
          IF ioctl_download='0' AND rom_exec_up='1' AND
            rom_ecs_up='1' AND rom_voice_up='1' AND rom_grom_up='1' THEN
            state<=sDOWN;
          END IF;
          
        ----------------------------------------------------
        -- Plain binary file
        WHEN sDOWN_BIN =>
          w_wrl<=wr_v AND     ioctl_addr(0);
          w_wrh<=wr_v AND NOT ioctl_addr(0);
          w_a <=unsigned(ioctl_addr(16 DOWNTO 1));
          
        ----------------------------------------------------
        -- Intellicart ROM format
          -- Number of zones
        WHEN sDOWN_ROM =>
          IF wr_v='1' THEN
            numzone<=to_integer(unsigned(ioctl_dout));
            state <=sDOWN_ROM2;
          END IF;
          
        WHEN sDOWN_ROM2 =>
          IF wr_v='1' THEN -- Ignore complement
            state<=sDOWN_ROM3;
          END IF;
          
          -- Data start / end
        WHEN sDOWN_ROM3 =>
          IF wr_v='1' THEN
            zone_min<=unsigned(ioctl_dout);
            state<=sDOWN_ROM4;
          END IF;

        WHEN sDOWN_ROM4 =>
          IF wr_v='1' THEN
            zone_max<=unsigned(ioctl_dout);
            state<=sDOWN_LOOP;
          END IF;
          adrs<=zone_min & '0' & x"00";
          
          -- Data copy
        WHEN sDOWN_LOOP =>
          IF wr_v='1' THEN
            w_wrl<=wr_v AND     adrs(0);
            w_wrh<=wr_v AND NOT adrs(0);
            w_a <=unsigned(adrs(16 DOWNTO 1));
            
            adrs<=adrs+1;
            IF (adrs + 1)=(zone_max+1) & '0' & x"00" THEN
              state<=sDOWN_CRC;
            END IF;
          END IF;
          
          -- Data CRC
        WHEN sDOWN_CRC =>
          IF wr_v='1' THEN -- CRC. Ignore
            state<=sDOWN_CRC2;
          END IF;
          
        WHEN sDOWN_CRC2 =>
          IF wr_v='1' THEN -- CRC Ignore
            numzone<=numzone-1;
            IF numzone>1 THEN
              state<=sDOWN_ROM3;
            ELSE
              state<=sDOWN_RANGE;
            END IF;
          END IF;
          numrange<=0;
          
        WHEN sDOWN_RANGE =>
          -- Enable Table : 16 bytes -> 32 zones
          -- 0 : Read Enable
          -- 1 : Write Enable
          -- 2 : Byte mem
          -- 3 : BankSwitch Enable
          IF wr_v='1' THEN
            icart_acc_dwr<='1';
            icart_acc_ddw<=to_integer(unsigned(ioctl_dout(3 DOWNTO 0)));
            icart_acc_da<=numrange*2 + 32 *15;
            state<=sDOWN_RANGE2;
          END IF;
          
        WHEN sDOWN_RANGE2 =>
          icart_acc_dwr<='1';
          icart_acc_ddw<=to_integer(unsigned(ioctl_dout(7 DOWNTO 4)));
          icart_acc_da<=numrange*2 + 1 + 32 *15;
          numrange<=(numrange+1) MOD 16;
          IF numrange=15 THEN
            state<=sDOWN_RANGE3;
            numrange<=0;
          ELSE
            state<=sDOWN_RANGE;
          END IF;
          
        WHEN sDOWN_RANGE3 =>
          -- Adress Restriction table : 32 bytes => 64 zones
          
          IF wr_v='1' THEN
            icart_map_dwr<='1';
            icart_map_ddw<=to_unsigned(numrange*8,8);
            icart_map_da<=numrange + 32*15;
            
            icart_fine_dwr<='1';
            icart_fine_ddw<=unsigned(ioctl_dout);
            icart_fine_da<=(numrange / 16) +
                           (numrange MOD 16)*2 + 32*15;
            
            numrange<=(numrange+1) MOD 32;
            IF numrange=31 THEN
              state<=sWAIT;
            END IF;
          END IF;
          
        WHEN sWAIT =>
          NULL;
		  
      END CASE;
    END IF;
  END PROCESS ReadRom;
  
  ioctl_wait<=ioctl_wait_l;
  
  ----------------------------------------------------------
  cart_dr<=(cart_drh & cart_drl) WHEN cart_acc='1' AND byen='0' ELSE
           (x"FF" & cart_drl)    WHEN cart_acc='1' AND byen='1' ELSE x"FFFF";
  
  cart_acc<=rden AND fine;
  
  ----------------------------------------------------------
  -- Icart memory
  CartH1:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      cart_drl<=cartl(to_integer(cad));
      IF cart_wrm='1' THEN
        cartl(to_integer(cad)):=cart_dw(7 DOWNTO 0);
      END IF;
      
      cart_drh<=carth(to_integer(cad));
      IF cart_wrm='1' THEN
        carth(to_integer(cad)):=cart_dw(15 DOWNTO 8);
      END IF;
    END IF;
  END PROCESS CartH1;
  
  CartH2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF w_wrl='1' THEN
        cartl(to_integer(w_a)):=w_d;
      END IF;
      
      IF w_wrh='1' THEN
        carth(to_integer(w_a)):=w_d;
      END IF;
    END IF;
  END PROCESS CartH2;
  
  cart_wrm<=cart_wr AND wren;
  
  ----------------------------------------------------------
  -- ICART mapping table
  icarmap:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      imap <=icart_map(idx); -- Remapping table
      IF icart_map_dwr='1' THEN
        icart_map(idx)<=icart_map_ddw;
      END IF;
    END IF;
  END PROCESS icarmap;
  
  icarmap2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF icart_pwr='1' THEN
        icart_map(cidx)<=icart_dw(7 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS icarmap2;

  cidx<=to_integer(ad(3 DOWNTO 0) & ad(4)) WHEN ioctl_download='0'
         ELSE icart_map_da;

  iacc <=icart_acc(idx) WHEN rising_edge(clksys);
  
  icaracc2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF icart_acc_dwr='1' THEN
        icart_acc(icart_acc_da)<=icart_acc_ddw;
      END IF;
    END IF;
  END PROCESS icaracc2;
  
  icart_pwr<=icart_wr AND bsen AND fine;
  
  ifine<=icart_fine(idx) WHEN rising_edge(clksys);
  
  icarfine2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF icart_fine_dwr='1' THEN
        icart_fine(icart_fine_da)<=icart_fine_ddw;
      END IF;
    END IF;
  END PROCESS icarfine2;

  ----------------------------------------------------------
  -- ROM WRITE
  -- EXEC  : 8192  = 4096  * 2    0000 => 1FFF
  -- GROM  : 2048                 2000 => 27FF
  -- VOICE : 2048                 2800 => 2FFF
  -- ECS   : 24576 = 12288 * 2    3000 => 8FFF
  
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
  
END struct;
