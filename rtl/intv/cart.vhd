--------------------------------------------------------------------------------
-- Intellivision Video
--------------------------------------------------------------------------------
-- DO 10/2024
--------------------------------------------------------------------------------
-- VHDL-1993
--------------------------------------------------------------------------------
-- Cartridge


LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY work;
USE work.base_pack.ALL;
USE work.intv_pack.ALL;

ENTITY cart IS
  PORT (
    mapp     : IN  std_logic_vector(3 DOWNTO 0); -- Mapping
    format   : IN  std_logic;   -- 0 = RAW+ICART  1 = CFG

    ------------------------------------
    -- Address
    ad       : IN  uv16;

    -- CPU Access to cartridge
    cart_dr  : OUT uv16;
    cart_dw  : IN  uv16;
    cart_wr  : IN  std_logic;
    cart_rd  : IN  std_logic;
    cart_rdy : OUT std_logic;

    -- Intellicart special registers
    icart_dw : IN uv16;
    icart_wr : IN std_logic;

    map_reset : OUT std_logic;
    clear     : OUT std_logic;

    -----------------------------------
    -- SDRAM
    sdram_init   : OUT  std_logic;
    sdram_wtbt   : OUT  std_logic_vector(1 DOWNTO 0);
    sdram_addr   : OUT  unsigned(24 DOWNTO 0); -- 32M * 2
    sdram_dout   : IN   std_logic_vector(15 DOWNTO 0);
    sdram_din    : OUT  std_logic_vector(15 DOWNTO 0);
    sdram_wr     : OUT  std_logic;
    sdram_rd     : OUT  std_logic;
    sdram_ready  : IN   std_logic;

    -----------------------------------
    ioctl_download    : IN  std_logic;
    ioctl_index       : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wr          : IN  std_logic;
    ioctl_addr        : IN  std_logic_vector(24 DOWNTO 0);
    ioctl_dout        : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wait        : OUT std_logic;

    -----------------------------------
    -- ROMs initialisation
    rom_aw : OUT uv16;
    rom_dw : OUT uv8;

    rom_grom_wr  : OUT std_logic;
    rom_exec_wr  : OUT std_logic;
    rom_ecs_wr   : OUT std_logic;
    rom_voice_wr : OUT std_logic;
    
    ------------------------------------
    map_src_zone   : IN uv8;
    map_dest_zone  : IN uv4;
    map_dest_page  : IN uv4;
    map_memattr    : IN uv2;
    map_vars       : IN uv5;
    ecspage        : IN arr_uv4(0 TO 15);
    parser         : OUT std_logic; -- Enable parser ROM

    ecs            : IN  std_logic;
    jlp            : IN  std_logic;
    ecsjlp_set     : OUT std_logic;
    ecs_up         : OUT std_logic;
    jlp_up         : OUT std_logic;
    
    -----------------------------------
    phi        : IN std_logic;  -- PHI clock enable
    clksys     : IN std_logic; -- 12x Pixel Clock
    reset      : IN std_logic;
    cpureset_n : IN std_logic;
    hwreset_n  : IN std_logic
    );
END ENTITY cart;

-------------------------------------------------------------------------------

ARCHITECTURE rtl OF cart IS

  SIGNAL remapped : std_logic;

  SIGNAL ioctl_wait_l,ioctl_download2,ioctl_wr2 : std_logic;
  SIGNAL ioctl_idx : uint6;

  SIGNAL map_cpt : uint4;
  
  SIGNAL mapcpt : natural RANGE 0 TO MAPS'length+2;
  
  SIGNAL format2 : std_logic;
  SIGNAL cfgmode : std_logic;

  SIGNAL rom_exec_up,rom_voice_up,rom_grom_up,rom_ecs_up : std_logic;

  SIGNAL adrs : uv17;
  TYPE enum_state IS (sIDLE,sDOWN,
                      -- System RAM
                      sROM,
                      -- Binary
                      sDOWN_BIN_CLR,sDOWN_BIN_CLR2,
                      sDOWN_BIN,sDOWN_BIN2,
                      -- Configuration mapping
                      sDOWN_CFG,sDOWN_CFG2,sDOWN_CFG3,sDOWN_CFG4,
                      -- Intellicart
                      sDOWN_ICART_CLR,sDOWN_ICART_CLR2,
                      sDOWN_ICART,sDOWN_ICART2,sDOWN_ICART3,sDOWN_ICART4,
                      sDOWN_LOOP,sDOWN_LOOP2,sDOWN_LOOP3,
                      sDOWN_CRC,sDOWN_CRC2,
                      sDOWN_RANGE,sDOWN_RANGE2,sDOWN_RANGE3,sWAIT);
  SIGNAL state : enum_state;
  SIGNAL down_dw : uv16;
  SIGNAL down_adrs : uv25;
  SIGNAL down_lastadrs : uv25;
  
  SIGNAL first : std_logic;
  
  SIGNAL cart_dri : uv16;
  SIGNAL cart_rdy_l : std_logic;

  SIGNAL imap : uv8;
  SIGNAL iacc : uint4;
  SIGNAL ifine : uv8;
  SIGNAL idx, cidx : uint9;
  SIGNAL rden,wren,bsen,byen : std_logic;
  SIGNAL fine : std_logic;
  SIGNAL icart_acc_dwr,icart_map_dwr : std_logic;
  SIGNAL icart_acc_ddw : uint4;
  SIGNAL icart_map_ddw : uv8;
  SIGNAL icart_map_da,icart_acc_da : uint9;
  SIGNAL icart_fine_dwr : std_logic;
  SIGNAL icart_fine_ddw : uv8;
  SIGNAL icart_fine_da : uint9;
    
  SIGNAL zone_min,zone_max : uv8;
  SIGNAL numrange,numzone : natural RANGE 0 TO 31;

  -- CFG Mapping MODE
  SIGNAL bin_loaded, cfg_loaded : std_logic;
  SIGNAL clr_loaded : std_logic;
  SIGNAL parser_reset : std_logic;

  -- Intellicart
  SIGNAL icart : std_logic;
  SIGNAL icart_pwr,cart_wrm : std_logic;

  SIGNAL down_req, down_ack : std_logic;
  SIGNAL down_16 : std_logic;
  
  TYPE enum_sdram_state IS (
    sDINIT, sDINIT2, sDINIT3, sDINIT4,
    sDIDLE, sDDOWN, sDCPU);
  SIGNAL sdram_state, sdram_state2 : enum_sdram_state;
  SIGNAL sdram_wr_l : std_logic;
  SIGNAL sdram_rd_l : std_logic;

  -- Remapper
  SIGNAL ad_mapper : uv24; -- Remapper address
  SIGNAL rmap_a : uv12; -- 4bits page + 8bits A[15:8]
  SIGNAL rmap_wr : std_logic;
  SIGNAL rmap_dr, rmap_dw : uv2;
  SIGNAL rmap_dwr : std_logic;

  SHARED VARIABLE rmap_mem : arr_uv2(0 TO 256 * 16 - 1) := (OTHERS => ATTR_ROM); 
  
  SIGNAL smap,mmap,mmap2 : uint4;
  
  SIGNAL epage : uv4;
  SIGNAL crc,xcrc : uv32;
  SIGNAL search,found : std_logic;
  
  SIGNAL ecs2 : std_logic;

  TYPE enum_sdmode IS (sDOWNLOAD,sRUN);
  SIGNAL sdmode : enum_sdmode;

  SIGNAL phi2, phi3, phi4, phi5 : std_logic;

-----------------------------------------------------------------------------
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
  
  -----------------------------------------------------------------------------
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
  
  -----------------------------------------------------------------------------
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
  -- ROM WRITE
  -- EXEC  : 8192  = 4096  * 2    0000 => 1FFF
  -- GROM  : 2048                 2000 => 27FF
  -- VOICE : 2048                 2800 => 2FFF
  -- ECS   : 24576 = 12288 * 2    3000 => 8FFF
  

  ----------------------------------------------------------
  -- ROM WRITE
  -- EXEC  : 8192  = 4096  * 2    0000 => 1FFF
  -- GROM  : 2048                 2000 => 27FF
  -- VOICE : 2048                 2800 => 2FFF
  -- ECS   : 24576 = 12288 * 2    3000 => 8FFF
  
-------------------------------------------------------------------------------

BEGIN

  Seq:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      -----------------------------------------------------
      idx<=to_integer(ad(15 DOWNTO 11)) + 32 * mmap; -- PHI2
      
      rden<=to_unsigned(iacc,4)(0); -- PHI4
      wren<=to_unsigned(iacc,4)(1); -- PHI4
      byen<=to_unsigned(iacc,4)(2); -- PHI4
      bsen<=to_unsigned(iacc,4)(3); -- PHI4
      
      -- [0xxx 0yyy]  : Enable between X and Y
      fine<=to_std_logic(to_integer(ifine(6 DOWNTO 4)) <=to_integer(ad(10 DOWNTO 8)) AND
                         to_integer(ifine(2 DOWNTO 0)) >=to_integer(ad(10 DOWNTO 8))); -- PHI3
      
      -- Mapping options
      --NIL / ROM / RAM8 / RAM16
      
      epage <= ecspage(to_integer(ad(15 DOWNTO 12))); -- PHI2

      IF remapped='1' THEN
        -- PHI3 : 4k entries = 16 pages * 256 blocks of 256 words
        rmap_a  <= epage & ad(15 DOWNTO 8);
      ELSE
        rmap_a  <= map_dest_page & map_dest_zone & ad(11 DOWNTO 8);
      END IF;

      rmap_dw <= map_memattr;
      
      rmap_wr <= '0';
      -- Soit mode reconfiguration : adressage mémoire par registres de zones configuration
      -- Soit mode intellicart (& RAW)
      -- Soit mode remappé : adressage par registre de pagination
      IF cfgmode = '1' AND remapped='1' THEN
        -- Remapped through CFG file
        ad_mapper <= AD_CART & epage & ad(15 DOWNTO 0);
      ELSIF cfgmode ='1' AND remapped='0' THEN
        IF ad(15 DOWNTO 12) = x"6" THEN -- SRC FILE AREA
          ad_mapper <= AD_CONV & map_src_zone & ad(11 DOWNTO 0);
        ELSIF    ad(15 DOWNTO 12) = x"5" THEN -- CFG FILE AREA
          ad_mapper <= AD_CFG & x"00" & ad(11 DOWNTO 0);
        ELSE  -- D DEST AREA
          rmap_wr <= cart_wr AND phi4; -- phi4
          ad_mapper <= AD_CART & map_dest_page & map_dest_zone & ad(11 DOWNTO 0);
        END IF;
      ELSE
        -- Normal access, intellicart or RAW format
        ad_mapper <= AD_CART & PAGE0 & (imap + ad(10 DOWNTO 8)) & ad(7 DOWNTO 0);
      END IF;
      
      
      -----------------------------------------------------
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
      IF parser_reset='1' THEN
        map_cpt <=0;
      END IF;
      IF map_cpt<15 THEN
        map_cpt<=map_cpt+1;
        map_reset<='1';        
      ELSE
        map_reset<='0';
      END IF;
    END IF;
  END PROCESS;

  ---------------------------------------------------------
  CRCCalc:PROCESS(clksys) IS
    
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

  ioctl_idx <= to_integer(unsigned(ioctl_index(5 DOWNTO 0)));
  
  ---------------------------------------------------------
  ReadRom:PROCESS(clksys) IS
    VARIABLE wre_v : std_logic;
  BEGIN
    IF rising_edge(clksys) THEN

      ioctl_wr2<=ioctl_wr;

      wre_v := ioctl_wr AND NOT ioctl_wr2;
      
      format2 <= format;
      IF format2 /= format THEN
        bin_loaded <= '0';
        cfg_loaded <= '0';
      END IF;

      down_req <= '0';
      down_16  <= '0';
      icart_acc_dwr<='0';
      icart_map_dwr<='0';
      icart_fine_dwr<='0';

      rom_dw<=unsigned(ioctl_dout);
      rom_exec_wr<='0';
      rom_grom_wr<='0';
      rom_ecs_wr <='0';
      rom_voice_wr<='0';
      
      rmap_dwr <= '0';

      clear<='0';

      -- down_adrs :
      -- 0     : hi/lo byte
      -- 16:1  : 64k adresses
      --      8:1 = 256
      --     16:9 = 256
      -- 20:17 : 4 pages
      -- 24:21 : 4 zone

      -----------------------------------------------------
      CASE state IS
        ---------------------------------------------------
        WHEN sIDLE =>
          ioctl_wait_l<='0';
          down_adrs<=(OTHERS =>'0');
          IF ioctl_download='1' THEN
            state<=sDOWN;
          END IF;
          rom_exec_up  <='0';
          rom_grom_up  <='0';
          rom_voice_up <='0';
          rom_ecs_up   <='0';
          
        -------------------------------------------------
        WHEN sDOWN =>
          ioctl_wait_l<='0';
          first <= '1';
          clear <= '1';
          down_adrs <= AD_CART & x"00000" & '0';

          IF ioctl_download='0' THEN
            state <= sIDLE;
            
          ELSIF ioctl_idx=0 THEN
            state <= sROM;

          ELSIF ioctl_idx=1 THEN
            IF ioctl_wr='1' AND ioctl_wait_l='0' AND
              (ioctl_dout=x"A8" OR ((ioctl_dout AND x"DF")=x"41")) THEN
              state <= sDOWN_ICART_CLR;
              icart <= '1';
              ioctl_wait_l <= '1';
              cfgmode <= '0';

            ELSIF ioctl_wr='1' AND ioctl_wait_l='0' THEN
              state <= sDOWN_BIN_CLR;
              icart <= '0';
              ioctl_wait_l <= '1';
              bin_loaded <= '1';
              IF bin_loaded='1' THEN
                cfg_loaded <= '0';
              END IF;
              cfgmode <= format;
            END IF;

          ELSIF ioctl_idx=2 THEN
            IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
              state <= sDOWN_CFG;
              icart <= '0';
              ioctl_wait_l <= '1';
              cfg_loaded <= '1';
              IF cfg_loaded='1' THEN
                bin_loaded <= '0';
              END IF;
            END IF;
            cfgmode <= format;
          END IF;
          
          -------------------------------------------------
          -- Internal ROMs : EXEC,GROM,VOICE,ECS
        WHEN sROM =>
          rom_aw<=unsigned(ioctl_addr(15 DOWNTO 0));
          IF unsigned(ioctl_addr)<16#2000# AND ioctl_idx=0 THEN
            rom_exec_wr<=wre_v;
            rom_exec_up<=rom_exec_up OR wre_v;

          ELSIF (unsigned(ioctl_addr)<16#2800# AND ioctl_idx=0) OR
            unsigned(ioctl_index)=16#40# THEN
            rom_grom_wr<=wre_v;
            rom_grom_up<=rom_grom_up OR wre_v;

          ELSIF (unsigned(ioctl_addr)<16#3000# AND unsigned(ioctl_index)=0) OR
            unsigned(ioctl_index)=16#80# THEN
            rom_voice_wr<=wre_v;
            rom_voice_up<=rom_voice_up OR wre_v;

          ELSIF unsigned(ioctl_index)=0 THEN
            rom_ecs_wr<=wre_v;
            rom_ecs_up<=rom_ecs_up OR wre_v;
            rom_aw<=unsigned(ioctl_addr(15 DOWNTO 0)) - x"3000";

          ELSIF unsigned(ioctl_index)=16#C0# THEN
            rom_ecs_wr<=wre_v;
            rom_ecs_up<=rom_ecs_up OR wre_v;

          END IF;
          
          IF ioctl_download='0' AND rom_exec_up='1' AND
            rom_ecs_up='1' AND rom_voice_up='1' AND rom_grom_up='1' THEN
            state<=sIDLE;
          END IF;
          
        -------------------------------------------------
        -- CLEAR CART
        WHEN sDOWN_BIN_CLR =>
          ioctl_wait_l<='1';
          down_dw <=x"FFFF";
          down_req <='1';
          state <= sDOWN_BIN_CLR2;
          rmap_dwr <= '1';
          
        WHEN sDOWN_BIN_CLR2 =>
          ioctl_wait_l<='1';
          down_dw <=x"FFFF";
          down_req <='1';
          IF down_ack='1' THEN
            down_req <= '0';
            down_adrs<=down_adrs+1;
            IF down_adrs=(AD_CART & x"FFFFF" & '1')
--pragma synthesis_off
              - (AD_CART & x"FF1FF" & '1')
--pragma synthesis_on
            THEN
              state <= sDOWN_BIN;
            ELSE
              state <= sDOWN_BIN_CLR;
            END IF;
          END IF;
          
        -- Upload CART
        WHEN sDOWN_BIN =>
          ioctl_wait_l <= '0';
          IF cfgmode = '1' THEN
            down_adrs <= AD_CONV & unsigned(ioctl_addr(20 DOWNTO 0));
          ELSE
            down_adrs <= AD_CART & unsigned(ioctl_addr(20 DOWNTO 0));
          END IF;
          down_dw   <= unsigned(ioctl_dout) & unsigned(ioctl_dout);
          IF wre_v='1' OR first='1' THEN
            down_req <= '1';
            ioctl_wait_l <= '1';
            state <= sDOWN_BIN2;
          END IF;
          IF ioctl_download = '0' THEN
            state <= sIDLE;
          END IF;
          first <= '0';
          
        WHEN sDOWN_BIN2 =>
          down_req <= '1';
          ioctl_wait_l <= '1';
          IF down_ack='1' THEN
            down_req <= '0';
            ioctl_wait_l <= '0';
            state <= sDOWN_BIN;
          END IF;

        ---------------------------------------------------
        -- CFG File. Stored in low byte of 16bits data
        WHEN sDOWN_CFG =>
          ioctl_wait_l <= '0';
          down_adrs <= AD_CFG & unsigned(ioctl_addr(19 DOWNTO 0) & '0');
          down_dw   <= x"00" & unsigned(ioctl_dout);
          down_16   <= '1';
          IF wre_v='1' OR first='1' THEN
            down_req <= '1';
            ioctl_wait_l <= '1';
            state <= sDOWN_CFG2;
            down_lastadrs <= down_adrs;
          END IF;
          IF ioctl_download = '0' THEN
            state <= sDOWN_CFG3;
          END IF;
          first <= '0';
          
        WHEN sDOWN_CFG2 =>
          down_req <= '1';
          down_16  <= '1';
          ioctl_wait_l <= '1';
          IF down_ack='1' THEN
            down_req <= '0';
            ioctl_wait_l <= '0';
            state <= sDOWN_CFG;
          END IF;

        WHEN sDOWN_CFG3 => 
          -- End file with x00
          ioctl_wait_l <= '1';
          down_adrs <= down_lastadrs + 2;
          down_dw   <= x"0000";
          down_16   <= '1';
          down_req  <= '1';
          state <= sDOWN_CFG4;
        
        WHEN sDOWN_CFG4 =>
          down_req <= '1';
          ioctl_wait_l <= '1';
          down_dw   <= x"0000";
          down_16   <= '1';
          IF down_ack='1' THEN
            down_req <= '0';
            state <= sIDLE;
          END IF;

        ---------------------------------------------------
        -- Intellicart ROM format
          -- Number of zones
        -- CLEAR CART
        WHEN sDOWN_ICART_CLR =>
          ioctl_wait_l<='1';
          down_dw <=x"FFFF";
          down_req <='1';
          state <= sDOWN_ICART_CLR2;
          rmap_dwr <= '1';

        WHEN sDOWN_ICART_CLR2 =>
          ioctl_wait_l<='1';
          down_dw <=x"FFFF";
          down_req <='1';
          IF down_ack='1' THEN
            down_req <= '0';
            down_adrs<=down_adrs+1;

            IF down_adrs=(AD_CART & x"FFFFF" & '1')
--pragma synthesis_off
              - (AD_CART & x"FF1FF" & '1')
--pragma synthesis_on
            THEN            
              state <= sDOWN_ICART;
            ELSE
              state <= sDOWN_ICART_CLR;
            END IF;
          END IF;
                    
        WHEN sDOWN_ICART =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN
            numzone<=to_integer(unsigned(ioctl_dout));
            state <= sDOWN_ICART2;
          END IF;
          
        WHEN sDOWN_ICART2 =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN -- Ignore complement
            state <= sDOWN_ICART3;
          END IF;
          
          -- Data start / end
        WHEN sDOWN_ICART3 =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN
            zone_min<=unsigned(ioctl_dout);
            state <= sDOWN_ICART4;
          END IF;

        WHEN sDOWN_ICART4 =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN
            zone_max<=unsigned(ioctl_dout);
            state <= sDOWN_LOOP3;
          END IF;
          adrs<=zone_min & '0' & x"00";
          
          -- Data copy
        WHEN sDOWN_LOOP =>
          ioctl_wait_l <= '0';
          down_adrs <= AD_CART & PAGE0 & unsigned(adrs(16 DOWNTO 0));
          down_dw   <= unsigned(ioctl_dout) & unsigned(ioctl_dout);
          IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            down_req <= '1';
            ioctl_wait_l <= '1';
            state <= sDOWN_LOOP2;
          END IF;
          IF ioctl_download = '0' THEN
            state <= sIDLE;
          END IF;
          
        WHEN sDOWN_LOOP2 =>
          down_req <= '1';
          ioctl_wait_l <= '1';
          IF down_ack='1' THEN
            down_req <= '0';
            ioctl_wait_l <= '0';
            adrs <= adrs+1;
            IF (adrs + 1)=(zone_max+1) & '0' & x"00" THEN
              state <= sDOWN_CRC;
            ELSE
              state <= sDOWN_LOOP3;
            END IF;
          END IF;
        
        WHEN sDOWN_LOOP3 =>
          ioctl_wait_l <= '0';
          IF ioctl_wr='0' THEN
            state <= sDOWN_LOOP;
          END IF;
          
          -- Data CRC
        WHEN sDOWN_CRC =>
          IF wre_v='1' THEN -- CRC. Ignore
            state<=sDOWN_CRC2;
          END IF;
          
        WHEN sDOWN_CRC2 =>
          IF wre_v='1' THEN -- CRC Ignore
            numzone <= numzone-1;
            IF numzone>1 THEN
              state<=sDOWN_ICART3;
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
          IF wre_v='1' THEN
            icart_acc_dwr<='1';
            icart_acc_ddw<=to_integer(unsigned(ioctl_dout(3 DOWNTO 0)));
            icart_acc_da<=numrange*2 + 32 *15;
            state<=sDOWN_RANGE2;
          END IF;
          
        WHEN sDOWN_RANGE2 =>
          icart_acc_dwr<='1';
          icart_acc_ddw<=to_integer(unsigned(ioctl_dout(7 DOWNTO 4)));
          icart_acc_da<=numrange*2+ 32 *15 + 1;
          numrange<=(numrange+1) MOD 16;
          IF numrange=15 THEN
            state<=sDOWN_RANGE3;
            numrange<=0;
          ELSE
            state<=sDOWN_RANGE;
          END IF;
          
        WHEN sDOWN_RANGE3 =>
          -- Adress Restriction table : 32 bytes => 64 zones
          IF wre_v='1' THEN
            icart_map_dwr<='1';
            icart_map_ddw<=to_unsigned(numrange*8,8);
            icart_map_da<=numrange + 32*15;
            
            icart_fine_dwr<='1';
            icart_fine_ddw<=unsigned(ioctl_dout);
            icart_fine_da<=(numrange / 16) +
                           (numrange MOD 16)*2 + 32*15;
            
            numrange<=(numrange+1) MOD 32;
            IF numrange = 31 THEN
              state <= sWAIT;
            END IF;
          END IF;
          IF ioctl_download = '0' THEN
            state <= sIDLE;
          END IF;
          
        WHEN sWAIT =>
          -- Skip trailing data.
          IF ioctl_download = '0' THEN
            state <= sIDLE;
          END IF;

      END CASE;

      IF clr_loaded='1' THEN
        bin_loaded <= '0';
        cfg_loaded <= '0';
      END IF;
      IF reset='1' THEN
        state <= sIDLE;
      END IF;
    END IF;
  END PROCESS ReadRom;
  
  ioctl_wait<=ioctl_wait_l;

  ---------------------------------------------------------
  -- Remapping through CFG file parser
  Parsor:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      clr_loaded <= '0';
      parser_reset <= '0';
    
      IF bin_loaded = '1' AND cfg_loaded = '1' AND format='1' THEN
        remapped   <= '0';
        clr_loaded <= '1'; -- Clear BIN+CFG files loaded
        parser     <= '1'; -- Enable parser ROM
        parser_reset <= '1';
      END IF;
        
      -- MAP_VARS :
      -- 0 : ECS SET
      -- 1 : ECS CLR 
      -- 2 : JLP SET
      -- 3 : JLP CLR
      -- 4 : RESET
      ecsjlp_set <= to_std_logic(map_vars(3 DOWNTO 0) /= 0);
      IF map_vars(0)='1' THEN
        ecs_up <= '1';
        jlp_up <= jlp;
      END IF;
      IF map_vars(1)='1' THEN
        ecs_up <= '0';
        jlp_up <= jlp;
      END IF;
      IF map_vars(2)='1' THEN
        jlp_up <= '1';
        ecs_up <= ecs;
      END IF;
      IF map_vars(3)='1' THEN
        jlp_up <= '0';
        ecs_up <= ecs;
      END IF;
      IF map_vars(4)='1' THEN
        -- RESET + Enable remapping + disable 
        remapped <= '1'; -- Switch to remapped memory map
        parser   <= '0'; -- Disable parser ROM
        parser_reset <= '1';
      END IF;

      IF format ='0' THEN
        parser   <= '0';
        remapped <= '0';
      END IF;

    END IF;
  END PROCESS Parsor;
  ---------------------------------------------------------
  -- ICART mapping table
  icarmap:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      imap <= icart_map(idx); -- Remapping table
      IF icart_map_dwr='1' THEN
        icart_map(idx) <= icart_map_ddw;
      END IF;
    END IF;
  END PROCESS icarmap;
  
  icarmap2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF icart_pwr='1' THEN
        icart_map(cidx) <= icart_dw(7 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS icarmap2;

  cidx<=to_integer(ad(3 DOWNTO 0) & ad(4)) WHEN ioctl_download='0'
         ELSE icart_map_da;

  -------------------------------------
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
  
  -------------------------------------
  ifine <= icart_fine(idx) WHEN rising_edge(clksys);
  
  icarfine2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF icart_fine_dwr='1' THEN
        icart_fine(icart_fine_da) <= icart_fine_ddw;
      END IF;
    END IF;
  END PROCESS icarfine2;

  -------------------------------------
  -- CPU ACCESS
  RMAP:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      rmap_dr <= rmap_mem(to_integer(rmap_a));
      IF rmap_wr='1' THEN
        rmap_mem(to_integer(rmap_a)) := rmap_dw;
      END IF;
    END IF;
  END PROCESS RMAP;

  -- DOWNLOAD ACCES
  RMAP2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      IF rmap_dwr='1' THEN
        rmap_mem(to_integer(down_adrs(20 DOWNTO 9))) := ATTR_NOMAP;
      END IF;
    END IF;
  END PROCESS RMAP2;

  ---------------------------------------------------------
  SDRAM_ALT:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      down_ack  <= '0';
      cart_rdy_l  <= '0';

      sdram_wr_l <= '0';
      sdram_rd_l <= '0';
      
      phi2 <= phi;
      phi3 <= phi2;
      phi4 <= phi3;
      phi5 <= phi4;

      sdram_state2 <= sdram_state;
      
      CASE sdram_state IS
        WHEN sDINIT =>
          sdram_state <= sDINIT2;
          sdram_init <= '0';

        WHEN sDINIT2 =>
          sdram_state <= sDINIT3;
          sdram_init <= '1';

        WHEN sDINIT3 =>
          IF sdram_ready='0' THEN
            sdram_state <= sDINIT4;
          END IF;

        WHEN sDINIT4 =>
          IF sdram_ready='1' THEN
            sdram_state <= sDIDLE;
          END IF;
          sdram_init <= '0';
              
        WHEN sDIDLE =>
          IF sdram_ready='1' THEN
            -- SDRAM controller is ready to accept new command
            IF down_req='1' AND down_ack='0' THEN
              -- Download ROM / Cartridge
              sdram_wr_l  <= '1';
              sdram_wtbt  <= (NOT down_adrs(0) OR down_16) & (down_adrs(0) OR down_16); -- Big endian download format
              sdram_addr  <= down_adrs(24 DOWNTO 1) & '0';
              sdram_din   <= std_logic_vector(down_dw);
              sdram_state <= sDDOWN;
              
            ELSIF cart_rd='1' AND cart_rdy_l='0' AND phi5='1' THEN
              -- CPU READ
              sdram_wtbt  <= "11";
              sdram_addr <= ad_mapper & '0';

              IF cfgmode = '1' AND remapped='1' THEN
                -- Remapped through CFG file
                sdram_rd_l  <= to_std_logic(rmap_dr /= ATTR_NOMAP);
              ELSIF cfgmode = '1' AND remapped='0' THEN
                -- Remapper access
                sdram_rd_l  <= '1';
              ELSE 
                -- Normal access, intellicart or RAW format
                sdram_rd_l <= rden;
              END IF;
              sdram_din   <= std_logic_vector(cart_dw);
              sdram_state <= sDCPU;
              
            ELSIF cart_wr='1' AND cart_rdy_l='0' AND phi5='1' THEN
              -- CPU WRITE
              sdram_wtbt  <= "11";
              sdram_addr <= ad_mapper & '0';

              IF cfgmode = '1' AND remapped='1' THEN
                -- Remapped through CFG file
                sdram_wr_l <= to_std_logic(rmap_dr = ATTR_RAM8 OR rmap_dr = ATTR_RAM16);
              ELSIF cfgmode = '1' AND remapped='0' THEN
                sdram_wr_l  <= '1';
              ELSE 
                -- Normal access, intellicart or RAW format
                sdram_wr_l  <= wren;
              END IF;
              sdram_din   <= std_logic_vector(cart_dw);
              sdram_state <= sDCPU;
            END IF;
          END IF;

        WHEN sDDOWN =>
          IF sdram_ready='1' AND sdram_wr_l='0' AND sdram_rd_l='0' AND sdram_state2=sDDOWN THEN
            down_ack    <= '1';
            sdram_state <= sDIDLE;
          END IF;

        WHEN sDCPU =>
          IF sdram_ready='1' AND sdram_wr_l='0' AND sdram_rd_l='0' AND sdram_state2=sDCPU THEN
            cart_dri    <= unsigned(sdram_dout);
            cart_rdy_l  <= '1';
            sdram_state <= sDIDLE;
          END IF;
      END CASE;

      IF hwreset_n='0' THEN
        sdram_state <= sDINIT;
      END IF;
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------
  cart_rdy <= cart_rdy_l;
  cart_dr<=cart_dri                       WHEN rden='1' AND fine='1' AND byen='0' AND remapped='0' ELSE
           (x"00" & cart_dri(7 DOWNTO 0)) WHEN rden='1' AND fine='1' AND byen='1' AND remapped='0' ELSE 
           cart_dri             WHEN (rmap_dr = ATTR_ROM OR rmap_dr = ATTR_RAM16) AND remapped='1' ELSE
           (x"00" & cart_dri(7 DOWNTO 0)) WHEN rmap_dr = ATTR_RAM8                AND remapped='1' ELSE
           x"FFFF";
  
  sdram_wr <= sdram_wr_l;
  sdram_rd <= sdram_rd_l;
  
  ---------------------------------------------------------

END ARCHITECTURE rtl;
