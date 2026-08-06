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

ENTITY cart IS
  PORT (
    mapp     : IN    std_logic_vector(3 DOWNTO 0); -- Mapping
    ecs      : IN    std_logic;                    -- ECS
    format   : IN    std_logic_vector(1 DOWNTO 0); -- Auto, Raw, Intellicart

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
    sdram_addr   : OUT  std_logic_vector(24 DOWNTO 0); -- 32M * 2
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
    
    -----------------------------------
    phi       : IN std_logic;  -- PHI clock enable
    clksys    : IN std_logic; -- 12x Pixel Clock
    reset     : IN std_logic;
    reset_na  : IN std_logic;
    hwreset_n : IN std_logic
    );
END ENTITY cart;

-------------------------------------------------------------------------------

ARCHITECTURE rtl OF cart IS

  TYPE type_jmap IS RECORD
    crc : uv32;
    m   : uint4;
  END RECORD;

  CONSTANT FORMAT_AUTO  : std_logic_vector(1 DOWNTO 0) := "00";
  CONSTANT FORMAT_RAW   : std_logic_vector(1 DOWNTO 0) := "01";
  CONSTANT FORMAT_ICART : std_logic_vector(1 DOWNTO 0) := "10";

  TYPE arr_jmap IS ARRAY (natural RANGE <>) OF type_jmap;
  CONSTANT MAPS : arr_jmap := (
    (x"4CC46A04",1),(x"D5F038B6",1),(x"A3ACD160",1),(x"4422868E",1), -- Championsip Tennis / Demo 5853 / Demo 5853 / King of the Mountain
    (x"C2063C08",1),(x"A12C27E1",1),                                 -- WSML Baseball / WSML Baseball (alt)
    (x"515E1D7E",2),(x"0BF464C6",2),(x"3289C8BA",2),(x"16BFB8EB",2), -- Body Slam SuperPro Wrestling / Chip Shot SuperPro Golf / Commando / SuperPro Decathlon
    (x"6802B191",2),(x"13EE56F1",2),(x"FF83FF80",2),(x"2C5FD5FA",2), -- Deep Pockets SuperPro Pool and Billards / Diner / Hover Foorce / Learning Fun I
    (x"632F6ADF",2),(x"B745C1CA",2),(x"BB939881",2),(x"800B572F",2), -- Learning Fun II / Stadium Mug Buggies / Pole Position / Skam Dunk SuperPro Baskedball
    (x"32076E9D",2),(x"A95021FC",2),(x"23DC808D",2),                 -- SuperPro Football / Spiker SuperPro Volleyball / ScarFinger
    (x"D1D352A0",3),                                                 -- Tower Of Doom
    (x"752FD927",4),(x"3825C25B",4),                                 -- USCF Chess / Land Battle (?)
    (x"4B23A757",5),(x"D8F99AA2",5),(x"159AF7F7",5),(x"A21C31C3",5), -- Congo Bongo / Defender / Dig Dug / Pac Man (Atarisoft)
    (x"6E4E8EB4",5),                                                 -- Pac Man (Intv)
    (x"D5363B8C",6),                                                 -- Centipede
    (x"13FF363C",7),(x"C047D487",7),(x"5E6A8CD8",7),(x"E806AD91",7), -- Atlantis / Beauty and the Beast / Demon Attack / MicroSurgeon
    (x"C83EEA4C",8),                                                 -- MTE201 Test Cart
    (x"CE8FC699",9),(x"095638C0",9));                                -- Game Factory / Triple Challenge
    
  SIGNAL ioctl_wait_l,ioctl_download2,ioctl_wr2 : std_logic;

  SIGNAL map_cpt : uint4;
  
  SIGNAL mapcpt : natural RANGE 0 TO MAPS'length+2;
  
  SIGNAL rom_exec_up,rom_voice_up,rom_grom_up,rom_ecs_up : std_logic;

  SIGNAL adrs : uv17;
  TYPE enum_state IS (sIDLE,sDOWN,
                      sDOWN_BIN,sDOWN_BIN2,sDOWN_BIN3,
                      sCLR1,sCLR2,sROM,
                      sDOWN_CART,sDOWN_CART2,sDOWN_CART3,sDOWN_CART4,
                      sDOWN_LOOP,sDOWN_LOOP2,sDOWN_LOOP3,
                      sDOWN_CRC,sDOWN_CRC2,
                      sDOWN_RANGE,sDOWN_RANGE2,sDOWN_RANGE3,sWAIT);
  SIGNAL state : enum_state;
  SIGNAL down_dw : uv8;
  SIGNAL down_adrs : uv17;
  SIGNAL first : std_logic;
  
  SIGNAL cart_acc : std_logic;
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

  SIGNAL icart : std_logic;
  SIGNAL icart_pwr,cart_wrm : std_logic;

  SIGNAL down_req, down_ack : std_logic;
  
  TYPE enum_sdram_state IS (
    sDINIT, sDINIT2, sDINIT3, sDINIT4,
    sDIDLE, sDDOWN, sDCPU);
  SIGNAL sdram_state, sdram_state2 : enum_sdram_state;
  SIGNAL sdram_wr_l : std_logic;
  SIGNAL sdram_rd_l : std_logic;

  SIGNAL smap,mmap,mmap2 : uint4;
  
  SIGNAL cad : uv16;

  SIGNAL crc,xcrc : uv32;
  SIGNAL search,found : std_logic;
  
  SIGNAL ecs2 : std_logic;

  TYPE enum_sdmode IS (sDOWNLOAD,sRUN);
  SIGNAL sdmode : enum_sdmode;

  SIGNAL phi2, phi3 : std_logic;
  
  ----------------------------------------------------------
  -- ROM WRITE
  -- EXEC  : 8192  = 4096  * 2    0000 => 1FFF
  -- GROM  : 2048                 2000 => 27FF
  -- VOICE : 2048                 2800 => 2FFF
  -- ECS   : 24576 = 12288 * 2    3000 => 8FFF
  
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
      idx<=to_integer(ad(15 DOWNTO 11)) + 32 * mmap;
      
      cad<=(imap + ad(10 DOWNTO 8)) & ad(7 DOWNTO 0);
      
      rden<=to_unsigned(iacc,4)(0);
      wren<=to_unsigned(iacc,4)(1);
      byen<=to_unsigned(iacc,4)(2);
      bsen<=to_unsigned(iacc,4)(3);
      
      -- [0xxx 0yyy]  : Enable between X and Y
      fine<=to_std_logic(to_integer(ifine(6 DOWNTO 4)) <=to_integer(ad(10 DOWNTO 8)) AND
                         to_integer(ifine(2 DOWNTO 0)) >=to_integer(ad(10 DOWNTO 8)));
      
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
  
  ---------------------------------------------------------
  ReadRom:PROCESS(clksys) IS
    VARIABLE wre_v : std_logic;
  BEGIN
    IF rising_edge(clksys) THEN

      ioctl_wr2<=ioctl_wr;

      wre_v := ioctl_wr AND NOT ioctl_wr2;
      
      IF ioctl_download='0' AND state/=sROM THEN
        state<=sIDLE;
      END IF;
      
      down_req <= '0';
      down_dw <=unsigned(ioctl_dout);
      icart_acc_dwr<='0';
      icart_map_dwr<='0';
      icart_fine_dwr<='0';

      rom_dw<=unsigned(ioctl_dout);
      rom_exec_wr<='0';
      rom_grom_wr<='0';
      rom_ecs_wr <='0';
      rom_voice_wr<='0';
      
      --ioctl_wait_l<=ioctl_wr;
      clear<='0';

      -----------------------------------------------------
      CASE state IS
        ---------------------------------------------------
        WHEN sIDLE =>
          down_adrs<=(OTHERS =>'0');
          IF ioctl_download='1' THEN
            ioctl_wait_l<='1';
            state<=sCLR1;
          END IF;
          rom_exec_up  <='0';
          rom_grom_up  <='0';
          rom_voice_up <='0';
          rom_ecs_up   <='0';
          
        WHEN sCLR1 =>
          down_dw <=x"00";
          ioctl_wait_l<='1';
          down_req <='1';
          state <= sCLR2;

        WHEN sCLR2 =>
          down_dw <=x"00";
          ioctl_wait_l<='1';
          down_req <='1';
          IF down_ack='1' THEN
            down_req <= '0';
            down_adrs<=down_adrs+1;
            IF down_adrs=x"FFFF" THEN
              state <= sDOWN;
            ELSE
              state <= sCLR1;
            END IF;
          END IF;
          
        WHEN sDOWN =>
          down_adrs <= unsigned(ioctl_addr(16 DOWNTO 0));
          ioctl_wait_l<='0';
          first <= '1';
          IF ioctl_download='0' THEN
            state <= sIDLE;
            
          ELSIF unsigned(ioctl_index)=0 THEN
            state <= sROM;

          ELSIF ioctl_wr='1' AND ioctl_wait_l='0' AND
            ((ioctl_dout=x"A8" OR ((ioctl_dout AND x"DF")=x"41") OR format=FORMAT_ICART) AND
            format/=FORMAT_RAW) THEN
            state <= sDOWN_CART;
            icart <= '1';
            ioctl_wait_l <= '1';

          ELSIF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            state <= sDOWN_BIN;
            icart <= '0';
            ioctl_wait_l <= '1';
          END IF;
          
          -------------------------------------------------
          -- Internal ROMs : EXEC,GROM,VOICE,ECS
        WHEN sROM =>
          rom_aw<=unsigned(ioctl_addr(15 DOWNTO 0));
          IF unsigned(ioctl_addr)<16#2000# AND unsigned(ioctl_index)=0 THEN
            rom_exec_wr<=wre_v;
            rom_exec_up<=rom_exec_up OR wre_v;

          ELSIF (unsigned(ioctl_addr)<16#2800# AND unsigned(ioctl_index)=0) OR
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
          
        ---------------------------------------------------
        -- Plain binary file
        WHEN sDOWN_BIN =>
          ioctl_wait_l <= '0';
          down_adrs <= unsigned(ioctl_addr(16 DOWNTO 0));
          down_dw   <= unsigned(ioctl_dout);
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
            state <= sDOWN_BIN3;
          END IF;

        WHEN sDOWN_BIN3 =>
          ioctl_wait_l <= '0';
          IF ioctl_wr='0' THEN
            state <= sDOWN_BIN;
          END IF;
          
        ---------------------------------------------------
        -- Intellicart ROM format
          -- Number of zones
        WHEN sDOWN_CART =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN
            numzone<=to_integer(unsigned(ioctl_dout));
            state <= sDOWN_CART2;
          END IF;
          
        WHEN sDOWN_CART2 =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN -- Ignore complement
            state <= sDOWN_CART3;
          END IF;
          
          -- Data start / end
        WHEN sDOWN_CART3 =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN
            zone_min<=unsigned(ioctl_dout);
            state <= sDOWN_CART4;
          END IF;

        WHEN sDOWN_CART4 =>
          ioctl_wait_l <= '0';
          IF wre_v='1' THEN
            zone_max<=unsigned(ioctl_dout);
            state <= sDOWN_LOOP3;
          END IF;
          adrs<=zone_min & '0' & x"00";
          
          -- Data copy
        WHEN sDOWN_LOOP =>
          ioctl_wait_l <= '0';
          down_adrs <= unsigned(adrs(16 DOWNTO 0));
          down_dw   <= unsigned(ioctl_dout);
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
              state<=sDOWN_CART3;
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

  ---------------------------------------------------------
  -- SDRAM
  --   sdram_init   : OUT  std_logic;
  --   sdram_wtbt   : OUT  std_logic_vector(1 DOWNTO 0);
  --   sdram_addr   : OUT  std_logic_vector(24 DOWNTO 0);
  --   sdram_dout   : IN   std_logic_vector(15 DOWNTO 0);
  --   sdram_din    : OUT  std_logic_vector(15 DOWNTO 0);
  --   sdram_we     : OUT  std_logic;
  --   sdram_rd     : OUT  std_logic;
  --   sdram_ready  : IN   std_logic;


  -- SDRAM
  -- CFG FILE  1MB
  -- RAW DATA  2MB : Input files
  -- OUTPUT    2MB : 16 pages * 64kWords

  -- [19:0]  = 1MW = 2MB
  -- [21:20] = 00 = RAM
  --           01 = RAW
  --           10 = CFG


  -- DOWNLOAD
  --   down_wrl
  --   down_wrh
  --   down_adrs    16bits
  --   down_dw    8bits
  --   w_rdy

  -- CPU
  --   ad       : IN  uv16;
  --   cart_dr  : OUT uv16;
  --   cart_dw  : IN  uv16;
  --   cart_wr  : IN  std_logic;
  --   cart_rd  : IN  std_logic;

  SDRAM_ALT:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      down_ack  <= '0';
      cart_rdy_l  <= '0';

      sdram_wr_l  <= '0';
      sdram_rd_l  <= '0';

      phi2 <= phi;
      phi3 <= phi2;

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
              sdram_wtbt  <= NOT down_adrs(0) & down_adrs(0); -- Big endian download format
              sdram_addr  <= std_logic_vector("00000000" & down_adrs(16 DOWNTO 1) & '0');
              sdram_din   <= std_logic_vector(down_dw) & std_logic_vector(down_dw);
              sdram_state <= sDDOWN;
              
            ELSIF cart_rd='1' AND cart_rdy_l='0' AND phi3='1' THEN
              -- CPU READ
              sdram_rd_l  <= '1';
              sdram_wr_l  <= '0';
              sdram_wtbt  <= "11";
              sdram_addr  <= std_logic_vector("00000000" & cad(15 DOWNTO 0) & '0');
              sdram_din   <= std_logic_vector(cart_dw);
              sdram_state <= sDCPU;

            ELSIF cart_wr='1' AND cart_rdy_l='0' AND phi3='1' THEN
              -- CPU WRITE
              sdram_rd_l  <= '0';
              sdram_wr_l  <= '1';
              sdram_wtbt  <= "11";
              sdram_addr  <= std_logic_vector("00000000" & cad(15 DOWNTO 0) & '0');
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
  cart_dr<=cart_dri                       WHEN cart_acc='1' AND byen='0' ELSE
           (x"FF" & cart_dri(7 DOWNTO 0)) WHEN cart_acc='1' AND byen='1' ELSE x"FFFF";
  
  cart_acc<=rden AND fine;

  sdram_wr <= sdram_wr_l;
  sdram_rd <= sdram_rd_l;
  
  ---------------------------------------------------------

END ARCHITECTURE rtl;
