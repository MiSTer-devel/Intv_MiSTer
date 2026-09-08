--------------------------------------------------------------------------------
-- Intellivision
--------------------------------------------------------------------------------
-- DO 8/2026
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY work;
USE work.base_pack.ALL;

PACKAGE intv_pack IS
  -----------------------------------------------------------------------------
  -- 4 3210 9876 5432 1098 7654 3210
  -- X XXXX XXXX XXXX XXXX XXXX XXXX : 25:0
  --           A AAAA AAAA AAAA AAA  : 64k * 16bits : CPU Adresses
  --      P PPP                      : 16 pages
  -- 0 000 = CARTRIDGE
  -- 0 001 = CONVERTED CARTRIDGE
  -- 0 010 = CFG

  CONSTANT AD_CART : uv4 := "0000"; -- CARTRIDGE
  CONSTANT AD_CONV : uv4 := "0001"; -- SOURCE TO CONVERT
  CONSTANT AD_CFG  : uv4 := "0010"; -- CFG Mapping
  CONSTANT PAGE0   : uv4 := "0000";

  CONSTANT ATTR_NOMAP : uv2 := "00";
  CONSTANT ATTR_ROM   : uv2 := "01";
  CONSTANT ATTR_RAM8  : uv2 := "10";
  CONSTANT ATTR_RAM16 : uv2 := "11";
  
  -----------------------------------------------------------------------------
  TYPE type_jmap IS RECORD
    crc : uv32;
    m   : uint4;
  END RECORD;

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
    
  -----------------------------------------------------------------------------
  FUNCTION crc8 (
    CONSTANT d   : IN unsigned(7 DOWNTO 0);
    CONSTANT crc : IN unsigned(31 DOWNTO 0)) RETURN unsigned;

END PACKAGE intv_pack;

--#############################################################################
PACKAGE BODY intv_pack IS

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

END PACKAGE BODY;
