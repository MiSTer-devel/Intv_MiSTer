--------------------------------------------------------------------------------
-- Intellivision
--------------------------------------------------------------------------------
-- AY-3-8914 Sound generator
--------------------------------------------------------------------------------
-- DO 8/2019
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY snd IS
  PORT (
    --------------------------
    ad       : IN  uv16;
    dw       : IN  uv8;
    dr       : OUT uv8;
    wr       : IN  std_logic;
    
    --------------------------
    sound    : OUT sv8;
    
    --------------------------
    pa_i      : IN  uv8;
    pa_o      : OUT uv8;
    pa_en     : OUT std_logic;
    pb_i      : IN  uv8;
    pb_o      : OUT uv8;
    pb_en     : OUT std_logic;
    
    tick     : IN  std_logic; --- 3.579545 / 16
    
    --------------------------
    clk      : IN  std_logic;
    reset_na : IN std_logic
    );
END ENTITY snd;


ARCHITECTURE rtl OF snd IS
  
  SIGNAL pa_in_i,pb_in_i : std_logic;

  ------------------------------------------------
  SIGNAL tone_a_per,tone_b_per,tone_c_per : uv12;
  SIGNAL tone_a_cpt,tone_b_cpt,tone_c_cpt : uv12;
  SIGNAL tone_a_amp,tone_b_amp,tone_c_amp : uv4;
  SIGNAL tone_a_out,tone_b_out,tone_c_out : std_logic;
  SIGNAL tone_a_en ,tone_b_en ,tone_c_en  : std_logic;
  SIGNAL tone_a_m  ,tone_b_m  ,tone_c_m   : std_logic;
  
  SIGNAL env_per,env_cpt : uv16;
  SIGNAL env_shape : uv4;
  SIGNAL env_seq : uv5;
  SIGNAL env_amp : uv4;
  
  SIGNAL noise_a_en,noise_b_en,noise_c_en : std_logic;
  SIGNAL noise_per,noise_cpt : uv5;
  SIGNAL noise_out  : std_logic;
  
  SIGNAL env_sus : std_logic;
  
  SIGNAL xxx_lev_a,xxx_lev_b,xxx_lev_c : signed(4 DOWNTO 0);
  SIGNAL xxx_sono : integer RANGE -100000 TO 100000;
  
  ------------------------------------------------
  SIGNAL right_right : std_logic;
  
  CONSTANT LOG : arr_uv8(0 TO 15):=(
    x"13",x"17",x"1B",x"20",x"26",x"2D",x"36",x"40",
    x"4C",x"5A",x"6B",x"80",x"98",x"B4",x"D6",x"FF");
  
  SIGNAL pot : uv8;
  
  SIGNAL poly17 : unsigned(16 DOWNTO 0);
  SIGNAL ticknoise,ticknoise2 : std_logic;
  ------------------------------------------------
BEGIN
  
  pa_en<=NOT pa_in_i;
  pb_en<=NOT pb_in_i;
  
  SONO:PROCESS(clk,reset_na) IS
    ------------------------------------
    FUNCTION mixgen (gen      : std_logic;
                     noise    : std_logic;
                     amp      : uv4;       -- Fixed amplitude
                     env      : uv4;       -- Envelope amplitude
                     m        : std_logic; -- 0= Fix 1=Envelope
                     en_gen   : std_logic; 
                     en_noise : std_logic) RETURN signed IS
      VARIABLE ampli_v : uv4;
      VARIABLE sig_v   : std_logic;
      VARIABLE out_v : uv5;
    BEGIN
      sig_v:=(en_noise AND noise) OR (en_gen AND gen);
      ampli_v:=mux(m,env,amp);
      
      IF sig_v='0' THEN
        out_v:='0' & ampli_v;
      ELSE
        out_v:= "00000" - ('0' & ampli_v);
      END IF;
      
      IF en_noise='0' AND en_gen='0' THEN
        out_v:="00000";
      END IF;
      
      RETURN signed(out_v);  
    END FUNCTION mixgen;
    
    ------------------------------------
    VARIABLE lev_v : signed(7 DOWNTO 0);
    VARIABLE p17z : std_logic;
  BEGIN
    IF reset_na='0' THEN
      tone_a_out<='0';
      tone_b_out<='0';
      tone_c_out<='0';

      tone_a_per<=(OTHERS =>'0');
      tone_b_per<=(OTHERS =>'0');
      tone_c_per<=(OTHERS =>'0');

      env_cpt<=(OTHERS =>'0');
      env_per<=(OTHERS =>'0');
      env_seq<="00000";
      
    ELSIF rising_edge(clk) THEN
      CASE ad(3 DOWNTO 0) IS
        WHEN "0000" => -- 0:R0 : Tone Generator. Channel A. Fine Tune Reg
          dr<=tone_a_per(7 DOWNTO 0);
          IF wr='1' THEN
            tone_a_per(7 DOWNTO 0)<=dw;
          END IF;
        WHEN "0100" => -- 4:R1 : Tone Generator. Channel A. Coarse Tune Reg
          dr<=x"0" & tone_a_per(11 DOWNTO 8);
          IF wr='1' THEN
            tone_a_per(11 DOWNTO 8)<=dw(3 DOWNTO 0);
          END IF;

        WHEN "0001" => -- 1:R2 : Tone Generator. Channel B. Fine Tune Reg
          dr<=tone_b_per(7 DOWNTO 0);
          IF wr='1' THEN
            tone_b_per(7 DOWNTO 0)<=dw;
          END IF;

        WHEN "0101" => -- 5:R3 : Tone Generator. Channel B. Coarse Tune Reg
          dr<=x"0" & tone_b_per(11 DOWNTO 8);
          IF wr='1' THEN
            tone_b_per(11 DOWNTO 8)<=dw(3 DOWNTO 0);
          END IF;

        WHEN "0010" => -- 2:R4 : Tone Generator. Channel C. Fine Tune Reg
          dr<=tone_c_per(7 DOWNTO 0);
          IF wr='1' THEN
            tone_c_per(7 DOWNTO 0)<=dw;
          END IF;

        WHEN "0110" => -- 6:R5 : Tone Generator. Channel C. Coarse Tune Reg
          dr<=x"0" & tone_c_per(11 DOWNTO 8);
          IF wr='1' THEN
            tone_c_per(11 DOWNTO 8)<=dw(3 DOWNTO 0);
          END IF;

        WHEN "1001" => -- 9:R6 : Noise Generator. Period
          dr<="000" & noise_per;
          IF wr='1' THEN
            noise_per<=dw(4 DOWNTO 0);
            noise_cpt<="00000";
          END IF;
          
        WHEN "1000" => -- 8:R7 : Mixer Control I/O Disable
          dr<=NOT(pb_in_i & pa_in_i &
                       noise_c_en & noise_b_en & noise_a_en &
                       tone_c_en & tone_b_en & tone_a_en);
          IF wr='1' THEN
            pb_in_i<=NOT dw(7);
            pa_in_i<=NOT dw(6);
            noise_c_en<=NOT dw(5);
            noise_b_en<=NOT dw(4);
            noise_a_en<=NOT dw(3);
            tone_c_en<=NOT dw(2);
            tone_b_en<=NOT dw(1);
            tone_a_en<=NOT dw(0);
          END IF;

        WHEN "1011" => -- B:R10 : Amplitude Control A
          dr<="000" & tone_a_m & tone_a_amp;
          IF wr='1' THEN
            tone_a_amp<=dw(3 DOWNTO 0);
            tone_a_m  <=dw(4);
          END IF;
        WHEN "1100" => -- C:R11 : Amplitude Control B
          dr<="000" & tone_b_m & tone_b_amp;
          IF wr='1' THEN
            tone_b_amp<=dw(3 DOWNTO 0);
            tone_b_m  <=dw(4);
          END IF;

        WHEN "1101" => -- D:R12 : Amplitude Control C
          dr<="000" & tone_c_m & tone_c_amp;
          IF wr='1' THEN
            tone_c_amp<=dw(3 DOWNTO 0);
            tone_c_m  <=dw(4);
          END IF;
          
        WHEN "0011" => -- 3:R13 : Envelope Generator Control. Fine Tune
          dr<=env_per(7 DOWNTO 0);
          IF wr='1' THEN
            env_per(7 DOWNTO 0)<=dw;
          END IF;
          
        WHEN "0111" => -- 7:R14 : Envelope Generator Control. Coarse Tune
          dr<=env_per(15 DOWNTO 8);
          IF wr='1' THEN
            env_per(15 DOWNTO 8)<=dw;
          END IF;
          
        WHEN "1010" => -- A:R15 : Envelope Shape / Cycle Control
          dr<=x"0" & env_shape;
          IF wr='1' THEN
            env_shape<=dw(3 DOWNTO 0);
            env_sus<='0';
            env_seq<="00000";
          END IF;
          
        WHEN "1110" => -- E:R16 : IO Port A data store
          dr<=pa_i;
          IF wr='1' THEN
            pa_o<=dw;
          END IF;
          
        WHEN "1111" => -- F:R17 : IO Port B data store
          dr<=pb_i;
          IF wr='1' THEN
            pb_o<=dw;
          END IF;

        WHEN OTHERS => NULL;
      END CASE;

      ------------------------------------------------------
      -- Tone generators
      IF tick='1' THEN
        IF tone_a_cpt/=x"000" THEN
          tone_a_cpt<=tone_a_cpt-1;
        ELSE
          tone_a_cpt<=tone_a_per;
          tone_a_out<=NOT tone_a_out;
        END IF;
        
        IF tone_b_cpt/=x"000" THEN
          tone_b_cpt<=tone_b_cpt-1;
        ELSE
          tone_b_cpt<=tone_b_per;
          tone_b_out<=NOT tone_b_out;
        END IF;
        
        IF tone_c_cpt/=x"000" THEN
          tone_c_cpt<=tone_c_cpt-1;
        ELSE
          tone_c_cpt<=tone_c_per;
          tone_c_out<=NOT tone_c_out;
        END IF;
      END IF;
        
      IF tone_a_en='0' THEN
        tone_a_out<='0';
        tone_a_cpt<=(OTHERS =>'0');
      END IF;

      IF tone_b_en='0' THEN
        tone_b_out<='0';
        tone_b_cpt<=(OTHERS =>'0');
      END IF;

      IF tone_c_en='0' THEN
        tone_c_out<='0';
        tone_c_cpt<=(OTHERS =>'0');
      END IF;
      
      ------------------------------------------------------
      -- Noise generators
      IF tick='1' THEN
        IF noise_cpt/="00000" THEN
          noise_cpt<=noise_cpt-1;
          ticknoise<='0';
        ELSE
          IF noise_per="00000" THEN
            noise_cpt<="00000";  
          ELSE
            noise_cpt<=noise_per-1;
          END IF;
          ticknoise<='1';
        END IF;

        ticknoise2<=ticknoise2 XOR ticknoise;

        IF ticknoise='1' AND ticknoise2='1' THEN
          p17z := '0';
          IF poly17 = "00000000000000000" THEN
            p17z := '1';
          END IF;
          poly17 <= (poly17(0) XOR poly17(2) XOR p17z) & poly17(16 DOWNTO 1);
        END IF;
      END IF;
      
      noise_out<=poly17(0);
      
      ------------------------------------------------------
      -- Envelope
      IF tick='1' THEN
        IF env_cpt/=x"0000" THEN
          env_cpt<=env_cpt-1;
        ELSE
          env_cpt<=env_per;
          env_seq<=env_seq+1;
          IF env_seq="01111" THEN
            env_sus<='1';
          END IF;
        END IF;
      END IF;

      IF env_sus='0' THEN -- First period
        CASE env_shape IS
          WHEN "0000" | "0001" | "0010" | "0011" | "1001" | -- \_______
               "1011" |  -- \"""""""
               "1000" |  -- \\\\\\\\
               "1010" => -- \/\/\/\/
            env_amp<="1111" - env_seq(3 DOWNTO 0);
            
          WHEN "0100" | "0101" | "0110" | "0111" | "1111" | -- /_______
               "1101" |  -- /"""""""
               "1100" |  -- ////////
               "1110" => -- /\/\/\/\
            env_amp<=env_seq(3 DOWNTO 0);
            
            IF env_seq(4)=env_shape(2) THEN
              env_amp<="1111" - env_seq(3 DOWNTO 0);
            ELSE
              env_amp<=env_seq(3 DOWNTO 0);
            END IF;
          WHEN OTHERS =>
            env_amp<="0000";
        END CASE;
      ELSE -- Following periods

        CASE env_shape IS
          WHEN "0000" | "0001" | "0010" | "0011" | "1001" |  -- \_______
               "0100" | "0101" | "0110" | "0111" | "1111" => -- /_______
            env_amp<="0000";
            
          WHEN "1011" |  -- \"""""""
               "1101" => -- /"""""""
            env_amp<="1111";
            
          WHEN "1000" => -- \\\\\\\\
            env_amp<="1111" - env_seq(3 DOWNTO 0);
            
          WHEN "1100" => -- ////////
            env_amp<=env_seq(3 DOWNTO 0);
            
          WHEN "1010" |  -- \/\/\/\/
               "1110" => -- /\/\/\/\
            IF env_seq(4)=env_shape(2) THEN
              env_amp<="1111" - env_seq(3 DOWNTO 0);
            ELSE
              env_amp<=env_seq(3 DOWNTO 0);
            END IF;
            
          WHEN OTHERS =>
            env_amp<="0000";
        END CASE;
      END IF;
      
      ------------------------------------------------------
      lev_v:=x"00";
      lev_v:=lev_v+mixgen(tone_a_out,noise_out,
                          tone_a_amp,env_amp,
                          tone_a_m,
                          tone_a_en,noise_a_en);
      xxx_lev_a<=mixgen(tone_a_out,noise_out,
                        tone_a_amp,env_amp,
                        tone_a_m,
                        tone_a_en,noise_a_en);
      
      lev_v:=lev_v+mixgen(tone_b_out,noise_out,
                          tone_b_amp,env_amp,
                          tone_b_m,
                          tone_b_en,noise_b_en);
      xxx_lev_b<=mixgen(tone_b_out,noise_out,
                        tone_b_amp,env_amp,
                        tone_b_m,
                        tone_b_en,noise_b_en);
      
      lev_v:=lev_v+mixgen(tone_c_out,noise_out,
                          tone_c_amp,env_amp,
                          tone_c_m,
                          tone_c_en,noise_c_en);
      xxx_lev_c<=mixgen(tone_c_out,noise_out,
                          tone_c_amp,env_amp,
                          tone_c_m,
                          tone_c_en,noise_c_en);
      sound<=lev_v;
      xxx_sono<=to_integer(lev_v);
      
    END IF;
  END PROCESS SONO;
  
END ARCHITECTURE rtl;
