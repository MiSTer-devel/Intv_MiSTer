//============================================================================
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
//============================================================================

module emu
(
    //Master input clock
    input         CLK_50M,

    //Async reset from top-level module.
    //Can be used as initial reset.
    input         RESET,

    //Must be passed to hps_io module
    inout  [45:0] HPS_BUS,

    //Base video clock. Usually equals to CLK_SYS.
    output        CLK_VIDEO,

    //Multiple resolutions are supported using different CE_PIXEL rates.
    //Must be based on CLK_VIDEO
    output        CE_PIXEL,

    //Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
    //if VIDEO_ARX[12] or VIDEO_ARY[12] is set then [11:0] contains scaled size instead of aspect ratio.
    output [12:0] VIDEO_ARX,
    output [12:0] VIDEO_ARY,

    output  [7:0] VGA_R,
    output  [7:0] VGA_G,
    output  [7:0] VGA_B,
    output        VGA_HS,
    output        VGA_VS,
    output        VGA_DE,    // = ~(VBlank | HBlank)
    output        VGA_F1,
    output [1:0]  VGA_SL,
    output        VGA_SCALER, // Force VGA scaler

    input  [11:0] HDMI_WIDTH,
    input  [11:0] HDMI_HEIGHT,
    output        HDMI_FREEZE,

`ifdef MISTER_FB
    // Use framebuffer in DDRAM (USE_FB=1 in qsf)
    // FB_FORMAT:
    //    [2:0] : 011=8bpp(palette) 100=16bpp 101=24bpp 110=32bpp
    //    [3]   : 0=16bits 565 1=16bits 1555
    //    [4]   : 0=RGB  1=BGR (for 16/24/32 modes)
    //
    // FB_STRIDE either 0 (rounded to 256 bytes) or multiple of pixel size (in bytes)
    output        FB_EN,
    output  [4:0] FB_FORMAT,
    output [11:0] FB_WIDTH,
    output [11:0] FB_HEIGHT,
    output [31:0] FB_BASE,
    output [13:0] FB_STRIDE,
    input         FB_VBL,
    input         FB_LL,
    output        FB_FORCE_BLANK,

`ifdef MISTER_FB_PALETTE
    // Palette control for 8bit modes.
    // Ignored for other video modes.
    output        FB_PAL_CLK,
    output  [7:0] FB_PAL_ADDR,
    output [23:0] FB_PAL_DOUT,
    input  [23:0] FB_PAL_DIN,
    output        FB_PAL_WR,
`endif
`endif

    output        LED_USER,  // 1 - ON, 0 - OFF.

    // b[1]: 0 - LED status is system status OR'd with b[0]
    //       1 - LED status is controled solely by b[0]
    // hint: supply 2'b00 to let the system control the LED.
    output  [1:0] LED_POWER,
    output  [1:0] LED_DISK,

    // I/O board button press simulation (active high)
    // b[1]: user button
    // b[0]: osd button
    output  [1:0] BUTTONS,

    input         CLK_AUDIO, // 24.576 MHz
    output [15:0] AUDIO_L,
    output [15:0] AUDIO_R,
    output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
    output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

    //ADC
    inout   [3:0] ADC_BUS,

    //SD-SPI
    output        SD_SCK,
    output        SD_MOSI,
    input         SD_MISO,
    output        SD_CS,
    input         SD_CD,

    //High latency DDR3 RAM interface
    //Use for non-critical time purposes
    output        DDRAM_CLK,
    input         DDRAM_BUSY,
    output  [7:0] DDRAM_BURSTCNT,
    output [28:0] DDRAM_ADDR,
    input  [63:0] DDRAM_DOUT,
    input         DDRAM_DOUT_READY,
    output        DDRAM_RD,
    output [63:0] DDRAM_DIN,
    output  [7:0] DDRAM_BE,
    output        DDRAM_WE,

    //SDRAM interface with lower latency
    output        SDRAM_CLK,
    output        SDRAM_CKE,
    output [12:0] SDRAM_A,
    output  [1:0] SDRAM_BA,
    inout  [15:0] SDRAM_DQ,
    output        SDRAM_DQML,
    output        SDRAM_DQMH,
    output        SDRAM_nCS,
    output        SDRAM_nCAS,
    output        SDRAM_nRAS,
    output        SDRAM_nWE,

`ifdef MISTER_DUAL_SDRAM
    //Secondary SDRAM
    //Set all output SDRAM_* signals to Z ASAP if SDRAM2_EN is 0
    input         SDRAM2_EN,
    output        SDRAM2_CLK,
    output [12:0] SDRAM2_A,
    output  [1:0] SDRAM2_BA,
    inout  [15:0] SDRAM2_DQ,
    output        SDRAM2_nCS,
    output        SDRAM2_nCAS,
    output        SDRAM2_nRAS,
    output        SDRAM2_nWE,
`endif

    input         UART_CTS,
    output        UART_RTS,
    input         UART_RXD,
    output        UART_TXD,
    output        UART_DTR,
    input         UART_DSR,

    // Open-drain User port.
    // 0 - D+/RX
    // 1 - D-/TX
    // 2..6 - USR2..USR6
    // Set USER_OUT to 1 to read from USER_IN.
    input   [6:0] USER_IN,
    output  [6:0] USER_OUT,

    input         OSD_STATUS
);

//////////////////////////////////////////////////////////////////

assign ADC_BUS  = 'Z;
assign USER_OUT = '1;
assign {UART_RTS, UART_TXD, UART_DTR} = 0;
assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = '0;  

assign VGA_F1 = 0;

assign AUDIO_S = 1;
assign AUDIO_MIX = 0;

assign LED_USER = 0;
assign LED_DISK = 0;
assign LED_POWER = 0;
assign HDMI_FREEZE = 0;

assign VGA_SCALER= 0;

assign BUTTONS = 0;

//////////////////////////////////////////////////////////////////

wire [1:0] ar = status[4:3];

wire [12:0] arx = (!ar) ? 12'd760 : (ar - 1'd1);
wire [12:0] ary = (!ar) ? 12'd561 : 12'd0;

`include "build_id.v"

// Status Bit Map:
//              Upper                          Lower
// 0         1         2         3          4         5         6
// 01234567890123456789012345678901 23456789012345678901234567890123
// 0123456789ABCDEFGHIJKLMNOPQRSTUV 0123456789ABCDEFGHIJKLMNOPQRSTUV
// XX XXXXXXXXXXXXXXXXXXXXX

localparam CONF_STR = {
    "Intellivision;;",
    "-;",
    "FS,ROMINTBIN;",
    "O58,MAP,Auto,0,1,2,3,4,5,6,7,8,9;",
    "OMN,Format,Auto,Raw,Intellicart;",
    "O9,ECS,Off,On;",
    "OA,Voice,On,Off;",
    "O34,Aspect ratio,Original,Full Screen,[ARC1],[ARC2];",
    "OCE,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
    "d0OH,Vertical Crop,Disabled,216p(5x);",
    "d0OIL,Crop Offset,0,2,4,8,10,12,-12,-10,-8,-6,-4,-2;",
    "OFG,Scale,Normal,V-Integer,Narrower HV-Integer,Wider HV-Integer;",
    "OB,Video standard,NTSC,PAL;",
    "O1,Swap Joystick,Off,On;",
    "-;",
    "R0,Reset;",
    "J1,Action Up,Action Left,Action Right,Clear,Enter,0,1,2,3,4,5,6,7,8,9;",
    "V,v",`BUILD_DATE
};

wire forced_scandoubler;
wire  [1:0] buttons;
wire [63:0] status;
wire [10:0] ps2_key;

wire        ioctl_download;
wire [7:0]  ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire [7:0]  ioctl_dout;
wire        ioctl_wait;
wire [31:0] joystick_0,joystick_1;
wire [15:0] joystick_analog_0,joystick_analog_1;
wire [21:0] gamma_bus;
wire clk_sys,pll_locked;

hps_io #(.CONF_STR(CONF_STR)) hps_io
(
    .clk_sys(clk_sys),
    .HPS_BUS(HPS_BUS),
    .joystick_0(joystick_0),
    .joystick_1(joystick_1),
    .joystick_analog_0(joystick_analog_0),
    .joystick_analog_1(joystick_analog_1),
    .forced_scandoubler(forced_scandoubler),
    .gamma_bus(gamma_bus),
    .buttons(buttons),
    .status(status),
    .status_menumask(en216p),
    .ioctl_download(ioctl_download),
    .ioctl_index(ioctl_index),
    .ioctl_wr(ioctl_wr),
    .ioctl_addr(ioctl_addr),
    .ioctl_dout(ioctl_dout),
    .ioctl_wait(ioctl_wait),
    .ps2_key(ps2_key)
);

wire pal     = status[11];
wire swap    = status[1];
wire ecs     = status[9];
wire ivoice  =!status[10];
wire [3:0] mapp    = status[8:5];
wire [1:0] format  = status[23:22];

wire [7:0] CORE_R,CORE_G,CORE_B;
wire       CORE_HS,CORE_VS,CORE_DE,CORE_CE;
wire       CORE_HBLANK,CORE_VBLANK;
   
intv_core intv_core
(
    .clksys(clk_sys),
    .pll_locked(pll_locked),
    .pal(pal),
    .swap(swap),
    .ecs(ecs),
    .ivoice(ivoice),
    .mapp(mapp),
    .format(format),
    .reset(RESET | status[0]),
    .vga_clk(CLK_VIDEO),
    .vga_ce(CORE_CE),
    .vga_r(CORE_R),
    .vga_g(CORE_G),
    .vga_b(CORE_B),
    .vga_hs(CORE_HS),
    .vga_vs(CORE_VS),
    .vga_de(CORE_DE),
    .vga_vb(CORE_VBLANK),
    .vga_hb(CORE_HBLANK),
    .joystick_0(joystick_0),
    .joystick_1(joystick_1),
    .joystick_analog_0(joystick_analog_0),
    .joystick_analog_1(joystick_analog_1),
    .ioctl_download(ioctl_download),
    .ioctl_index(ioctl_index),
    .ioctl_wr(ioctl_wr),
    .ioctl_addr(ioctl_addr),
    .ioctl_dout(ioctl_dout),
    .ioctl_wait(ioctl_wait),
    .ps2_key(ps2_key),
    .audio_l(AUDIO_L),
    .audio_r(AUDIO_R)
);

wire [2:0] scale = status[14:12];
wire [2:0] sl = scale ? scale - 1'd1 : 3'd0;

assign VGA_SL = sl[1:0];

wire       vcrop_en = status[17];
wire [3:0] vcopt    = status[21:18];
reg        en216p;
reg  [4:0] voff;
always @(posedge CLK_VIDEO) begin
	en216p <= ((HDMI_WIDTH == 1920) && (HDMI_HEIGHT == 1080) && !forced_scandoubler && !scale);
	voff <= (vcopt < 6) ? {vcopt,1'b0} : ({vcopt,1'b0} - 5'd24);
end

wire vga_de;
video_freak video_freak
(
    .*,
    .VGA_DE_IN(vga_de),
    .ARX((!ar) ? arx : (ar - 1'd1)),
    .ARY((!ar) ? ary : 12'd0),
    .CROP_SIZE((en216p & vcrop_en) ? 10'd216 : 10'd0),
    .CROP_OFF(voff),
    .SCALE(status[16:15])
);

video_mixer #(.LINE_LENGTH(520), .GAMMA(1)) video_mixer
(
    .scandoubler(scale || forced_scandoubler),
    .hq2x(scale==1),
    .gamma_bus(gamma_bus),

    .CLK_VIDEO(CLK_VIDEO),
    .ce_pix(CORE_CE),
    .R(CORE_R),
    .G(CORE_G),
    .B(CORE_B),
    .HSync(CORE_HS),
    .VSync(CORE_VS),
    .HBlank(CORE_HBLANK),
    .VBlank(CORE_VBLANK),

    .CE_PIXEL(CE_PIXEL),
    .VGA_R(VGA_R),
    .VGA_G(VGA_G),
    .VGA_B(VGA_B),
    .VGA_VS(VGA_VS),
    .VGA_HS(VGA_HS),
    .VGA_DE(vga_de)
);

pll pll
(
    .refclk(CLK_50M),
    .reconfig_to_pll(reconfig_to_pll),
    .reconfig_from_pll(reconfig_from_pll),
    .locked(pll_locked),
    .outclk_0(clk_sys)
);

wire [63:0] reconfig_to_pll;
wire [63:0] reconfig_from_pll;
wire        cfg_waitrequest;
reg         cfg_write;
reg   [5:0] cfg_address;
reg   [31:0] cfg_data;

pll_cfg pll_cfg
(
    .mgmt_clk(CLK_50M),
    .mgmt_reset(0),
    .mgmt_waitrequest(cfg_waitrequest),
    .mgmt_read(0),
    .mgmt_readdata(),
    .mgmt_write(cfg_write),
    .mgmt_address(cfg_address),
    .mgmt_writedata(cfg_data),
    .reconfig_to_pll(reconfig_to_pll),
    .reconfig_from_pll(reconfig_from_pll)
);


// NTSC : 3.579545MHz *  12 =  42.95454MHz
// PAL  : 4MHz        *  12 =  48MHz
  
// STIC : CLK * 12
// IVOICE : CLK
  
reg tv_reset = 0;
always @(posedge CLK_50M) begin
    reg pald = 0, pald2 = 0;
    reg [2:0] state = 0;

    pald  <= pal;
    pald2 <= pald;

    cfg_write <= 0;
    if(pald2 != pald) state <= 1;

    if(!cfg_waitrequest) begin
        if(state) state<=state+1'd1;
        case(state)
               0: tv_reset <= 0;
               1: begin
                         tv_reset <= 1;
                         cfg_address <= 0; // Waitrequest mode
                         cfg_data <= 0;
                         cfg_write <= 1;
                    end
               2: begin
                         cfg_address <= 3; // N counter
                         cfg_data <= 32'h00010000;
                         cfg_write <= 1;
                    end
               3: begin
                         cfg_address <= 4; // M counter
                         cfg_data <= 32'h00000404;
                         cfg_write <= 1;
                    end
               4: begin
                         cfg_address <= 5; // C0 counter
                         cfg_data <= pald2 ? 32'h00020504 : 32'h00000505;
                         cfg_write <= 1;
                    end
               5: begin
                         cfg_address <= 7; // M frac
                         cfg_data <= pald2 ? 32'hA3D709E8 : 32'h9745BF27;
                         cfg_write <= 1;
                    end
               6: begin
                         cfg_address <= 2; // Start reconf
                         cfg_data <= 0;
                         cfg_write <= 1;
                    end
          endcase
     end
end

endmodule
