----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:13:26 04/30/2021 
-- Design Name: 
-- Module Name:    MMU_Z80 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity MMU_Z80 is
end MMU_Z80;

architecture Behavioral of MMU_Z80 is

begin



module decoder(
    input 	[7:0] D,		// Data bus from CPU (Z80???)
    input 	[2:0] AL,		// Address bus low bits
    input 	[15:14] AH,		// Address bus high bits
    input 	nRESET,			// System reset
    input 	nMREQ,			// Memory request
    input 	nCFGCS,			// Config chip select
    input 	nWR,			// Write strobe
    
	output 	[21:14] MA,		// Mapped address lines
    output	nROMCS,			// ROM chip select
    output 	nRAMCS,			// RAM0 chip select
    output 	nRAMCS1,		// RAM1 chip select
    output 	nRAMCS2			// RAM2 chip select

    );


reg [7:0] MALat [3:0];		// array of latches 8 bits wide x 2 bits deep.
reg nPAGEEN;				// Single bit page enable latch.

// First decodeer to generate nPAGEWR and nPAGEENWR
assign	nPAGEWR 	= !(!nWR & !nCFGCS & !AL[2]);
assign	nPAGEENWR 	= !(!nWR & !nCFGCS & AL[2]);

// Now decode nROMCS and nRAMCSx
assign 	nROMCS		= !(!nMREQ & !MA[20] & !MA[19]);
assign 	nRAMCS		= !(!nMREQ & !MA[20] &  MA[19]);
assign 	nRAMCS1		= !(!nMREQ &  MA[20] & !MA[19]);
assign 	nRAMCS2		= !(!nMREQ &  MA[20] &  MA[19]);

// Setup to address the array
wire [1:0] LatA;			
assign LatA[1:0] = AL[1:0];

// Latch MA lines when nPAGEWR is asserted low.
always @(negedge nPAGEWR)
begin
  MALat[LatA[1:0]] <= D[7:0];
end

// PageEn write logic, if Reset is asserted then reset PAGEEN
// else if nPAGEENWR is asserted, then PAGEEN is latched from D0.
always @(negedge nPAGEENWR or negedge nRESET)
begin
  if (!nRESET)
    nPAGEEN <= 1'b1;
  else
    nPAGEEN <= !D[0];
end

// MA outputs
wire [1:0] LatO;
assign LatO[1:0] = AH[15:14];

// If nPAGEEN is low then assign the appropreate latch to the MA outputs
// else tristate if nPAGEEN is high
assign MA[21:14] = nPAGEEN ? 8'bz : MALat[LatO[1:0]];

endmodule


end Behavioral;