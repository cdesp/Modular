--------------------------------------------------------------------------------
--
--   FileName:         hw_image_generator.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 64-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 05/10/2013 Scott Larson
--     Initial Public Release
--    
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

ENTITY hw_image_generator IS
  PORT(
    disp_ena :  IN   STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
    column   :  IN   INTEGER;    --column pixel coordinate
	 row      :  IN   INTEGER;    --row pixel coordinate  
	 rowstart :  IN   STD_LOGIC;  --new row ('1' = new row for 2 clks, '0' = normal)

  --  red      :  OUT  STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
  --  green    :  OUT  STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
  --  blue     :  OUT  STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '0'); --blue magnitude output to DAC
  	 rgbi	    :  OUT  STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');  --red,green,blue,intensity magnitude output to DAC
	 istext	 :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY TEXT or graphics
	 islowres :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY lowres or hires
	 datain	 :  IN   STD_LOGIC_VECTOR(7 DOWNTO 0);
	 datace   :  OUT  STD_LOGIC := '1'; --'0' enables memory chip --maybe let it always on
	 addrout  :  OUT  STD_LOGIC_VECTOR(14 DOWNTO 0) -- A0 - A14 for addressing the 32k of memory chip
	 
-- use the DP ram to store font													
--	 fntdatin :  IN   STD_LOGIC_VECTOR(7 DOWNTO 0); -- font data in
--	 fntadout :  OUT  STD_LOGIC_VECTOR(11 DOWNTO 0); -- 2560 bytes to store 8x10 font on eprom or copy from rom to upper DP ram?
	 );
END hw_image_generator;


-- Display an image from memory chip
ARCHITECTURE behavior OF hw_image_generator IS

SIGNAL columnh:INTEGER RANGE 0 TO 320-1 :=0;
SIGNAL rowh:INTEGER RANGE 0 TO 200-1 :=0;
SIGNAL txrow:INTEGER RANGE 0 TO 20-1 := 0;    --TEXT ROW
SIGNAL txcolumn:INTEGER RANGE 0 TO 40-1 := 0; --TEXT COLUMN
SIGNAL txfontline:INTEGER RANGE 0 TO 10-1 :=0; --WHICH LINE OF TEXT CHAR IS PRINTED
SIGNAL txfontpixel:INTEGER RANGE 0 TO 8-1 :=0; --WHICH PIXEL OF TEXT CHAR IS PRINTED
SIGNAL charaddr:INTEGER RANGE 0 TO 800- 1:=0; --THE ADDRESS OF THE char
SIGNAL fontaddr:INTEGER RANGE 0 TO 2560-1 :=0; --THE ADDRESS OF THE FONT ON MEMORY
SIGNAL PXLLEFT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL PXLRIGHT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL memaddr:INTEGER RANGE 0 TO 32768-1 :=0;
BEGIN

  PXLRIGHT <= datain(3 DOWNTO  0) WHEN (rowstart='1') OR (COLUMN MOD 4=0 AND COLUMN>2) ELSE PXLRIGHT;
  PXLLEFT  <= datain(7 DOWNTO  4) WHEN (rowstart='1') OR (COLUMN MOD 4=0 AND COLUMN>2) ELSE PXLLEFT;
  memaddr <= (row/2)*160 + (column/4) WHEN COLUMN MOD 4=0 ELSE MEMADDR; --double pixel
  addrout <= std_logic_vector(to_unsigned(memaddr, addrout'length));
  
  RGBI <=  "0000" WHEN DISp_ena = '0' ELSE
			  PXLLEFT WHEN COLUMN MOD 4=0 ELSE
			  PXLLEFT WHEN COLUMN MOD 4=1 ELSE
			  PXLRIGHT WHEN COLUMN MOD 4=2 ELSE
			  PXLRIGHT WHEN COLUMN MOD 4=3 ELSE
			  "0000";	 
	 
END behavior;
