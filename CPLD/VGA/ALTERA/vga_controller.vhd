--------------------------------------------------------------------------------
--
--   FileName:         vga_controller.vhd
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
--   Version 1.1 03/07/2018 Scott Larson
--     Corrected two minor "off-by-one" errors
--    
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


--640x400x70Hz 25,175 MHZ clock
ENTITY vga_controller IS
  GENERIC(
    h_pulse  : INTEGER := 96;    --horiztonal sync pulse width in pixels
    h_bp     : INTEGER := 48;    --horiztonal back porch width in pixels
    h_pixels : INTEGER := 640;   --horiztonal display width in pixels
    h_fp     : INTEGER := 16;    --horiztonal front porch width in pixels
    h_pol    : STD_LOGIC := '0';  --horizontal sync pulse polarity (1 = positive, 0 = negative)
    v_pulse  : INTEGER := 2;      --vertical sync pulse width in rows
    v_bp     : INTEGER := 35;     --vertical back porch width in rows
    v_pixels : INTEGER := 400;   --vertical display width in rows
    v_fp     : INTEGER := 12;      --vertical front porch width in rows
    v_pol    : STD_LOGIC := '1'); --vertical sync pulse polarity (1 = positive, 0 = negative)
  PORT(
    pixel_clk : IN   STD_LOGIC;  --pixel clock at frequency of VGA mode being used
    reset_n   : IN   STD_LOGIC;  --active low asycnchronous reset
    h_sync    : OUT  STD_LOGIC;  --horiztonal sync pulse
    v_sync    : OUT  STD_LOGIC;  --vertical sync pulse
    n_blank   : OUT  STD_LOGIC;  --direct blacking output to DAC
    n_sync    : OUT  STD_LOGIC; --sync-on-green output to DAC
  	 rgbi	    :  OUT  STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');  --red,green,blue,intensity magnitude output to DAC
	 istext	 :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY TEXT or graphics
	 islowres :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY lowres or hires
	 datain	 :  IN   STD_LOGIC_VECTOR(7 DOWNTO 0);
	 datace   :  OUT  STD_LOGIC := '1'; --'0' enables memory chip --maybe let it always on
	 addrout  :  OUT  STD_LOGIC_VECTOR(14 DOWNTO 0) -- A0 - A14 for addressing the 32k of memory chip
	 );
END vga_controller;

ARCHITECTURE behavior OF vga_controller IS
  CONSTANT h_period : INTEGER := h_pulse + h_bp + h_pixels + h_fp; --total number of pixel clocks in a row
  CONSTANT v_period : INTEGER := v_pulse + v_bp + v_pixels + v_fp; --total number of rows in column
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
SIGNAL RSTRT:STD_LOGIC;  --ROW START FOR MEMORY CHIP SETUP  
SIGNAL   disp_ena  : STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
SIGNAL    column    : INTEGER RANGE 0 TO 640- 1:=0;    --horizontal pixel coordinate
SIGNAL row       : INTEGER RANGE 0 TO 400- 1:=0;    --vertical pixel coordinate	 

function is_even(val : integer) return boolean is
    constant vec: signed(31 downto 0) := to_signed(val, 32);
begin
   return vec(0) = '0';
end;



BEGIN



  n_blank <= '1';  --no direct blanking
  n_sync <= '0';   --no sync on green
  
  PROCESS(pixel_clk, reset_n)
    VARIABLE h_count : INTEGER RANGE 0 TO h_period - 1 := 0;  --horizontal counter (counts the columns)
    VARIABLE v_count : INTEGER RANGE 0 TO v_period - 1 := 0;  --vertical counter (counts the rows)
  BEGIN
  
    IF(reset_n = '0') THEN    --reset asserted
      h_count := 0;             --reset horizontal counter
      v_count := 0;             --reset vertical counter
      h_sync <= NOT h_pol;      --deassert horizontal sync
      v_sync <= NOT v_pol;      --deassert vertical sync
      disp_ena <= '0';          --disable display
      column <= 0;              --reset column pixel coordinate
      row <= 0;                 --reset row pixel coordinate
      
    ELSIF(pixel_clk'EVENT AND pixel_clk = '1') THEN

	 
      --counters
      IF(h_count < h_period - 1) THEN    --horizontal counter (pixels)
        h_count := h_count + 1;
      ELSE
        h_count := 0;
        IF(v_count < v_period - 1) THEN  --veritcal counter (rows)
          v_count := v_count + 1;
        ELSE
          v_count := 0;
        END IF;
      END IF;

      --horizontal sync signal
      IF(h_count < h_pixels + h_fp OR h_count >= h_pixels + h_fp + h_pulse) THEN
        h_sync <= NOT h_pol;    --deassert horiztonal sync pulse
      ELSE
        h_sync <= h_pol;        --assert horiztonal sync pulse
      END IF;
      
      --vertical sync signal
      IF(v_count < v_pixels + v_fp OR v_count >= v_pixels + v_fp + v_pulse) THEN
        v_sync <= NOT v_pol;    --deassert vertical sync pulse
      ELSE
        v_sync <= v_pol;        --assert vertical sync pulse
      END IF;
      
      --set pixel coordinates
      IF(h_count < h_pixels) THEN  --horiztonal display time
        column <= h_count;           --set horiztonal pixel coordinate
	     COLumnh <= h_count / 2;
		ELSE
        COLUMN <=0;		
		  COLumnh <= 0;
      END IF;
      IF(v_count < v_pixels) THEN  --vertical display time
        row <= v_count;              --set vertical pixel coordinate
		  rowh <= v_count / 2;
		ELSE
        ROWH <= 0;		
      END IF;

      --set display enable output
      IF(h_count < h_pixels AND v_count < v_pixels) THEN  --display time
        disp_ena <= '1';                                    --enable display
      ELSE                                                --blanking time
        disp_ena <= '0';                                    --disable display
      END IF;

		IF (h_count > h_period - 3)  THEN
		  RSTRT <= '1';		  
		ELSE
		  RSTRT <= '0';
		END IF;
		
		IF RSTRT='1' THEN
		  MEMADDR <= ROWH*160;
		ELSIF COLUMN MOD 4=2 THEN
		 MEMADDR<= MEMADDR +1;
		ELSE
	    MEMADDR<= MEMADDR;	
		END IF;
		
    END IF;
  END PROCESS;

  
  PXLRIGHT <= datain(3 DOWNTO  0) WHEN (RSTRT='1') OR ( IS_even(COLUMNH) AND columnH>0) ELSE PXLRIGHT;
  PXLLEFT  <= datain(7 DOWNTO  4) WHEN (RSTRT='1') OR ( IS_even(COLUMNH) AND columnH>0) ELSE PXLLEFT;
  --MEMADDR <= RowH*160 WHEN (RSTRT='1') ELSE
    --         MEMADDR+1 WHEN IS_even(COLUMNH)
		--		 ELSE MEMADDR;
--  memaddr <= RowH*160 WHEN (RSTRT='1') ELSE
--             RowH*160 + (columnH/2) WHEN IS_even(COLUMNH) AND ROW<100 ELSE  --double pixel on each byte
--             RowH*160 + (columnH/2)+1 WHEN IS_even(COLUMNH) ELSE  --double pixel on each byte
--				 memaddr+1; 
             
  
  addrout <= std_logic_vector(to_unsigned(memaddr, addrout'length));
  
  --RGBI <=  "0000" WHEN DISp_ena = '0' ELSE
	--		  PXLLEFT WHEN COLUMNh REM 2=0 ELSE
	--		  PXLRIGHT;
	
			  --PXLLEFT WHEN COLUMN MOD 4=1 ELSE
			 -- PXLRIGHT WHEN COLUMNh MOD 2=1 ELSE
			 -- PXLRIGHT WHEN COLUMN MOD 4=3 ELSE
			  --"0000";	 
	RGBI <= "0000" WHEN DISp_ena = '0' ELSE
			  PXLLEFT WHEN IS_even(COLUMNH) ELSE
           PXLRIGHT;
			  
			  
			  
			  
END behavior;
