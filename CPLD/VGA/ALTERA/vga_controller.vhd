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
use     ieee.math_real.all;


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

--SIGNAL txcolumn:INTEGER RANGE 0 TO 40-1 := 0; --TEXT COLUMN
SIGNAL txfontline:INTEGER RANGE 0 TO 10-1 :=0; --WHICH LINE OF TEXT CHAR IS PRINTED
SIGNAL txfontpixel:INTEGER RANGE 0 TO 8-1 :=0; --WHICH PIXEL OF TEXT CHAR IS PRINTED
--SIGNAL charaddr:INTEGER RANGE 0 TO 800- 1:=0; --THE ADDRESS OF THE char
--SIGNAL fontaddr:INTEGER RANGE 0 TO 2560-1 :=0; --THE ADDRESS OF THE FONT ON MEMORY
SIGNAL PXLOUT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL PXLLEFT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL PXLRIGHT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL memaddr:INTEGER RANGE 0 TO 32768-1 :=0;
SIGNAL FONTaddr:INTEGER RANGE 0 TO 8192-1 :=0;
SIGNAL CHARaddr:INTEGER RANGE 0 TO 2048-1 :=0;
SIGNAL COLRaddr:INTEGER RANGE 0 TO 2048-1 :=0;
SIGNAL PXLFORE:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0001";  --DATA FROM VIDEO MEM FORE COLOR
SIGNAL PXLBACK:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM BACK COLOR
SIGNAL PXLTEXT:STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";  --PATTERN OF TEXT CHAR DATA FROM VIDEO MEM
SIGNAL TEXTCHAR:INTEGER RANGE 0 TO 256-1:=0 ; --DATA FROM VIDEO MEM WHICH CHAR
SIGNAL PXLFOREnx:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0010";  --DATA FROM VIDEO MEM FORE COLOR
SIGNAL PXLBACKnx:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM BACK COLOR
SIGNAL PXLTEXTnx:STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";  --PATTERN OF TEXT CHAR DATA FROM VIDEO MEM
SIGNAL SETUPCHAR:INTEGER RANGE 0 TO 3:=0;
SIGNAL RSTRT:STD_LOGIC;  --ROW START FOR MEMORY CHIP SETUP  
SIGNAL   disp_ena  : STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
SIGNAL    column    : INTEGER RANGE 0 TO 640- 1:=0;    --horizontal pixel coordinate
SIGNAL row       : INTEGER RANGE 0 TO 400- 1:=0;    --vertical pixel coordinate	 
SIGNAL vidset:STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";  --video settings bit 1 graph=0/text=1 ,  bit 0 320=0,640=1

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
	 --VARIABLE txrow:INTEGER RANGE 0 TO 20-1 := 0;    --TEXT ROW

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
		ELSE
        COLUMN <=0;		
      END IF;
      IF(v_count < v_pixels) THEN  --vertical display time
        row <= v_count;              --set vertical pixel coordinate
		ELSE
		  ROW<=0;
      END IF;

      --set display enable output
      IF(h_count < h_pixels AND v_count < v_pixels) THEN  --display time
        disp_ena <= '1';                                    --enable display
		  SETUPCHAR<=0;
      ELSE                                                --blanking time
        disp_ena <= '0';                                    --disable display
      END IF;
		
		IF h_count>h_pixels-5 THEN	
	    SETUPCHAR<=SETUPCHAR+1;
		ELSE
	    SETUPCHAR<=0;	
		END IF; 
		

	  IF vidset="00" THEN  --GRAPHICS 320X200X4
		IF (h_count > h_period - 3)  THEN
		  RSTRT <= '1';		  
		ELSE
		  RSTRT <= '0';
		END IF;
		
		
		IF RSTRT='1' THEN
		  MEMADDR <= (ROW/2)*160;
		ELSIF COLUMN MOD 4=2 THEN
		 MEMADDR<= MEMADDR +1;
		ELSE
	    MEMADDR<= MEMADDR;	
		END IF;
		IF (RSTRT='1') OR ( COLUMN MOD 4=3) THEN
	     PXLRIGHT <= datain(3 DOWNTO  0); 
        PXLLEFT  <= datain(7 DOWNTO  4); 
		END IF;
		
		IF (COLUMN MOD 4=0) OR (COLUMN MOD 4=1) THEN
		  PXLOUT <= PXLLEFT; 
		ELSE  
		  PXLOUT <=PXLRIGHT;
		END IF;
		
	  ELSIF vidset="10" THEN  --TEXT 320X200X4 DOUBLE PIXELS
		TXFontline<=TXFontline;
		TEXTCHAR<=TEXTCHAR;
		PXLTEXTnx<=PXLTEXTnx;
		PXLTEXT<=PXLTEXT;
		PXLBACK<=PXLBACK;
		PXLFORE<=PXLFORE;
		MEMADDR<= MEMADDR;	
		CHARaddr<=CHARaddr;
  		--IF (h_count > h_period - 2)  THEN
		--  RSTRT <= '1';		  
		--ELSE
		--  RSTRT <= '0';
		--END IF;
		--IF RSTRT='1'  THEN
		
		
		
		IF h_count>h_pixels+2 THEN
		  IF SETUPCHAR=0 THEN		    
		    CHARaddr <= ROW/20*40; --DOUBLE PIXELS VERT
			 TXFontline<= (ROW/2) MOD 10; -- 0..9
		    MEMADDR<=CHARaddr;				
		  ELSIF SETUPCHAR=1 THEN
		    TEXTCHAR<=to_integer(unsigned(DATAIN));
			 MEMADDR<=1024 + TEXTCHAR;	
		  ELSIF SETUPCHAR=2 THEN		  
		   --PXLFORE<=datain(3 DOWNTO  0); 
		   --PXLBACK<=datain(7 DOWNTO  4); 		  
		   MEMADDR<=4096+TEXTCHAR+(txfontline*256); --FONT AT ADDR 4096 
		  ELSIF SETUPCHAR=3 THEN
		   PXLTEXT<=DATAIN;
			--PXLTEXTnx<=DATAIN;
			--RSTRT<='1';
		  END IF;  
		 END IF; 

		IF COLUMN mod 16=1 THEN
		 CHARaddr<= CHARaddr +1;		
		END IF;
		 
		IF (COLUMN MOD 16)=2  THEN --NEXT CHAR	 
		  MEMADDR<=CHARaddr;	--CHARACTER ADDRESS	 		          		  	
		ELSIF COLUMN MOD 16=4 THEN  --READ CHAR , SETUP CHAR COLOR MEM
		 TEXTCHAR<=to_integer(unsigned(DATAIN));	--GET TEXT CHARACTER 
	    MEMADDR<=1024 + TEXTCHAR;	 --TEXT COLOR ADDRESS
		ELSIF COLUMN MOD 16=6 THEN  --READ COLOR , SETUP PATTERN
		-- PXLFOREnx<=datain(3 DOWNTO  0); 
		-- PXLBACKnx<=datain(7 DOWNTO  4); 		 
		 MEMADDR<=4096+TEXTCHAR+(txfontline*256); --FONT PATTERN ADDRESS
      ELSIF COLUMN MOD 16=8 THEN  --READ PATTERN			
		  PXLTEXTnx<=DATAIN;	  --GET FONT PATTERN 
		END IF;
		
		
		IF  ( COLUMN MOD 16=15) THEN -- SETUP NEXT CHAR PATTERN AND COLORS	     
        PXLTEXT  <= PXLTEXTnx; 
		  PXLFORE  <= PXLFOREnx;
		  PXLBACK  <= PXLBACKnx;		  
		  RSTRT<='0';		
		END IF;
		
	
		IF PXLTEXT(7-((COLUMN MOD 16)/2))='0' THEN -- PRINT PIXEL
	     PXLOUT <= PXLBACK; 
		 ELSE   
		  PXLOUT <= PXLFORE; 
		 END IF;

	  ELSIF vidset="01" THEN  --GRAPHICS 640X400X1

	  ELSE  -- "11" TEXT 640X400X1 
		
	  END IF;	

		
    END IF;
  END PROCESS;

  

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
			  PXLOUT;
			  
			  
			  
			  
END behavior;
