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
--use ieee.math_real.all; 


  


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
    --reset_n   : IN   STD_LOGIC;  --active low asycnchronous reset
    h_sync    : OUT  STD_LOGIC;  --horizontal sync pulse
    v_sync    : OUT  STD_LOGIC;  --vertical sync pulse
   -- n_blank   : OUT  STD_LOGIC;  --direct blacking output to DAC
  --  n_sync    : OUT  STD_LOGIC; --sync-on-green output to DAC
  	 rgbi	    :  OUT  STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');  --red,green,blue,intensity magnitude output to DAC
     scrend	 :  OUT   STD_LOGIC;  -- '0' IF DISPLAY FINISHED
     scrst    :  OUT STD_LOGIC;  -- '0' IF DISPLAY START
	 --istext	 :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY TEXT or graphics
	 --islowres :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY lowres or hires
	 datain	 :  IN   STD_LOGIC_VECTOR(7 DOWNTO 0);
	-- datace   :  OUT  STD_LOGIC := '1'; --'0' enables memory chip --maybe let it always on
	 addrout  :  OUT  STD_LOGIC_VECTOR(15 DOWNTO 0) -- A0 - A15 for addressing the 64k of memory chip
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
SIGNAL PXLBYTE:STD_LOGIC_VECTOR(7 DOWNTO 0) ;  --DATA FROM VIDEO MEM
SIGNAL PXLBYTEnx:STD_LOGIC_VECTOR(7 DOWNTO 0) ;  --DATA FROM VIDEO MEM
SIGNAL memaddr:INTEGER RANGE 0 TO 65536-1 :=0;
SIGNAL FONTaddr:INTEGER RANGE 0 TO 8192-1 :=0;
SIGNAL CHARaddr:INTEGER RANGE 0 TO 4096-1 :=0;
SIGNAL COLRaddr:INTEGER RANGE 0 TO 2048-1 :=0;
SIGNAL PXLFORE:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0001";  --DATA FROM VIDEO MEM FORE COLOR
SIGNAL PXLBACK:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM BACK COLOR
SIGNAL PXLTEXT:STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";  --PATTERN OF TEXT CHAR DATA FROM VIDEO MEM
SIGNAL TEXTCHAR:INTEGER RANGE 0 TO 256-1:=0 ; --DATA FROM VIDEO MEM WHICH CHAR
SIGNAL PXLFOREnx:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0010";  --DATA FROM VIDEO MEM FORE COLOR
SIGNAL PXLBACKnx:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM BACK COLOR
SIGNAL PXLTEXTnx:STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";  --PATTERN OF TEXT CHAR DATA FROM VIDEO MEM
SIGNAL SETUPCHAR:INTEGER RANGE 0 TO 9:=0;
SIGNAL disp_ena  : STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
SIGNAL vidset:STD_LOGIC_VECTOR(1 DOWNTO 0) := "01";  --video settings bit 1 graph=0/text=1 ,  bit 0 320=0,640=1
SIGNAL vidbuf: STD_LOGIC := '0'; --video settings bit 7 is buffer 0 start at 0 or 1 start at 32768
SIGNAL membuf: STD_LOGIC := '0'; -- memaddr high bit to control double buffering
SIGNAL Rpixel_clk  : STD_LOGIC;
SIGNAL RSTRT  : STD_LOGIC;
SIGNAL READREGISTER:INTEGER RANGE 0 TO 9 :=0;  --THIS IS THE VIDEO MODE REGISTER BIT 0 AND 1 ARE USED
SIGNAL READREGISTER2:INTEGER RANGE 0 TO 9 :=0;  -- THIS IS THE DEFAULT FORE AND BACK COLOR FOR MONOCHROME MODES
SIGNAL CONFIGREG:STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
SIGNAL PAT  : STD_LOGIC;
SIGNAL SHOWPAT  : STD_LOGIC :='0';

--SPRITE STUFF
type SprReadState is (SRS_IDLE,SRS_INIT,SRS_Start,SRS_AddrLo,SRS_AddrHi,SRS_XLo,SRS_XHi,SRS_YLo,SRS_YHi,SRS_Wid,SRS_Hei,SRS_DataST,SRS_DataRD);
type Spr_Struct is record
        addr:integer range 0 to 8192-1;  --read from vram
        xpos:integer range 0 to 640;    --read from vram
        ypos:integer range 0 to 400;    --read from vram
        spwd:integer range 0 to 32-1;     --read from vram
        spht:integer range 0 to 32-1;     --read from vram
        intram:integer range 0 to 8192-1; -- internal ram address
     end record; 
constant MAXSPR : integer := 2;
type Spr_Arr is array (0 to MAXSPR) of Spr_Struct;  
signal SPRITES : Spr_Arr; --an array of sprites information should be populated/transfer from external video ram
signal sprno :integer RANGE 0 to MAXSPR; --MAXSPR sprites max
signal sprnod :integer RANGE 0 to MAXSPR; --MAXSPR sprites max
SIGNAL sprstate : SprReadState := SRS_IDLE;
SIGNAL SPRPXLLEFTNXT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --pixel data for sprite
SIGNAL SPRPXLRIGHTNXT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --pixel data for sprite


Signal dout_o: std_logic_vector(7 downto 0);
Signal clk_i : std_logic;
Signal oce_i : std_logic := '1';
Signal ce_i : std_logic := '1';
Signal reset_i : std_logic := '1';
Signal wre_i : std_logic := '1';
Signal ad_i : std_logic_vector(12 downto 0);
Signal din_i : std_logic_vector(7 downto 0);


component Gowin_SPRAM
    port (
        dout: out std_logic_vector(7 downto 0);
        clk: in std_logic;
        oce: in std_logic;
        ce: in std_logic;
        reset: in std_logic;
        wre: in std_logic;
        ad: in std_logic_vector(12 downto 0);
        din: in std_logic_vector(7 downto 0)
    );
end component;

--
function is_even(val : integer) return boolean is
    constant vec: signed(31 downto 0) := to_signed(val, 32);
begin
   return vec(0) = '0';
end;


BEGIN


SPRMEM: Gowin_SPRAM
    port map (
        dout => dout_o,
        clk => clk_i,
        oce => oce_i,
        ce => ce_i,
        reset => reset_i,
        wre => wre_i,
        ad => ad_i,
        din => din_i
    );

 PROCESS(Rpixel_clk,MEMBUF,PAT)
    VARIABLE h_count : INTEGER RANGE 0 TO h_period - 1 := 0;  --horizontal counter (counts the columns)
    VARIABLE v_count : INTEGER RANGE 0 TO v_period - 1 := 0;  --vertical counter (counts the rows)
    VARIABLE column    : INTEGER RANGE 0 TO 640- 1:=0;    --horizontal pixel coordinate
    VARIABLE row       : INTEGER RANGE 0 TO 400- 1:=0;    --vertical pixel coordinate	 
    VARIABLE LETCOL:INTEGER RANGE 0 TO 7 := 0;  --LETTER COLUMN

    
    VARIABLE dattemp : INTEGER RANGE 0 to 255;
    VARIABLE sprbytes : integer RANGE 0 to 1000;
    VARIABLE intram : integer range 0 to 32768; --keep track of the data we write on internal ram
    VARIABLE sprx:integer range 0 to 64-1;
    VARIABLE spry:integer range 0 to 64-1;
    VARIABLE SPRXPRE:integer range 0 to 128-1;
    VARIABLE SPRcol    : INTEGER RANGE 0 TO 640- 1:=0;
    VARIABLE SPRrow    : INTEGER RANGE 0 TO 200- 1:=0;
    VARIABLE spridx:integer range 0 to 32-1;
    VARIABLE SPRSTEVEN:BOOLEAN;
    VARIABLE SPRPXLLEFT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --pixel data for sprite
    VARIABLE SPRPXLRIGHT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --pixel data for sprite

    --VARIABLE sprstateNext : SprReadState;
  
    procedure ReadSpriteData  is
    begin
       case sprstate is 
          when SRS_IDLE => 
                MemAddr <= MemAddr;
          when SRS_INIT =>
                intram := 0;
                sprno <= 0;
                sprstate <=  SRS_START;                
          when SRS_Start => 
                MemAddr <= 32010;                
                sprstate <= SRS_AddrLo;
          when SRS_AddrLo =>  
                dattemp := to_integer(unsigned(datain));
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_AddrHi;
          when SRS_AddrHi =>      
                SPRITES(sprno).addr <= to_integer(unsigned(datain)) * 256 + dattemp;
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_XLo;
          when SRS_XLo =>      
                dattemp := to_integer(unsigned(datain));
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_XHi;
          when SRS_XHi =>      
                SPRITES(sprno).XPos <= to_integer(unsigned(datain)) * 256 + dattemp;
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_YLo;
          when SRS_YLo =>      
                dattemp := to_integer(unsigned(datain));
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_YHi;
          when SRS_YHi =>      
                SPRITES(sprno).YPos <= to_integer(unsigned(datain)) * 256 + dattemp;
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_Wid;
          when SRS_Wid =>      
                SPRITES(sprno).spwd <= to_integer(unsigned(datain));
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_Hei;
          when SRS_Hei =>      
                SPRITES(sprno).spht <= to_integer(unsigned(datain));
                MemAddr <= MemAddr + 1;
                sprstate <= SRS_AddrLo;     --next sprite
                sprno <= sprno +1;
                if sprno >= MAXSPR then -- max 5 sprites for start                   
                  sprstate <= SRS_DataST; --read sprite graphics
                  sprno <= 0;                  
                end if;
           when SRS_DataST => --read sprite images
             if sprno >= MAXSPR then --no more data
                  sprstate <= SRS_IDLE;
             elsif SPRITES(sprno).addr /= 0 then                
                memaddr <= SPRITES(sprno).addr;
                sprbytes := SPRITES(sprno).spwd * SPRITES(sprno).spht;
                if sprbytes = 0 then    --sanity check
                 sprbytes := 1;
                end if;
                SPRITES(sprno).intram <= intram;
                sprstate <=SRS_DataRD;
                ad_i <=  std_logic_vector(to_unsigned(intram, ad_i'length));
                wre_i <= '1';   --WHIEN HIGH WRITE IS ENABLED
                ce_i <= '1';    --ACTIVE HIGH 
             else                 
                sprno <= sprno + 1;
             end if;
            when SRS_DataRD => 
                wre_i <= '1';   --WHIEN HIGH WRITE IS ENABLED
                ce_i <= '1';    --ACTIVE HIGH 
                din_i <= datain;
                memaddr <= memaddr + 1;
                ad_i <=  std_logic_vector(to_unsigned(intram, ad_i'length));
                intram:=intram+1;                
                if sprbytes = 0 then --no more bytes to read
                  sprstate <= SRS_DataST;
                  sprno <= sprno + 1;
                ELSE
                  sprbytes := sprbytes -1;
                end if;
             WHEN others =>
              sprstate <= SRS_Idle;
        end case;
    end procedure;



  BEGIN
    
    clk_i <= Rpixel_clk;
  

   IF(Rpixel_clk'EVENT AND Rpixel_clk = '1') THEN

	  
      
      PXLBACK<=PXLBACK;
	  PXLFORE<=PXLFORE;

      wre_i<='0';
      reset_i <='0';
      ce_i <= '0';  
      oce_i <= '0';  

	 
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
        column := h_count;           --set horiztonal pixel coordinate	     
		ELSE
          COLUMN :=0;		
      END IF;
      IF(v_count < v_pixels) THEN  --vertical display time
        row := v_count;              --set vertical pixel coordinate
		ELSE
		  ROW:=0;
      END IF;

      --set display enable output
      IF(h_count < h_pixels AND v_count < v_pixels) THEN  --display time
        disp_ena <= '1';                                    --enable display
      ELSE                                                --blanking time
        disp_ena <= '0';                                    --disable display
      END IF;
		
		--set display finished
		IF (v_count = v_pixels and h_count<10)  THEN
		  SCREND<='1';
		ELSE
		  SCREND<='0';
		END IF;  

      

	  IF h_count>h_pixels-7 THEN	--8
	    SETUPCHAR<=SETUPCHAR+1;
	  ELSE
	    SETUPCHAR<=0;	
	  END IF; 

      if sprstate= SRS_DataST or sprstate =  SRS_DataRD then
        membuf<='1';  --ALWAYS HIGH FOR sprite data = buffer1
      else 
        membuf<='0';  --ALWAYS LOW FOR REGISTERS
      end if;

      if v_count=v_pixels then
          SHOWPAT <='0';
      elsif v_count = v_pixels+1 and h_count=0  then 
         sprstate <= SRS_INIT;         
      end if;
      if sprstate /= SRS_IDLE then
           ReadSpriteData;       
      end if;   
     
      if v_count=v_period-6 and sprstate=SRS_wid then --this should not be required
         --sprstate <= SRS_IDLE;         
      end if;       



	  IF v_count=v_period-3 and h_count>h_pixels-10 and h_count<h_pixels-4 THEN	
	    READREGISTER<=READREGISTER+1;        
	  ELSE
	    READREGISTER<=0;        
	  END IF; 

	  IF v_count=v_period-4 and h_count>h_pixels-10 and h_count<h_pixels-4 THEN	
	    READREGISTER2<=READREGISTER2+1;
	  ELSE
	    READREGISTER2<=0;	
	  END IF; 

      IF v_count=v_period-1 THEN 
        ROW:=0;
        COLUMN:=0;
        MEMADDR<=0;
      END if;

      VIDSET<=VIDSET;
      VIDBUF<=VIDBUF;
      IF READREGISTER=1 THEN
        MEMADDR<=32760;        
      ELSIF READREGISTER=2 THEN
        CONFIGREG<=DATAIN;
      ELSIF READREGISTER=3 THEN
        VIDSET<=CONFIGREG(1 DOWNTO  0);
        VIDBUF<=CONFIGREG(7);
      END IF;
	
      IF READREGISTER2=1 THEN
        MEMADDR<=32761;
      ELSIF READREGISTER2=2 THEN
        CONFIGREG<=DATAIN;
      ELSIF READREGISTER2=3 THEN
        PXLFORE<=CONFIGREG(3 DOWNTO  0);
        PXLBACK<=CONFIGREG(7 DOWNTO  4);
      END IF;


     -- ******** SCREEN DISPLAY **********
      --VIDSET   0/1                   0/1
      --VIDSET LOW/HIGH RESOLUTION  GRAPHICS/TEXT
     IF READREGISTER=0 AND  READREGISTER2=0 AND sprstate = SRS_IDLE  THEN 
      membuf<=VIDBUF;
	  IF vidset="00" THEN  --GRAPHICS 320X200X4
        MEMADDR<= MEMADDR;	

        --SETUP 1ST BYTE
        IF h_count = h_period - 4 THEN
		  MEMADDR <= (ROW/2)*160;
		ELSIF h_count = h_period - 3 THEN
	     PXLRIGHT <= datain(3 DOWNTO  0); 
         PXLLEFT  <= datain(7 DOWNTO  4); 
		END IF;
		        
        IF COLUMN MOD 4=2 THEN
		 MEMADDR<= MEMADDR +1;
        ELSIF ( COLUMN MOD 4=3) THEN
	     PXLRIGHT <= datain(3 DOWNTO  0); 
         PXLLEFT  <= datain(7 DOWNTO  4); 
		END IF;
		
		IF (COLUMN MOD 4=0) OR (COLUMN MOD 4=1) THEN
		  PXLOUT <= PXLLEFT; 
		ELSE  
		  PXLOUT <=PXLRIGHT;
		END IF;

		
	  ELSIF vidset="01" THEN  --TEXT 320X200X4 DOUBLE PIXELS
	    TXFontline<=TXFontline;
		TEXTCHAR<=TEXTCHAR;
		PXLTEXTnx<=PXLTEXTnx;
        PXLBACKnx<=PXLBACKnx;
		PXLFOREnx<=PXLFOREnx;
		PXLTEXT<=PXLTEXT;
	
 
		MEMADDR<= MEMADDR;	
		CHARaddr<=CHARaddr;
		
		
		
		IF h_count>h_pixels+2 THEN
		  IF SETUPCHAR=1 THEN		    
		    CHARaddr <= ROW/20*40; --DOUBLE PIXELS VERT
			 TXFontline<= (ROW/2) MOD 10; -- 0..9
		    MEMADDR<=CHARaddr;				
		  ELSIF SETUPCHAR=2 THEN
		    TEXTCHAR<=to_integer(unsigned(DATAIN));
			 MEMADDR<=1024 + CHARaddr;	--setup char colors 
		  ELSIF SETUPCHAR=3 THEN		  
		   PXLFOREnx<=datain(3 DOWNTO  0); 
		   PXLBACKnx<=datain(7 DOWNTO  4); 		  
		   MEMADDR<=4096+TEXTCHAR+(txfontline*256); --FONT AT ADDR 4096 on buffer 0 ONLY
           membuf<='0';
		  ELSIF SETUPCHAR=4 THEN
		   PXLTEXTnx<=DATAIN;
           membuf<='0';
          ELSIF SETUPCHAR=5 THEN
            PXLFORE<=PXLFOREnx;
            PXLBACK<=PXLBACKnx;
            PXLTEXT<=PXLTEXTnx;
		  END IF;  
		 END IF; 

		IF COLUMN mod 16=1 THEN
		 CHARaddr<= CHARaddr +1;		
		END IF;
		 
		IF (COLUMN MOD 16)=2  THEN --NEXT CHAR	 
		 MEMADDR<=CHARaddr;	--CHARACTER ADDRESS	 		          		  	
		ELSIF COLUMN MOD 16=4 THEN  --READ CHAR , SETUP CHAR COLOR MEM
		 TEXTCHAR<=to_integer(unsigned(DATAIN));	--GET TEXT CHARACTER 	     
         MEMADDR<=1024 + CHARaddr;	 --TEXT COLOR ADDRESS
		ELSIF COLUMN MOD 16=7 THEN  --READ COLOR , SETUP PATTERN
		 PXLFOREnx<=datain(3 DOWNTO  0); 
		 PXLBACKnx<=datain(7 DOWNTO  4); 		 
		 MEMADDR<=4096+TEXTCHAR+(txfontline*256); --FONT PATTERN ADDRESS on buffer 0 ONLY
         membuf<='0';
        ELSIF COLUMN MOD 16=8 THEN  --READ PATTERN			
		 PXLTEXTnx<=DATAIN;	  --GET FONT PATTERN 
         membuf<='0';
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
       

	  ELSIF vidset="10" THEN  --GRAPHICS 640X400X1
        PXLBYTEnx <= PXLBYTEnx;
        MEMADDR<= MEMADDR;	

        --PREPARE FIRST BYTE
        IF (h_count = h_period - 4)  THEN
		  MEMADDR <= ROW*80; --640/8 PIXELS PER BYTE
		ELSIF (h_count = h_period - 3) THEN
		  PXLBYTEnx <= datain; 
          MEMADDR<= MEMADDR +1; 
        ELSIF (h_count = h_period - 2) THEN
          PXLBYTE<=PXLBYTEnx;
		END IF;
		
        IF ( COLUMN MOD 8=6) THEN
	      PXLBYTEnx <= datain;
		  MEMADDR<= MEMADDR +1; 
        ELSIF COLUMN MOD 8=7 THEN
          PXLBYTE<=PXLBYTEnx;
        END IF;

        IF PXLBYTE(7-COLUMN MOD 8)='0' THEN
		  PXLOUT <= PXLBACK; 
        ELSE
          PXLOUT <= PXLFORE;
        END IF;

        
	  ELSE  -- "11" TEXT 640X400X1 
        --8 PIXELS PER BYTE
	    TXFontline<=TXFontline;
		TEXTCHAR<=TEXTCHAR;
		PXLTEXT<=PXLTEXT;
        PXLTEXTnx<=PXLTEXTnx;
 
		MEMADDR<= MEMADDR;	
		CHARaddr<=CHARaddr;
	    LETCOL := COLUMN MOD 8;
		
		
		IF h_count>h_pixels+2 THEN --AFTER VISIBLE PIXELS
          CASE SETUPCHAR IS            
            WHEN 1 => 	CHARaddr <= (ROW/10)*80; 
			            TXFontline<= ROW MOD 10; -- 0..9
            WHEN 2 => MEMADDR<=CHARaddr;				
            WHEN 3 => TEXTCHAR<=to_integer(unsigned(DATAIN));
		              MEMADDR<=4096+TEXTCHAR+(txfontline*256); --FONT AT ADDR 4096 on buffer 0 ONLY
                      membuf<='0';
            WHEN 4 => PXLTEXTnx<=DATAIN;membuf<='0';
            WHEN 5 => PXLTEXT<=PXLTEXTnx;
                      CHARaddr<= CHARaddr +1;
            WHEN OTHERS => CHARaddr<=CHARaddr;
          END CASE;
	    ELSE 

          case LETCOL is
            when 0 TO 2 => MEMADDR<=CHARaddr;	--CHARACTER ADDRESS	 			
            when 3 TO 4 => TEXTCHAR<=to_integer(unsigned(DATAIN));	--GET TEXT CHARACTER 	     
                      MEMADDR<=4096+TEXTCHAR+(txfontline*256); --FONT PATTERN ADDRESS on Buffer 0 ONLY	
                      membuf<='0';
            when 5 TO 6 => PXLTEXTnx<=DATAIN;membuf<='0';	  --GET FONT PATTERN 		
            when 7 => PXLTEXT  <= PXLTEXTnx; 
                      CHARaddr<= CHARaddr +1;
          end case;


		END IF; 


		IF PXLTEXT(7-LETCOL)='1' THEN -- PRINT PIXEL
	      PXLOUT <= PXLFORE; 
		 ELSE   
          PXLOUT <= PXLBACK; 
 
		 END IF;
		
	  END IF;	
     END IF;
	

      -- ************* SPRITE DISPLAY  ************
     if disp_ena='1'  AND  vidset(1)='0'  then --320x200 4bit/px ONLY
      SPRcol := COLUMN /2;
      SPRrow := row /2;
      wre_i <= '0';     --READ ONLY
      ce_i <= '1';    --ACTIVE  
      oce_i <='0';   -- OUTPUT REGISTERS INACTIVE
      FOR SPRNO IN 0 TO 0 LOOP
            spridx:=SPRNO; 
       if SPRITES(spridx).addr /=0 then    
         if SPRrow >= SPRITES(spridx).ypos and SPRrow < SPRITES(spridx).ypos+(SPRITES(spridx).spht) then -- lines is ok                    
             spry:= (SPRrow - SPRITES(spridx).ypos);

             If SPRCOL = (SPRITES(spridx).xpos-2) THEN
              ad_i<= Std_logic_vector(to_unsigned(SPRITES(spridx).intram  + (spry * SPRITES(spridx).spwd/2), ad_i'length  ));
              SPRXPRE:=0; SPRX:=0;
              SPRSTEVEN := (SPRCOL+2) MOD 2=0; -- IF WE START AT EVEN COLUMN 
            END IF;
             If SPRCOL = (SPRITES(spridx).xpos-1) THEN
              SPRPXLLEFT:=dout_o(7 downto 4);
              SPRPXLRIGHT:=dout_o(3 downto 0);                
             END IF;


          if SPRcol >= SPRITES(spridx).xpos and SPRcol < SPRITES(spridx).xpos+SPRITES(spridx).spwd then--columns is ok
            --find the pixel to be displayed            
            
           
            
            SPRX:= (SPRCOL - SPRITES(spridx).Xpos)/2; --NEXT            
            IF SPRX/=SPRXPRE  THEN              
              SPRXPRE:=SPRX;
              SPRPXLLEFT:=SPRPXLLEFTNXT;
              SPRPXLRIGHT:=SPRPXLRIGHTNXT;
            ELSE
              ad_i<= Std_logic_vector(to_unsigned(SPRITES(spridx).intram+1   + sprx + (spry * SPRITES(spridx).spwd/2), ad_i'length  ));                                   
              SPRPXLLEFTNXT<=dout_o(7 downto 4);
              SPRPXLRIGHTNXT<=dout_o(3 downto 0);
            END IF;     
            
            

            --print the sprx,spry pixel from internal ram                        
            
            
            IF (SPRCOL MOD 2 =1 AND SPRSTEVEN) OR (SPRCOL MOD 2 =0 AND NOT SPRSTEVEN) THEN
              IF SPRPXLRIGHT/="0000" THEN
                PXLOUT <= SPRPXLRIGHT;               
              END IF;  
            else
              IF SPRPXLLEFT/="0000" THEN
                PXLOUT <= SPRPXLLEFT;
              END IF;
            end if;            
 
          end if; --columns
         end if; --rows       
       end if;  --sprites
      END LOOP;

      --if SPRITES(sprno).addr=1 then
       -- PXLOUT <= "0011";
      --end if;
     end if;

    END IF;

    
    


    
    PAT<='0';
    IF MEMBUF='0' THEN
     IF   COLUMN >= 600 AND COLUMN<=606 AND ROW>=10 AND ROW<=17   THEN
       PAT<='1';
     END IF;
    ELSE
     IF   ( COLUMN = 605 OR COLUMN=606) AND ROW>=10 AND ROW<=17   THEN
       PAT <= '1';       
     END IF;
    END IF;          
    IF PAT='1' AND SHOWPAT='1' THEN  
      PXLOUT <= "1111"; 
    END IF;

   -- IF ROW<100 THEN
    --   PXLOUT<="1100";
    --ELSE
     --  PXLOUT<="0100";
    --END IF;

    scrst <= '0';
  END PROCESS;

  Rpixel_clk<=pixel_clk;
  addrout <= MEMBUF & std_logic_vector(to_unsigned(memaddr, addrout'length -1 )) ;
  RGBI <= "0000" WHEN DISp_ena = '0' ELSE
            PXLOUT;
			  
END behavior;
