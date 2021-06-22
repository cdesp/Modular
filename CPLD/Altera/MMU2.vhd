-- Copyright (C) 1991-2013 Altera Corporation
-- Your use of Altera Corporation's design tools, logic functions 
-- and other software and tools, and its AMPP partner logic 
-- functions, and any output files from any of the foregoing 
-- (including device programming or simulation files), and any 
-- associated documentation or information are expressly subject 
-- to the terms and conditions of the Altera Program License 
-- Subscription Agreement, Altera MegaCore Function License 
-- Agreement, or other applicable license agreement, including, 
-- without limitation, that your use is for the sole purpose of 
-- programming logic devices manufactured by Altera and sold by 
-- Altera or its authorized distributors.  Please refer to the 
-- applicable agreement for further details.

-- PROGRAM		"Quartus II 64-Bit"
-- VERSION		"Version 13.0.1 Build 232 06/12/2013 Service Pack 1 SJ Web Edition"
-- CREATED		"Sat May 01 15:07:40 2021"

LIBRARY ieee;
USE ieee.std_logic_1164.all; 


--LIBRARY work;

ENTITY regn IS
	GENERIC ( N : INTEGER := 8 ) ;
	PORT ( D : IN STD_LOGIC_VECTOR(N-1 DOWNTO 0) ;
			 Resetn, Clock : IN STD_LOGIC ;
			 Q : OUT STD_LOGIC_VECTOR(N-1 DOWNTO 0) ) ;
END regn ;

ARCHITECTURE behavioral OF regn IS
BEGIN
	PROCESS ( Resetn, Clock )
	BEGIN
		IF Resetn = '0' THEN
			Q <= (OTHERS => '0') ;
		ELSIF rising_edge(Clock) THEN
			Q <= D ;
		END IF ;
	END PROCESS ;
END behavioral ;

LIBRARY ieee;
USE ieee.std_logic_1164.all; 
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


ENTITY MMU2 IS 
	PORT
	(	   
		A13 		:  IN  STD_LOGIC;
		A14 		:  IN  STD_LOGIC;
		A15 		:  IN  STD_LOGIC;
		nMREQ 	:  IN  STD_LOGIC;
		nINTMMU 	:  IN  STD_LOGIC;
		nWR 		:  IN  STD_LOGIC;
		nRESET 	:  IN  STD_LOGIC;
		DATA 		:  IN  std_logic_vector(8-1 downto 0) ;  -- Data bus from CPU (Z80???)
	   EA 		:  OUT  std_logic_vector(21-1 downto 13);   -- Extended Address bus to mem chips
	--ROM/RAM CHIPS ENABLE SIGNALS
      nCE0		:  OUT STD_LOGIC;
	   nCE1		:  OUT STD_LOGIC;
      nCE2		:  OUT STD_LOGIC;
	   nCE3		:  OUT STD_LOGIC;
      nCE4		:  OUT STD_LOGIC;
		nRSTO		:	OUT STD_LOGIC
	);
END MMU2;

ARCHITECTURE behavioral OF MMU2 IS 

   COMPONENT regn
	GENERIC ( N : INTEGER := 8 ) ;
	PORT ( D : IN STD_LOGIC_VECTOR(N-1 DOWNTO 0) ;
			 Resetn, Clock : IN STD_LOGIC ;
			 Q : OUT STD_LOGIC_VECTOR(N-1 DOWNTO 0) ) ;
	END COMPONENT;


	signal Bank0 :  std_logic_vector(8-1 downto 0);
	signal Bank1 :  std_logic_vector(8-1 downto 0);
	signal Bank2 :  std_logic_vector(8-1 downto 0);
	signal Bank3 :  std_logic_vector(8-1 downto 0);
	signal Bank4 :  std_logic_vector(8-1 downto 0);
	signal Bank5 :  std_logic_vector(8-1 downto 0);
	signal Bank6 :  std_logic_vector(8-1 downto 0);
	signal Bank7 :  std_logic_vector(8-1 downto 0);
	SIGNAL nSetBANK0: STD_LOGIC;
	SIGNAL nSetBANK1: STD_LOGIC;
	SIGNAL nSetBANK2: STD_LOGIC;
	SIGNAL nSetBANK3: STD_LOGIC;
	SIGNAL nSetBANK4: STD_LOGIC;
	SIGNAL nSetBANK5: STD_LOGIC;
	SIGNAL nSetBANK6: STD_LOGIC;
	SIGNAL nSetBANK7: STD_LOGIC;
	--SIGNAL nEnBANK0: STD_LOGIC;
	signal BANKNUM:std_logic_vector(3-1 DOWNTO 0);
	signal EXTADDR:std_logic_vector(21-1 downto 13);
	signal nRST: STD_LOGIC;
	

BEGIN
   
	
	Bankreg0 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK0,
	  Q => Bank0
	);
	Bankreg1 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK1,
	  Q => Bank1
	);
	Bankreg2 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK2,
	  Q => Bank2
	);
	Bankreg3 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK3,
	  Q => Bank3
	);
	Bankreg4 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK4,
	  Q => Bank4
	);
	Bankreg5 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK5,
	  Q => Bank5
	);
   Bankreg6 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK6,
	  Q => Bank6
	);
	Bankreg7 : regn 
	generic map (  N => 8)
	PORT MAP(
	  D => DATA,
	  Resetn => nRST,
	  Clock => nSetBANK7,
	  Q => Bank7
	);

	nRST <= nRESET; --'1'
	nRSTO <=nRST;

	BANKNUM <= A15 & A14 & A13;
	--BY USING OUT(C),A C GETS DECODED ON nINTMMU AND B GOES ON A8-A15 SO WE CAN CONTROL THIS WITH REGISTER B ON Z80
	--IF IT DOESN'T WORK THEN WE PUT A0,A1,A2 AT BANKNUM
	--LD B,3 ;FOR BANK 3
	--SLA B ;5 TIMES TO GO INTO BIT 7-BIT 5
	--LD C,OUTPORT
	--LD A,PAGE ;PAGE TO PUT ON BANK3
   nSetBANK0 <= '0' WHEN BANKNUM="000" and nINTMMU ='0' AND nWR='0' ELSE '1';
	nSetBANK1 <= '0' WHEN BANKNUM="001" and nINTMMU ='0' AND nWR='0' ELSE '1';
	nSetBANK2 <= '0' WHEN BANKNUM="010" and nINTMMU ='0' AND nWR='0' ELSE '1';
	nSetBANK3 <= '0' WHEN BANKNUM="011" and nINTMMU ='0' AND nWR='0' ELSE '1';
   nSetBANK4 <= '0' WHEN BANKNUM="100" and nINTMMU ='0' AND nWR='0' ELSE '1';
	nSetBANK5 <= '0' WHEN BANKNUM="101" and nINTMMU ='0' AND nWR='0' ELSE '1';
	nSetBANK6 <= '0' WHEN BANKNUM="110" and nINTMMU ='0' AND nWR='0' ELSE '1';
	nSetBANK7 <= '0' WHEN BANKNUM="111" and nINTMMU ='0' AND nWR='0' ELSE '1';
	
	--nEnBANK0 <= '0' WHEN A13='0' and A14='0' and A15='0' and nMREQ='0' ELSE '1';
		
	
   EXTADDR <=    BANK0 WHEN BANKNUM="000"
				ELSE BANK1 WHEN BANKNUM="001"
				ELSE BANK2 WHEN BANKNUM="010"
				ELSE BANK3 WHEN BANKNUM="011"
				ELSE BANK4 WHEN BANKNUM="100"
				ELSE BANK5 WHEN BANKNUM="101"
				ELSE BANK6 WHEN BANKNUM="110"
				ELSE BANK7 WHEN BANKNUM="111"
				ELSE BANK0;
	
	EA <= EXTADDR WHEN nMREQ='0' ELSE "00000000"; --"ZZZZZZZZ"
	
	--32KB MINIMUM CHIP CAPACITY
	--REMOVE AND nMREQ='0'  CHIPS ALREADY CONTROL THAT
	nCE0 <= '0' WHEN EXTADDR(20 DOWNTO 15) ="000000" AND nMREQ='0' ELSE '1'; --$00000 EXT ADDR  32KB EEPROM
	nCE1 <= '0' WHEN EXTADDR(20 DOWNTO 15) ="000001" AND nMREQ='0' ELSE '1'; --$08000 EXT ADDR  32KB RAM
	nCE2 <= '0' WHEN EXTADDR(20 DOWNTO 15) ="000010" AND nMREQ='0' ELSE '1'; --$10000 EXT ADDR  32KB DUAL PORT RAM
	nCE3 <= '0' WHEN EXTADDR(20 DOWNTO 15)>="000011"  AND  EXTADDR(20 DOWNTO 15)<="001010" AND nMREQ='0' ELSE '1'; --$18000 - $56000 EXT ADDR  256KB FLASH MEMORY
	nCE4 <= '0' WHEN EXTADDR(20 DOWNTO 15)>="001011" AND nMREQ='0' ELSE '1'; --$58000

END behavioral;