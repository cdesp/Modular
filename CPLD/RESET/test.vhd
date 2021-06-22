library ieee;
use ieee.std_logic_1164.all;
 
entity test is
  port (
    nRESET : in  std_logic;
    oRESET : out  std_logic
    );  
end test;
 
architecture behave of test is
 
begin
 
 oReset<=nReset;
   
end behave;