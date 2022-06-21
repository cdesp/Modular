--Copyright (C)2014-2021 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--GOWIN Version: V1.9.8
--Part Number: GW1N-LV1QN48C6/I5
--Device: GW1N-1
--Created Time: Thu Jun 09 20:29:12 2022

--Change the instance name and port connections to the signal names
----------Copy here to design--------

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

your_instance_name: Gowin_SPRAM
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

----------Copy end-------------------
