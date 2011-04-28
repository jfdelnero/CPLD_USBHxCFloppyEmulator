-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-----------H----H--X----X-----CCCCC----22222----0000-----0000------11----------
----------H----H----X-X-----C--------------2---0----0---0----0--1--1-----------
---------HHHHHH-----X------C----------22222---0----0---0----0-----1------------
--------H----H----X--X----C----------2-------0----0---0----0-----1-------------
-------H----H---X-----X---CCCCC-----222222----0000-----0000----1111------------
-------------------------------------------------------------------------------
----------------------------------------- http://jeanfrancoisdelnero.free.fr --
--============================================================================-
--  HxCFloppyEmu
--  Floppy drive emulator Project
--
--  http://jeanfrancoisdelnero.free.fr 
--	HxC2001   -   2006 - 2008
--
-- Design units   : 
--
-- File name      : ControlCore.vhd
--
-- Purpose        : Main part of the floppy emulator : Core State machine
--					
--
-- Dependencies   :		IEEE.Std_Logic_1164
--						IEEE.STD_LOGIC_arith
--
--============================================================================-
--                                                                           --
-- Copyright (C) 2006, 2007, 2008 Jean-François DEL NERO                     --
--                                                                           --
-- This file is part of HxCFloppyEmulator.                                   --
--                                                                           --
-- HxCFloppyEmulator may be used and distributed without restriction provided--
-- that this copyright statement is not removed from the file and that any   --
-- derivative work contains the original copyright notice and the associated --
-- disclaimer.                                                               --
--                                                                           --
-- HxCFloppyEmulator is free software; you can redistribute it               --
-- and/or modify  it under the terms of the GNU General Public License       --
-- as published by the Free Software Foundation; either version 2            --
-- of the License, or (at your option) any later version.                    --
--                                                                           --
-- HxCFloppyEmulator is distributed in the hope that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                      --
--   See the GNU General Public License for more details.                    --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with HxCFloppyEmulator; if not, write to the Free Software          --
-- Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA--
--                                                                           --
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Revision list
-- Version   Author                 Date                        Changes
--
-- 1.0    Jean-François DEL NERO  23 march 2008			Major update:
--
--                                                      MFM/FM output generator (HeadShifter) rewritten.
--                                                      It can now do any bitrate between 
--                                                      63kbit/s and 1Mbit/s with a 62.5ns step.
--                                                      The emulator can now handle bitrate-protected floppies ;-)
--
--                                                      The SRAM is now used like a ring buffer (1 buffer of 8KB).
--
--                                                      The master state machine run now at 16Mhz 
--                                                      to allow fast opcode execution / mfm loading.
--
--                                                      "Validate Track" opcode removed (same functionnality in "SENDTRACKCODE opcode".
--                                                      "SETINDEX" opcode modified: 
--                                                      "SENDTRACKCODE" added (2 byte : 0x3 <track number>)
--                                                      "SETBITRATE" opcode added  (2 bytes: 0xD <period value>)
--                                                      "NOP" opcode added  (2 bytes : 0x7 XX) 
--                                                      "Disk Changed" and "Ready" signals 
--                                                      are now software driven
--
--                                                      Track position register is now 8 bits.
--
--                                                      SRAM_CS_not is now driven (for the SRAM standby mode)
--
-- 0.5    Jean-François DEL NERO  19 November 2006      Jumper-free drive select added
--        jeanfrancoisdelnero < > free.fr
-- 0.4    Jean-François DEL NERO  11 November 2006      500kbits/s support added
--                                                      2*1Ko and 2*2Ko buffer size available
--                                                      Write protect signal added
--                                                      Shugart and IBM PC mode available
-- 0.2    Jean-François DEL NERO  16 September 2006     MFM Pulse Generator rewritten
-- 0.1    Jean-François DEL NERO  25 June 2006          First public version
--------------------------------------------------------------------------------
-------------------------------------------------------------------------------- 
-- package ControlCore
-------------------------------------------------------------------------------- 
library IEEE;
  
use IEEE.std_logic_arith.all;
use IEEE.STD_LOGIC_1164.all;
use ieee.std_logic_unsigned.all;
-------------------------------------------------------------------------------- 

entity ControlCore is 
	port (
	-- FTDI chip if
		FTDI_DATA: inout std_logic_vector(7 downto 0);
		FTDI_RD_not: out std_logic;
		FTDI_WR: out std_logic;
		FTDI_TXE_not: in std_logic;
		FTDI_RXF_not: in std_logic;

		-- SRAM IF
		SRAM_DATA: inout std_logic_vector(7 downto 0);
		SRAM_ADR: out std_logic_vector(14 downto 0);
		SRAM_RD_not: out std_logic;
		SRAM_WR_not: out std_logic;
		SRAM_CS_not: out std_logic;

		-- Track position
		Head_Position: in std_logic_vector(6 downto 0);				
		Clear_Head_Position: out std_logic;	
		Head_moved : in std_logic;				
		Ack_head_move : out std_logic;				
		
		-- Floppy IF
		FLOPPY_READYSIGNAL: out std_logic;			
		FLOPPY_SIDE: in std_logic;		
		FLOPPY_DSKCHG  : out std_logic;	
		FLOPPY_INDEX: out std_logic;
		FLOPPY_WPT: out std_logic;
		FLOPPY_SEL0: in std_logic;
		FLOPPY_SEL1: in std_logic;
		FLOPPY_SEL2: in std_logic;
		FLOPPY_SEL3: in std_logic;						
		FLOPPY_SELECT:  out std_logic;						
		FLOPPY_DATA: out std_logic;			
		
		Step_LED: out std_logic;

		-- Clock and reset
		clock: in std_logic;		
		reset_not: in std_logic
		);
end ControlCore;

-------------------------------------------------------------------------------- 
-------------------------------------------------------------------------------- 

architecture arch of ControlCore is

constant ADDRESSBUSWITDH: natural:=13;

------------------------------------------------------
-- FM/MFM data shifter
--
component HeadShifter
	port (		
		-- input // data bus
		DATABUS: in std_logic_vector(3 downto 0);
		SPEEDCONFIG: in std_logic_vector(7 downto 0);
		
		DATALOADED: out std_logic;
		
		FLOPPY_DATA: out std_logic; -- FM/MFM data line

		clock: in std_logic;  -- Clock the shifter
		reset_not: in std_logic
		);
end component;


------------------------------------------------------
-- Opcode / data latch 
--
component Latch4 is
	port (
		D: in std_logic_vector(3 DOWNTO 0); -- Data in
		Q: out std_logic_vector(3 DOWNTO 0);-- Data out
		wr: in std_logic;		    --Store command
		clock: in std_logic;		  	
		reset_not: in std_logic
		);
end component;
---------------------------------------------------------------------------------
type master_state_machine_type is 
		(
			IDLE,
			SPRELOADS0,SPRELOADS1,SPRELOADS2, SPRELOADS3,SPRELOADS4,SPRELOADS5,		
			SENDTRACKNUMBER0,SENDTRACKNUMBER1,SENDTRACKNUMBER2,
			WRITES0,WRITES1,WRITES2,WRITES3,WRITES4,WRITES5
		);
						
signal master_fsm: master_state_machine_type ;

---------------------------------
signal	preload	:     std_logic_vector(3 downto 0);		
signal	preloaded	: std_logic_vector(3 downto 0);	
signal  pload :       std_logic;
---------------------------------
signal write_adr: std_logic_vector(ADDRESSBUSWITDH-1 downto 0); 	-- write address 
signal read_adr:  std_logic_vector(ADDRESSBUSWITDH-1 downto 0);	-- read address 
signal SRAM_busdir: std_logic;		-- sram bus state 1:write 0:read
------------------------------------------------------------------  
signal ftdibusdir: std_logic;		-- ftdi bus state 1:write 0:read

signal FLOPPY_SEL: std_logic;	
signal FLOPPY_SELreg:  std_logic_vector(1 downto 0);
signal FLOPPY_SELdisable: std_logic;	
					
signal index_signal: std_logic;
signal writeprotect_signal: std_logic;			
signal diskchanged_signal: std_logic;
signal ready_signal: std_logic;

signal mfm_speed_config : std_logic_vector(7 downto 0);			

signal rxfstate : std_logic;
signal FLOPPY_DATA_shifter : std_logic;
signal FLOPPY_DATA_MASK : std_logic;

signal amigareadymode : std_logic;
signal shiftertoggle : std_logic;

signal loadtoggle: std_logic;  
signal FTDI_RXF_not_synced: std_logic;  

signal increadadr: std_logic;
signal incwriteadr: std_logic;

signal FLOPPY_SIDE_synced: std_logic;

begin 

	HDC:	HeadShifter    port map(preloaded,mfm_speed_config,shiftertoggle,FLOPPY_DATA_shifter,clock,reset_not);
	PL :	Latch4  port map(preload,preloaded,pload,clock,reset_not);
	
	Clear_Head_Position<=FLOPPY_SELdisable;
	
	---------------------------------------------------------------------------------------
	-- Floppy signals process
	--
	floppyif : process(Head_moved,FLOPPY_DATA_shifter,FLOPPY_SEL,index_signal,writeprotect_signal,diskchanged_signal,amigareadymode,ready_signal)
	begin
		if(Head_moved='0')
		then
			step_led<='0';
			FLOPPY_DATA<=  FLOPPY_DATA_shifter and FLOPPY_SEL;
		else
			step_led<='1';
			FLOPPY_DATA<='0';
		end if;
		
		FLOPPY_SELECT<=FLOPPY_SEL;
		FLOPPY_INDEX<=  index_signal and FLOPPY_SEL;
		FLOPPY_WPT  <= writeprotect_signal and FLOPPY_SEL;
		FLOPPY_DSKCHG<= diskchanged_signal and FLOPPY_SEL;
			
		if(amigareadymode='0') 
		then
			FLOPPY_READYSIGNAL<=ready_signal and FLOPPY_SEL;	
		else
			if(Head_moved='0')
			then
				FLOPPY_READYSIGNAL<=ready_signal and FLOPPY_SEL;
			else
				FLOPPY_READYSIGNAL<='0';
			end if;			
		end if;
	end process;
	
	-----------------------------------------------------------
	-- Select signals decoder
	selectproc : process(reset_not,FLOPPY_SELdisable,FLOPPY_SELreg,FLOPPY_SEL0,FLOPPY_SEL1,FLOPPY_SEL2,FLOPPY_SEL3)
	begin
		if (FLOPPY_SELdisable='0')
		then
			case FLOPPY_SELreg is
					when  "00" =>
					FLOPPY_SEL<=FLOPPY_SEL0;
					when  "01" =>
					FLOPPY_SEL<=FLOPPY_SEL1;					
					when  "10" =>
					FLOPPY_SEL<=FLOPPY_SEL2;					
					when  "11" =>			
					FLOPPY_SEL<=FLOPPY_SEL3;										
			end case;
		else
			FLOPPY_SEL<='0';										
		end if;	
	end process;


	--------------------------------------------------------------------------
	--SRAM access mux
	srammux : process(FTDI_DATA,SRAM_busdir,read_adr,write_adr,reset_not)
	begin
		SRAM_ADR<=(others=>'0');
		if(SRAM_busdir='1') 
		then
			-- FTDI -> SRAM
			SRAM_ADR(ADDRESSBUSWITDH-1 downto 0)<=write_adr(ADDRESSBUSWITDH-1 downto 0);				
			SRAM_DATA<= FTDI_DATA;
		else
			-- SRAM -> shifter/opcode register
			SRAM_ADR(ADDRESSBUSWITDH-1 downto 0)<=read_adr(ADDRESSBUSWITDH-1 downto 0);
			SRAM_DATA<=(others=>'Z');
		end if;
		
	end process;

	--------------------------------------------------------------------------	
	-- Send track number Circuit
	readtrackpos : process(Head_moved,ftdibusdir,Head_Position,FLOPPY_SEL)
	begin
		if( ftdibusdir='1')
		then
			-- Head position -> FTDI chip
			FTDI_DATA<=Head_moved & Head_Position(6 downto 0);	
		else
			-- FTDI chip -> CPLD
			FTDI_DATA<=(others=>'Z');		
		end if;
	end process;
	
	--------------------------------------------------------------------------
	--data bus Mux (side)
	sidemux : process(FLOPPY_SIDE_synced,SRAM_DATA)
	begin
		if(FLOPPY_SIDE_synced='1') 
		then
				-- Side 1
				preload<=SRAM_DATA(3 downto 0);
		else
				-- Side 0
				preload<=SRAM_DATA(7 downto 4);	
		end if;
	end process;

	--------------------------------------------------------------------------
	-- read & write address incrementer (ring buffer)
	process (clock, reset_not)	
	begin				
		if(reset_not='0')
		then
			write_adr<=(others=>'0');
			read_adr<=(others=>'0');
		elsif (clock'event and clock = '1') 
		then		
		
			if(incwriteadr='1')
			then
				write_adr<=write_adr + conv_std_logic_vector(1, ADDRESSBUSWITDH);
			end if;

			if(increadadr='1')
			then
				read_adr<=read_adr + conv_std_logic_vector(1, ADDRESSBUSWITDH);
			end if;
		
		end if;
	end process;
	
	
	------------------------------------------------------------------
	------------------------------------------------------------------
	-- Master State Machine
	------------------------------------------------------------------
	------------------------------------------------------------------
	
	process (clock,reset_not)	
		begin				
			if (reset_not = '0')
			then
				master_fsm <= IDLE;			
				loadtoggle<='0';
				mfm_speed_config<="01000000";
				FLOPPY_SELdisable<='1';
			elsif (clock'event and clock = '1') 
			then
				FTDI_RXF_not_synced<=FTDI_RXF_not;
			
				-- a step append > We need to "hide" the actual datas
				Ack_head_move<='0';
		
				-- default values
				SRAM_busdir<='0';
				
				SRAM_RD_not<='1';
				SRAM_WR_not<='1';
				SRAM_CS_not<='0';
				FTDI_RD_not<='1';
				FTDI_WR<='0';
				
				pload<='0';
				ftdibusdir<='0';
				
				increadadr<='0';
				incwriteadr<='0';
							
				case master_fsm is
				
			----------------------------------------------------------------
			-- Idle state : wait for a shifter event or USB data.
			--
					when IDLE =>
						
						if (loadtoggle/=shiftertoggle)
						then		
							-- first priority -> load the shifter
							FLOPPY_SIDE_synced<=FLOPPY_SIDE;		
							master_fsm <= SPRELOADS0;
						else
							if(FTDI_RXF_not_synced='0')
							then
								-- second priority -> write usb data to the sram
								master_fsm <= WRITES0;
							else
								-- stay in idle state
								SRAM_CS_not<='1';
								master_fsm <= IDLE;
							end if;
						end if;	
					
					
			----------------------------------------------------------------
			-- Preload data/opcode from sram
			--
			--
					when SPRELOADS0 =>
							SRAM_RD_not<='0'; --read data from sram
							master_fsm <= SPRELOADS1;
					when SPRELOADS1 =>
							SRAM_RD_not<='0'; 
							pload<='1';		-- store data in the latch (next cycle)
							increadadr<='1'; -- increment the read address (next cycle)							
							master_fsm <= SPRELOADS2;
					when SPRELOADS2 =>
							SRAM_RD_not<='1'; 
							master_fsm <= SPRELOADS3;
					when SPRELOADS3 =>	
							SRAM_RD_not<='0'; -- read data from sram
							master_fsm <= SPRELOADS4;							
					when SPRELOADS4 =>
							SRAM_RD_not<='0'; -- read data from sram
							master_fsm <= SPRELOADS5;
					when SPRELOADS5 =>
							-- decode the opcode
							case preloaded is
								when "1100" =>		--SETINDEX opcode
									index_signal<=     		SRAM_DATA(0); -- change index state
									ready_signal<=			SRAM_DATA(1);
									diskchanged_signal<=	SRAM_DATA(2);
									writeprotect_signal<=	SRAM_DATA(3);
									amigareadymode<=		SRAM_DATA(4);
									FLOPPY_SELdisable<=		SRAM_DATA(5);
									FLOPPY_SELreg(0)<=		SRAM_DATA(6);
									FLOPPY_SELreg(1)<=		SRAM_DATA(7);
									increadadr<='1';
									master_fsm <= IDLE;
																		
								when "1101" =>		--SETBITRATE opcode
									mfm_speed_config<=SRAM_DATA(7 downto 0); 	
									increadadr<='1';
									master_fsm <= IDLE;
														
								when "0011" => 		--SENDTRACKCODE opcode											
									if(FTDI_TXE_not='0')
									then
										SRAM_RD_not<='0'; -- read data from sram
										master_fsm <= SENDTRACKNUMBER0;
									else
										master_fsm <= IDLE;
										increadadr<='1';
									end if;
									
								when "0111" =>		--NOP opcode
									increadadr<='1';
									master_fsm <= IDLE;
									
								when others =>	-- it's not an opcode -> data already loaded.
									loadtoggle<=shiftertoggle;
									master_fsm <= IDLE;
									
							end case;
							
							

			----------------------------------------------------------------
			-- Send track position to the PC.
			--
					--                     |T11___T12_      
					-- TXE#  _________________/       \____
					--            ___T7____    T8       ___
					-- WR    ____/         \___________/
					--                | T9 | T10|
					-- D     -------T3<   D     >----------
					--
					--
					-- T7 - WR Active Pulse Width (min 50ns)
					-- T8 - WR to RD Pre-Charge Time (min 50ns)
					-- T9 - Data Setup Time before WR Inactive (min 20ns)
					-- T10 - Data Hold Time from WR Inactive (min 0ns)
					-- T11 - WR Inactive to TXE# (min 5ns max 25ns)
					-- T12 - TXE Inactive After WR Cycle (min 80ns)
					
					when SENDTRACKNUMBER0 =>
							FTDI_WR<='1';  
							ftdibusdir<='1';  -- put the track position on the ftdi bus  
							SRAM_RD_not<='0'; -- read data from sram
							master_fsm <= SENDTRACKNUMBER1;
					when SENDTRACKNUMBER1 =>
							FTDI_WR<='1';     --add some delay...
							SRAM_RD_not<='0'; -- read data from sram
							ftdibusdir<='1'; 
							master_fsm <= SENDTRACKNUMBER2;
					when SENDTRACKNUMBER2 =>
							ftdibusdir<='1';  -- write to the usb fifo
							
							if(Head_Position(6 downto 0)=SRAM_DATA(6 downto 0))
							then
								Ack_head_move<='1';
							end if;
							
							increadadr<='1';
							master_fsm <= IDLE;				
							
			----------------------------------------------------------------
			-- store data from the USB FIFO FT245 to the sram
			-- 			
					--       ______                 ____
					-- RXF#        \_______________/    \____
					--       _______             T5_______ 
					-- RD#          \____T1_____/         \___
					--
					-- D     -------T3<   D     >-------------
					--
					--
					-- T1 - RD Active Pulse Width (min 50ns)
					-- T2 - RD to RD Pre-Charge Time (min 50ns +T6)
					-- T3 - RD Active to Valid Data (max 50ns)
					-- T4 - Valid Data Hold Time from RD Inactive (min 0 ns)
					-- T5 - RD Inactive to RXF# (max 25ns)
					-- T6 - RXF Inactive After RD Cycle (min 80ns)
					--
					when WRITES0 =>
							SRAM_busdir<='1'; -- sram bus in write mode
							FTDI_RD_not<='0';-- read from ftdi chip
							master_fsm <= WRITES1;											
					when WRITES1 =>
							SRAM_busdir<='1';
							SRAM_WR_not<='0'; -- write to sram
							FTDI_RD_not<='0';-- read from ftdi chip
							master_fsm <= WRITES2;
					when WRITES2 =>
							SRAM_busdir<='1'; -- sram bus in write mode			
							SRAM_WR_not<='0'; -- write to sram
							FTDI_RD_not<='0'; -- read from ftdi chip							
							master_fsm <= WRITES3;
					when WRITES3 =>
							SRAM_busdir<='1'; -- sram bus in write mode		
							incwriteadr<='1';	 -- increment the write address
							master_fsm <= WRITES4;																							
					when WRITES4 =>				--add 62.5ns delay...
							master_fsm <= WRITES5;	
					when WRITES5 =>				--add 62.5ns delay...		
							master_fsm <= IDLE;
		
					when others =>	master_fsm <= IDLE;
				end case;
			end if;
	end process;

END arch;
