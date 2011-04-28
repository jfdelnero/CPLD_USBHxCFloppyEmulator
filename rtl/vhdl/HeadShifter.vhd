-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-----------H----H--X----X-----CCCCC----22222----0000-----0000------11----------
----------H----H----X-X-----C--------------2---0----0---0----0--1--1-----------
---------HHHHHH-----X------C----------22222---0----0---0----0-----1------------
--------H----H----X--X----C----------2-------0----0---0----0-----1-------------
-------H----H---X-----X---CCCCC-----222222----0000-----0000----1111------------
-------------------------------------------------------------------------------
----------------------------------------- http://jeanfrancoisdelnero.free.fr --
--===========================================================================--
--  HxCFloppyEmu
--  Floppy drive emulator Project
--
--  http://jeanfrancoisdelnero.free.fr 
--	HxC2001   -   2006 - 2008
--
-- Design units   : 
--
-- File name      : HeadShifter.vhd
--
-- Purpose        : MFM data shifter
--		
--
-- Dependencies   : 	IEEE.Std_Logic_1164
--			IEEE.std_logic_signed
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
------------------------------------------------------------------------------- 
-- package HeadShifter
------------------------------------------------------------------------------- 

library IEEE;

use IEEE.STD_LOGIC_1164.all;
use ieee.std_logic_unsigned.all;

------------------------------------------------------------------------------- 
------------------------------------------------------------------------------- 

------------------------------------------------------
-- MFM data shifter
--
entity HeadShifter is
	port (		
		-- input // data bus
		DATABUS: in std_logic_vector(3 downto 0);
		SPEEDCONFIG: in std_logic_vector(7 downto 0);
		
		DATALOADED: out std_logic;
		
		FLOPPY_DATA: out std_logic; -- MFM data line

		clock: in std_logic;  -- Clock the shifter
		reset_not: in std_logic
		);
end HeadShifter;

------------------------------------------------------------------------------------------

architecture arch of HeadShifter is
--------------------------------------------

type state_shifter is (STATE0,STATE1,STATE2,STATE3);
					
signal FLOPPYDATAH0 : std_logic; -- shifter -> pulse generator link
signal speedcnt: std_logic_vector(7 downto 0);
signal tmp: std_logic_vector(3 downto 0);  
signal bitcnt: state_shifter;  
signal bitcounter: std_logic_vector(1 downto 0);
signal loadtoggle: std_logic;  
------------------------------------------------------------------------------------------

BEGIN 
   

	process (speedcnt,SPEEDCONFIG,tmp,clock,reset_not)      
	begin   
	
		if (reset_not = '0') 
		then
			speedcnt(7 downto 0)<="00000000";
			bitcounter<="00";
			loadtoggle<='0';
			tmp<="0000";
		elsif (clock'event and clock = '1') 
		then 
			if(speedcnt(7 downto 1)/="0000000")
			then
				-- decrement the bitrate prescaler
				speedcnt(7 downto 1)<=speedcnt(7 downto 1)-"0000001"; 	
			else
			
					-- test we have done a full mfm code (2 bits)
					-- 
					if(bitcounter(0)='0' or speedcnt(0)='0')
					then			
						
						-- increment the bit counter 
						bitcounter<=bitcounter+"01";
						
						-- shift the data register	
						tmp <= tmp(2 downto 0) & '0';
						
						if(bitcounter="00")
						then
							-- we have done all the four bits (2 mfm bits) 
							-- we need to reload the shifter.
							loadtoggle<=not(loadtoggle);
							tmp<=DATABUS;
						end if;
									
						-- reload of the bitrate prescaler
						if(SPEEDCONFIG(7 downto 0)<"00001110")
						then 
							-- the bitrate can't be higher than 1Mbits/s
							speedcnt<="00001110";
						else
							speedcnt<=SPEEDCONFIG;
						end if;
					else
						-- to have the 62.5ns step precision for 2 bits (1 mfm bit). 
						if(speedcnt(0)='1')
						then
							speedcnt(0)<='0';
						end if;
					end if;
					
			end if;	
		end if;   
		
		-- 25% duty cycle signal generator
		if(speedcnt(7 downto 1)>('0' & SPEEDCONFIG(7 downto 2)))
		then
			FLOPPY_DATA <= tmp(3);
		else
			FLOPPY_DATA <= '0';
		end if;
	end process; 

	DATALOADED<=loadtoggle;	

END arch;