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
-- File name      : TrackCore.vhd
--
-- Purpose        : Track counter
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
-- package TrackCore
------------------------------------------------------------------------------- 

library IEEE;

use IEEE.std_logic_arith.all;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_signed.all;

------------------------------------------------------------------------------- 
------------------------------------------------------------------------------- 

entity TrackCore is
	port (
		FLOPPY_DRIVE_SELECT: in std_logic;
		HEADTRACKPOSITION:   out std_logic_vector(7 downto 0); -- track position value
		HEADMOVED: out std_logic;
		ackheadmove: in std_logic;
		FLOPPY_STEP: in std_logic;	-- Step command
		FLOPPY_DIR:  in std_logic;	-- Step direction
		FLOPPY_TRK00  : out std_logic;	-- Track 0 indicator
		clear_cnt: in std_logic;
		clock: in std_logic;		
		reset_not: in std_logic
	     );
end  TrackCore;

------------------------------------------------------------------------------------------

architecture arch of TrackCore is
	signal track00signal: std_logic;
	signal  trackposition : std_logic_vector(7 downto 0);
	signal  stepsignal : std_logic;
	signal  stepsignal2 : std_logic;

begin 
	FLOPPY_TRK00<=track00signal and FLOPPY_DRIVE_SELECT;
	-------------------------------------------------------
	-- Track Circuit
	trackcounter : process(FLOPPY_STEP,FLOPPY_DIR,trackposition,clock,reset_not)
	begin 
		if (reset_not='0')	
		then 
			track00signal<='1';
			trackposition<=(others=>'0');
		elsif (clock='1' and clock'EVENT)
		then
			-- resync step signal (metastability issue)
			stepsignal<=FLOPPY_STEP;
			stepsignal2<=stepsignal;
			
			if(ackheadmove='1')
			then
				HEADMOVED<='0';
			end if;
	
			if(FLOPPY_DRIVE_SELECT='1')
			then
				if(stepsignal2='1')
				then
					HEADMOVED<='1';
				end if;
				
				if(stepsignal/=stepsignal2 and stepsignal='1')
				then
					if(FLOPPY_DIR='1') then
						 trackposition<=trackposition + conv_std_logic_vector(1, 8);
					else 
						if (trackposition/="00000000") 
						then 
							trackposition<=trackposition - conv_std_logic_vector(1, 8); 
						end if;
					end IF;
				end if;
			end IF;
			
			if(trackposition="00000000")
			then 
				track00signal<='1'; 
			else 
				track00signal<='0'; 
			end if;	
			
			if(clear_cnt='1')
			then
				trackposition<="00000000";
			end if;
	
		end IF;
	end process;
	
	HEADTRACKPOSITION<=trackposition;
	
end arch;