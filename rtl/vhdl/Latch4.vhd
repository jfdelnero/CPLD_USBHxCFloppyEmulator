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
-- File name      : Latch4.vhd
--
-- Purpose        : 4-bits Register
--		    
--
-- Dependencies   : 	IEEE.Std_Logic_1164
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
-- package Latch4
------------------------------------------------------------------------------- 

library IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

------------------------------------------------------------------------------- 

ENTITY Latch4 IS
	port (
		D: in std_logic_vector(3 DOWNTO 0); -- Data in
		Q: out std_logic_vector(3 DOWNTO 0);-- Data out
		wr: in std_logic;		    --Store command
		clock: in std_logic;		    --Store command		
		reset_not: in std_logic
		);
end Latch4;

-------------------------------------------------------------------------------- 
-------------------------------------------------------------------------------- 

ARCHITECTURE arch of Latch4 is
begin
	
	latch4 : process(clock,wr,reset_not)
	begin
		if(reset_not='0') 
		then 
			Q<="0000"; 
		else 
			if(clock='1' and clock'event) 
			then
				if(wr='1') 
				then 
					Q<=D; 
				end if;
			end if;
		end if;
	end process;
end arch;