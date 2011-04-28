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
--	HxC2001   -  2006 - 2008
--
-- Design units   : 
--
-- File name      : HxCFloppyEmu.vhd (top file)
--
-- Purpose        : Implements an floppy drive emulator.
--		    		This design are based on the USB chip FT245BM. 
--                  
--
-- Dependencies   : 	IEEE.Std_Logic_1164
--			IEEE.STD_LOGIC_arith
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
-------------------------------------------------------------------------------- 
-- package FloppyEmu
-------------------------------------------------------------------------------- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_arith.all;

-------------------------------------------------------------------------------- 

entity HxCFloppyEmu is
	port (
		
		-- FTDI chip interface
		FTDI_DATA   : inout std_logic_vector(7 downto 0);
		FTDI_RD_not : out std_logic;
		FTDI_WR     : out std_logic;
		FTDI_TXE_not: in std_logic;
		FTDI_RXF_not: in std_logic;
		
		--	32Ko SRAM
		SRAM_DATA : inout std_logic_vector(7 downto 0);
		SRAM_ADDR : out std_logic_vector(14 downto 0);
		SRAM_CS_not : out std_logic;
		SRAM_WRITE_not : out std_logic;
		SRAM_READ_not : out std_logic;

		-- Floppy interface		
		FLOPPY_DS0: in std_logic;
		FLOPPY_DS1: in std_logic;
		FLOPPY_DS2: in std_logic;
		FLOPPY_MTRON: in std_logic;

		-- Head control lines
		FLOPPY_SIDE: in std_logic;
		FLOPPY_STEP: in std_logic;
		FLOPPY_DIR: in std_logic;
		FLOPPY_TRK00  : out std_logic;
					
		-- Others floppy control lines
		FLOPPY_WPT : out std_logic;
		FLOPPY_INDEX: out std_logic;
		FLOPPY_DATA : out std_logic;		
		FLOPPY_DSKCHG : out std_logic;	
		FLOPPY_READY : out std_logic;	
		
		-- Some Leds...
		LED1_not: out std_logic; 
		LED2_not: out std_logic;
		LED3_not: out std_logic;		
		
		-- Clock Input 
		clock: in std_logic;
		reset_not: in std_logic
		);
end HxCFloppyEmu;
-------------------------------------------------------------------------------- 
-------------------------------------------------------------------------------- 

architecture struct of HxCFloppyEmu is

------------------------------------------------------
-- Control core of the Floppy Emulator
--
component ControlCore is
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
end component;

------------------------------------------------------
-- Track position counter...
--
component TrackCore 
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
end component;

------------------------------------------------------

signal trackvalue	: std_logic_vector(6 downto 0);	
signal step_led	: std_logic;	
signal FLOPPY_SEL: std_logic;	
signal clear_track_cnt: std_logic;						
signal headmoved: std_logic;
signal ackheadmoved: std_logic;
		
begin

	LED1_not<=not(FLOPPY_SEL); 	-- floppy activity led
	LED2_not<=not(step_led); 	-- step led
	LED3_not<=FTDI_RXF_not;  	-- usb activity led
	
	THM:  TrackCore port map
				(
					FLOPPY_DRIVE_SELECT=>FLOPPY_SEL,
					HEADTRACKPOSITION(6 downto 0)=>trackvalue,
					HEADMOVED=>headmoved,
					ackheadmove=>ackheadmoved,
					FLOPPY_STEP=>FLOPPY_STEP,
					FLOPPY_DIR=>FLOPPY_DIR,
					FLOPPY_TRK00=>FLOPPY_TRK00,
					clear_cnt=>clear_track_cnt,
					clock=>clock,
					reset_not=>reset_not
				);
			
	CC:   ControlCore port map
				(
					-- usb / ftdi bus
					FTDI_DATA=>FTDI_DATA,
					FTDI_RD_not=>FTDI_RD_not,
					FTDI_WR=>FTDI_WR,
					FTDI_TXE_not=>FTDI_TXE_not,
					FTDI_RXF_not=>FTDI_RXF_not,
					
					-- sram bus 
					SRAM_DATA=>SRAM_DATA,
					SRAM_ADR=>SRAM_ADDR,
					SRAM_RD_not=>SRAM_READ_not,
					SRAM_WR_not=>SRAM_WRITE_not,
					SRAM_CS_not=>SRAM_CS_not,
					
					-- head position signals
					Head_Position=>trackvalue,
					Clear_Head_Position=>clear_track_cnt,
					Head_moved=>headmoved,
					Ack_head_move=>ackheadmoved,
					
					-- external floppy signals
					FLOPPY_READYSIGNAL=>FLOPPY_READY,
					FLOPPY_SIDE=>FLOPPY_SIDE,
					FLOPPY_DSKCHG=>FLOPPY_DSKCHG,
					FLOPPY_INDEX=>FLOPPY_INDEX,
					FLOPPY_WPT=>FLOPPY_WPT,
					FLOPPY_SEL0=>FLOPPY_DS0,
					FLOPPY_SEL1=>FLOPPY_DS1,
					FLOPPY_SEL2=>FLOPPY_DS2,
					FLOPPY_SEL3=>FLOPPY_MTRON,
					FLOPPY_SELECT=>FLOPPY_SEL,
					FLOPPY_DATA=>FLOPPY_DATA,

					step_led=>step_led,
					
					-- clock & reset
					clock=>clock,
					reset_not=>reset_not
					);

 end struct;


