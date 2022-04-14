	\\ Acorn DFS 1.20/2.24/2.26
	\\ DFS224.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather


if sys=224
	\ DFS 2.24 for MASTER
	CPU 1	; 62C02
	sram=104
else
	\ DFS 2.26
	sram=105
endif

	INCLUDE "acorn_os_eq.asm"
	INCLUDE "filesys_eq.asm"
if sys=120
	INCLUDE "netsys_eq.asm"
endif

	ORG &8000
	\GUARD &C000

.langentry
if sys=120
	JMP NFS_LANGUAGE_ENTRY
else
	EQUB 0,0,0
endif

.serventry
if sys=120
	JMP NFS_SERVICE_ENTRY
else
if sys=224
	JMP SRAM_SERVICE_ENTRY
else
	JMP DFS_SERVICE_ENTRY
endif
endif

.romtype
	EQUB &82

.copywoffset
	EQUB copyright-&8001

.binversion
if sys=120
	EQUB &83
else
if sys=224
	EQUB &79
else
	EQUB &7B
endif
endif

.title
	EQUS "DFS"

if sys=120
	EQUB ",NET", 0
else
if sys=224
	EQUS 0, "2.24", 0
else
	EQUS 0, "2.26", 0
endif
endif

.copyright
	EQUS "(C)"

if sys<>120
	EQUS "1985"
	EQUS " Acorn", 0
endif

if sys=120
	INCLUDE "netsys.asm"
	INCLUDE "tubehost_dnfs.asm"
	INCLUDE "netsys_adlc.asm"

.DFS_CODE_START
	JMP CHECK_DFS
endif

	INCLUDE "filesys.asm"		;FILING SYSTEM

if sys<>224
	INCLUDE "utils.asm"		;UTILITIES
endif

if sys=224
.Prt2spaces
	JSR Prtspace			;Print 2 spaces

.Prtspace
	PHA 				;Print space
	LDA #&20
	JSR OSASCI
	PLA 
	CLC 
	RTS
endif

if sys<>120
	\ This traps INKEY(-256) to misreport OS version.
.TRAP_OSBYTE_SET
	JSR rememberAXY			;Trap INKEY(-256) [BOOT Z]
	LDA #&40			;Set drive mode bit 6 (Drive 0)
	STA swsp+&10DE
	LDA #&A8			;Read addr of ROM ptr table
	JSR osbyteX00YFF
	STX &B0				;Normally YX=&0D9F
	STY &B1				;"Extended vectors"
	LDY #&0F
	LDA #&4C
	STA swsp+&10E2
	LDA &020A			;&20A=BYTEV
	STA swsp+&10E3
	LDA &020B
	STA swsp+&10E4			;So, 10E2 JMP "OSBYTE"
	PHP 
	SEI 				;Disable interrupts
	LDA #&0F
	STA &020A
	LDA #&FF
	STA &020B			;BYTEV=&FF0F=extended vect call
	LDA #LO(TRAP_OSBYTE)
	STA (&B0),Y
	INY 
	LDA #HI(TRAP_OSBYTE)
	STA (&B0),Y
	INY 
	LDA PagedRomSelector_RAMCopy
	STA (&B0),Y			;Extended vector=&ACB2
	PLP 
	RTS

.TRAP_OSBYTE
{
	CMP #&00			;Trap OSBYTE &81 "INKEY"
	BEQ TRAP_OSBYTE_RESET

	CMP #&81			;INKEY(-256)=Identify OS
	BNE trap_osbyte_cont

	CPY #&FF
	BNE trap_osbyte_cont

	CPX #&00
	BNE trap_osbyte_cont

	DEX 				;BBC OS 1.00/1.20  X=&FF
	RTS
 
.trap_osbyte_cont
	JMP swsp+&10E2			;JMP previous OSBYTE routine
}

.TRAP_OSBYTE_RESET
	PHP 				;Reset BYTEV to previous value
	SEI 				;Disable interrupts
	LDA swsp+&10E3
	STA &020A
	LDA swsp+&10E4
	STA &020B
	LDA #&00
	LDX #&01
	PLP 
	RTS

if sys=226
	INCLUDE "tubehost230.asm"	;TUBE HOST 2.30
endif

	INCLUDE "sram.asm"		;SRAM 1.04/1.05
endif

if sys=226
	\ This bit is for the Model B+.

.DFS_SERVICE_ENTRY
	CMP #&01			;X=this rom no.
	BNE SERVICE2B_AlternativeBanner	;A=service type

	PHA 
	TYA 
	PHA 
	LDA #&8F			;Issue Paged Rom Service Request
	LDX #&2B			;X=service type
	LDY PagedRomSelector_RAMCopy	;Y=argument
	JSR OSBYTE
	PLA 
	TAY 
	LDX PagedRomSelector_RAMCopy	;Restore X & A
	PLA 

.Label_BEDD_sramserventry
	JMP SRAM_SERVICE_ENTRY

.SERVICE2B_AlternativeBanner
	CMP #&2B			;'Display alternative banner'
	BNE Label_BEDD_sramserventry

	CPY PagedRomSelector_RAMCopy	;Y=calling rom no.
	BEQ Label_BEEE_me		;If I issued service req.

.Label_BEE8_exitbanner
	LDX PagedRomSelector_RAMCopy
	LDA #&00			;Prevent other roms servicing call
	BEQ Label_BEDD_sramserventry

.Label_BEEE_me
	LDA #&72			;Write Shadow/Main toggle
	LDX #&00
	JSR OSBYTE
	JSR OSBYTE
	BVS Label_BEE8_exitbanner	;If no shadow ram?

	LDA #&EA
	JSR osbyteX00YFF__A_X_
	BNE Label_BEE8_exitbanner	;If Tube present

	LDA #&D7
	LDY #&7F
	JSR OSBYTE_X_0__A_X_		;Don't print OS startup message
	BPL Label_BEE8_exitbanner	;If already disabled

	JSR OSNEWL
	LDX #&00
	JSR Print_BANNER_X		;"Acorn OS "
	LDA #&FD
	JSR osbyteX00YFF__A_X_
	BEQ Label_BF46_exit		;If Soft-Break

	TSX 				;Save stack pointer
	LDY #&30			;Copy code on to stack

.Label_BF1C_loop
	LDA TestForSWRams-1,Y
	PHA 
	DEY 
	BNE Label_BF1C_loop

	TXA 
	TAY 
	LDA #&01			;Push code address
	PHA 
	TSX 
	INX 
	TXA 
	PHA 
	LDX #&0F			;First rom
	LDA #&00
	STA &B0
	RTS 				;Return to code in stack xxx

.Label_BF33_reentry
	TYA 				;Return from code in stack
	TAX 
	TXS 				;Restore stack pointer
	LDA &B0
	ASL A
	ASL A
	CLC 
	ADC #&0D
	TAX 				;X=(?&B0)*4 + &D
	JSR Print_BANNER_X
	LDX #&0A
	JSR Print_BANNER_X		;"K"

.Label_BF46_exit
	JSR OSNEWL
	JSR OSNEWL
	JMP Label_BEE8_exitbanner

.Label_BF4F_bannerstr
	EQUS "Acorn OS ", 0		;X=&00 = "Acorn OS "
	EQUS "K", 7, 0			;X=&0A = "K" + BEEP
	EQUS "64", 0, 0			;X=&0D = "64" no sram
	EQUS "80", 0, 0			;X=&11 = "80" 1 sram
	EQUS "96", 0, 0			;X=&15 = "96" 2 srams
	EQUS "112", 0			;X=&19 = "112" 3 srams
	EQUS "128", 0			;X=&1D = "128" 4 srams
	EQUS "144", 0			;X=&21 = "144" 5 srams
	EQUS "160", 0			;X=&25 = "160" 6 srams

.Label_BF78_loop
	JSR OSWRCH
	INX

.Print_BANNER_X
	LDA Label_BF4F_bannerstr,X
	BNE Label_BF78_loop

	RTS

.osbyteX00YFF__A_X_
	LDY #&FF

.OSBYTE_X_0__A_X_
	LDX #&00
	JSR OSBYTE
	TXA 
	RTS

.TestForSWRams
{
	LDA PagedRomSelector_RAMCopy	;Code copied to stack
	PHA 				;Test for srams

.Label_BF8E_LOOP
	STX PagedRomSelector_RAMCopy
	STX PagedRomSelector
	LDA &BFFF
	PHA 
	EOR #&A5
	STA &BFFF
	CMP &BFFF
	BNE Label_BFA3_notwrite		;If not writable

	INC &B0				;It's writable

.Label_BFA3_notwrite
	PLA 
	STA &BFFF
	DEX 
	BMI Label_BFB2_exit		;if exit loop

	CPX #&0B
	BNE Label_BF8E_LOOP		;Skip roms 2 to &B

	LDX #&01
	BNE Label_BF8E_LOOP		;always

.Label_BFB2_exit
	PLA 
	STA PagedRomSelector_RAMCopy
	STA PagedRomSelector
	JMP Label_BF33_reentry
}
endif

	\\ END OF ROM

