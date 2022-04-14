	\\ Acorn SRAM 1.04/1.05
	\\ sram.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

	\\ ****** SRAM 1.04/1.05 ******

	\ sram = 104 for 1.04
	\ sram = 105 for 1.05

.sram_helpstr
	EQUS 10, "SRAM "
if sram=104
	EQUS "1.04"
else
	EQUS "1.05"
endif
	EQUS 13, 10
	EQUS "  SRDATA  <id.>", 13, 10
	EQUS "  SRLOAD  <filename> <sram address> (<id.>) (Q)", 13, 10
	EQUS "  SRREAD  <dest. start> <dest. end> <sram start> (<id.>)", 13, 10
	EQUS "  SRROM   <id.>", 13, 10
	EQUS "  SRSAVE  <filename> <sram start> <sram end> (<id.>) (Q)", 13, 10
	EQUS "  SRWRITE <source start> <source end> <sram start> (<id.>)", 13, 10
	EQUS "End addresses may be replaced by +<length>", 13, 10
if sram=105
	EQUS 0
endif

.SRAM_SERVICE_ENTRY
{
	JSR DFS_SERVICECALLS		;Do DFS service calls
	PHA 				;Save A,Y and word &B8
	TAX 
	LDA &B8
	PHA 
	LDA &B9
	PHA 
	TYA 
	PHA 
	TXA 
	LDX PagedRomSelector_RAMCopy
	JSR SRAM_SetB8PtrToPWsp

	BIT &B9
if sram=104
	BPL label_ABBA
	BVC sramserv_exit
	BVS label_ABBC

.label_ABBA
	BVS sramserv_exit

.label_ABBC
else
	BMI sramserv_exit		;If -ve ROM disabled so exit
endif

	CMP #&08			;A=8 Unrecognised OSWORD
	BNE sramserv_call02		;If not Service &08

	LDA &EF				;A=most recent OSWORD call
	CMP #&43
	BNE sramserv_ow42		;If not OSWORD &43

	JSR SRAM_OSWORD_LOADSAVE	;Save/Load to/from SW Ram

.sramserv_returnA_0
	TSX 				;Return with A=&00
	LDA #&00
	STA &0104,X
	BEQ sramserv_exit		;always

.sramserv_ow42
	CMP #&42
	BNE sramserv_exit		;IF not OSWORD &42

	LDY #&09			;Block txf to/from SW Ram

.sramow42_loop1
	LDA (&F0),Y			;->Control Block
	STA &00B4,Y
	DEY 				;wBC=SRAM address
	CPY #&08
	BCS sramow42_loop1		;If >=8

.sramow42_loop2
	LDA (&F0),Y			;B0-B7 = ctrl block 0 - 7
	STA &00B0,Y
	DEY 
	BPL sramow42_loop2		;If >0

	CLI 				;enable interrupts
	JSR SRAM_OWSORD_BLOCKXFR
	JMP sramserv_returnA_0

.sramserv_call02
	CMP #&02			;A=2 Private workspace claim
	BNE sramserv_call06		;If not Service &02

	JSR SRAM_SetupPWSP

.sramserv_exit
	PLA 				;Restore A,X,Y and word &B0
	TAY 
	PLA 
	STA &B9
	PLA 
	STA &B8
	PLA 
	LDX PagedRomSelector_RAMCopy
	RTS

.sramserv_call06
	CMP #&06			;Break
	BNE sramserv_call04		;If not Service &06

	LDY #&FF
	LDA (&B8),Y
	CMP #&4E
	BNE sramserv_exit

	JSR SRAM_CloseFile
	JMP sramserv_exit

.sramserv_call04
	CMP #&04			;Unrecognised command
	BNE sramserv_call07		;If not Service &04

	JSR SRAM_IDCommand
	BCS sramserv_exit

if sram=104
	ASL A
	BEQ sramserv_exit

	TAX
	LDA sramcmd_ADDR-2,X
	STA &B0
	LDA sramcmd_ADDR-1,X
	STA &B1
	JSR jumpB0
	JMP sramserv_returnA_0
else
	CPX #&04
	BEQ sramserv_exit

	JSR sramserv_callcommandroutine
	JMP sramserv_returnA_0

.sramserv_callcommandroutine
	LDA sramcmd_SRAM,X		;Return to SRAM routine
	PHA 
	LDA sramcmd_SRAM+1,X
	PHA 
	RTS
endif

.sramserv_call07
	CMP #&07			;Unrecognised OSBYTE
	BNE sramserv_call09		;If not Service &07

	LDA &EF
	CMP #&44
	BNE sramserv_ob45		;If not OSBYTE &44

	LDY #&EE			;Test for Sideways Ram presence

.sramserv_obcall
	LDA (&B8),Y			;A=PWSP?&EE
	AND #&3F
if sram=105
	STA &B0
	LDY #&FE
	LDA (&B8),Y			;A=PWSP?&FE
	AND #&08
	ASL A
	ASL A
	ASL A
	ASL A
	ORA &B0
endif
	STA &F0				;A=Value to return in X
	JMP sramserv_returnA_0

.sramserv_ob45
	CMP #&45
	BNE sramserv_exit		;If not OSBYTE &45

	LDY #&FD
	BNE sramserv_obcall		;always

.sramserv_call09
	CMP #&09			;*HELP
	BNE sramserv_exit		;If not Service &09

	LDA #&0D
	JSR SRAM_FindNextChrA
	BCS Label_B297_loop		;If not end of line

	LDX #&00

.sramserv_helploop
	LDA sram_banner,X
	JSR OSWRCH
	INX 
	CPX #&14
	BNE sramserv_helploop

.sramserv_jmpexit
	JMP sramserv_exit

.sram_banner
	EQUS 10, "SRAM "
if sram=104
	EQUS "1.04"
else
	EQUS "1.05"
endif
	EQUS 13, 10
	EQUS "  SRAM", 13, 10

.Label_B297_loop
	JSR SRAM_IDCommand		;Is "SRAM" in the *HELP parameters
if sram=104
	BCC LABEL_AC8F

	LDA (TextPointer),Y
	CMP #&2E
	BNE sramserv_jmpexit

	JSR SUB_B4B5
	JMP LABEL_AC92

.LABEL_AC8F
	TAX
	BNE sramserv_jmpexit

.LABEL_AC92
	JSR SRAM_EndofStr
	BCS sramserv_jmpexit
else
	CPX #&04
	BEQ Label_B2B2_helpsram		;If "SRAM"

	LDA #&00

.Label_B2A0_loop
	TAX 				;X=last chr
	INY 
	LDA (TextPointer),Y
	CMP #&0D
	BEQ sramserv_jmpexit		;If end of string

	CMP #&20
	BEQ Label_B2A0_loop		;If " "

	CPX #&20
	BNE Label_B2A0_loop		;If last chr not space
	BEQ Label_B297_loop		;always
endif

.Label_B2B2_helpsram
	LDY #&00			;*HELP SRAM

.Label_B2B4_loop1
	LDA sram_helpstr,Y
	JSR OSWRCH
	INY 
	BNE Label_B2B4_loop1

.Label_B2BD_loop2
	LDA sram_helpstr+&100,Y
if sram=105
	BEQ sramserv_jmpexit
endif

	JSR OSWRCH
	INY
if sram=104
	CPY #&3B
endif
	BNE Label_B2BD_loop2		;always
if sram=104
	BEQ sramserv_jmpexit
endif
}

if sram=104
.jumpB0	JMP (&B0)
	RTS
endif

.SRAM_OSWORD_LOADSAVE
{
	JSR SRAM_SetB8PtrToPWsp		;OSWORD Load/Save
	LDA #&EE			;FO->Control Block
	STA &B8
	LDY #&0B

.Label_B2D1loop
	LDA (&F0),Y
	CPY #&00
	BNE Label_B2E1

	AND #&C0
	STA &BC
	LDA (&B8),Y
	AND #&3F			;Set PWSP?&EE op bits (6&7)
	ORA &BC

.Label_B2E1
	STA (&B8),Y
	DEY 
	BPL Label_B2D1loop

	CLI
}
 
.SRAM_LOADSAVE
if sram=104
	JSR SRAM_SetB8PtrToPWsp
	LDA #&00
	TAY
	LDX #&BA
	JSR OSARGS
	PHA
endif
	JSR SRAM_SetB8PtrToPWsp
	LDY #&F2			;AX=Start Address
	LDA (&B8),Y
	TAX 
	INY 
	LDA (&B8),Y
	JSR SRAM_BIT_PWSP_EE
	BVS Label_B305			;If no rom ID given

	LDY #&F1
	LDA (&B8),Y
	TAY 
	CPY #&14			;Y=rom id
	BCC Label_B314			;If Y<&14

.SRAM_ERR_IllegalParameter
	LDX #&00			;Illegal parameter
	JMP SRAM_ReportErrorX

.Label_B305
	JSR SRAM_PseudoAddr
	STY &BA				;Update address
	LDY #&F3
	STA (&B8),Y
	TXA 
	DEY 
	STA (&B8),Y
	LDY &BA

.Label_B314
	JSR SRAM_CheckROMID		;A=0/FF
	TAX 				;Save A
	TYA 				;Set ROM Nr
	LDY #&F1
	STA (&B8),Y
	TXA 				;Restore A
	LDY #&EE
	EOR (&B8),Y
	AND #&40
	BNE SRAM_ERR_IllegalParameter	;If trying to LOAD to "SRDATA"

if sram=104
	PLA
	TAX
else
	TAY 				;Y=0=file handle
	LDX #&BA			;X=ZP control block
	JSR OSARGS
	JSR SRAM_SetB8PtrToPWsp
	TAX 				;X=current filing system
endif
	BNE Label_B337			;If >0

	LDX #&02			;No filing system
	JMP SRAM_ReportErrorX

.Label_B337
	PHA 
	JSR SRAM_SetROMInfo
	PLA 
	CMP #&04
	BCS Label_B3BA			;If FS>=4 e.g.DFS

	JSR SRAM_BIT_PWSP_EE
	BPL Label_B375_save		;If SAVING

	JSR SRAM_OpenFileForInput	;LOAD
	JSR SRAM_CheckEOF
	BNE Label_B371_CLOSEFILE	;If EOF
	BEQ Label_B352_loadentry	;always

.Label_B34F_loadloop
	JSR SRAM_IncAddress

.Label_B352_loadentry
	LDY #&FA
	LDA (&B8),Y
	TAY 				;Y=filehandle
	JSR OSBGET
	JSR SRAM_SetB8PtrToPWsp
	LDY #&F2			;Copy Address to BA
	JSR SRAM_CopyWordfromPWSP
	TAX 				;Save A
	LDY #&F1
	LDA (&B8),Y
	TAY 				;Y=rom no
	TXA 				;Restore A
	JSR SRAM_WriteSWRam
	JSR SRAM_CheckEOF
	BEQ Label_B34F_loadloop

.Label_B371_CLOSEFILE
	JSR SRAM_CloseFile
	RTS

.Label_B375_save
	JSR SRAM_OpenFileForOutput	;SAVE
	JSR SRAM_FileLenIs0
	BEQ Label_B371_CLOSEFILE
	BNE Label_B382_saveentry	;always

.Label_B37F_saveloop
	JSR SRAM_IncAddress

.Label_B382_saveentry
	LDY #&F4
	LDA (&B8),Y
	SEC 
	SBC #&01
	STA (&B8),Y
	CMP #&FF
	BNE Label_B397

	INY 
	LDA (&B8),Y
	SEC 
	SBC #&01
	STA (&B8),Y

.Label_B397
	LDX #&F6
	LDY #&F2
	JSR SRAM_CopyWordfromPWSPX
	LDY #&F1
	LDA (&B8),Y
	TAY 
	JSR OSRDRM
	TAX 
	LDY #&FA
	LDA (&B8),Y
	TAY 
	TXA 
	JSR OSBPUT
	JSR SRAM_SetB8PtrToPWsp
	JSR SRAM_FileLenIs0
	BNE Label_B37F_saveloop
	BEQ Label_B371_CLOSEFILE	;always

.Label_B3BA
	LDY #&F9
	LDA (&B8),Y
	BMI Label_B3DE			;If buf size>=32k use all ram
	DEY 
	ORA (&B8),Y
	BNE Label_B406			;If buffer size>0
	LDA #&00			;Set buffer size = &100
	STA (&B8),Y
	LDA #&01
	INY 
	STA (&B8),Y
	LDY #&F6			;Buffer start = PSWP+&100
	LDA #&00
	STA (&B8),Y
	LDX &B9
	INX 
	TXA 
	INY 
	STA (&B8),Y
	JMP Label_B406

.Label_B3DE
	LDA #&84			;Use all free ram for buffer
	JSR OSBYTE
	TYA 				;YX=HIMEM
	PHA 
	TXA 
	PHA 
	LDA #&83
	JSR OSBYTE			;YX=PAGE
	JSR SRAM_SetB8PtrToPWsp
	STX &BA
	STY &BB
	LDY #&F6			;Buffer start=PAGE
	JSR SRAM_CopyWordtoPWSP
	PLA 				;Buffer size=HIMEM-PAGE
	SEC 
	SBC &BA
	LDY #&F8
	STA (&B8),Y
	PLA 
	SBC &BB
	INY 
	STA (&B8),Y

.Label_B406
	JSR SRAM_BIT_PWSP_EE
	BMI Label_B40E			;If LOADING
	JMP Label_B4D8_save

.Label_B40E
	JSR SRAM_OpenFileForInput	;LOADING
	TSX 
	TXA 
	SEC 
	SBC #&10
	TAX 
	TXS 				;S=S-&10
	LDY #&F0			;Filename
	LDA (&B8),Y
	PHA 
	DEY 
	LDA (&B8),Y
	PHA 
	DEX 
	LDY #&01			;YX=&100+S
	LDA #&05
	JSR OSFILE			;Get file's cat info
	JSR SRAM_SetB8PtrToPWsp
	TSX 
	LDA &010B,X			;Get file length
	LDY #&F4			;Word BA=filelen
	STA (&B8),Y
	STA &BA
	LDA &010C,X
	INY 
	STA (&B8),Y
	STA &BB
	LDA &010D,X
	ORA &010E,X
	BEQ Label_B44B			;If filelen<&10000

.SRAM_ERROR_IllegalAddress
	LDX #&01			;Illegal address
	JMP SRAM_ReportErrorX

.Label_B44B
if sram=105
	LDX #&12			;Discard control block
endif

.Label_B44D_loop
if sram=104
	JSR SUB_B23D
else
	PLA 
	DEX 
	BNE Label_B44D_loop
endif

	BIT SRAM_SET_V			;V=1 (SEV!)
	LDA &BA
	ORA &BB
	BNE Label_B45E_LOOP		;If filelen>0

.Label_B45A_closefile
	JSR SRAM_CloseFile
	RTS

.Label_B45E_LOOP
	LDX #&BC			;Block LOOP
	LDY #&F8
	JSR SRAM_CopyWordfromPWSPX	;Word BC=bufsize
	LDY #&BA
	JSR SRAM_CmpWordZP
	BCS Label_B470			;If bufsize>=filelen

	JSR SRAM_CopyWordZP		;filelen=bufsize
	CLV

.Label_B470
	LDY #&FB
	JSR SRAM_CopyWordtoPWSP		;bytecount=filelen(=buffsize)
	BVC Label_B4AF			;If bufsize<filelen

	JSR SRAM_CloseFile
	TSX 				;Build OSFILE control block
	TXA 
	SEC 
	SBC #&0B
	TAX 
	TXS 
	LDA #&00
	PHA 
	LDA #&FF
	PHA 
	PHA 
	LDY #&F7
	LDA (&B8),Y
	PHA 
	DEY 
	LDA (&B8),Y
	PHA 
	LDY #&F0
	LDA (&B8),Y
	PHA 
	DEY 
	LDA (&B8),Y
	PHA 
	TSX 
	INX 
	LDY #&01
	LDA #&FF
	JSR OSFILE			;Load file to buffer
	JSR SRAM_SetB8PtrToPWsp
if sram=104
	JSR SUB_B23D
else
	LDX #&12			;Discard control block

.Label_B4A8_loop
	PLA 
	DEX 
	BNE Label_B4A8_loop
endif
	JMP Label_B4B4

.Label_B4AF
	LDA #&04			;File larger than buffer size
	JSR SRAM_GBPB_A_call		;A=4=get bytes ignore new ptr

.Label_B4B4
	LDX #&B1			;Word B1=Bufstart
	LDY #&F6
	JSR SRAM_CopyWordfromPWSPX
	LDX #&B3			;Word B3=Addr
	LDY #&F2
	JSR SRAM_CopyWordfromPWSPX
	LDX #&BE			;Word BE=Bytecount
	LDY #&FB
	JSR SRAM_CopyWordfromPWSPX
	SEC 
	JSR SRAM_MoveBlock
	LDX #&B3
	JSR SRAM_Len_ByteCount		;Len-=bytecount
	BEQ Label_B45A_closefile	;If len=0

	CLV 
	JMP Label_B45E_LOOP

.Label_B4D8_save
	JSR SRAM_OpenFileForOutput	;SAVING
	BIT SRAM_SET_V
	PHP 

.Label_B4DF_LOOP
	LDY #&F4			;BA= filelen
	JSR SRAM_CopyWordfromPWSP
	JSR SRAM_FileLenIs0
	BNE Label_B4EE			;If filelen>0

.Label_B4E9_closefile
	PLP 
	JSR SRAM_CloseFile
	RTS 

.Label_B4EE
	LDX #&BC
	LDY #&F8			;BC= bufsize
	JSR SRAM_CopyWordfromPWSPX
	LDY #&BA
	JSR SRAM_CmpWordZP
	BCS Label_B502			;If bufsize>=filelen

	JSR SRAM_CopyWordZP		;len=bufsize
	PLP 
	CLV 				;V=0=Using GBPB
	PHP 

.Label_B502
	LDY #&FB
	JSR SRAM_CopyWordtoPWSP		;bytecount = len (=bufsize)
	LDX #&B1
	LDY #&F2
	JSR SRAM_CopyWordfromPWSPX	;B1= address
	LDX #&B3
	LDY #&F6
	JSR SRAM_CopyWordfromPWSPX	;B3= bufstart
	LDX #&BE
	LDY #&FB
	JSR SRAM_CopyWordfromPWSPX	;BE= bytecount
	CLC 
	JSR SRAM_MoveBlock
	LDX #&B1
	JSR SRAM_Len_ByteCount		;len-=bytecount
	PLP 
	PHP 
	BVS Label_B534			;If not using GBPB

	LDA #&02			;A=2=Put bytes ignore seq ptr
	JSR SRAM_GBPB_A_call
	PLP 				;? Already clear
	CLV 				;?
	PHP 				;?
	JMP Label_B4DF_LOOP

.Label_B534
	JSR SRAM_CloseFile		;Use osfile instead
	LDA #&FF			;Buffer start+bytecount
	PHA 
	PHA 
	LDY #&F6
	JSR SRAM_CopyWordfromPWSP
	LDY #&FB
	LDA &BA
	CLC 
	ADC (&B8),Y
	TAX 
	LDA &BB
	INY 
	ADC (&B8),Y
	PHA 
	TXA 
	PHA 
	LDA #&FF			;Buffer start
	PHA 
	PHA 
	LDA &BB
	PHA 
	LDA &BA
	PHA 
	LDA #&FF			;Load/Exec addr = 0
	LDX #&08

.Label_B55E_LOOP
	PHA 
	DEX 
	BNE Label_B55E_LOOP

	LDY #&F0			;Filename
	LDA (&B8),Y
	PHA 
	DEY 
	TSX 
	LDA (&B8),Y
	PHA 
	LDY #&01
	LDA #&00			;Save block
	JSR OSFILE
	JSR SRAM_SetB8PtrToPWsp

if sram=104
	JSR SUB_B23D
else
	LDX #&12			;Discard control block

.Label_B578_LOOP
	PLA 
	DEX 
	BNE Label_B578_LOOP
endif

	JMP Label_B4E9_closefile

.SRAM_SET_V
	RTI 				;Used to set V flag

.SRAM_CopyWordZP
	LDA &00,X
	STA &0000,Y
	LDA &01,X
	STA &0001,Y
	RTS

.SRAM_CmpWordZP
{
	LDA &01,X
	CMP &0001,Y
	BNE Label_B597

	LDA &00,X
	CMP &0000,Y

.Label_B597
	RTS
}

.SRAM_GBPB_A_call
{
	PHA 				;Build OSGBPB control block
	PHA 				;Seq Pointer
	PHA 
	PHA 
	LDA #&00			;Number of bytes
	PHA 
	PHA 
	LDY #&FC
	LDA (&B8),Y
	PHA 
	DEY 
	LDA (&B8),Y
	PHA 
	LDA #&FF			;Address: Buffer start
	PHA 
	PHA 
	LDY #&F7
	LDA (&B8),Y
	PHA 
	DEY 
	LDA (&B8),Y
	PHA 
	TSX 
	LDY #&FA			;File hanlde
	LDA (&B8),Y
	PHA 
	LDA &0109,X			;Value of A on entry
	LDY #&01
	JSR OSGBPB
	LDX #&0D			;Discard ctrl block
if sram=104
	JSR SUB_B23F
else

.Label_B5C6
	PLA 
	DEX 
	BNE Label_B5C6
endif

	JSR SRAM_SetB8PtrToPWsp
	RTS
}

.SRAM_OpenFileForInput
	LDA #&40			;Open file for input

.Label_B5D0
{
	PHA 
	LDY #&EF
	LDA (&B8),Y
	TAX 				;X-PWSP?&EF
	INY 
	LDA (&B8),Y
	TAY 				;Y=PWSP?&F0
	PLA 
	JSR OSFIND
	TAX 				;X=filehandle
	BNE Label_B5E6

	LDX #&04			;File not found
	JMP SRAM_ReportErrorX

.Label_B5E6
	LDY #&FA
	JSR SRAM_SetB8PtrToPWsp
	STA (&B8),Y			;PWSP?&FA=file handle
}
.Label_B5ED_rts
	RTS

.SRAM_OpenFileForOutput
	LDA #&80			;Open file for output
	BNE Label_B5D0			;always

.SRAM_CloseFile
	LDY #&FA
	LDA (&B8),Y			;A=filehandle
	BEQ Label_B5ED_rts		;If file not open

	PHA 
	LDA #&00			;Close file
	STA (&B8),Y
	PLA 
	TAY 
	LDA #&00
	JSR OSFIND
	JMP SRAM_SetB8PtrToPWsp

.SRAM_CheckEOF
	LDY #&FA
	LDA (&B8),Y
	TAX 				;X=filehandle
	LDA #&01
	JSR Label_B86F_jmp_FSCV_	;EOF?
	JSR SRAM_SetB8PtrToPWsp
	TXA 
	RTS

.SRAM_FileLenIs0
	LDY #&F4
	LDA (&B8),Y
	INY 
	ORA (&B8),Y
	RTS

.SRAM_Len_ByteCount
{
	STX &BC
	LDY #&FB
	JSR SRAM_CopyWordfromPWSP	;Word BA=bytecount
	LDY #&F4
	LDA (&B8),Y			;Length=Length-bytecount
	SEC 
	SBC &BA
	STA (&B8),Y
	STA &BA
	INY 
	LDA (&B8),Y
	SBC &BB
	STA (&B8),Y
	STA &BB
	ORA &BA
	BEQ Label_B65E_exit		;If length=0

	LDX &BC
	JSR SRAM_BIT_PWSP_EE
	BVC Label_B655			;If Absolute address

	LDA &01,X			;Pseudo address
	CMP #&C0
	BCC Label_B655

	LDA #&10
	STA &00,X
	LDA #&80
	STA &01,X
	JSR SRAM_PseudoNextROM

.Label_B655
	LDX &BC
	LDY #&F2
	JSR SRAM_CopyWordtoPWSPX
	LDA #&FF

.Label_B65E_exit
	RTS
}

.SRAM_IncAddress
{
	LDX #&BD
	LDY #&F2
	JSR SRAM_CopyWordfromPWSPX	;Copy addr to BD
	INC &BD
	BNE Label_B684

	INC &BE				;Inc address
	BEQ Label_B68C_illegaladdress

	JSR SRAM_BIT_PWSP_EE
	BVC Label_B684			;If ROM ID given

	LDA &BE
	CMP #&C0
	BNE Label_B684			;If addr<>&C000

	JSR SRAM_PseudoNextROM

if sram=105
	LDA #&10
	STA &BD
	LDA #&80
	STA &BE				;addr=&8010
endif

.Label_B684
if sram=104
	LDA &BD
	LDY #&F2
	STA (&B8),Y
	LDA &BE
	INY
	STA (&B8),Y
else
	LDX #&BD
	LDY #&F2
	JSR SRAM_CopyWordtoPWSPX	;Copy addr from BD
endif
	RTS
}

.Label_B68C_illegaladdress
	JMP SRAM_ERROR_IllegalAddress

.SRAM_PseudoNextROM
{
	LDY #&F1			;Next ROM block
	LDA (&B8),Y			;A=ROM nr
	CLC 
	ADC #&01
if sram=104
	CMP #&08
else
	CMP #&10
endif
	BCS Label_B68C_illegaladdress	;If >=

if sram=105
	CMP #&02			;0,1 -> 2
	BEQ Label_B68C_illegaladdress	;If >=

	CMP #&0E			;C,D -> E
	BNE Label_B6A6

	LDY #&FE			;?
	AND (&B8),Y

.Label_B6A6
endif
	LDY #&F1
	STA (&B8),Y			;Set ROM
	TAY 
	JSR SRAM_CheckROMID		;Check SRDATA rom
	BEQ Label_B68C_illegaladdress

	RTS
}

.SRAM_PseudoAddr
{
	LDY #&10			;A=startaddr page

.Label_B6B3_loop
	CMP Label_B6CA,Y		;3F,7F,BF,FF
	BCC Label_B6CA			;If <
	BNE Label_B6C2

	PHA 
	TXA 				;X=startaddr page offset
	CMP Label_B6D6-&10,Y		;F0,E0,D0,F0
	PLA 
	BCC Label_B6CA			;If <

.Label_B6C2
	INY 
	CPY #&14
	BCC Label_B6B3_loop

	JMP SRAM_ERROR_IllegalAddress

.Label_B6CA
	PHA 				;AX=AX+IJ
	TXA 
	CLC 
	ADC Label_B6DE-&10,Y		;J=10,20,30,40
	TAX 
	PLA 

.Label_B6D2
	ADC Label_B6D2,Y		;I=80,40,00,C0
	RTS

.Label_B6D6	
	EQUB  &F0, &E0, &D0, &C0, &3F, &7F, &BF, &FF
.Label_B6DE
	EQUB  &10, &20, &30, &40, &80, &40, &00, &C0
}

.SRAM_PseudoToReal
{
	CPY #&10
	BCS Label_B6EB			;If >=&10

	RTS 

.Label_B6EB
	TYA 				;Y=Y-4
	SEC 
if sram=104
	SBC #&0C
	TAY
else
	SBC #&04
	TAY 
	CPY #&0E
	BCS Label_B6F5			;Y>=&E (Y was >=&12)
endif

	RTS 				;Y=&C or Y=&D

.Label_B6F5
	TYA 
	AND #&01
	LDY #&FE			;?
	ORA (&B8),Y
	TAY 
	RTS
}

.SRAM_CheckROMID
{
	JSR SRAM_PseudoToReal
	TYA 
	TAX 				;Save Y
	LDA SRAM_ROMBit_Table,Y
	PHA 
	LDY #&EE
	AND (&B8),Y
	BNE Label_B718			;If SRAM
	LDA (&B8),Y
	AND #&C0
	CMP #&80			;Load with given ROM ID
	BEQ Label_B718
	JMP SRAM_ERROR_IllegalAddress

.Label_B718
	PLA 
	LDY #&FD
	AND (&B8),Y			;SRData SRAM?
	PHA 
	TXA 				;Restore Y
	TAY 
	PLA 
	BEQ Label_B725
	LDA #&FF
.Label_B725
	RTS
}

.SRAM_ROMBit_Table
if sram=104
	EQUB  &00, &00, &00, &00, &01, &02, &04, &08
	EQUB  &00, &00, &00, &00, &00, &00, &00, &00
else
	EQUB  &04, &08, &00, &00, &00, &00, &00, &00
	EQUB  &00, &00, &00, &00, &01, &02, &10, &20
endif

.wrtswramX2Y
	STY PagedRomSelector_RAMCopy	;Copied to PWSP+&100
	STY PagedRomSelector		;Write byte in sideways ram/rom
	LDY #&00
	STA (&BA),Y
	STX PagedRomSelector_RAMCopy
	STX PagedRomSelector
	RTS

.SRAM_WriteSWRam
{
	STA &BF
	TXA 				;Save A,X,Y
	PHA 
	TYA 
	PHA 
	LDX PagedRomSelector_RAMCopy
	LDA PagedROM_PrivWorkspaces,X
	STA &B1
	INC &B1
	LDA #&00
	STA &B0				;(B0)->PWSP+&100
	LDY #&0E			;Copy routine wrtsram

.Label_B75A_loop
	LDA wrtswramX2Y,Y
	STA (&B0),Y
	DEY 
	BPL Label_B75A_loop

	PLA 
	PHA 
	TAY 				;Get Y = target rom
	LDA &BF				;Get A = value
	JSR jumpB0
	PLA 				;Restore A,X,Y
	TAY 
	PLA 
	TAX 
	LDA &BF
	RTS

if sram=105
.jumpB0
	JMP (&00B0)
endif
}

.Label_B774_copyblock
{
	STA PagedRomSelector_RAMCopy	;Copied to stack page
	STA PagedRomSelector

.Label_B779
	LDA (&B1),Y
	STA (&B3),Y
	INY 
	BNE Label_B785
	INC &B2
	INC &B4
	DEX 

.Label_B785
	CPY &B5
	BNE Label_B779			;If no last byte

	TXA 
	BNE Label_B779			;If not last page

	PLA
	STA PagedRomSelector_RAMCopy
	STA PagedRomSelector
if sram=104
	JMP LABEL_B182
else
	JMP Label_B7D5
endif
}


if sram=104
.Label_B7B6_moveblock			;B163
{
	LDA &B5
	ORA &B6
	BEQ LABEL_B198

	LDX #&20

.LABEL_B16B
	LDA Label_B774_copyblock,X
	PHA 
	DEX 
	BPL LABEL_B16B

	TSX 
	LDA PagedRomSelector_RAMCopy
	PHA 
	LDA #&01
	PHA 
	TXA 
	PHA 
	LDA &B0
	LDX &B6
	LDY #&00
	RTS 
}

.LABEL_B182
{
	LDX #&21
	JSR SUB_B23F
	LDX #&02

.LABEL_B189
	LDA &B1,X
	CLC 
	ADC &B5
	STA &B1,X
	BCC LABEL_B194

	INC &B2,X

.LABEL_B194
	DEX 
	DEX 
	BPL LABEL_B189
}

.LABEL_B198
	RTS 
endif

.SRAM_MoveBlock
	LDY #&F1			;Buffer <-> ROM
	LDA (&B8),Y
	STA &B0				;BO=ROM Nr
	LDX &BE				;YX=bytecount
	LDY &BF
	LDA #&00
	ROL A
	ASL A
	STA &B7
	INC &B7				;B7= (C=0) 1 or (C=1) 3
	JSR SRAM_BIT_PWSP_EE
	BVS Label_B7ED_pseudo		;If pseudo addressing
	BVC Label_B7B2			;always

.Label_B7AE
	LDX &BE
	LDY &BF

.Label_B7B2
	STX &B5				;B5=bytecount
	STY &B6

if sram=104
	JMP Label_B7B6_moveblock
else
.Label_B7B6_moveblock
{
	LDA &B5
	ORA &B6
	BEQ Label_B7EC_rts		;If bytecount=0

	LDX #&20			;Copy code to stack page

.Label_B7BE
	LDA Label_B774_copyblock,X
	PHA 
	DEX 
	BPL Label_B7BE

	TSX 
	LDA PagedRomSelector_RAMCopy	;Current ROM nr
	PHA 
	LDA #&01			;Push address of code in stack
	PHA 
	TXA 
	PHA 
	LDA &B0				;Get ROM nr
	LDX &B6				;Pages to copy
	LDY #&00			;Return to code in stack
	RTS
}

.Label_B7D5
{
	LDX #&21			;Jump here from code in stack

.Label_B7D7_loop
	PLA 				;Discard code in stack
	DEX 
	BNE Label_B7D7_loop

	LDX #&02			;addresses += len

.Label_B7DD_loop
	LDA &B1,X
	CLC 
	ADC &B5
	STA &B1,X
	BCC Label_B7E8

	INC &B2,X

.Label_B7E8
	DEX 
	DEX 
	BPL Label_B7DD_loop
}
.Label_B7EC_rts
	RTS
endif

.Label_B7ED_pseudo
	LDA #&00			;Move block: pseudo addressing
	SEC 
	LDX &B7				;sraddr @ B0+X
	SBC &B0,X			;Word B5=&C000-sraddr
	STA &B5				;=bytes in this rom
	LDA #&C0
	SBC &B1,X
	STA &B6
	LDX #&B5
	LDY #&BE
	JSR SRAM_CmpWordZP
	BCS Label_B7AE			;If wB5 >= length : only 1 rom
	LDA &BE				;len = len - bytes this rom
	SEC 
	SBC &B5
	STA &BE
	LDA &BF
	SBC &B6
	STA &BF
	JSR Label_B7B6_moveblock	;move block
	LDA #&10			;next rom
	LDX &B7
	STA &B0,X
	LDA #&80
	STA &B1,X			;dest addr=&8010
	JSR SRAM_PseudoNextROM
	LDY #&F1
	LDA (&B8),Y
	STA &B0				;?B0=rom
	JMP Label_B7ED_pseudo

.SRAM_SetB8PtrToPWsp
if sram=105
	PHP 				;Set vector &B0 to point to
endif
	PHA 				;private workspace
	TXA 
	PHA 
	LDA #&00
	STA &B8
	LDX PagedRomSelector_RAMCopy
	LDA PagedROM_PrivWorkspaces,X
	STA &B9
	PLA 
	TAX 
	PLA 
if sram=105
	PLP
endif 
	RTS

.SRAM_CopyWordfromPWSP
	LDX #&BA

.SRAM_CopyWordfromPWSPX
	PHA 
	LDA (&B8),Y
	STA &00,X
	INY 
	LDA (&B8),Y
	STA &01,X
	DEY 
	PLA 
	RTS

.SRAM_CopyWordtoPWSP
	LDX #&BA

.SRAM_CopyWordtoPWSPX
	PHA 
	LDA &00,X
	STA (&B8),Y
	INY 
	LDA &01,X
	STA (&B8),Y
	DEY 
	PLA 
	RTS

.SRAM_BIT_PWSP_EE
	PHA 
	TYA 
	PHA 
	LDY #&EE
	LDA (&B8),Y
if sram=104
	STA &F6
	PLA
	TAY
	PLA
	BIT &F6
else
	STA &B8
	PLA 
	TAY 
	PLA 
	BIT &B8
	JSR SRAM_SetB8PtrToPWsp
endif
	RTS

.Label_B86F_jmp_FSCV_
	JMP (FSCV)

if sram=104
.SUB_B23D
	LDX #&12

.SUB_B23F
	STX &F7
	PLA 
	TAY 
	PLA 
	STA &F6
	TSX 
	TXA 
	CLC 
	ADC &F7
	TAX 
	TXS 
	LDA &F6
	PHA 
	TYA 
	PHA 
	RTS 
endif

.sram_str_ROMHeader_Data		;ROM header for SRDATA
	EQUB  &60, &00, &00, &60
	EQUB  &00, &00, &02, &0C
	EQUB  &FF, "RAM"

.sram_str_Copyright
	EQUB  &00, "(C)"

.SRAM_SetupPWSP
{
	LDA #&00			;Setup private workspace
	LDY #&FD			;(PWSP+EE to PWSP+FF)
	STA (&B8),Y
	LDY #&EE
	STA (&B8),Y
	LDY #&FA
	STA (&B8),Y
	LDY #&FF
	LDA #&4E
	STA (&B8),Y
	LDY #&0F

.sram_spwsp_loop
	LDA SRAM_ROMBit_Table,Y
	BEQ Label_B8E6

	TYA 
	PHA 				;Save Y
	LDA #&08			;word &F6 = word &BA = &8008
	STA &F6				;"Binary Version"
	STA &BA
	LDA #&80
	STA &F7
	STA &BB
	JSR OSRDRM			;Read byte in paged rom at (&F6)
	STA &BD
	PLA 
	PHA 
	TAY 				;Get Y, target ROM nr
	LDA &BD
	EOR #&FF			;Try writing to "ROM"
	JSR SRAM_WriteSWRam
	JSR OSRDRM
	CMP &BD
	BEQ Label_B8E4			;If same it's a ROM

	PLA 
	PHA 
	TAX 				;X=Rom Nr
	LDY #&EE
	LDA (&B8),Y
	ORA SRAM_ROMBit_Table,X		;Set bit
	STA (&B8),Y
	TXA 
	TAY 
	LDA &BD
	JSR SRAM_WriteSWRam		;Reset byte in SW RAM
	JSR SRAM_CheckSWRAMY
	CMP #&02
	BNE Label_B8E4			;If not SRDATA

	LDA SRAM_ROMBit_Table,Y
	LDY #&FD
	ORA (&B8),Y
	STA (&B8),Y

.Label_B8E4
	PLA 				;Restore Y
	TAY 

.Label_B8E6
	DEY 
	BPL sram_spwsp_loop		;LOOP

if sram=105
	LDY #&EE
	LDA (&B8),Y
	LDX #&00
	AND #&0C
	BNE Label_B8F5

	LDX #&0E

.Label_B8F5
	TXA 
	LDY #&FE
	STA (&B8),Y
endif
	RTS
}

.SRAM_ReportErrorX
{
	LDA #&00			;Report Error X
	STA &0100
	LDA errnos,X			;Error number
	STA &0101
	LDA erroffset+1,X		;Start of next string
	STA &BF
	LDY erroffset,X			;Start of this string
	LDX #&00

.Label_B910
	LDA err1,Y			;Copy string to &102
	STA &0102,X
	INX 
	INY 
	CPY &BF
	BCC Label_B910

	LDA #&00
	STA &0102,X			;String terminator
	JMP &0100			;BREAK!

.Label_B924

.err1	EQUS "Illegal parameter"
.err2	EQUS "Illegal address"
.err3	EQUS "No filing system"
.err4	EQUS "Bad command"
.err5	EQUS "File not found"
.err6	EQUS "RAM occupied"

.erroffset
	EQUB err1-err1
	EQUB err2-err1
	EQUB err3-err1
	EQUB err4-err1
	EQUB err5-err1
	EQUB err6-err1
	EQUS erroffset-err1
	
.errnos	EQUB &80, &81, &82, &FE, &D6, &83	;Error numbers
}

.SRAM_CheckSWRAMY
{
	TXA 				;Save X & Y
	PHA 				;Y=Rom Nr
	TYA 
	PHA 
	LDX #&07
	STX &F6
	LDA #&80
	STA &F7
	JSR OSRDRM			;ROM Y  ?&8007
	STA &F6				;Copyright offset
	LDX #&00

.Label_B999_loop
	STX &BF
	PLA 
	PHA 
	TAY 				;Y=Rom Nr
	JSR OSRDRM			;Is it (C)?
	LDX &BF
	CMP sram_str_Copyright,X
	BNE Label_B9EE_resultFF		;If not (C)

	INC &F6
	BNE Label_B9AE

	INC &F7

.Label_B9AE
	INX 
	CPX #&04
	BCC Label_B999_loop

	LDA #&02			;Yes, ROM has valid (C) marker
	STA &BF				;Result 02
	LDX #&0F			;Is it a DATA SRAM?
	LDA #&80
	STA &F7				;Word &F6=&800F

.Label_B9BD_LOOP
	STX &F6
	PLA 
	PHA 
	TAY 				;Y=Rom No,
	JSR OSRDRM			;A=Rom?800F+
	LDX &F6
	CMP sram_str_ROMHeader_Data,X	;02 0C FF "RAM" 00 "(C)"?
	BNE Label_B9D9_NOTdata		;If not DATA sram

.Label_B9CC
	DEX 
	CPX #&06
	BCS Label_B9BD_LOOP		;If X>=6

.Label_B9D1
	CLC
 
.Label_B9D2_exit
	PLA 
	TAY 
	PLA 
	TAX 
	LDA &BF				;A=result: 1=SRROM 2=SRDATA
	RTS 				;0=Other Valid (C), else FF

.Label_B9D9_NOTdata
	CPX #&0A			;M (C)   Marked as ROM?
	BNE Label_B9E8_result00

	CMP #&4F			;"O"
	BNE Label_B9E8_result00

	LDA #&01			;Result 01
	STA &BF
	JMP Label_B9CC

.Label_B9E8_result00
	LDA #&00			;Valid copyright not owned by
	STA &BF				;SRAM1.05
	BEQ Label_B9D1			;always

.Label_B9EE_resultFF
	LDA #&FF			;Not valid copyright
	STA &BF
	SEC 
	BCS Label_B9D2_exit		;always
}

.SRAM_IDCommand
{
if sram=104
	JSR SRAM_paramskipspaces
	TYA 
	PHA 
	LDX #&00

.LABEL_B3CC
	BIT SRAM_SET_V

.LABEL_B3CF
	LDA (TextPointer),Y
	AND #&DF
	CMP #&0D
	BEQ LABEL_B3E5

	CMP sramcmd_SRAM,X
	BNE LABEL_B3F9

.LABEL_B3DC
	INX 
	LDA sramcmd_SRAM,X
	BEQ LABEL_B407

	INY 
	BNE LABEL_B3CF

.LABEL_B3E5
	INX 
	LDA sramcmd_SRAM,X
	BNE LABEL_B3E5

	PLA 
	PHA 
	TAY 
	INX 
	INX 
	LDA sramcmd_SRAM,X
	BNE LABEL_B3CC

	PLA 
	TAY 
	SEC 
	RTS 

.LABEL_B3F9
	LDA (TextPointer),Y
	CMP #&2E
	BNE LABEL_B40E
	BVS LABEL_B3E5

.LABEL_B401
	INX 
	LDA sramcmd_SRAM,X
	BNE LABEL_B401

.LABEL_B407
	PLA 
	LDA sramcmd_SRAM+1,X
	INY 
	CLC 
	RTS
 
.LABEL_B40E
	ORA #&20
	CMP sramcmd_SRAM,X
	BNE LABEL_B3E5
	CLV 
	BVC LABEL_B3DC

else	; sram = 105
	LDX #&00			;SRAM command?

.Label_B9F7_loop1
	TYA 
	PHA 				;Save Y

.Label_B9F9_loop2
	LDA (TextPointer),Y
	BMI Label_BA10

	CMP #&2E
	BEQ Label_BA19_fullstop		;If ="." (C=1)

	CMP #&41
	BCC Label_BA10			;If <"A"

	EOR sramcmd_SRAM,X
	AND #&DF			;ignore case
	BNE Label_BA15			;If no match

	INY 
	INX 
	BNE Label_B9F9_loop2

.Label_BA10
	LDA sramcmd_SRAM,X
	BMI Label_BA28			;If end of cmd str

.Label_BA15
	CLC 
	PLA 
	TAY 				;Restore Y
	DEY 

.Label_BA19_fullstop
	INY 

.Label_BA1A_loop
	INX 				;Ignore rest of cmd in table
	LDA sramcmd_SRAM-1,X
	BEQ Label_BA2E_notsramcmd	;If end of table
	BPL Label_BA1A_loop
	BCS Label_BA27			;If ended by full stop

	INX 
	BCC Label_B9F7_loop1		;always

.Label_BA27
	DEX 

.Label_BA28
	PLA 				;forget Y
	JSR SRAM_paramskipspaces
	CLC 				;Exit C=0 X=points to address
	RTS 				;in table

.Label_BA2E_notsramcmd
	SEC 				;Exit C=1 not sram command
	RTS
endif
}

if sram=105
.sramcmd_SRAM
	EQUS "SRAM", &FF, &FF
	EQUS "SRLOAD", HI(SRAM_SRLOAD-1), LO(SRAM_SRLOAD-1)
	EQUS "SRSAVE", HI(SRAM_SRSAVE-1), LO(SRAM_SRSAVE-1)
	EQUS "SRWRITE", HI(SRAM_SRWRITE-1), LO(SRAM_SRWRITE-1)
	EQUS "SRREAD", HI(SRAM_SRREAD-1), LO(SRAM_SRREAD-1)
	EQUS "SRDATA", HI(SRAM_SRDATA-1), LO(SRAM_SRDATA-1)
	EQUS "SRROM", HI(SRAM_SRROM-1), LO(SRAM_SRROM-1)
	EQUB &00
endif

.SRAM_GetAddressParam
{
	TXA 				;Get HEX Address parameter
	PHA 
	JSR SRAM_paramskipspaces
	LDA #&00
	STA &BC				;L=0
	STA &BD
	STA &BE
	STA &BF
	SEC 
	PHP 

.Label_BA78_LOOP
	LDA (TextPointer),Y
	CMP #&30			;If <"0"
	BCC Label_BAB4_bad

	CMP #&3A			;If <="9"
	BCC Label_BA8C

	CMP #&47
	BCS Label_BAB4_bad		;If >"F"

	CMP #&41
	BCC Label_BAB4_bad		;If <"A"

	SBC #&07

.Label_BA8C
	SEC 
	SBC #&30
	PLP 
	PHP 
	PHA 
	LDX #&04			;L=L*&10

.Label_BA94_loop
	ASL &BC
	ROL &BD
	BVC Label_BA9E

	ROL &BE
	ROL &BF

.Label_BA9E
	BCS Label_BAB0_overlfow		;If overflow

	DEX 
	BNE Label_BA94_loop

	PLA 
	ORA &BC				;L=L+A
	STA &BC
	PLP 
	CLC 
	PHP 
	INY 
	BEQ Label_BAB1
	BCC Label_BA78_LOOP

.Label_BAB0_overlfow
	PLA 

.Label_BAB1
	PLP 
	SEC 				;C=1
	PHP 

.Label_BAB4_bad
	PLP 
	PLA 
	TAX 
	RTS
}

.Label_BAB8_LOOP
	INY 				;Nothing else on command line!
if sram=104
	BNE SRAM_paramskipspaces
	JMP SRAM_ERR_BadCommand
endif

.SRAM_paramskipspaces
	LDA (TextPointer),Y
	CMP #&20			;Skip spaces
	BEQ Label_BAB8_LOOP

if sram=104
	CLC
else
	CMP #&0D			;End of line
endif
	RTS

.SRAM_EndofStr
	LDA #&0D

.SRAM_FindNextChrA
{
	PHA 
	JSR SRAM_paramskipspaces
	PLA 
	CMP (TextPointer),Y
	BNE Label_BAD0
	CLC 
	INY 
	RTS 

.Label_BAD0
	SEC 
	RTS
}

.SRAM_SUB_GetParam
{
	JSR SRAM_paramskipspaces
	TYA 
	PHA 
	CLC 				;Word PWSP?&EF=TP+Y
	ADC TextPointer			;Start of string
	LDY #&EF
	STA (&B8),Y
	LDA TextPointer+1
	ADC #&00
	INY 
	STA (&B8),Y
	PLA 
	TAY 				;Find end of parameter
	BIT SRAM_SET_V			;V=1   (No SEV command!)

.Label_BAEA_loop
	LDA (TextPointer),Y
	CMP #&20
	BEQ Label_BAFD

	CMP #&0D
	BEQ Label_BAFD

	CLV 				;V=0
	INY 
	BNE Label_BAEA_loop
}

.SRAM_ERR_BadCommand
	LDX #&03			;Bad command
	JMP SRAM_ReportErrorX

.Label_BAFD
	BVS SRAM_ERR_BadCommand		;If V=1 no parameter found
	RTS

if sram=104
.SUB_B4B5
	INY
	BEQ SRAM_ERR_BadCommand
	RTS
endif

.SRAM_GetIDParam
{
	STX &BF				;Get ID param (hex or decimal
	JSR SRAM_paramskipspaces	;value 0-F or 0-15 OR W,X,Y,Z)
if sram=104
	LDA (TextPointer),Y
endif
	LDX #&03

.Label_BB07_LOOP
	CMP Label_BB3E_ub,X
	BCS Label_BB36_exit		;If A>=

	CMP Label_BB3A_lb,X
	BCC Label_BB33_next		;If A<

	SBC Label_BB42_sbc,X
if sram=104
	JSR SUB_B4B5
else
	INY 
endif
	CMP #&01			;2 digit decimal number?
	BNE Label_BB2A_NOT1

	LDA (TextPointer),Y		;Next character
	CMP #&36
	BCS Label_BB28			;If >="6"

	CMP #&30
	BCC Label_BB28			;If <"0"

	SBC #&26			;A=&A,&B,&C,&D,&E,&F
if sram=104
	JSR SUB_B4B5
	JMP Label_BB2A_NOT1
else
	INY 
	BNE Label_BB2A_NOT1
endif

.Label_BB28
	LDA #&01

.Label_BB2A_NOT1
	PHA 
	JSR SRAM_paramskipspaces
	PLA 
	LDX &BF
	CLC 				;Rom Nr 0-&F
	RTS 				;Or W,X,Y,Z &10-&13

.Label_BB33_next
	DEX 
	BPL Label_BB07_LOOP

.Label_BB36_exit
	LDX &BF				;Restore X
	SEC 
	RTS

.Label_BB3A_lb
	EQUB &30, &41, &57, &77		;<

.Label_BB3E_ub
	EQUB &3A, &47, &5B, &7B		;>=

.Label_BB42_sbc
	EQUB &30, &37, &47
if sram=104
	EQUB &6A
else
	EQUB &67
endif
}

if sram=104
.sramcmd_SRAM
	EQUS "sram", 0, 0
	EQUS "SRlOAD", 0, 1
	EQUS "SRsAVE", 0, 2
	EQUS "SRwRITE", 0, 3
	EQUS "SRReAD", 0, 4
	EQUS "SRdATA", 0,5
	EQUS "SRRoM", 0, 6
	EQUB 0

.sramcmd_ADDR
	EQUS LO(SRAM_SRLOAD), HI(SRAM_SRLOAD)
	EQUS LO(SRAM_SRSAVE), HI(SRAM_SRSAVE)
	EQUS LO(SRAM_SRWRITE), HI(SRAM_SRWRITE)
	EQUS LO(SRAM_SRREAD), HI(SRAM_SRREAD)
	EQUS LO(SRAM_SRDATA), HI(SRAM_SRDATA)
	EQUS LO(SRAM_SRROM), HI(SRAM_SRROM)
endif

.SRAM_SRLOAD
	LDA #&C0			;*SRLOAD
	BNE Label_BB4C

.SRAM_SRSAVE
	LDA #&40			;*SRSAVE

.Label_BB4C
{
	PHA 
	JSR SRAM_SUB_GetParam		;Filename?
	CLV 
	JSR SRAM_GetAddressParam
	BCS Label_BB6F			;If error

	STY &BA
	LDX #&BC
	LDY #&F2
	JSR SRAM_CopyWordtoPWSPX	;Copy address to PWSP+&F2
	LDY &BA
	PLA 
	PHA 
	BMI Label_BB90_loading		;If loading

	LDA #&2B			;"+"
	JSR SRAM_FindNextChrA
	PHP 
	CLV 
	JSR SRAM_GetAddressParam	;Get end address

.Label_BB6F
	BCS Label_BBD0_badcmd

	STY &BA
	PLP 
	BCC Label_BB89			;If S+E
	LDY #&F2			;Calc length
	SEC 
	LDA &BC
	SEC 
	SBC (&B8),Y
	STA &BC
	INY 
	LDA &BD
	SBC (&B8),Y
	BCC Label_BBD0_badcmd

	STA &BD

.Label_BB89
	LDX #&BC
	LDY #&F4
	JSR SRAM_CopyWordtoPWSPX	;Copy length to PWSP+&F4

.Label_BB90_loading
	LDY &BA
	JSR SRAM_GetIDParam		;Get rom number
	STY &BA
	BCS Label_BBA1			;If not valid

	LDY #&F1
	STA (&B8),Y
	PLA 
	AND #&80
	PHA 

.Label_BBA1
	PLA 
	STA &BB
	LDY #&EE
	LDA (&B8),Y
	AND #&3F
	ORA &BB
	STA (&B8),Y
	LDY &BA
	JSR SRAM_paramskipspaces
if sram=104
	LDA (TextPointer),Y
endif
	AND #&DF			;ucase
	LDX #&00
	CMP #&51
	BNE Label_BBBE			;If ="Q" X=&FF else X=0

if sram=104
	JSR SUB_B4B5
else
	INY
endif
	LDX #&FF

.Label_BBBE
	JSR SRAM_EndofStr
	BCS Label_BBD0_badcmd		;If not end of string

	LDY #&F8
	LDA #&00
	STA (&B8),Y			;PWSP?&F8=0
	INY 
	TXA 
	STA (&B8),Y			;PWSP?&F9=X (0,FF)
	JMP SRAM_LOADSAVE
}

.Label_BBD0_badcmd
	JMP SRAM_ERR_BadCommand

.SRAM_SRWRITE
	LDA #&C0			;*SRWRITE
	BNE Label_BBD9

.SRAM_SRREAD
	LDA #&40			;*SRREAD

.Label_BBD9
{
	PHA				;<START ADDRESS> <END ADDRESS> <START ADDRESS> (<ROM ID>)
	BIT SRAM_SET_V
	JSR SRAM_GetAddressParam	;Get start address (normal ram)
	BCS Label_BBD0_badcmd
	LDX #&03			;!B1=start address
.Label_BBE4_loop
	LDA &BC,X
	STA &B1,X
	DEX 
	BPL Label_BBE4_loop
	LDA #&2B			;"+"
	JSR SRAM_FindNextChrA
	BCS Label_BBF3			;If "+" V=0 else V=1
	CLV 
.Label_BBF3
	JSR SRAM_GetAddressParam	;Get end address
	BCS Label_BBD0_badcmd
	BVC Label_BC13			;If not "+"
	SEC 
	LDX #&00
	SEC 
.Label_BBFE_loop
	LDA &BC,X			;!BC=!BC-!B1 = length
	SBC &B1,X
	STA &BC,X
	INX 
	TXA 
	AND #&04
	BEQ Label_BBFE_loop
	LDA &BE
	ORA &BF
	BEQ Label_BC13			;If <=&FFFF
	JMP SRAM_ERROR_IllegalAddress
.Label_BC13
	LDA &BC
	STA &B5
	LDA &BD
	STA &B6				;wB5=wBC = length
	CLV 
	JSR SRAM_GetAddressParam	;Get address in SRAM
	BCS Label_BBD0_badcmd
	JSR SRAM_GetIDParam		;Get ROM ID
	BCS Label_BC2C_noromid		;If no ROM ID
	STA &B7
	PLA 
	AND #&BF			;1011 1111 = clr bit 6
	PHA 				;=absolute address
.Label_BC2C_noromid
	JSR SRAM_EndofStr
	BCS Label_BBD0_badcmd
	PLA 
	STA &B0
	JMP SRAM_OWSORD_BLOCKXFR
}

.SRAM_SRDATA
	JSR SRAM_GetIDParam		;*SRDATA <ROM ID>

.Label_BC3A_C_1_badcmd
{
	BCS Label_BBD0_badcmd
	PHA 
	JSR SRAM_EndofStr
	BCS Label_BBD0_badcmd
	PLA 
	TAY 				;Y=rom
	JSR SRAM_CheckROMID
	BNE Label_BC64			;If already data
	JSR SRAM_CheckSWRAMY
	TAX 
	BNE Label_BC54
	LDX #&05			;RAM occupied
	JMP SRAM_ReportErrorX
.Label_BC54
	STY &BF				;OK so far
	LDA SRAM_ROMBit_Table,Y
	LDY #&FD			;Set bit in PWSP?&FD
	ORA (&B8),Y
	STA (&B8),Y
	LDY &BF
	JSR SRAM_ROMHeader_Y_RomNr
.Label_BC64
	JSR SRAM_SetROMInfo_02_Y_RomNr
	RTS
}

.SRAM_ROMHeader_Y_RomNr
{
	LDX #&0F			;Copy Data Marker to
	STX &BA				;ROM at &800F
	LDA #&80
	STA &BB				;Word &BA = &800F
.Label_BC70_LOOP
	LDA sram_str_ROMHeader_Data,X
	CPX #&01
	BNE Label_BC78
	TYA 
.Label_BC78
	JSR SRAM_WriteSWRam		;Rom No. in 8001
	DEX 				;Dec. Word &BA
	STX &BA
	BPL Label_BC70_LOOP
	RTS
}

.SRAM_SRROM
{
	JSR SRAM_GetIDParam		;*SRROM <ROM ID>
	BCS Label_BC3A_C_1_badcmd
	PHA 
	JSR SRAM_EndofStr
	BCS Label_BC3A_C_1_badcmd
	PLA 
	TAY 
	JSR SRAM_CheckROMID
	BEQ Label_BCB2			;If not data anyway
.Label_BC93
	STY &BC
	JSR SRAM_ROMHeader_Y_RomNr
	LDA #&0A
	STA &BA
	LDA #&4F			;"O"
	JSR SRAM_WriteSWRam		;Change RAM to ROM in header
	LDA SRAM_ROMBit_Table,Y		;Clear bit in PWSP?&FD
	EOR #&FF
	LDY #&FD
	AND (&B8),Y
	STA (&B8),Y
	LDY &BC
	JSR SRAM_SetROMInfo_02_Y_RomNr
	RTS 
.Label_BCB2
	JSR SRAM_CheckSWRAMY		;Check again if writable
	TAX 
	BNE Label_BC93			;If writable
	RTS
}

.SRAM_OWSORD_BLOCKXFR
{
	LDA &B0				;Set PWSP?&EE operation bits
	AND #&C0
	STA &BE
	LDY #&EE
	LDA (&B8),Y
	AND #&3F
	ORA &BE
	STA (&B8),Y
	BIT &B0
	BVS Label_BCD6_pseudo		;If pseudo addressing

	LDY &B7				;ROM ID
	CPY #&14
	BCC Label_BCE1

.Label_BCD3_ILLParam
	JMP SRAM_ERR_IllegalParameter

.Label_BCD6_pseudo
	LDX &BC
	LDA &BD
	JSR SRAM_PseudoAddr
	STX &BC
	STA &BD

.Label_BCE1
	JSR SRAM_CheckROMID		;A=00 OR FF
	PHA 
	TYA 
	LDY #&F1
	STA (&B8),Y			;PWSP?&F1=ROM NR
	PLA 
	EOR &B0
	AND #&40
	BNE Label_BCD3_ILLParam

	JSR SRAM_SetROMInfo
	JSR SRAM_TUBE_PRESENT
	BCS Label_BD1B_tubepres		;If tube present

.Label_BCF9_rw
	LDX #&B5
	LDY #&BE
	JSR SRAM_CopyWordZP		;wBE=wB5 = len
	ROL &B0
	BCC Label_BD0E_read		;If read

	LDX #&BC			;wB3=wBC = sram addr
	LDY #&B3			;C=1

.Label_BD08
	JSR SRAM_CopyWordZP
	JMP SRAM_MoveBlock

.Label_BD0E_read
	LDX #&B1
	LDY #&B3
	JSR SRAM_CopyWordZP		;wB3=wB1 = RAM start
	LDX #&BC
	LDY #&B1			;wB1=wBC = SRAM start
	BNE Label_BD08			;C=0   always

.Label_BD1B_tubepres
	LDA &B3
	AND &B4
	CMP #&FF
	BEQ Label_BCF9_rw		;If ram addr FFFFxxxx to host

	LDA &BC				;Save SRAM address
	PHA 
	LDA &BD
	PHA 
	LDX #&03

.Label_BD2B_loop
	LDA &B1,X			;!BA=!B1 = main RAM address
	STA &BA,X
	DEX 
	BPL Label_BD2B_loop

	LDX #&B5
	LDY #&F4
	JSR SRAM_CopyWordtoPWSPX	;PSWP w&F4=w&B5 = length
	BIT &B0
	BMI Label_BD40_tubewrite	;If writing
	JMP Label_BDBA_tuberead

.Label_BD40_tubewrite
	PLA 				;Writing to SRAM from TUBE ram
	STA &B4
	PLA 
	STA &B3				;wB3 = sram addr

.Label_BD46_LOOP
	LDX #&BE
	LDY #&F4
	JSR SRAM_CopyWordfromPWSPX	;wBE = PSWP wF4 = length
	LDA &BF
	BNE Label_BD7E_notlastpage	;If not last page

	LDX &BE
	BEQ Label_BDB9_RTS		;If 0 bytes left

	LDA #&00
	STA (&B8),Y
	INC &B9				;->2nd page of PWSP
	JSR SRAM_TUBE_CLAIM
	LDA #&00			;Multi-byte transfer to host
	LDX #&BA
	LDY #&00			;YX=00BA
	JSR TubeCode
if sram=104
	LDY #&0B
else
	LDY #&07
endif
	JSR Label_BE89_waitloopY	;Move block from 2ndP to buffer
if sram=104
	NOP
	NOP
endif

.Label_BD6C_loop
	LDA TUBE_R3_DATA
	STA (&B8),Y
if sram=104
	LDX #&08
else
	LDX #&03
endif
	JSR Label_BE8D_waitloopX
if sram=105
	NOP
endif 
	INY 
if sram=104
	EQUB &CC, &BE, &00		;CPY &00BE
else
	CPY &BE				;last byte?
endif
	BNE Label_BD6C_loop
	BEQ Label_BDA3_endblock		;always

.Label_BD7E_notlastpage
	LDA #&00			;wBE=&0100
	STA &BE
	LDA #&01
	STA &BF
	INC &B9				;->2nd page of PWSP
	JSR SRAM_TUBE_CLAIM
	LDA #&06			;256 byte transfer to host
	LDX #&BA
	LDY #&00
	JSR TubeCode
if sram=104
	LDY #&08
else
	LDY #&05
endif
	JSR Label_BE89_waitloopY
if sram=104
	NOP
	NOP
endif

.Label_BD99_loop
	LDA TUBE_R3_DATA		;Transfer bytes to buffer
	STA (&B8),Y
if sram=104
	JSR LABEL_B8AB
	LDA &00
else
	LDA (&B8),Y
endif
	INY 
	BNE Label_BD99_loop

.Label_BDA3_endblock
	JSR SRAM_TUBE_RELEASE
	LDX #&B8
	LDY #&B1
	JSR SRAM_CopyWordZP		;wB1=wB8 -> PWSP2
	DEC &B9				;->1st page of PWSP
	SEC 
	JSR SRAM_MoveBlock		;Move block from buffer to SRAM
	JSR Label_BE47_next256
	JMP Label_BD46_LOOP

.Label_BDB9_RTS
	RTS

.Label_BDBA_tuberead
	PLA 				;Reading from SRAM to TUBE ram
	STA &B2
	PLA 
	STA &B1				;wB1 = Sram addr

.Label_BDC0_LOOP
	LDX #&BE
	LDY #&F4
	JSR SRAM_CopyWordfromPWSPX	;wBE = PSWP wF4 = length
	LDA &BF
	BNE Label_BDD3_notlastpage	;If not last page

	LDX &BE
	BEQ Label_BDB9_RTS		;If 0 bytes left

	LDA #&01
	BNE Label_BDDD			;always

.Label_BDD3_notlastpage
	LDA #&00
	STA &BE
	LDA #&01
	STA &BF				;len=&100
	LDA #&07

.Label_BDDD
	PHA 				;A=1 / 7
	INC &B9				;PWSP 2nd page
	LDX #&B8
	LDY #&B3
	JSR SRAM_CopyWordZP		;wB3 = wB8 = buffer addr
	LDA &BE				;Save ?BE
	PHA 
	DEC &B9				;PWSP page 1
	CLC 
	JSR SRAM_MoveBlock		;Move block from SRAM to buffer
	PLA 				;Restore ?BE
	STA &BE
	PLA 
	CMP #&01
	BNE Label_BE21_notlastpage	;If not last page

	LDA #&00
	LDY #&F4
	STA (&B8),Y
	INC &B9				;PWSP page 2
	JSR SRAM_TUBE_CLAIM
	LDA #&01			;Multi byte transfer to 2nd Pro
	LDX #&BA
	LDY #&00
	JSR TubeCode
	LDY #&00

.Label_BE0E_loop

	LDA (&B8),Y
	STA TUBE_R3_DATA
if sram=104
	LDX #&08
else
	LDX #&03
endif
	JSR Label_BE8D_waitloopX
if sram=104
	NOP
endif
	INY 
if sram=105
	CPY &BE				;idle?
endif
	CPY &BE
	BNE Label_BE0E_loop		;If not last byte
	BEQ Label_BE3C_endblock		;always

.Label_BE21_notlastpage
	INC &B9				;PWSP page 2
	JSR SRAM_TUBE_CLAIM
	LDA #&07			;Transfer 256 bytes to 2nd Pro
	LDX #&BA
	LDY #&00
	JSR TubeCode
	LDY #&00

.Label_BE31_loop
	LDA (&B8),Y
	STA TUBE_R3_DATA
if sram=104
	LDX #&03

.LABEL_B845
	DEX
	BNE LABEL_B845
else
	NOP 
	NOP 
	NOP 
endif
	INY 
	BNE Label_BE31_loop

.Label_BE3C_endblock
	JSR SRAM_TUBE_RELEASE
	DEC &B9				;PWSP page 1
	JSR Label_BE47_next256
	JMP Label_BDC0_LOOP

.Label_BE47_next256
	LDX #&01			;2nd Pro Ram addr +=&100
	INC &BA,X
	BNE Label_BE52

	INX 
	CPX #&04
	BCC Label_BE47_next256		;if <4

.Label_BE52
	LDY #&F5
	LDA (&B8),Y
	SEC 
	SBC #&01
	BCC Label_BE7A_rts		;if len-&100<0 , i.e. A was 0

	STA (&B8),Y			;len = len - &100
	JSR SRAM_FileLenIs0
	BEQ Label_BE7A_rts

	LDX &B7				;X=rom id
	JSR SRAM_BIT_PWSP_EE
	BVC Label_BE7A_rts		;if absolute addressing

	LDA &B1,X
	CMP #&C0
	BCC Label_BE7A_rts		;If not &C000

	LDA #&10			;wB0=&8010
	STA &B0,X
	LDA #&80
	STA &B1,X
	JSR SRAM_PseudoNextROM

.Label_BE7A_rts
	RTS
}

.SRAM_TUBE_CLAIM
	LDA #&C8			;SRAM claiming TUBE
	JSR TubeCode
	BCC SRAM_TUBE_CLAIM

	RTS

.SRAM_TUBE_RELEASE
	LDA #&88			;SRAM releasing TUBE
	JSR TubeCode
	RTS

.Label_BE89_waitloopY
	DEY 
	BNE Label_BE89_waitloopY

	RTS

.Label_BE8D_waitloopX
	DEX 
	BNE Label_BE8D_waitloopX

	RTS

.SRAM_TUBE_PRESENT
	LDA #&EA			;Test if tube present
	LDX #&00
	LDY #&FF
	JSR OSBYTE
	CPX #&FF

.LABEL_B8AB
	RTS

.SRAM_SetROMInfo
	JSR SRAM_BIT_PWSP_EE
	BPL Label_BEC1_exit		;If save
	BVS Label_BEC1_exit		;If no ROM ID given

	LDA #&00
	PHA 
	LDY #&F1
	LDA (&B8),Y			;ROM Nr

.Label_BEAB
	PHA 
	LDA #&AA			;ROM Info Table address
	LDX #&00
	LDY #&FF
	JSR OSBYTE
	JSR SRAM_SetB8PtrToPWsp
	STX &BA
	STY &BB
	PLA 
	TAY 				;Y=Rom NR
	PLA 
	STA (&BA),Y

.Label_BEC1_exit
	RTS

.SRAM_SetROMInfo_02_Y_RomNr
	LDA #&02
	PHA 
	TYA 
	BPL Label_BEAB			;always

	\\ ***** END OF SRAM CODE
