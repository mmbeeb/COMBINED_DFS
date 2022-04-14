	\\ Acorn NFS 3.60
	\\ netsys.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

.roff	EQUS "ROFF", 0

.Data_8019
	EQUB nfs_error1-nfs_error0
	EQUB nfs_error2-nfs_error0
	EQUB nfs_error3-nfs_error0
	EQUB nfs_error4-nfs_error0
	EQUB nfs_error5-nfs_error0
	EQUB nfs_error6-nfs_error0
	EQUB nfs_error7-nfs_error0
	EQUB nfs_error8-nfs_error0

	\ This data is returned if MACHINETYPE is requested.
.Data_8021
	EQUB &01	; BBC Micro
	EQUB &00	;
	EQUB &60	; NFS software version
	EQUB &03	; i.e. 03.60

.nfs_call_table1
	EQUB LO(NFS_CALL_00-1)		; No operation
	EQUB LO(NFS_SERVICE_01-1)	; Absolute workspace claim
	EQUB LO(NFS_SERVICE_02-1)	; Private workspace claim
	EQUB LO(NFS_SERVICE_03-1)	; Auto-boot
	EQUB LO(NFS_SERVICE_04-1)	; Unrecognised command
	EQUB LO(NFS_SERVICE_05-1)	; Unrecognised interrupt
	EQUB LO(NFS_CALL_00-1)		; No SERVICE &06
	EQUB LO(NFS_SERVICE_07-1)	; Unrecognised OSBYTE
	EQUB LO(NFS_SERVICE_08-1)	; Unrecognised OSWORD
	EQUB LO(NFS_SERVICE_09-1)	; *HELP
	EQUB LO(NFS_CALL_00-1) 		; No SERVICE &0A
	EQUB LO(NFS_SERVICE_0B-1)	; NMI release
	EQUB LO(NFS_SERVICE_0C-1)	; NMI claim
	EQUB LO(NFS_CALL_0D-1)		; SERVICE &12 Initialise filing system
	EQUB LO(NFS_CALL_0E-1)
	EQUB LO(NFS_CALL_0F-1)
	EQUB LO(NFS_CALL_10-1)
	EQUB LO(NFS_CALL_11-1)
	EQUB LO(NFS_CALL_12-1)
	EQUB LO(NFS_CALL_13-1)		; FSC 0 *OPT
	EQUB LO(NFS_CALL_14-1)		; FSC 1 EOF?
	EQUB LO(NFS_CALL_15-1)		; FSC 2 */ command
	EQUB LO(NFS_CALL_16-1)		; FSC 3 Unrecognised OS command
	EQUB LO(NFS_CALL_17-1)		; FSC 4 *RUN (Same as &15)
	EQUB LO(NFS_CALL_18-1)		; FSC 5 *CAT
	EQUB LO(NFS_CALL_19-1)		; FSC 6 New filing system taking over
	EQUB LO(NFS_CALL_1A-1)		; FSC 7 File handle range
	EQUB LO(NFS_CALL_1B-1)
	EQUB LO(NFS_CALL_1C-1)
	EQUB LO(NFS_CALL_1D-1)
	EQUB LO(NFS_CALL_1E-1)
	EQUB LO(NFS_CALL_1F-1)
	EQUB LO(NFS_CALL_20-1)
	EQUB LO(NFS_CALL_21-1)		; Osbyte &32
	EQUB LO(NFS_CALL_22-1)		; Osbyte &33
	EQUB LO(NFS_CALL_23-1)		; Osbyte &34
	EQUB LO(NFS_CALL_24-1)		; Osbyte &35

.nfs_call_table2
	EQUB HI(NFS_CALL_00-1)
	EQUB HI(NFS_SERVICE_01-1)
	EQUB HI(NFS_SERVICE_02-1)
	EQUB HI(NFS_SERVICE_03-1)
	EQUB HI(NFS_SERVICE_04-1)
	EQUB HI(NFS_SERVICE_05-1)
	EQUB HI(NFS_CALL_00-1)
	EQUB HI(NFS_SERVICE_07-1)
	EQUB HI(NFS_SERVICE_08-1)
	EQUB HI(NFS_SERVICE_09-1)
	EQUB HI(NFS_CALL_00-1)
	EQUB HI(NFS_SERVICE_0B-1)
	EQUB HI(NFS_SERVICE_0C-1)
	EQUB HI(NFS_CALL_0D-1)
	EQUB HI(NFS_CALL_0E-1)
	EQUB HI(NFS_CALL_0F-1)
	EQUB HI(NFS_CALL_10-1)
	EQUB HI(NFS_CALL_11-1)
	EQUB HI(NFS_CALL_12-1)
	EQUB HI(NFS_CALL_13-1)
	EQUB HI(NFS_CALL_14-1)
	EQUB HI(NFS_CALL_15-1)
	EQUB HI(NFS_CALL_16-1)
	EQUB HI(NFS_CALL_17-1)
	EQUB HI(NFS_CALL_18-1)
	EQUB HI(NFS_CALL_19-1)
	EQUB HI(NFS_CALL_1A-1)
	EQUB HI(NFS_CALL_1B-1)
	EQUB HI(NFS_CALL_1C-1)
	EQUB HI(NFS_CALL_1D-1)
	EQUB HI(NFS_CALL_1E-1)
	EQUB HI(NFS_CALL_1F-1)
	EQUB HI(NFS_CALL_20-1)
	EQUB HI(NFS_CALL_21-1)
	EQUB HI(NFS_CALL_22-1)
	EQUB HI(NFS_CALL_23-1)
	EQUB HI(NFS_CALL_24-1)

.NFS_SERVICE_07				; Unrecognised OSBYTE
{
	LDA &EF				; C=0
	SBC #&31
	CMP #&04
	BCS Label_80E3			; If A <= &30 OR A >= &35

	TAX				; Osbyte &31 to &34
	LDA #&00
	STA &A9
	TYA
	LDY #&21			; Routines &21 to &24
	BNE DoNFSCall			; always
}


	\ *I AM <number><identifier><password>[RETURN]
	\ e.g. *I AM ROBERT ACORN[RETURN]
	\ Or to hide the password:
	\ *I AM <number><identifier>:[RETURN]
	\      <password>[RETURN]
	\ e.g. *I AM ROBERT:[RETURN]
	\      ACORN[RETURN] (password not shown on screen)
	\ The optional number is the Station ID of the fileserver.
	\ ptrBB + Y -> parameters.
.Label_8081
	INY
.Sub_8082_I_AM
{
	LDA (ptrBBL),Y
	CMP #&20
	BEQ Label_8081			; If = ' '

	CMP #&3A
	BCS Label_809D			; If > '9'

	JSR Sub_8677_DecNum
	BCC Label_8098			; If no '.'

	STA &0E01			; FS Station Net

	INY
	JSR Sub_8677_DecNum

.Label_8098
	BEQ Label_809D
	STA &0E00			; FS Station ID

.Label_809D
	JSR Sub_8D82_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05
					; On exit X = Y

.Loop_80A0
	DEY
	BEQ Sub_80C5_CommandLine	; Exit loop, Y = 0 :. FS Function 0 = Command line

	LDA &0F05,Y
	CMP #&3A
	BNE Loop_80A0			; If <> ':'

	JSR OSWRCH			; Prompt

.Loop_80AD				; Get password
	JSR Sub_84A1_Test_ESCAPE
	JSR OSRDCH
	STA &0F05,Y
	INY
	INX
	CMP #&0D
	BNE Loop_80AD			; If <> CR

	JSR OSNEWL
	BNE Loop_80A0
}

.Sub_80C1_CommandLine
	JSR Sub_8D82_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05
	TAY				; Y = 0 :. FS Function 0 = Command line

.Sub_80C5_CommandLine
	JSR Sub_83C7_Transaction	; Y = FS function

	LDX &0F03
	BEQ Return_80F6			; If X = 0 then successful so exit

	LDA &0F05
	LDY #&17
	BNE DoNFSCall			; always


.NFS_FSCV_ENTRY
	JSR Sub_8649_SetPointers	; setup pointers
	CMP #&08
	BCS Return_80F6			; if >=8 exit

	TAX
	TYA
	LDY #&13			; Calls &13 to &1A
	BNE DoNFSCall			; always (C=0)

.NFS_LANGUAGE_ENTRY
	CPX #&05

.Label_80E3
	BCS Return_80F6			; exit if X>=5

	LDY #&0E			; Calls &0E to &12

.DoNFSCall
	INX				; Call routine X+Y, C Preserved
	DEY
	BPL DoNFSCall

	TAY
	LDA nfs_call_table2-1,X		; Push address of routine-1 on to stack and 'return' to it
	PHA
	LDA nfs_call_table1-1,X
	PHA
	LDX ptrBBL

.Return_80F6
.NETV_CALL_0
.NFS_CALL_00
	RTS

.NFS_SERVICE_ENTRY
	BIT &028F			; If bit 7 clear NFS has priority, else DFS has priority
	PHP				; (&28f are the start options)
	BPL START_NFS

	JSR DFS_CODE_START		; -> CHECK_DFS

.START_NFS
{
	PHA
	CMP #&01
	BNE nfs1_cont

	LDA ADLC_REG0			; Test if ADLC present?
	AND #&ED
	BNE nfs1_noECONET		; If not present

	LDA ADLC_REG1
	AND #&DB
	BEQ nfs1_cont			; If present

.nfs1_noECONET
	ROL PagedROM_PrivWorkspaces,X	; Set bit 7 if ADLC NOT present
	SEC
	ROR PagedROM_PrivWorkspaces,X

.nfs1_cont
	LDA PagedROM_PrivWorkspaces,X
	ASL A
	PLA				; A=service call number
	BMI SERVICEFE_TUBEPostInit	; branch if service call >=&80
	BCS Label_8191			; branch if Bit 7 set = no ADLC
}

.SERVICEFE_TUBEPostInit
{
	CMP #&FE			; TUBE sys post init
	BCC NFS_SERVICE_12
	BNE SERVICEFF_TUBEInit

	CPY #&00
	BEQ NFS_SERVICE_12

	LDX #&06			; SERVICE CALL &FE
	LDA #&14
	JSR OSBYTE			; Enable ESCAPE pressed event

.servFE_tubemsg_loop
	BIT TUBE_R1_STATUS		; Print TUBE start up message
	BPL servFE_tubemsg_loop

	LDA TUBE_R1_DATA
	BEQ label_8181

	JSR OSWRCH
	JMP servFE_tubemsg_loop

.SERVICEFF_TUBEInit
	LDA #&AD
	STA &0220			; EVNTV=06AD
	LDA #&06
	STA &0221
	LDA #&16
	STA &0202			; BRKV=&0016
	LDA #&00
	STA &0203
	LDA #&8E
	STA TUBE_R1_STATUS
	LDY #&00			; COPY TUBE CODE TO &400

.servFF_copytubecode_loop
	LDA TUBE_CODE_400,Y
	STA &0400,Y
	LDA TUBE_CODE_400+&100,Y
	STA &0500,Y
	LDA TUBE_CODE_400+&200,Y
	STA &0600,Y
	DEY
	BNE servFF_copytubecode_loop

	JSR &0421			; CALL TUBE CODE
	LDX #&60			; Copy error handling code

.servFF_copytubezpcode_loop
	LDA TUBE_ZP_CODE_0016,X
	STA &16,X
	DEX
	BPL servFF_copytubezpcode_loop

.label_8181
	LDA #&00			; lang routine 1 => RTS
}

.NFS_SERVICE_12
	CMP #&12			; &12 = Initialise filing system; Y=filing system number
	BNE NFS_SERVICE_OTHER
	CPY #&05			; 5 = NFS
	BNE NFS_SERVICE_OTHER		; If not NFS
	LDA #&0D
	BNE Label_8193			; always

.NFS_SERVICE_OTHER
	CMP #&0D
.Label_8191
	BCS label_81af			; If A >= &0D

.Label_8193
	TAX
	LDA &A9
	PHA
	LDA &A8
	PHA
	STX &A9
	STY &A8
	TYA
	LDY #&00
	JSR DoNFSCall			; X = ?&A9 = Service Call Nr (A), A = ?&A8 = Y on entry
	LDX &A9
	PLA
	STA &A8
	PLA
	STA &A9
	TXA
	LDX PagedRomSelector_RAMCopy

.label_81af
	PLP				; BIT &028F ?
	BMI Label_81E9			; If bit 7 set exit because
	JMP DFS_CODE_START		; the DFS routine has already been called


	\* Trap *ROFF and *NET commands.
	\* (See NFS_CALL_16 for other commands.)
.NFS_SERVICE_04				; Unrecognised command
	LDX #&0C
	JSR Sub_8362_NFS_or_ROFF
	BNE Label_81EA			; Not '*ROFF'

.NFS_CALL_24				; Osbyte &35
	LDY #&04
	LDA (ptr9CL_PWS0),Y
	BEQ Label_81E3

	LDA #&00
	TAX
	STA (ptr9CL_PWS0),Y
	TAY
	LDA #&C9
	JSR OSBYTE			; Enable keyboard

	LDA #&0A
	JSR Sub_90C4

.Sub_81D2
{
	STX ptr9EL_PWS1
	LDA #&CE			; R/W Econet OS call interception status

.Loop_81D6
	LDX ptr9EL_PWS1
	LDY #&7F
	JSR OSBYTE
	ADC #&01
	CMP #&D0			; &CF = Read ... and &D0 = Write Econet character status
	BEQ Loop_81D6
}

.Label_81E3
	LDA #&00
	STA &A9
	STA ptr9EL_PWS1


.Label_81E9
	RTS

.Label_81EA
	LDX #&05
	JSR Sub_8362_NFS_or_ROFF
	BNE Label_8215			; Not '*NFS'

.NFS_CALL_0D				; SERVICE &12 Initialise Filing System
{
	JSR Sub_8218_NewFS		; New fs taking over
	SEC
	ROR &A8
	JSR Sub_827B_ClaimWorkspace	; Claim vectors + static workspace (?&A8<>0)
	LDY #&1D			; Copy saved values from private workspace to &E00-&E08

.Loop_81FC
	LDA (ptr9CL_PWS0),Y
	STA &0DEB,Y
	DEY
	CPY #&14
	BNE Loop_81FC

	BEQ Sub_8264_CopyVectors	; always -> copy vectors etc.
}

.NFS_SERVICE_09				; *HELP
	JSR PrintString
	EQUS 13, "NFS 3.60", 13

.Label_8215
	LDY &A8
	RTS

	\* Tell other ROMS new FS taking over
.Sub_8218_NewFS
	LDA #&06
	JMP (FSCV)			; New FS taking over

.NFS_SERVICE_03				; AUTOBOOT
{
	JSR Sub_8218_NewFS
	LDA #&7A			; Keyboard scan
	JSR OSBYTE
	TXA
	BMI Label_8232			; No key pressed 
	EOR #&55
	BNE Label_8215			; If not "N"
	TAY
	LDA #&78
	JSR OSBYTE			; Write key

.Label_8232
	JSR PrintString
	EQUS "Econet Station "
	LDY #&14
	LDA (ptr9CL_PWS0),Y
	JSR Sub_8DBD_PrintDecimal	; Print station nr.
	LDA #&20
	BIT ADLC_REG1
	BEQ Label_825F

	JSR PrintString
	EQUS " No Clock"
	NOP

.Label_825F
	JSR PrintString
	EQUB 13,13
}

.Sub_8264_CopyVectors			; Copy vectors from table
{
	LDY #&0D
.Loop_8266
	LDA Data_829A,Y
	STA &0212,Y
	DEY
	BPL Loop_8266

	JSR Sub_8325_Set_NETV		; Setup & copy NETV extended vector

	LDY #&1B
	LDX #&07
	JSR Sub_8339_CopyVectors	; Copy rest of extended vectors
	STX &A9
}

	\* Claim static workspace and vectors
.Sub_827B_ClaimWorkspace
{
	LDA #&8F			; Issue ROM Service call (X=type)
	LDX #&0F			; Vectors claimed
	JSR OSBYTE
	LDX #&0A			; Claim Static Workspace
	JSR OSBYTE

	LDX &A8
	BNE Label_82C2			; Exit, else autoboot.

	LDX #LO(Data_8292)
	LDY #HI(Data_8292)
	JMP NFS_CALL_16			; FSC 3 Unrecognised command @ YX

.Data_8292
	EQUS "I .BOOT", 13
}

	\ Vector Table
.Data_829A
	EQUW &FF1B
	EQUW &FF1E
	EQUW &FF21
	EQUW &FF24
	EQUW &FF27
	EQUW &FF2A
	EQUW &FF2D

	\ Extended Vectors
	\ Note: 3rd byte ignored (someone's initials?)
.Data_82A8
	EQUW NFS_FILEV_ENTRY
	EQUB &4A
	EQUW NFS_ARGSV_ENTRY
	EQUB &44
	EQUW NFS_BGETV_ENTRY
	EQUB &57
	EQUW NFS_BPUTV_ENTRY
	EQUB &42
	EQUW NFS_GBPBV_ENTRY
	EQUB &41
	EQUW NFS_FINDV_ENTRY
	EQUB &52
	EQUW NFS_FSCV_ENTRY

	\ This next routine is in the middle of the table
	\ and thus must always be 7 bytes long.
.NFS_SERVICE_01				; Absolute workpace claim
	CPY #&10			; Need at least 2 pages (&0E,&0F)
	BCS Label_82C2

	LDY #&10

.Label_82C2
	RTS

	EQUW NFS_NETV_ENTRY

.NFS_SERVICE_02				; Private workspace claim
{
	STY ptr9CH_PWS0			; Y=first page available
	INY
	STY ptr9EH_PWS1

	LDA #&00
	LDY #&04
	STA (ptr9CL_PWS0),Y		; ?&9C value?

	LDY #&FF

	STA ptr9CL_PWS0
	STA ptr9EL_PWS1
	STA &A8
	STA &0D62

	TAX
	LDA #&FD
	JSR OSBYTE			; Read hard/soft BREAK
	TXA
	BEQ Label_8316			; X=0=soft

	LDY #&15
	LDA #&FE
	STA &0E00
	STA (ptr9CL_PWS0),Y

	LDA #&00
	STA &0E01
	STA ProtectionMask_D63
	STA ShowInfo
	STA &0E05

	INY
	STA (ptr9CL_PWS0),Y

	LDY #&03
	STA (ptr9EL_PWS1),Y
	DEY
	LDA #&EB
	STA (ptr9EL_PWS1),Y

.Loop_8307
	LDA &A8				; Set up control blocks
	JSR Sub_8E55			; Y=A*12
	BCS Label_8316			; A>17

	LDA #&3F			; Mark control block as empty
	STA (ptr9EL_PWS1),Y
	INC &A8
	BNE Loop_8307

.Label_8316
	LDA _INTOFF_STATIONID		; A = Station ID
	LDY #&14
	STA (ptr9CL_PWS0),Y

	JSR Sub_9633_Listen_Start	; START LISTENING!

	LDA #&40
	STA RXTX_Flags_D64
}

.Sub_8325_Set_NETV
	LDA #&A8			; Read address of extended vectors
	LDX #&00
	LDY #&FF
	JSR OSBYTE
	STX &F6
	STY &F7
	LDY #&36
	STY &0224			; NETV = &FF36
	LDX #&01			; Just copy 1 extended vector (NETV)

.Sub_8339_CopyVectors
{
.Loop_8339
	LDA Data_82A8-&1B,Y
	STA (&F6),Y
	INY
	LDA Data_82A8-&1B,Y
	STA (&F6),Y
	INY
	LDA PagedRomSelector_RAMCopy
	STA (&F6),Y
	INY
	DEX
	BNE Loop_8339

	LDY ptr9EH_PWS1
	INY
	RTS
}

.NFS_CALL_19				; FSC 6 New fs taking over
{
	LDY #&1D			; Copy &E00 to &E08 to private workspace

.Label_8353
	LDA &0DEB,Y			; &0DEB+&15=&E00
	STA (ptr9CL_PWS0),Y
	DEY
	CPY #&14
	BNE Label_8353

	LDA #&77			; Close *SPOOL and *EXEC files
	JMP OSBYTE
}

.Sub_8362_NFS_or_ROFF			; X=&05 NFS or X=&0C ROFF
{
	LDY &A8

.Loop_8364
	LDA (TextPointer),Y
	CMP #&2E			; '.'
	BEQ Loop_837D

	AND #&DF			; Upper case
	BEQ Label_8377

	CMP binversion,X		; 'NFS' or 'ROFF'
	BNE Label_8377

	INY
	INX
	BNE Loop_8364

.Label_8377
	LDA binversion,X
	BEQ Sub_837E

	RTS				; Z=0
}

.Loop_837D				; Skip trailing spaces
	INY

.Sub_837E
	LDA (TextPointer),Y
	CMP #&20
	BEQ Loop_837D

	EOR #&0D			; If CR then Z=1
	RTS

	\ Setup receive buffer
.Sub_8387_SetupBuffer
	LDA #&90

.Sub_8389_SetupBuffer
	JSR Sub_8395_SetupBuffer	; A&X preserved
	STA &C1				; Receiving Port
	LDA #&03
	STA &C4				; Buffer start : &FFFF0F03
	DEC &C0				; &7F (bit 7 cleared)
	RTS

	\ Set up buffer control block at &00C0
	\ A, X preserved, Y = &FF
.Sub_8395_SetupBuffer
{
	PHA
	LDY #&0B

.Loop_8398
	LDA Data_83AD,Y
	STA &00C0,Y
	CPY #&02
	BPL Label_83A8			; If Y >= 2

	LDA &0E00,Y			; Copy FS Station ID & Net
	STA &00C2,Y

.Label_83A8
	DEY
	BPL Loop_8398

	PLA
	RTS
}

	\ Osword &14 blank control block
.Data_83AD
	EQUB &80
	EQUB &99			; Receiving Port/Destination Port
	EQUB &00			; FS Station ID
	EQUB &00			; FS Station Net
	EQUB &00, &0F			; &FFFF0F00 (buffer start)
.Data_83B3				; This byte is BITed to set V.
	EQUB &FF, &FF
	EQUB &FF, &0F, &FF, &FF		; &FFFF0FFF (buffer end)

	\ Only called by GPBP
	\ A = Internal Handle, X = 8 bytes, Y = FS 10 / FS 11
.Sub_83B9_GPBP_Transaction
	PHA
	SEC
	BCS Label_83CF			; always

	\ Only called by FILEV
	\ Transaction, but don't set file handle of main dir
.Sub_83BD_Transaction
	CLV
	BVC Label_83CE			; always

	\ *BYE
.Sub_83C0_BYE
	LDA #&77			; Close any *SPOOL or *EXEC files
	JSR OSBYTE

	LDY #&17			; Function 23 = End Session

	\ Transmit FS Function frame, and await reply.
	\ Y = FS Function Code.
	\ X = Size of data block (<256)
.Sub_83C7_Transaction
	CLV

	\ Y = FS Function
	\ X = Size of data block
.Sub_83C8_Transaction
	LDA &0E02			; File handle of main directory
	STA &0F02

.Label_83CE
	CLC

.Label_83CF
{
	STY &0F01			; FS Function Code

	LDY #&01			; File handles of current directory and library

.Loop_83D4
	LDA &0E03,Y
	STA &0F03,Y
	DEY
	BPL Loop_83D4
}					; C = 0


	\\\ X = length of data.
	\\\ C = 1, then it's a GPBP call.
	\\\ If V=1 then &2A is added to any error code.
	\\\ Exit: Any error is reported, else exits with A=X=0
.Sub_83DD_Transaction
	PHP

	LDA #&90
	STA &0F00			; Reply Port

	JSR Sub_8395_SetupBuffer	; Setup tx control block @ &00C0
					; X preserved

	TXA				; Calc size of block (data + 5 bytes)
	ADC #&05
	STA &C8				; Buffer end = &FFFF0Faa

	PLP
	BCS Label_8408			; If GPBP

	PHP
	JSR Sub_85F7_TRANSMIT_C0	; TRANSMIT!
	PLP

	\ Wait for reply from FS on Port &90
.Sub_83F3_WaitForReply
	PHP
	JSR Sub_8387_SetupBuffer	; Setup rx control block @ &00C0 (Port = &90)
	JSR Sub_8530_PollForReply	; Exit: Y = 0
	PLP

	\ Entry: Y = 0
	\ If error received, report it!
.Sub_83FB
{
	INY
	LDA (&C4),Y			; X = Return Code
	TAX
	BEQ Label_8407			; If Return Code = 0 then no error

	BVC Label_8405

	ADC #&2A			; A += &2A + 1
					; :. for A to be 0 :
					; A = (&100-&2B) = &D5 = Object not found

.Label_8405
	BNE Sub_847A_ReportNetError	; always

.Label_8407
	RTS
}

	\ GPBP only
.Label_8408
	PLA				; A = Internal Handle

	LDX #&C0
	INY				; Y:X = &00C0
	JSR Sub_9266_Do_Transaction_ptr9A

	STA &B3
	BCC Sub_83FB			; always


.NFS_BPUTV_ENTRY
	CLC

	\\\ C=0, BPUT; C=1, BGET
	\\\ Y = file handle
.Sub_8414
{
	JSR Sub_8657_ESCAPE_disable	; ESCAPE disabled

	PHA
	STA &0FDF
	TXA
	PHA
	TYA
	PHA
	PHP

	STY &BA
	JSR Sub_869B_FileHandleY
	STY &0FDE			; Y = Internal Handle
	STY &CF

	LDY #&90			; Reply Port
	STY &0FDC

	JSR Sub_8395_SetupBuffer	; Exit: Y = &FF

	LDA #&DC			; Buffer start = &FFFF0FDC
	STA &C4

	LDA #&E0			; Buffer end   = &FFFF0FE0
	STA &C8

	INY				; Y = 0

	LDX #&09			; X = 9 = FS Function 9 Write Byte

	PLP
	BCC Label_8441			; If BPUT

	DEX				; X = 8 = FS Function 8 Read Byte

.Label_8441
	STX &0FDD

	LDA &CF				; A = Internal Handle
	LDX #&C0			; Y:X = &00C0
	JSR Sub_9266_Do_Transaction_ptr9A	; Transmit and wait for reply

	LDX &0FDD
	BEQ Label_8498			; If Return Code = 0 (no error)

	\ Error!

	LDY #&1F			; Copy error string
.Loop_8452
	LDA &0FDC,Y
	STA &0FE0,Y
	DEY
	BPL Loop_8452

	\ If it's the SPOOL or EXEC file, close it.

	TAX				; Y = &FF, X = A (0)
	LDA #&C6
	JSR OSBYTE			; *EXEC file handle = X, *SPOOL file handle = Y

	LDA #LO(Data_8529)		; *SP.
	CPY &BA
	BEQ Label_846D			; If SPOOL file

	LDA #LO(Data_852D)		; *E.
	CPX &BA
	BNE Label_8473			; If not EXEC file

.Label_846D
	TAX
	LDY #HI(Data_8529)		; Assume same page for both.
	JSR OSCLI

.Label_8473
	LDA #&E0			; ptrC4 = &0FE0
	STA &C4

	LDX &0FDD			; X = error
}

	\ Entry: X = Return Code
.Sub_847A_ReportNetError
{
	STX &0E09			; Return Code

	LDY #&01
	CPX #&A8
	BCS Label_8487			; If X >= &A8

	LDA #&A8
	STA (&C4),Y			; Let Return Code = &A8

.Label_8487
	LDY #&FF

.Loop_8489
	INY
	LDA (&C4),Y			; Copy error string.
	STA &0100,Y
	EOR #&0D
	BNE Loop_8489			; If not CR

	STA &0100,Y
	BEQ Label_84EA			; always -> JMP &100
}

.Label_8498
	STA &0E08

	PLA
	TAY
	PLA
	TAX
	PLA

.Return_84A0
	RTS

.Sub_84A1_Test_ESCAPE
	LDA &FF				; Test for ESCAPE
	AND ESC_ON
	BPL Return_84A0

	LDA #&7E			; Acknowledge ESCAPE
	JSR OSBYTE
	JMP Error_Report_Sub_8512	; Report ESCAPE error

.NFS_CALL_0F
	LDY #&04
	LDA (ptr9CL_PWS0),Y
	BEQ Label_84B8
.Label_84B5
	JMP Sub_92F0_RestoreProtectionMask

.Label_84B8
	ORA #&09
	STA (ptr9CL_PWS0),Y
	LDX #&80
	LDY #&80
	LDA (ptr9CL_PWS0),Y
	PHA
	INY
	LDA (ptr9CL_PWS0),Y
	LDY #&0F
	STA (ptr9EL_PWS1),Y
	DEY
	PLA
	STA (ptr9EL_PWS1),Y
	JSR Sub_81D2
	JSR Sub_9188
	LDX #&01
	LDY #&00
	LDA #&C9
	JSR OSBYTE			; Lock keyboard

.NFS_CALL_11
{
	JSR Sub_92F0_RestoreProtectionMask
	LDX #&02
	LDA #&00
.Loop_84E4
	STA &0100,X
	DEX
	BPL Loop_84E4
}

.Label_84EA
	JMP &0100

.NFS_CALL_12
	LDY #&04
	LDA (ptr9CL_PWS0),Y
	BEQ Label_84B8

	LDY #&80
	LDA (ptr9CL_PWS0),Y
	LDY #&0E
	CMP (ptr9EL_PWS1),Y
	BNE Label_84B5

.NFS_CALL_0E
	LDY #&82
	LDA (ptr9CL_PWS0),Y
	TAY
	LDX #&00
	JSR Sub_92F0_RestoreProtectionMask
	LDA #&99
	JMP OSBYTE			; Insert char into buffer

.Error_NoReply_Sub_850C
	LDA #&08
	BNE Error_Report_Sub_8514	; Error 8 = No Reply

.Error_Report_Sub_8510
	LDA (ptr9AL,X)

.Error_Report_Sub_8512
	AND #&07

.Error_Report_Sub_8514			; Report Error A
{
	TAX
	LDY Data_8019-1,X		; Error message offset
	LDX #&00
	STX &0100
.Loop_851D
	LDA Data_8580,Y			; Error message
	STA &0101,X
	BEQ Label_84EA
	INY
	INX
	BNE Loop_851D
}

.Data_8529
	EQUS "SP.", 13

.Data_852D
	EQUS "E.", 13

	\* Pending reply - keep polling until time-out.
	\* Entry: ptr9A -> control block
	\* Exit: Y=0, A<>0, Z=0
.Sub_8530_PollForReply
{
	LDA #&2A
	PHA				; S0

	LDA RXTX_Flags_D64
	PHA				; S1

	LDX ptr9AH
	BNE Label_8540			; If not zero page

	ORA #&80
	STA RXTX_Flags_D64

.Label_8540
	LDA #&00
	PHA				; S2
	PHA				; S3
	TAY
	TSX

.Label_8546
	LDA (ptr9AL),Y
	BMI Label_8559			; Reply received

	DEC &0101,X			; S3
	BNE Label_8546
	DEC &0102,X			; S2
	BNE Label_8546
	DEC &0104,X			; S1
	BNE Label_8546

.Label_8559
	PLA				; S3
	PLA				; S2

	PLA				; S1
	STA RXTX_Flags_D64

	PLA				; S0
	BEQ Error_NoReply_Sub_850C
	RTS
}

.NFS_BGETV_ENTRY
{
	SEC
	JSR Sub_8414

	SEC
	LDA #&FE
	BIT &0FDF
	BVS Label_857F
	CLC
	PHP
	LDA &CF
	PLP
	BMI Label_8579
	JSR Sub_86D5

.Label_8579
	JSR Sub_86D0
	LDA &0FDE

.Label_857F
	RTS
}

	\\ Error Messages
.Data_8580
.nfs_error0
	EQUS &A0, "Line Jammed", 0
.nfs_error1
	EQUS &A1, "Net Error", 0
.nfs_error2
	EQUS &A2, "Not listening",0
.nfs_error3
	EQUS &A3, "No Clock", 0
.nfs_error4
.nfs_error5
.nfs_error6
	EQUS &11, "Escape", 0
.nfs_error7
	EQUS &CB, "Bad Option", 0
.nfs_error8
	EQUS &A5, "No reply", 0

.Sub_85CF
	LDY #&0E
	LDA (ptrBBL),Y
	AND #&3F
	LDX #&04
	BNE Sub_85DD			; always

.Sub_85D9
	AND #&1F
	LDX #&FF

.Sub_85DD
{
	STA &B8
	LDA #&00

.Loop_85E1
	INX
	LSR &B8
	BCC Label_85E9

	ORA Data_85EC,X

.Label_85E9
	BNE Loop_85E1
	RTS

.Data_85EC
	EQUB  &50, &20, &05, &02, &88, &04, &08, &80
	EQUB  &10, &01, &02
}

.Sub_85F7_TRANSMIT_C0
	LDX #&C0			; ptr9A = &00C0
	STX ptr9AL
	LDX #&00
	STX ptr9AH

	\ ptr9A -> Control block
.Sub_85FF_TRANSMIT_ptr9A
{
	LDA #&FF			; Counter=&FF
	LDY #&60
	PHA
	TYA
	PHA
	LDX #&00
	LDA (ptr9AL,X)			; A=Control byte

.Label_860A
	STA (ptr9AL,X)
	PHA

.Label_860D
	ASL &0D62
	BCC Label_860D			; Wait until ?&D62=&80 : Line not busy?

	LDA ptr9AL
	STA ptrA0L
	LDA ptr9AH
	STA ptrA0H			; ptrA0 -> buffer control block

	JSR Sub_9630_Transmit_Start

.Label_861D
	LDA (ptr9AL,X)
	BMI Label_861D			; Wait for message

	ASL A
	BPL Label_8643			; If b6=0 no error SUCCESS!
	ASL A
	BEQ Label_863F			; A was &40 Network jammed (error 0)

	JSR Sub_84A1_Test_ESCAPE	; Check for ESCAPE

	PLA
	TAX				; Control byte
	PLA
	TAY				; Y
	PLA
	BEQ Label_863F			; If Counter=0: Force error 0
	SBC #&01			; Counter = Counter-1
	PHA
	TYA
	PHA
	TXA

.Label_8637				; Idle then try again
	DEX
	BNE Label_8637
	DEY
	BNE Label_8637
	BEQ Label_860A			; always

.Label_863F
	TAX
	JMP Error_Report_Sub_8510	; Report error

.Label_8643
	PLA
	PLA
	PLA
	JMP Sub_8657_ESCAPE_disable
}

.Sub_8649_SetPointers			; Set up pointers
	STX TextPointer
	STY TextPointer+1

.Sub_864D_SetPointers
	STA &BD

	STX ptrBBL
	STY ptrBBH

	STX ptrBEL
	STY ptrBEH

	\ Clears bit 7 of &97
	\ All registers preserved
.Sub_8657_ESCAPE_disable
	PHP				; Preserve C
	LSR ESC_ON			; Bit 7 = 0
	PLP
	RTS

.PrintString
{
	PLA
	STA &B0
	PLA
	STA &B1
	LDY #&00
.Label_8664
	INC &B0
	BNE Label_866A
	INC &B1
.Label_866A
	LDA (&B0),Y
	BMI Label_8674
	JSR OSASCI
	JMP Label_8664
.Label_8674
	JMP (&00B0)
}

	\ Read decimal number (i.e. station net + id)
	\ C=1 if ends with '.'
.Sub_8677_DecNum
{
	LDA #&00
	STA &B2

.Loop_867B
	LDA (ptrBBL),Y
	CMP #&2E
	BEQ Label_8697			; If A='.' ; exit C = 1
	BCC Label_8696			; If A<'.' ; exit C = 0

	AND #&0F
	STA &B3
	ASL &B2
	LDA &B2
	ASL A
	ASL A
	ADC &B2
	ADC &B3
	STA &B2
	INY
	BNE Loop_867B

.Label_8696
	CLC

.Label_8697
	LDA &B2
	RTS
}

.Sub_869A_FileHandleA
	TAY

.Sub_869B_FileHandleY
	CLC

.Sub_869C_FileHandleY			; File handle Y to bit mask
{
	PHA				; Preserve A & X
	TXA
	PHA
	TYA

	BCC Label_86A4			; If C=0 and Y=0; Return Y=&FF
	BEQ Label_86B3			; If Y=0; Return Y=0

.Label_86A4
	SEC
	SBC #&1F
	TAX
	LDA #&01

.Label_86AA
	ASL A				; C->0
	DEX
	BNE Label_86AA

	ROR A
	TAY
	BNE Label_86B3

	DEY				; If Y=0 let Y=&FF

.Label_86B3
	PLA
	TAX
	PLA
	RTS				; Y=mask
}

.Sub_86B7
{
	LDX #&1F			; Bit mask to file handle
.Label_86B9				; 10000000=&27,01000000=&26,...,00000001=&20
	INX
	LSR A
	BNE Label_86B9
	TXA				; A=&20 to max &27
	RTS
}

	\ Is !&B0 = !&B4?
.Sub_86BF_Cmp
{
	LDX #&04

.Loop_86C1
	LDA &AF,X
	EOR &B3,X
	BNE Label_86CA			; If !&B0 <> !&B4

	DEX
	BNE Loop_86C1

.Label_86CA
	RTS
}

.NFS_CALL_1A				; FSC 7 Handle range
	LDX #&20
	LDY #&27
	RTS

.Sub_86D0
	ORA &0E07
	BNE Label_86DA
.Sub_86D5
	EOR #&FF
	AND &0E07
.Label_86DA
	STA &0E07
	RTS

	\ Only used by FILEV
	\ ptrBB -> OSWORD control block
	\ Copy address of filename to TextPointer,
	\ then copy filename to &E30
.Sub_86DE_CopyFilename
{
	LDY #&01
.Label_86E0
	LDA (ptrBBL),Y
	STA TextPointer,Y
	DEY
	BPL Label_86E0
}

.Sub_86E8
	LDY #&00

	\ Copy Filename to &E30, and terminate with CR
.Sub_86EA
{
	LDX #&FF
	CLC				; String terminated by 1st space, CR, or 2nd quotation mark
	JSR GSINIT
	BEQ Label_86FD			; If null string

.Label_86F2				; Copy string to &0E30
	JSR GSREAD
	BCS Label_86FD

	INX
	STA &0E30,X
	BCC Label_86F2			; always

.Label_86FD
	INX

	LDA #&0D			; Terminate string with CR
	STA &0E30,X

	LDA #&30			; Set pointer ptrBE to string
	STA ptrBEL			; (ptrBE = &0E30)
	LDA #&0E
	STA ptrBEH
	RTS
}

.NFS_FILEV_ENTRY
	JSR Sub_864D_SetPointers	; Set pointers

	\ ptrBB -> control block
	JSR Sub_86DE_CopyFilename	; Copy filename, ptrBE -> copy
					; Exit : Y = 0

	LDA &BD				; Value of A on entry
	BPL Label_8790			; If <&80

	CMP #&FF
	BEQ Label_871D			; If LOAD

	JMP Label_89B3			; Restore AXY and exit

	\ OSFILE A = &FF LOAD
.Label_871D				; Load the named file
	JSR Sub_8D82_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05
					; i.e. from &0E30 to &0F05

	LDY #&02			; FS Function = 2 = Load

	\ Y = FS function (2 Load or 5 Load as command)
.Sub_8722_LOAD
{
	LDA #&92			; Reply Port
	STA ESC_ON			; Enable ESCAPE key
	STA &0F02

	JSR Sub_83BD_Transaction	; Send load request, exits with X=0

	LDY #&06
	LDA (ptrBBL),Y			; Lowest byte of Exec address
	BNE Label_873A			; If 0 use file's address

	\ Use control block address
	JSR Sub_882F_Set_PtrB0		; Set ptrB0 to first byte
	JSR Sub_8841_CopyToBlock	; Copy rcvd file info to OSWORD block.
	BCC Label_8740			; always

	\ Use file's address
.Label_873A
	JSR Sub_8841_CopyToBlock	; Update OSWORD control block first!
	JSR Sub_882F_Set_PtrB0

.Label_8740
	LDY #&04

.Loop_8742
	LDA &B0,X			; !&C8 = !&B0 ("buffer end")
	STA &C8,X
	ADC &0F0D,X			; !&B4 = !&B0 + !&F0D (File size)
	STA &B4,X
	INX
	DEY
	BNE Loop_8742

	SEC
	SBC &0F10
	STA &B7

	JSR Sub_8765_GetRemoteData

	LDX #&02

.Loop_875A
	LDA &0F10,X			; #&0F05 = #&0F10 (Attributes & Date?)
	STA &0F05,X
	DEX
	BPL Loop_875A

	BMI Sub_87D8			; always
}

	\ Fill buffer with data from remote station.
	\ Entry: !&C8 = Buffer start, !&B4 = Buffer end
.Sub_8765_GetRemoteData
{
	JSR Sub_86BF_Cmp
	BEQ Label_878F			; If !&B0 = !&B4

	LDA #&92			; Receive Port
	STA &C1

.Loop_876E
	LDX #&03

.Loop_8770
	LDA &C8,X			; Let buffer start = buffer end
	STA &C4,X

	LDA &B4,X			; Let buffer end = !&B4
	STA &C8,X
	DEX
	BPL Loop_8770

	LDA #&7F			; Get some data.
	STA &C0

	JSR Sub_8530_PollForReply

	LDY #&03

.Loop_8784
	LDA &00C8,Y
	EOR &00B4,Y
	BNE Loop_876E			; If !&C8 <> !&B4 then there's more to get!

	DEY
	BPL Loop_8784

.Label_878F
	RTS
}

	\ OSFILE A<&80
.Label_8790
{
	BEQ Label_8795			; If A = 0 = SAVE
	JMP Label_88D1

	\ OSFILE A=&00 SAVE
.Label_8795

	\ !&F0D =  End Address - Start Address (Length)
	\ !&B0 = Start address
	\ !&B4 = End Address
	\ and update OSWORD block, such that
	\ ptrBB!&A = End Address - Start Address (Length)
	LDX #&04
	LDY #&0E

.Loop_8799
	LDA (ptrBBL),Y			; &E
	STA ptrA6L,Y

	JSR Sub_884E_Y_MINUS_4

	SBC (ptrBBL),Y			; &A
	STA &0F03,Y

	PHA
	LDA (ptrBBL),Y
	STA ptrA6L,Y

	PLA
	STA (ptrBBL),Y

	JSR Sub_883B_Y_PLUS_5		; &F

	DEX
	BNE Loop_8799

	LDY #&09

.Loop_87B7				; Copy rest of OSWORD block to &F03
	LDA (ptrBBL),Y
	STA &0F03,Y
	DEY
	BNE Loop_87B7

	LDA #&91
	STA ESC_ON			; Enable ESCAPE

	STA &0F02			; Reply Port
	STA &B8

	LDX #&0B			; Copy filename
	JSR Sub_8D84_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05+X

	LDY #&01			; FS function = 1
	JSR Sub_83BD_Transaction

	LDA &0F05			; Dest Port
	JSR Sub_8853_SendRemoteData	; Send Data
}

.Sub_87D8
{
	LDA &0F03			; Command Code from FS

	PHA
	JSR Sub_83F3_WaitForReply
	PLA

	LDY ShowInfo
	BEQ Label_8817			; Don't show info

	LDY #&00
	TAX
	BEQ Loop_87EF			; If Command Code = 0 i.e. completed

	JSR Sub_8D98
	BMI Label_8803			; always

.Loop_87EF
	LDA (ptrBEL),Y			; Print filename
	CMP #&21
	BCC Label_87FB			; If A <= ' '

	JSR OSASCI
	INY
	BNE Loop_87EF

.Label_87FB				; Pad with spaces
	JSR Sub_8D7B_PrintSpace
	INY
	CPY #&0C
	BCC Label_87FB

.Label_8803
	LDY #&05			; Load address
	JSR Sub_8D70_Print4Hex_ptrBB_Y

	LDY #&09			; Exec address
	JSR Sub_8D70_Print4Hex_ptrBB_Y

	LDY #&0C			; Length
	LDX #&03			; 3 byte
	JSR Sub_8D72_PrintXHex_ptrBB_Y

	JSR OSNEWL

.Label_8817
	STX &0F08
	LDY #&0E
	LDA &0F05
	JSR Sub_85D9

.Label_8822
	STA (ptrBBL),Y
	INY
	LDA &0EF7,Y
	CPY #&12
	BNE Label_8822

	JMP Label_89B3			; Restore A,X,Y and exit
}

	\ Copy Load address to &B0
.Sub_882F_Set_PtrB0
{
	LDY #&05

.Loop_8831
	LDA (ptrBBL),Y
	STA &00AE,Y
	DEY
	CPY #&02
	BCS Loop_8831

	\ Y doesn't matter
}

.Sub_883B_Y_PLUS_5
	INY

.Sub_883C_Y_PLUS_4
	INY
	INY
	INY
	INY
	RTS

	\ Copy file address etc from &F05 to control block
.Sub_8841_CopyToBlock
{
	LDY #&0D
	TXA

.Loop_8844
	STA (ptrBBL),Y
	LDA &0F02,Y
	DEY
	CPY #&02
	BCS Loop_8844

	\ Y doesn't matter
}

.Sub_884E_Y_MINUS_4
	DEY

.Sub_884F_Y_MINUS_3
	DEY
	DEY
	DEY
	RTS

	\ A = Destination Port
.Sub_8853_SendRemoteData
{
	PHA				; A = Destination Port

	JSR Sub_86BF_Cmp
	BEQ Label_88CD			; If !&B0 = !&B4

.Loop_8859
	LDA #&00
	PHA
	PHA
	TAX

	LDA &0F07
	PHA
	LDA &0F06
	PHA

	LDY #&04			; Buffer Start = !&C4 = !&B0
	CLC				; Buffer End = !&C8 = !&B0 + A * &100
					; !&B0 = !&C8
.Loop_8869
	LDA &B0,X
	STA &C4,X
	PLA
	ADC &B0,X
	STA &C8,X
	STA &B0,X
	INX
	DEY
	BNE Loop_8869

	SEC

.Loop_8879
	LDA &00B0,Y
	SBC &00B4,Y
	INY
	DEX
	BNE Loop_8879
	BCC Label_888E			; IF !&B0 < !&B4

	LDX #&03			; Buffer End = !&C8 = !&B4

.Loop_8887
	LDA &B4,X
	STA &C8,X
	DEX
	BPL Loop_8887

	\ C=1=Last Block

.Label_888E
	PLA

	PHA
	PHP

	STA &C1				; Dest Port

	LDA #&80
	STA &C0

	JSR Sub_85F7_TRANSMIT_C0	; Send Block of Data

	LDA &B8
	JSR Sub_8389_SetupBuffer

	PLP
	BCS Label_88CD			; If no more data

	LDA #&91
	STA &C1
	INC &C4

	JSR Sub_8530_PollForReply
	BNE Loop_8859			; always
}

.NFS_CALL_14				; FSC 1 EOF?
	PHA
	TXA

	JSR Sub_869A_FileHandleA

	TYA
	AND &0E07
	TAX
	BEQ Label_88CD

	PHA
	STY &0F05

	LDY #&11			; FS function = 17
	LDX #&01			; Length = 1
	JSR Sub_83C7_Transaction

	PLA
	LDX &0F05
	BNE Label_88CD

	JSR Sub_86D5

.Label_88CD
	PLA
	LDY ptrBBH

	RTS

	\ OSFILE 0 < A < &80
.Label_88D1
	STA &0F05
	CMP #&06
	BEQ Label_8917
	BCS Label_8922

	CMP #&05
	BEQ Label_8930

	CMP #&04
	BEQ Label_8926

	CMP #&01
	BEQ Label_88FB

	ASL A
	ASL A
	TAY
	JSR Sub_884F_Y_MINUS_3
	LDX #&03

.Label_88EE
	LDA (ptrBBL),Y
	STA &0F06,X
	DEY
	DEX
	BPL Label_88EE

	LDX #&05
	BNE Label_8910

.Label_88FB
	JSR Sub_85CF
	STA &0F0E
	LDY #&09
	LDX #&08

.Label_8905
	LDA (ptrBBL),Y
	STA &0F05,X
	DEY
	DEX
	BNE Label_8905

	LDX #&0A

.Label_8910
	JSR Sub_8D84_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05+X

	LDY #&13			; FS function = 19
	BNE Label_891C			; always

.Label_8917
	JSR Sub_8D82_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05

	LDY #&14			; FS function = 20

.Label_891C
	BIT Data_83B3			; Set V
	JSR Sub_83C8_Transaction

.Label_8922
	BCS Label_8966
	BCC Label_8997			; always

.Label_8926
	JSR Sub_85CF

	STA &0F06
	LDX #&02
	BNE Label_8910			; always

.Label_8930
	LDX #&01
	JSR Sub_8D84_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05+X

	LDY #&12			; FS function = 18
	JSR Sub_83C7_Transaction

	LDA &0F11
	STX &0F11
	STX &0F14
	JSR Sub_85D9
	LDY #&0E
	STA (ptrBBL),Y
	DEY
	LDX #&0C

.Label_894D
	LDA &0F05,X
	STA (ptrBBL),Y
	DEY
	DEX
	BNE Label_894D

	INX
	INX
	LDY #&11

.Label_895A
	LDA &0F12,X
	STA (ptrBBL),Y
	DEY
	DEX
	BPL Label_895A

	LDA &0F05

.Label_8966
	BPL Label_89B5

.NFS_ARGSV_ENTRY
{
	JSR Sub_864D_SetPointers
	CMP #&03
	BCS Label_89B3

	CPY #&00
	BEQ Label_89BA

	JSR Sub_869B_FileHandleY
	STY &0F05
	LSR A
	STA &0F06
	BCS Label_8999

	LDY #&0C			; FS function = 12
	LDX #&02			; Length = 2
	JSR Sub_83C7_Transaction

	STA &BD
	LDX ptrBBL
	LDY #&02
	STA &03,X

.Label_898E
	LDA &0F05,Y
	STA &02,X
	DEX
	DEY
	BPL Label_898E
}
.Label_8997
	BCC Label_89B3

.Label_8999
	TYA
	PHA
	LDY #&03

.Label_899D
	LDA &03,X
	STA &0F07,Y
	DEX
	DEY
	BPL Label_899D

	LDY #&0D			; FS function = 13
	LDX #&05			; Length = 5
	JSR Sub_83C7_Transaction

	STX &BD
	PLA
	JSR Sub_86D0

.Label_89B3				; Restore A,X,Y
	LDA &BD

.Label_89B5
	LDX ptrBBL
	LDY ptrBBH
	RTS

.Label_89BA
	CMP #&02
	BEQ Label_89C5
	BCS Label_89D4

	TAY
	BNE Label_89C8

	LDA #&0A
.Label_89C5
	LSR A
	BNE Label_89B5

.Label_89C8
	LDA &0E0A,Y
	STA (ptrBBL),Y
	DEY
	BPL Label_89C8

	STY &02,X
	STY &03,X

.Label_89D4
	LDA #&00
	BPL Label_89B5

.NFS_FINDV_ENTRY
	JSR Sub_8649_SetPointers
	SEC
	JSR Sub_869C_FileHandleY
	TAX
	BEQ Label_8A10

	AND #&3F
	BNE Label_89D4

	TXA
	EOR #&80
	ASL A
	STA &0F05
	ROL A
	STA &0F06
	JSR Sub_86E8
	LDX #&02
	JSR Sub_8D84_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05+X

	LDY #&06			; FS function = 6
	BIT Data_83B3			; Set V
	JSR Sub_83C8_Transaction
	BCS Label_89B5

	LDA &0F05
	TAX
	JSR Sub_86D0
	TXA
	JSR Sub_86B7
	BNE Label_89B5

.Label_8A10
	TYA
	BNE Label_8A1A

	LDA #&77
	JSR OSBYTE			; Close andy SPOOL or EXEC files.

	LDY #&00

.Label_8A1A
	STY &0F05

	LDX #&01			; Length = 1
	LDY #&07			; FS function = 7
	JSR Sub_83C7_Transaction

	LDA &0F05
	JSR Sub_86D0

.Label_8A2A
	BCC Label_89B3			; Restore A,X,Y and exit

.NFS_CALL_13				; FSC A=0 *OPT X,Y
{					; On entry C=0
	BEQ Label_8A39			; If X=0

	CPX #&04
	BNE Label_8A36			; If X<>4

	CPY #&04
	BCC Label_8A43			; If Y<4

.Label_8A36
	DEX
	BNE Label_8A3E			; X wasn't 1

	\ *OPT 0,Y or *OPT 1,Y
	\ If Y<>0 show on screen info
.Label_8A39
	STY ShowInfo
	BCC Label_8A50			; always

.Label_8A3E
	LDA #&07
	JMP Error_Report_Sub_8512	; Report Error 7 = Bad Option

	\ *OPT 4,Y (Y<4)
	\ Set Autostart Option (option sent to FS)
.Label_8A43
	STY &0F05

	LDY #&16			; FS Function = 22 = Write Autostart Option
	JSR Sub_83C7_Transaction

	LDY ptrBBH			; Original Y
	STY &0E05

.Label_8A50
	BCC Label_8A2A
}

.Sub_8A52
	LDY #&09
	JSR Sub_8A59
.Sub_8A57
	LDY #&01
.Sub_8A59
	CLC

.Sub_8A5A
{
	LDX #&FC

.Label_8A5C
	LDA (ptrBBL),Y
	BIT &B2
	BMI Label_8A68

	ADC &0E0A,X
	JMP Label_8A6B

.Label_8A68
	SBC &0E0A,X

.Label_8A6B
	STA (ptrBBL),Y
	INY
	INX
	BNE Label_8A5C

	RTS
}

.NFS_GBPBV_ENTRY
{
	JSR Sub_864D_SetPointers

	TAX
	BEQ Label_8A7D			; If A = 0

	DEX
	CPX #&08
	BCC Label_8A80			; If A < 9

.Label_8A7D
	JMP Label_89B3			; Restore A,X,Y and exit.

.Label_8A80
	TXA				; A = A - 1
	LDY #&00
	PHA

	CMP #&04
	BCC Label_8A8B			; If A < 4 (5)

	JMP Label_8B31

	\ ptrBB -> OSWORD control block
	\ A=1,2,3, or 4
.Label_8A8B
	LDA (ptrBBL),Y
	JSR Sub_869A_FileHandleA
	STY &0F05			; Internal Hanlde

	LDY #&0B			; Copy sequential pointer
	LDX #&06			; and Number of bytes to transfer

.Label_8A97
	LDA (ptrBBL),Y
	STA &0F06,X
	DEY
	CPY #&08
	BNE Label_8AA2

	DEY

.Label_8AA2
	DEX
	BNE Label_8A97

	PLA				; X = 0
	LSR A
	PHA
	BCC Label_8AAB			; Don't use sequential pointer

	INX				; X = 1

.Label_8AAB
	STX &0F06

	LDY #&0B			; FS Function 11 Write Bytes
	LDX #&91

	PLA
	PHA
	BEQ Label_8AB9			; If PUT to media

	LDX #&92
	DEY				; FS Function 10 Read Bytes

.Label_8AB9
	STX &0F02
	STX &B8

	LDX #&08
	LDA &0F05			; A = Internal Handle

	JSR Sub_83B9_GPBP_Transaction	; Transmit

	LDA &B3
	STA &0E08

	LDX #&04

.Loop_8ACD
	LDA (ptrBBL),Y
	STA &00AF,Y
	STA &00C7,Y
	JSR Sub_883C_Y_PLUS_4
	ADC (ptrBBL),Y
	STA &00AF,Y
	JSR Sub_884F_Y_MINUS_3
	DEX
	BNE Loop_8ACD

	INX

.Label_8AE4
	LDA &0F03,X
	STA &0F06,X
	DEX
	BPL Label_8AE4

	PLA
	BNE Label_8AF8

	LDA &0F02			; Dest Port
	JSR Sub_8853_SendRemoteData
	BCS Label_8AFB			; always

.Label_8AF8
	JSR Sub_8765_GetRemoteData

.Label_8AFB
	JSR Sub_83F3_WaitForReply

	LDA (ptrBBL,X)
	BIT &0F05
	BMI Label_8B08

	JSR Sub_86D5

.Label_8B08
	JSR Sub_86D0
	STX &B2
	JSR Sub_8A52
	DEC &B2
	SEC
	JSR Sub_8A5A
	ASL &0F05
	JMP Label_89D4

.Label_8B1C
	LDY #&15			; FS function = 21
	JSR Sub_83C7_Transaction

	LDA &0E05
	STA &0F16
	STX &B0
	STX &B1
	LDA #&12
	STA &B2
	BNE Sub_8B7F

.Label_8B31
	LDY #&04

	LDA TubePresent_D67
	BEQ Label_8B3F

	CMP (ptrBBL),Y
	BNE Label_8B3F

	DEY
	SBC (ptrBBL),Y

.Label_8B3F
	STA &A9

.Label_8B41
	LDA (ptrBBL),Y
	STA &00BD,Y
	DEY
	BNE Label_8B41

	PLA
	AND #&03
	BEQ Label_8B1C

	LSR A
	BEQ Label_8B53
	BCS Label_8BBE

.Label_8B53
	TAY
	LDA &0E03,Y
	STA &0F03
	LDA &0E04
	STA &0F04
	LDA &0E02
	STA &0F02
	LDX #&12
	STX &0F01
	LDA #&0D
	STA &0F06
	STA &B2
	LSR A
	STA &0F05

	CLC
	JSR Sub_83DD_Transaction

	STX &B1
	INX
	STX &B0

.Sub_8B7F
	LDA &A9
	BNE Label_8B94
	LDX &B0
	LDY &B1

.Label_8B87
	LDA &0F05,X
	STA (ptrBEL),Y
	INX
	INY
	DEC &B2
	BNE Label_8B87
	BEQ Label_8BBB

.Label_8B94
	JSR Sub_8C13
	LDA #&01
	LDX ptrBBL
	LDY ptrBBH
	INX
	BNE Label_8BA1

	INY

.Label_8BA1
	JSR TubeCode
	LDX &B0

.Label_8BA6
	LDA &0F05,X
	STA TUBE_R3_DATA

	INX
	LDY #&06

.Label_8BAF
	DEY
	BNE Label_8BAF
	DEC &B2
	BNE Label_8BA6

	LDA #&83
	JSR TubeCode			; Release TUBE (Econet Filing System)

.Label_8BBB
	JMP Label_89D4

.Label_8BBE
	LDY #&09
	LDA (ptrBBL),Y
	STA &0F06
	LDY #&05
	LDA (ptrBBL),Y
	STA &0F07
	LDX #&0D
	STX &0F08
	LDY #&02
	STY &B0
	STY &0F05

	INY				; FS function = 3
	JSR Sub_83C7_Transaction

	STX &B1
	LDA &0F06
	STA (ptrBBL,X)
	LDA &0F05
	LDY #&09
	ADC (ptrBBL),Y
	STA (ptrBBL),Y
	LDA &C8
	SBC #&07
	STA &0F06
	STA &B2
	BEQ Label_8BFA

	JSR Sub_8B7F

.Label_8BFA
	LDX #&02

.Label_8BFC
	STA &0F07,X
	DEX
	BPL Label_8BFC

	JSR Sub_8A57
	SEC
	DEC &B2
	LDA &0F05
	STA &0F06
	JSR Sub_8A5A
	BEQ Label_8BBB
}

.Sub_8C13
	LDA #&C3
	JSR TubeCode			; Claim TUBE (Econet Filing System)
	BCC Sub_8C13

	RTS

	; Y:X -> command string
.NFS_CALL_16				; FSC 3 Unrecognised OS command
{
	JSR Sub_8649_SetPointers

	LDX #&FF
	STX &B9
	STX ESC_ON

.Loop_8C24
	LDY #&FF

.Loop_8C26
	INY
	INX

.Label_8C28
	LDA Data_8C4B,X
	BMI Label_8C45

	EOR (ptrBEL),Y
	AND #&DF
	BEQ Loop_8C26

	DEX

.Loop_8C34
	INX
	LDA Data_8C4B,X
	BPL Loop_8C34

	LDA (ptrBEL),Y
	INX
	CMP #&2E
	BNE Loop_8C24			; If <> '.'

	INY
	DEX
	BCS Label_8C28

.Label_8C45
	PHA
	LDA Data_8C4B+1,X
	PHA
	RTS

.Data_8C4B
	EQUS "I."
	EQUB HI(Sub_80C1_CommandLine-1)
	EQUB LO(Sub_80C1_CommandLine-1)
	EQUS "I AM"
	EQUB HI(Sub_8082_I_AM-1)
	EQUB LO(Sub_8082_I_AM-1)
	EQUS "EX"
	EQUB HI(Sub_8C61_EX-1)
	EQUB LO(Sub_8C61_EX-1)
	EQUS "BYE", &0D
	EQUB HI(Sub_83C0_BYE-1)
	EQUB LO(Sub_83C0_BYE-1)
	EQUB HI(Sub_80C1_CommandLine-1)
	EQUB LO(Sub_80C1_CommandLine-1)

.Sub_8C61_EX
	LDX #&01
	LDA #&03
	BNE Label_8C72			; always
}

.NFS_CALL_18				; FSC 5 *CAT
	LDX #&03
	STX &B9
	LDY #&FF
	STY ESC_ON
	INY
	LDA #&0B

.Label_8C72
{
	STA &B5
	STX &B7

	LDA #&06
	STA &0F05
	JSR Sub_86EA			; Copy parameter to (BE)

	LDX #&01
	JSR Sub_8D84_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05+X

	LDY #&12			; FS function = 18
	JSR Sub_83C7_Transaction	; Load catalogue

	LDX #&03
	JSR Sub_8D47			; Print Directory Name
	JSR PrintString
	EQUS "("
	LDA &0F13			; Directory Cycle Number
	JSR Sub_8DBD_PrintDecimal
	JSR PrintString
	EQUS ")     "
	LDY &0F12
	BNE Label_8CB0
	JSR PrintString
	EQUS "Owner", 13
	BNE Label_8CBA

.Label_8CB0
	JSR PrintString
	EQUS "Public", 13

.Label_8CBA
	LDY #&15			; FS function = 21
	JSR Sub_83C7_Transaction

	INX
	LDY #&10
	JSR Sub_8D49

	JSR PrintString
	EQUS "    Option "

	LDA &0E05
	TAX
	JSR Utils_PrintHexByte

	JSR PrintString
	EQUS " ("

	LDY Data_8D54,X

.Loop_8CE2
	LDA Data_8D54,Y
	BMI Label_8CED

	JSR OSASCI
	INY
	BNE Loop_8CE2

.Label_8CED
	JSR PrintString
	EQUS ")", 13
	EQUS "Dir. "

	LDX #&11
	JSR Sub_8D47

	JSR PrintString
	EQUS "     Lib. "

	LDX #&1B
	JSR Sub_8D47

	JSR OSNEWL

.Label_8D11
	STY &0F06
	STY &B4
	LDX &B5
	STX &0F07
	LDX &B7
	STX &0F05
	LDX #&03
	JSR Sub_8D84_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05+X

	LDY #&03			; FS function = 3
	JSR Sub_83C7_Transaction

	INX
	LDA &0F05
	BNE Label_8D33
	JMP OSNEWL

.Label_8D33
	PHA

.Label_8D34
	INY
	LDA &0F05,Y
	BPL Label_8D34

	STA &0F04,Y
	JSR Sub_8D9F
	PLA
	CLC
	ADC &B4
	TAY
	BNE Label_8D11

.Sub_8D47				; Print string at &F05+X
	LDY #&0A			; 10 chrs

.Sub_8D49
	LDA &0F05,X
	JSR OSASCI
	INX
	DEY
	BNE Sub_8D49
	RTS
}

	\ Boot options
.Data_8D54
	EQUB Data_8D7F - Data_8D54
	EQUB Data_8D92 - Data_8D54
	EQUB Data_8DBA - Data_8D54
	EQUB Data_8D6C - Data_8D54

.Data_8D58
	EQUS "L."

.Data_8D5A
	EQUS "!BOOT", 13

.Data_8D60
	EQUS "E.!"

.Data_8D63
	EQUS "BOOT"

.Data_8D67
	EQUB 13

	\ Assumes all in same page
.Data_8D68
	EQUB LO(Data_8D67)
	EQUB LO(Data_8D58)
	EQUB LO(Data_8D5A)
	EQUB LO(Data_8D60)

.Data_8D6C
	EQUS "Exec"

.Sub_8D70_Print4Hex_ptrBB_Y
	LDX #&04

.Sub_8D72_PrintXHex_ptrBB_Y
{
.Loop_8D72
	LDA (ptrBBL),Y
	JSR Utils_PrintHexByte
	DEY
	DEX
	BNE Loop_8D72
}

.Sub_8D7B_PrintSpace
	LDA #&20
	BNE Label_8DD9			; always

.Data_8D7F
	EQUS "Off"

	\ Copy string from ptrBE+Y to &0F05
.Sub_8D82_CopyString_ptrBE
	LDX #&00

	\ Copy string from ptrBE+Y to &0F05+X
.Sub_8D84_CopyString_ptrBE
{
	LDY #&00
.Loop_8D86
	LDA (ptrBEL),Y
	STA &0F05,X
	INX
	INY
	EOR #&0D
	BNE Loop_8D86
}
.Return_8D91
	RTS

.Data_8D92
	EQUS "Load"

.NFS_CALL_1B
.Label_8D96
	LDX #&00

	\ Print string at &0F05+X.
	\ String terminated if bit 7 set.
.Sub_8D98
	LDA &0F05,X
	BMI Return_8D91			; Exit loop
	BNE Label_8DB4

.Sub_8D9F
	LDY &B9
	BMI Label_8DB2

	\ Leading 00s replace with spaces
	INY
	TYA
	AND #&03
	STA &B9
	BEQ Label_8DB2

	JSR PrintString
	EQUS "  "
	BNE Label_8DB7			; always

.Label_8DB2
	LDA #&0D

.Label_8DB4
	JSR OSASCI

.Label_8DB7
	INX
	BNE Sub_8D98			; always

.Data_8DBA
	EQUS "Run"

.Sub_8DBD_PrintDecimal			; Print 3 fig. decimal number (A=number)
{
	TAY
	LDA #100
	JSR Sub_8DCA

	LDA #10
	JSR Sub_8DCA

	LDA #1

.Sub_8DCA
	STA &B8
	TYA
	LDX #&2F
	SEC

.Loop_8DD0
	INX
	SBC &B8
	BCS Loop_8DD0

	ADC &B8
	TAY
	TXA
}
.Label_8DD9
	JMP OSASCI

.NFS_CALL_15				; FSC 2 */
.NFS_CALL_17				; FSC 4 *RUN
	JSR Sub_86E8
	JSR Sub_8D82_CopyString_ptrBE	; Copy string from ptrBE+Y to &0F05

.NFS_CALL_1F
	LDY #&00
	CLC
	JSR GSINIT

.Label_8DE8
	JSR GSREAD
	BCC Label_8DE8

	JSR Sub_837E
	CLC
	TYA
	ADC TextPointer
	STA &0E0A
	LDA TextPointer+1

	ADC #&00
	STA &0E0B

	LDX #&0E			; PtrBB = &0E10
	STX ptrBBH
	LDA #&10
	STA ptrBBL

	STA &0E16
	LDX #&4A

	LDY #&05			; FS function = 5
	JSR Sub_8722_LOAD

	LDA TubePresent_D67
	BEQ Label_8E29

	ADC &0F0B
	ADC &0F0C
	BCS Label_8E29

	JSR Sub_8C13

	LDX #&09
	LDY #&0F
	LDA #&04
	JMP TubeCode

.Label_8E29
	ROL A
	JMP (&0F09)

.NFS_CALL_20
	STY &0E04
	BCC Label_8E35

.NFS_CALL_1E
	STY &0E03

.Label_8E35
	JMP Label_89B3

.NFS_CALL_1C
	SEC

.NFS_CALL_1D
	LDX #&03
	BCC Label_8E43

.Label_8E3D
	LDA &0F05,X
	STA &0E02,X

.Label_8E43
	DEX
	BPL Label_8E3D
	BCC Label_8E35

	LDY &0E05			; Why?

	LDX Data_8D68,Y
	LDY #HI(Data_8D68)
	JMP OSCLI

.Sub_8E53
	LDA &F0				; X value of most recent OSBYTE/OSWORD

.Sub_8E55				; Calc control block offset if valid
{
	ASL A
	ASL A
	PHA
	ASL A
	TSX
	ADC &0101,X			; Assume C=0
	TAY				; Y=A*12
	PLA				; A=A*4
	CMP #&48
	BCC Label_8E66			; If A>=72 Y=0:A=0:C=1 ; 18*4

	LDY #&00
	TYA

.Label_8E66
	RTS
}

.NFS_CALL_21				; Osbyte &32 Poll Transmit
	LDY #&6F
	LDA (ptr9CL_PWS0),Y
	BCC Label_8E7A

.NFS_CALL_22				; Osbyte &33 Poll Reception
					; X=RECEIVE control block number
					; exit: X=flag: if top bit set msg received
	JSR Sub_8E53			; Y=X*12
	BCS Label_8E78			; If X>17

	LDA (ptr9EL_PWS1),Y
	CMP #&3F
	BNE Label_8E7A			; If block not empty

.Label_8E78
	LDA #&00

.Label_8E7A
	STA &F0
	RTS

.NFS_CALL_23				; Osbyte &34 Delete a RECEIVE control block
					; X=control block number
	JSR Sub_8E53			; Y=X*12
	BCS Label_8E78			; if X>17
	LDA #&3F
	STA (ptr9EL_PWS1),Y			; Set block as empty
	RTS

.NFS_SERVICE_08				; Unrecognised OSWORD
{
	LDA &EF
	SBC #&0F			; Trap OSWORDS &10 through &14
	BMI Return_8EB7

	CMP #&05
	BCS Return_8EB7

	JSR Sub_8E9F
	LDY #&02			; Restore &AA,&AB,&AC

.Loop_8E96
	LDA (ptr9CL_PWS0),Y
	STA &00AA,Y
	DEY
	BPL Loop_8E96

	RTS

.Sub_8E9F
	TAX
	LDA Data_8EBD,X
	PHA
	LDA Data_8EB8,X
	PHA
	LDY #&02			; Save &AA,&AB,&AC

.Loop_8EAA
	LDA &00AA,Y
	STA (ptr9CL_PWS0),Y
	DEY
	BPL Loop_8EAA

	INY				; Y=0
	LDA (&F0),Y			; A=1st byte in control block
	STY &A9

.Return_8EB7
	RTS				; 'Return' to Osword routine: A=function,Y=0,C=0

.Data_8EB8
	EQUB LO(OSWORD10_SUB_8EC1-1)
	EQUB LO(OSWORD11_SUB_8F7B-1)
	EQUB LO(OSWORD12_SUB_8EDB-1)
	EQUB LO(OSWORD13_SUB_8F00-1)
	EQUB LO(OSWORD14_SUB_8FEF-1)
.Data_8EBD
	EQUB HI(OSWORD10_SUB_8EC1-1)
	EQUB HI(OSWORD11_SUB_8F7B-1)
	EQUB HI(OSWORD12_SUB_8EDB-1)
	EQUB HI(OSWORD13_SUB_8F00-1)
	EQUB HI(OSWORD14_SUB_8FEF-1)
}

.OSWORD10_SUB_8EC1			; OSWORD &10 Setup TRANSMIT control block
	ASL &0D62
	TYA				; Y=0
	BCC Label_8EFC			; If busy? - Transmit fail!

	LDA ptr9CH_PWS0				
	STA ptrABH
	STA ptrA0H
	LDA #&6F
	STA ptrABL			; ptrAB -> PWSP1+&6F
	STA ptrA0L			; ptrA0 -> PWSP1+&6F
	LDX #&0F			; C=1, Y=0
	JSR Copy_Control_Block_Sub_8F1C	; Copy control block to ptrAB (16 bytes)
	JMP Sub_9630_Transmit_Start	; ptrA0 -> buffer control block

.OSWORD12_SUB_8EDB			; OSWORD &12 Read the argument block
	LDA ptr9CH_PWS0			; PWSP1
	STA ptrABH
	LDY #&7F
	LDA (ptr9CL_PWS0),Y			; Size of argument block
	INY
	STY ptrABL			; ptrAB -> PWSP1+&80
	TAX
	DEX
	LDY #&00
	JSR Copy_Control_Block_Sub_8F1C	; C=0: Copy data from (AB) to (F0)
	JMP Sub_92F0_RestoreProtectionMask

.Label_8EF1				; Read nr of argument blocks, and arg block buffer size
	LDY #&7F
	LDA (ptr9CL_PWS0),Y			; Nr? of argument blocks
	LDY #&01
	STA (&F0),Y
	INY
	LDA #&80			; Max size of buffer

.Label_8EFC
	STA (&F0),Y
	RTS

.Data_8EFF
	EQUB  &FF, &01


	\\ OSWORD &13 R/W Station information
.OSWORD13_SUB_8F00
{
	CMP #&06			; A=function code
	BCS Label_8F46			; A>=6
	CMP #&04
	BCS Label_8F2B			; A>=4
	LSR A				; C=Write
	LDX #&0D			; X=page &D
	TAY
	BEQ Label_8F11			; If A was 0 or 1 Y=0 R/W FS No.

	LDX ptr9EH_PWS1			; else A was 1 or 2 Y=1 R/W PS No. : X=page PWSP2

.Label_8F11
	STX ptrABH
	LDA Data_8EFF,Y			; A was 0/1 = &FF else was 2/3 = &01
	STA ptrABL
	LDX #&01			; 2 bytes
	LDY #&01			; FS &DFF+1=&E00, PS &01+1=PWSP2+&02 (RECEIVE Ctrl block 0)
}

.Copy_Control_Block_Sub_8F1C		; Copy data: C=0 ptrAB to ptrF0;  C=1 ptrF0 to ptrAB
{					; X+1 bytes, Y=offset
.Loop_8F1C
	BCC Label_8F22
	LDA (&F0),Y
	STA (ptrABL),Y

.Label_8F22
	LDA (ptrABL),Y
	STA (&F0),Y
	INY
	DEX
	BPL Loop_8F1C

	RTS
}

.Label_8F2B				; A=4 or A=5 R/W Protection Mask
	LSR A				; C=Write
	INY
	LDA (&F0),Y
	BCS Label_8F36

	LDA ProtectionMask_D63
	STA (&F0),Y

.Label_8F36
	STA ProtectionMask_D63
	STA ProtectionMaskCopy_D65
	RTS

.Label_8F3D				; Read local station number
	LDY #&14
	LDA (ptr9CL_PWS0),Y
	LDY #&01
	STA (&F0),Y
	RTS

.Label_8F46
{
	CMP #&08
	BEQ Label_8F3D			; Rd station id
	CMP #&09
	BEQ Label_8EF1			; Nr of Arg blocks
	BPL Label_8F69			; A>=9 and A<&89

	LDY #&03
	LSR A
	BCC Label_8F70

	STY &A8

.Label_8F57
	LDY &A8
	LDA (&F0),Y
	JSR Sub_869A_FileHandleA	; File handle to bit mask
	TYA
	LDY &A8
	STA &0E01,Y
	DEC &A8
	BNE Label_8F57
	RTS

.Label_8F69
	INY
	LDA &0E09			; Error number
	STA (&F0),Y
	RTS

.Label_8F70
	LDA &0E01,Y			; 
	JSR Sub_86B7			; Convert to file handle
	STA (&F0),Y			; Store in OW block
	DEY
	BNE Label_8F70

	RTS
}


	\\ OSWORD &11 Setup RECEIVE control block
.OSWORD11_SUB_8F7B
{					; (F0)->control block
	LDX ptr9EH_PWS1			; Set ptr -> Page 2 of private workspace
	STX ptrABH
	STY ptrABL			; Y=0

	ROR RXTX_Flags_D64		; ????

	LDA (&F0),Y			; Control block number
	STA &AA
	BNE Label_8FA6			; Read and delete RECEIVE block

	LDA #&03			; Start at block 3 (0-2 reserved?)

.Label_8F8D
	JSR Sub_8E55			; Y=A*12, A=A*4
	BCS Label_8FCF			; A was >17

	LSR A
	LSR A				; X=A/4
	TAX

	LDA (ptrABL),Y			; Flag
	BEQ Label_8FCF

	CMP #&3F
	BEQ Label_8FA1			; if block empty

	INX
	TXA
	BNE Label_8F8D			; always

.Label_8FA1				; A=first empty block
	TXA
	LDX #&00
	STA (&F0,X)			; Save to OW control block

.Label_8FA6
	JSR Sub_8E55			; Y=A*12
	BCS Label_8FCF			; If A was >17
	DEY
	STY ptrABL			; Set (AB)+1->control block

	LDA #&C0
	LDY #&01
	LDX #&0B			; Copy 12 bytes
	CPY &AA				; C=1 if Y>=&?AA, i.e. if ?&AA=0
	ADC (ptrABL),Y			; Flag
	BEQ Label_8FBD			; If Flag=&3F+&C0+1=0  i.e. If ?&AA=0,Flag=&3F
					; note C becomes 1
	BMI Label_8FCA			; If Flag<&40 or Flag>=&C0

.Label_8FBC
	CLC

.Label_8FBD
	JSR Copy_Control_Block_Sub_8F1C	; Copy block (C preserved)
	BCS Label_8FD1			; If C=1 F0->AB else AB->F0

	LDA #&3F			; Mark block as empty
	LDY #&01
	STA (ptrABL),Y
	BNE Label_8FD1			; always

.Label_8FCA
	ADC #&01
	BNE Label_8FBC			; If Flag was not &3F
	DEY

.Label_8FCF
	STA (&F0),Y			; Set OW control block number to zero

.Label_8FD1
	ROL RXTX_Flags_D64		; ???
	RTS
}

.Sub_8FD5
{
	LDY #&1C			; REC cb2 Buffer start = OW cb address +1
	LDA &F0
	ADC #&01			; C=1 so +2
	JSR Sub_8FE6
	LDY #&01
	LDA (&F0),Y			; OW size of rest of block
	LDY #&20			; REC cb2 Buffer end
	ADC &F0

.Sub_8FE6
	STA (ptr9EL_PWS1),Y
	INY
	LDA &F1
	ADC #&00
	STA (ptr9EL_PWS1),Y
	RTS
}


	\\ OSWORD &14 Communicate with the file server.
.OSWORD14_SUB_8FEF
{
	CMP #&01			; A=OW?0
	BCS Label_903E			; If>=1  (<>0)

	\ OW?0 = 0
	LDY #&23			; PWSP2+&17=RECEIVE control block 2

.Loop_8FF6
	LDA Data_83AD-&18,Y		; Ctrl block template
	BNE Label_8FFE			; If Y not &19,&1A,&1B: Port & Station

	LDA &0DE6,Y			; &DE6+&1A=&E00 is FS station

.Label_8FFE
	STA (ptr9EL_PWS1),Y
	DEY
	CPY #&17
	BNE Loop_8FF6			; C=1

	INY
	STY ptr9AL

	JSR Sub_8FD5			; Set REC cb2 buffer addresses -> OW cb +2

	LDY #&02
	LDA #&90
	STA ESC_ON			; Enable ESCAPE
	STA (&F0),Y			; Reply port number = &90

	INY
	INY				; Y=4

.Loop_9015
	LDA &0DFE,Y			; &DFE+4=&E02
	STA (&F0),Y			; Copy file handles of directories: main, current dir + library
	INY
	CPY #&07
	BNE Loop_9015

	LDA ptr9EH_PWS1
	STA ptr9AH			; (9A)->REC cb2

	CLI				; Allow IRQs
	JSR Sub_85FF_TRANSMIT_ptr9A	; TRANSMIT!!!!!!!  

	LDY #&20
	LDA #&FF			; REC cb2 Buffer end=&FFFF
	STA (ptr9EL_PWS1),Y
	INY
	STA (ptr9EL_PWS1),Y
	LDY #&19
	LDA #&90
	STA (ptr9EL_PWS1),Y		; REC cb2 Port=&90
	DEY
	LDA #&7F
	STA (ptr9EL_PWS1),Y		; REC cb2 Ctrl byte=&7F
	JMP Sub_8530_PollForReply

	\ OW?0 > 0
.Label_903E
	PHP

	LDY #&01
	LDA (&F0),Y
	TAX
	INY
	LDA (&F0),Y
	INY
	STY ptrABL
	LDY #&72
	STA (ptr9CL_PWS0),Y
	DEY
	TXA
	STA (ptr9CL_PWS0),Y

	PLP
	BNE Label_9071			; If OW?0 <> 1

.Label_9055
	LDY ptrABL
	INC ptrABL
	LDA (&F0),Y
	BEQ Label_9070

	LDY #&7D
	STA (ptr9CL_PWS0),Y
	PHA
	JSR Sub_917F
	JSR Sub_907C

.Label_9068
	DEX
	BNE Label_9068

	PLA
	EOR #&0D
	BNE Label_9055

.Label_9070
	RTS

	\ OW?0 > 1
.Label_9071
	JSR Sub_917F
	LDY #&7B
	LDA (ptr9CL_PWS0),Y
	ADC #&03
	STA (ptr9CL_PWS0),Y

.Sub_907C
	CLI
	JMP Sub_85FF_TRANSMIT_ptr9A	; TRANSMIT!!!
}


	\\ Calls through NETV
	\\ A = Operation (0 to 8 inclusive)
	\\ See AUG Page 260
.NFS_NETV_ENTRY
{
	PHP
	PHA
	TXA
	PHA
	TYA
	PHA

	TSX
	LDA &0103,X			; Restore A
	CMP #&09
	BCS Label_9092			; A>=9

	TAX
	JSR Sub_9099

.Label_9092
	PLA
	TAY
	PLA
	TAX
	PLA
	PLP

.Label_9098
	RTS

.Sub_9099
	LDA Data_90AD,X
	PHA
	LDA Data_90A4,X
	PHA

	LDA &EF				; OSBYTE 'A'
	RTS

.Data_90A4
	EQUB LO(NETV_CALL_0-1)		; 0 Exit
	EQUB LO(NETV_CALL_1-1)		; 1 Network Printer
	EQUB LO(NETV_CALL_1-1)		; 2 Network Printer
	EQUB LO(NETV_CALL_1-1)		; 3 Network Printer
	EQUB LO(NETV_CALL_4-1)		; 4 Write character attempted
	EQUB LO(NETV_CALL_5-1)		; 5 Network Printer
	EQUB LO(NETV_CALL_0-1)		; 6 Exit (Read character attempted)
	EQUB LO(NETV_CALL_7-1)		; 7 OSBYTE attempted
	EQUB LO(NETV_CALL_8-1)		; 8 OSWORD attempted

.Data_90AD
	EQUB HI(NETV_CALL_0-1)
	EQUB HI(NETV_CALL_1-1)
	EQUB HI(NETV_CALL_1-1)
	EQUB HI(NETV_CALL_1-1)
	EQUB HI(NETV_CALL_4-1)
	EQUB HI(NETV_CALL_5-1)
	EQUB HI(NETV_CALL_0-1)
	EQUB HI(NETV_CALL_7-1)
	EQUB HI(NETV_CALL_8-1)
}

	\\ NETV call with A=4
	\\ Write character attempted.
	\\ Y = character to be outputted.
	\\ (Enabled by OSBYTE &D0.)
.NETV_CALL_4
	TSX
	ROR &0106,X
	ASL &0106,X			; Set carry flag (on exit)

	TYA
	LDY #&DA
	STA (ptr9EL_PWS1),Y

	LDA #&00

.Sub_90C4
	LDY #&D9
	STA (ptr9EL_PWS1),Y

	LDA #&80

	LDY #&0C
	STA (ptr9EL_PWS1),Y

	LDA ptr9AL			; Push ptr9A
	PHA
	LDA ptr9AH
	PHA

	STY ptr9AL			; ptr9A = ptr9E + &C
	LDX ptr9EH_PWS1
	STX ptr9AH

	JSR Sub_85FF_TRANSMIT_ptr9A	; Transmit

	LDA #&3F
	STA (ptr9AL,X)

	PLA				; Pull ptr9A
	STA ptr9AH
	PLA
	STA ptr9AL
	RTS


	\\ NETV call with A=7
	\\ OSBYTE attempted.
	\\ Entry A = OSBYTE 'A'
.NETV_CALL_7
{
	LDY &F1				; OSBYTE 'Y'
	CMP #&81
	BEQ Label_9101			; If INKEY

	LDY #&01
	LDX #&09
	JSR Sub_913C
	BEQ Label_9101

	DEY
	DEY
	LDX #&0E
	JSR Sub_913C
	BEQ Label_9101

	INY

.Label_9101
	LDX #&02
	TYA
	BEQ Label_913B

	PHP
	BPL Label_910A

	INX

.Label_910A
	LDY #&DC

.Label_910C
	LDA &0015,Y
	STA (ptr9EL_PWS1),Y
	DEY
	CPY #&DA
	BPL Label_910C

	TXA
	JSR Sub_90C4
	PLP
	BPL Label_913B

	LDA #&7F
	LDY #&0C
	STA (ptr9EL_PWS1),Y

.Label_9123
	LDA (ptr9EL_PWS1),Y
	BPL Label_9123

	TSX
	LDY #&DD
	LDA (ptr9EL_PWS1),Y
	ORA #&44
	BNE Label_9134

.Label_9130
	DEY
	DEX
	LDA (ptr9EL_PWS1),Y

.Label_9134
	STA &0106,X
	CPY #&DA
	BNE Label_9130

.Label_913B
	RTS
}

	\ Is OSBYTE in list?
.Sub_913C
	CMP Data_9145,X
	BEQ Return_9144

	DEX
	BPL Sub_913C

.Return_9144
	RTS

.Data_9145
	EQUB &04, &09, &0A, &15, &9A, &9B, &E1, &E2
	EQUB &E3, &E4, &0B, &0C, &0F, &79, &7A

.NETV_CALL_8				; NETV call with A=8
{
	LDY #&0E
	CMP #&07
	BEQ Label_915E

	CMP #&08			; OSWORD attempted?
	BNE Return_9144

.Label_915E
	LDX #&DB
	STX ptr9EL_PWS1

.Loop_9162
	LDA (&F0),Y
	STA (ptr9EL_PWS1),Y
	DEY
	BPL Loop_9162

	INY
	DEC ptr9EL_PWS1

	LDA &EF
	STA (ptr9EL_PWS1),Y
	STY ptr9EL_PWS1
	LDY #&14
	LDA #&E9
	STA (ptr9EL_PWS1),Y

	LDA #&01
	JSR Sub_90C4

	STX ptr9EL_PWS1
}

.Sub_917F
	LDX #&0D
	LDY #&7C
	BIT Data_83B3			; Set V
	BVS Loop_918D			; always

.Sub_9188
	LDY #&17
	LDX #&1A

.Sub_918C
	CLV

.Loop_918D
{
	LDA Data_91B4,X
	CMP #&FE
	BEQ Label_91B0

	CMP #&FD
	BEQ Label_91AC

	CMP #&FC
	BNE Label_91A4

	LDA ptr9CH_PWS0
	BVS Label_91A2

	LDA ptr9EH_PWS1

.Label_91A2
	STA ptr9AH

.Label_91A4
	BVS Label_91AA

	STA (ptr9EL_PWS1),Y
	BVC Label_91AC

.Label_91AA
	STA (ptr9CL_PWS0),Y

.Label_91AC
	DEY
	DEX
	BPL Loop_918D

.Label_91B0
	INY
	STY ptr9AL
	RTS

.Data_91B4
	EQUB  &85, &00, &FD, &FD, &7D, &FC, &FF, &FF
	EQUB  &7E, &FC, &FF, &FF, &00, &00, &FE, &80
	EQUB  &93, &FD, &FD, &D9, &FC, &FF, &FF, &DE
	EQUB  &FC, &FF, &FF, &FE, &D1, &FD, &FD, &1F
	EQUB  &FD, &FF, &FF, &FD, &FD, &FF, &FF
}

.NETV_CALL_5				; NETV call with A=5
{
	DEX
	CPX &F0
	BNE Label_91E7

	LDA #&1F
	STA &0D61
	LDA #&41

.Label_91E7
	STA &99
}
.Return_91E9
	RTS

.NETV_CALL_1				; Call via NETV when A=1,2 or 3 (X=A)
{					; Control network printer (Y=printer type)
	CPY #&04			; Printer type = 4 = network printer
	BNE Return_91E9

	TXA
	DEX
	BNE Label_9218			; if A<>1

	TSX
	ORA &0106,X
	STA &0106,X

.Loop_91F9
	LDA #&91			; Get chr from printer buffer
	LDX #&03
	JSR OSBYTE
	BCS Return_91E9			; If buffer empty

	TYA				; Y = character
	JSR Sub_920F
	CPY #&6E
	BCC Loop_91F9

	JSR Sub_9237
	BCC Loop_91F9

.Sub_920F
	LDY &0D61
	STA (ptr9CL_PWS0),Y
	INC &0D61
	RTS

.Label_9218
	PHA
	TXA
	EOR #&01
	JSR Sub_920F
	EOR &99
	ROR A
	BCC Label_922A

	ROL A
	STA &99
	JSR Sub_9237

.Label_922A
	LDA &99
	AND #&F0
	ROR A
	TAX
	PLA
	ROR A
	TXA
	ROL A
	STA &99
	RTS
}

.Sub_9237
	LDY #&08
	LDA &0D61
	STA (ptr9EL_PWS1),Y

	LDA ptr9CH_PWS0
	INY
	STA (ptr9EL_PWS1),Y
	LDY #&05
	STA (ptr9EL_PWS1),Y

	LDY #&0B
	LDX #&26
	JSR Sub_918C

	DEY
	LDA &99
	PHA
	ROL A
	PLA
	EOR #&80
	STA &99
	ROL A
	STA (ptr9EL_PWS1),Y

	LDY #&1F
	STY &0D61
	LDA #&00
	TAX
	LDY ptr9EH_PWS1

	CLI

	\ Y:X -> rcv buffer control block
	\ A = Internal Handle
	\ Exit: C=0

.Sub_9266_Do_Transaction_ptr9A
{
	STX ptr9AL			; ptr9A -> rcv control block
	STY ptr9AH

	PHA
	AND &0E08
	BEQ Label_9272

	LDA #&01

.Label_9272
	LDY #&00
	ORA (ptr9AL),Y
	PHA
	STA (ptr9AL),Y

	JSR Sub_85FF_TRANSMIT_ptr9A

	LDA #&FF			; Buffer end = &FFFFFFFF
	LDY #&08
	STA (ptr9AL),Y
	INY
	STA (ptr9AL),Y

	PLA
	TAX

	LDY #&D1

	PLA
	PHA
	BEQ Label_928F

	LDY #&90

.Label_928F
	TYA
	LDY #&01
	STA (ptr9AL),Y			; Receiving Port

	TXA
	DEY
	PHA

.Loop_9297
	LDA #&7F
	STA (ptr9AL),Y

	JSR Sub_8530_PollForReply

	PLA
	PHA
	EOR (ptr9AL),Y
	ROR A
	BCS Loop_9297			; If b0 set

	PLA
	PLA
	EOR &0E08
	RTS
}

.NFS_CALL_10
{
	LDA &AD
	PHA

	LDA #&E9
	STA ptr9EL_PWS1			; Offset of private workspace

	LDY #&00
	STY &AD

	LDA &0350			; Word &350 = Address of top left corner
	STA (ptr9EL_PWS1),Y		; of screen (as is sent to 6845).
	INC ptr9EL_PWS1
	LDA &0351
	PHA

	TYA				; A = 0

.Loop_92C2
	STA (ptr9EL_PWS1),Y		; Logical colour

	LDX ptr9EL_PWS1
	LDY ptr9EH_PWS1
	LDA #&0B
	JSR OSWORD			; Read palette (logical colour ?(Y:X)).
					; (Y:X)!1 = physical colour

	PLA
	LDY #&00
	STA (ptr9EL_PWS1),Y

	INY
	LDA (ptr9EL_PWS1),Y
	PHA

	LDX ptr9EL_PWS1
	INC ptr9EL_PWS1
	INC &AD
	DEY

	LDA &AD
	CPX #&F9
	BNE Loop_92C2

	PLA
	STY &AD

	INC ptr9EL_PWS1
	JSR Sub_92F7
	INC ptr9EL_PWS1

	PLA
	STA &AD
}

.Sub_92F0_RestoreProtectionMask
	LDA ProtectionMaskCopy_D65
	STA ProtectionMask_D63
	RTS

.Sub_92F7
	LDA &0355
	STA (ptr9EL_PWS1),Y
	TAX
	JSR Sub_930A
	INC ptr9EL_PWS1
	TYA
	STA (ptr9EL_PWS1,X)
	JSR Sub_9308

.Sub_9308
	LDX #&00

.Sub_930A
{
	LDY &AD
	INC &AD

	INC ptr9EL_PWS1

	LDA Data_931E,Y
	LDY #&FF
	JSR OSBYTE

	TXA
	LDX #&00
	STA (ptr9EL_PWS1,X)
	RTS

.Data_931E
	EQUB &85	; Read bottom of display RAM for MODE X, exit Y:X = address.
	EQUB &C2	; R/W mark period count (flashing colours).
	EQUB &C3	; R/W space period count (flashing colours).
}

