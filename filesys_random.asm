	\\ Acorn DFS
	\\ filesys_random.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

.fscv7_hndlrange	
	LDX #&11			;lowest hndl issued
	LDY #&15			;highest hndl poss.

.closeallfiles_exit
	RTS

.fscv6_shutdownfilesys
	JSR rememberAXY			;Close any SPOOL or

.CloseSPOOL_EXECfiles
	LDA #&77			;EXEC files
if sys<>224
	JMP OSBYTE			;(Causes ROM serv.call &10)
else
	JSR OSBYTE
	JMP SERVICE0A_claim_statworkspace
endif

if sys>120
.CMD_CLOSE
	LDA #&20			;\ *CLOSE
	STA swsp+&1086			;\
endif

.CloseAllFiles_Osbyte77
	JSR fscv6_shutdownfilesys

.CloseAllFiles
	LDA #&00			;intch=intch+&20

.closeallfiles_loop
	CLC 
	ADC #&20
	BEQ closeallfiles_exit

	TAY 
	JSR CloseFileY
	BNE closeallfiles_loop		;always

.CloseFilesY
	LDA #&20			;Update catalogue if write only
	STA swsp+&1086
	TYA 
	BEQ CloseAllFiles_Osbyte77	;If y=0 Close all files

	JSR CheckChannelY

.CloseFileY
{
	PHA		 		;Save A
	JSR IsHndlinUseY		;(Saves X to &10C5)
	BCS closefile_exit		;If file not open

	LDA swsp+&111B,Y		;bit mask
	EOR #&FF
	AND swsp+&10C0
	STA swsp+&10C0			;Clear 'open' bit
	LDA swsp+&1117,Y		;A=flag byte
	AND #&60
	BEQ closefile_exit		;If bits 5&6=0

	JSR Channel_SetDirDrvGetCatEntry
	LDA swsp+&1117,Y		;If file extended and not
	AND swsp+&1086			;forcing buffer to disk
	BEQ closefile_buftodisk		;update the file length

	LDX swsp+&10C3			;X=cat offset
	LDA swsp+&1114,Y		;File lenth = EXTENT
	STA swsp+&0F0C,X		;Len lo
	LDA swsp+&1115,Y
	STA swsp+&0F0D,X		;Len mi
	LDA swsp+&1116,Y
	JSR Aasl4			;Len hi
	EOR swsp+&0F0E,X		;"mixed byte"
	AND #&30
	EOR swsp+&0F0E,X
	STA swsp+&0F0E,X
	JSR SaveCatToDisk		;Update catalog
	LDY swsp+&10C2

.closefile_buftodisk
	JSR ChannelBufferToDiskY2	;Restores Y

.closefile_exit
	LDX swsp+&10C5			;Restore X (IsHndlInUse)
	PLA 				;Restore A
	RTS
}

.Channel_SetDirDrvGetCatEntry
	JSR Channel_SetDirDriveY

.Channel_GetCatEntryY
{
	LDX #&06			;Copy filename from

.chnl_getcatloop
	LDA swsp+&110C,Y		;channel info to &C5
	STA &C5,X
	DEY 
	DEY 
	DEX 
	BPL chnl_getcatloop

	JSR get_cat_entry80
	BCC errDISKCHANGED		;If file not found

	STY swsp+&10C3			;?&10C3=cat file offset
	LDY swsp+&10C2			;Y=intch
}
.chkdskchangexit
	RTS

.Channel_SetDirDriveY
	LDA swsp+&110E,Y		;Directory
	AND #&7F
	STA DirectoryParam
	LDA swsp+&1117,Y		;Drive
	JMP SetCurrentDriveA

.CheckForDiskChange
	JSR rememberAXY
	LDA swsp+&0F04
	JSR LoadCurDrvCat
	CMP swsp+&0F04
	BEQ chkdskchangexit		;If cycle no not changed!

.errDISKCHANGED
	JSR errDISK
	EQUS &C8, "changed", 0

	\ OSFIND: A=&40 ro, &80 wo, &C0 rw
.FINDV_ENTRY
	AND #&C0			;Bit 7=open for output
	BNE findv__0_openfile		;Bit 6=open for input

	JSR rememberAXY
	JMP CloseFilesY			;Close file #Y

.findv__0_openfile
	JSR rememberXYonly		;Open file
	STX &BA				;YX=Location of filename
	STY &BB
	STA &B4				;A=Operation
	BIT &B4
	PHP 
	JSR read_afspBA_reset
	JSR parameter_fsp
	JSR get_cat_firstentry
	BCS findv_filefound		;If file found

	PLP 
	BVC findv_createfile		;If not read only = write only

	LDA #&00			;A=0=file not found
	RTS 				;EXIT

.findv_createfile
{
	PHP 				;Clear data
	LDA #&00			;B7-C3=0
	LDX #&07			;1074-107B=0

.findv_loop1
	STA &BC,X
	STA swsp+&1074,X
	DEX 
	BPL findv_loop1

if sys>120
	DEC &BE				;\
	DEC &BF				;\
	DEC swsp+&1076			;\
	DEC swsp+&1077			;\
endif
	LDA #&40
	STA &C3				;End address = &4000
	JSR CreateFile_fspBA		;Creates 40 sec buffer
}
.findv_filefound
{
	PLP 				;in case another file created
	PHP 
	BVS findv_readorupdate		;If opened for read or update

	JSR CheckFileNotLockedY		;If locked report error

.findv_readorupdate
	JSR IsFileOpenY			;Exits with Y=intch, A=flag
	BCC findv_openchannel		;If file not open

.findv_loop2
	LDA swsp+&110C,Y
	BPL errFILEOPEN			;If already opened for writing

if sys=120
	BIT &B4
else
	PLP 				;\ Slightly difference
	PHP 				;\
endif
	BMI errFILEOPEN			;If opening again to write

	JSR IsFileOpenContinue		;** File can only be opened  **
	BCS findv_loop2			;** once if being written to **

.findv_openchannel
	LDY swsp+&10C2			;Y=intch
	BNE SetupChannelInfoBlock
}

.errTOOMANYFILESOPEN
	JSR Brk100_Err_ChkIfBuf
	EQUS &C0, "Too many open", 0

.errFILEOPEN
	JSR Brk100_Err_ChkIfBuf
	EQUS &C2, "Open", 0

.SetupChannelInfoBlock
{
	LDA #&08
	STA swsp+&10C4

.chnlblock_loop1
	LDA swsp+&0E08,X		;Copy file name & attributes
	STA swsp+&1100,Y		;to channel info block
	INY 
	LDA swsp+&0F08,X
	STA swsp+&1100,Y
	INY 
	INX 
	DEC swsp+&10C4
	BNE chnlblock_loop1

	LDX #&10
	LDA #&00			;Clear rest of block

.chnlblock_loop2
	STA swsp+&1100,Y
	INY 
	DEX 
	BNE chnlblock_loop2

	LDA swsp+&10C2			;A=intch
	TAY 
	JSR Alsr5
	ADC #HI(swsp+&1100)
	STA swsp+&1113,Y		;Buffer page
	LDA swsp+&10C1
	STA swsp+&111B,Y		;Mask bit
	ORA swsp+&10C0
	STA swsp+&10C0			;Set bit in open flag byte
	LDA swsp+&1109,Y		;Length0
	ADC #&FF			;If Length0>0 C=1
	LDA swsp+&110B,Y		;Length1
	ADC #&00
	STA swsp+&1119,Y		;Sector count
	LDA swsp+&110D,Y		;Mixed byte
	ORA #&0F
	ADC #&00			;Add carry flag
	JSR Alsr4and3			;Length2
	STA swsp+&111A,Y
	PLP 
	BVC chnlblock_setBit5		;If not read = write
	BMI chnlblock_setEXT		;If updating

	LDA #&80			;Set Bit7 = Read Only
	ORA swsp+&110C,Y
	STA swsp+&110C,Y

.chnlblock_setEXT
	LDA swsp+&1109,Y		;EXTENT=file length
	STA swsp+&1114,Y
	LDA swsp+&110B,Y
	STA swsp+&1115,Y
	LDA swsp+&110D,Y
	JSR Alsr4and3
	STA swsp+&1116,Y

.chnlblock_cont
	LDA CurrentDrv			;Set drive
	ORA swsp+&1117,Y
	STA swsp+&1117,Y
	TYA 				;convert intch to handle
	JSR Alsr5
	ORA #&10
	RTS 				;RETURN A=handle

.chnlblock_setBit5
	LDA #&20			;Set Bit5 = Update cat file len
	STA swsp+&1117,Y		;when channel closed
	BNE chnlblock_cont		;always
}

.IsFileOpenContinue
	TXA 				;Continue looking for more
	PHA 				;instances of file being open
	JMP fop_nothisfile

.IsFileOpenY
	LDA #&00
	STA swsp+&10C2
	LDA #&08
	STA &B5				;Channel flag bit
	TYA 
	TAX 				;X=cat offset
	LDY #&A0			;Y=intch

.fop_main_loop
{
	STY &B3
	TXA 				;save X
	PHA 
	LDA #&08
	STA &B2				;cmpfn_loop counter
	LDA &B5
	BIT swsp+&10C0
	BEQ fop_channelnotopen		;If channel not open

	LDA swsp+&1117,Y
	EOR CurrentDrv
	AND #&03
	BNE fop_nothisfile		;If not current drv?

.fop_cmpfn_loop
	LDA swsp+&0E08,X		;Compare filename
	EOR swsp+&1100,Y
	AND #&7F
	BNE fop_nothisfile

	INX 
	INY 
	INY 
	DEC &B2
	BNE fop_cmpfn_loop

	SEC 
	BCS fop_matchifC_1		;always

.fop_channelnotopen
	STY swsp+&10C2			;Y=intch = allocated to new channel
	STA swsp+&10C1			;A=Channel Flag Bit
}

.fop_nothisfile
	SEC 
	LDA &B3
	SBC #&20
	STA &B3				;intch=intch-&20
	ASL &B5				;flag bit << 1
	CLC 

.fop_matchifC_1
	PLA 				;restore X
	TAX 
	LDY &B3				;Y=intch
	LDA &B5				;A=flag bit
	BCS fop_exit
	BNE fop_main_loop		;If flag bit <> 0

.fop_exit
	RTS 				;Exit: A=flag Y=intch

.ChannelBufferToDiskY_A0
	JSR ReturnWithA_0

.ChannelBufferToDiskY
{
	LDA swsp+&10C0			;Force buffer save
	PHA 				;Save opened channels flag byte
	LDA #&00			;Don't update catalogue
	STA swsp+&1086
	TYA 				;A=handle
	BNE chbuf1

	JSR CloseAllFiles
	BEQ chbuf2			;always

.chbuf1
	JSR CloseFilesY

.chbuf2
	PLA 				;Restore
	STA swsp+&10C0
	RTS 
}

.ReturnWithA_0
	PHA 				;Sets the value of A
	TXA 				;restored by rememberAXY
	PHA 				;after returning from calling
	LDA #&00			;sub routine to 0
	TSX 
	STA &0109,X
	PLA 
	TAX 
	PLA 
	RTS

.ARGSV_ENTRY
	JSR rememberAXY
	CMP #&FF
	BEQ ChannelBufferToDiskY_A0	;If file(s) to media

	CPY #&00
	BEQ argsv_Y_0

if sys=224
	CMP #&04
else
	CMP #&03
endif
	BCS argsv_exit			;If A>=3

	JSR ReturnWithA_0

if sys=224
	CMP #&03
	BEQ sub_97A9
endif
	CMP #&01
	BNE argsv_rdseqptr_or_filelen

	JMP argsv_WriteSeqPointerY

.argsv_Y_0
	CMP #&02			;If A>=2
	BCS argsv_exit

	JSR ReturnWithA_0
	BEQ argsv_filesysnumber		;If A=0

	LDA #&FF
	STA &02,X			;4 byte address of
	STA &03,X			;"rest of command line"
	LDA swsp+&10D9			;(see *run code)
	STA &00,X
	LDA swsp+&10DA
	STA &01,X

.argsv_exit
	RTS 

.argsv_filesysnumber
	LDA #&04			;A=4 on exit = Disc filing system
	TSX 
	STA &0105,X
	RTS 

.argsv_rdseqptr_or_filelen
	JSR CheckChannelY		;A=0 OR A=2
	STY swsp+&10C2
	ASL A				;A becomes 0 or 4
	ADC swsp+&10C2
	TAY 
	LDA swsp+&1110,Y
	STA &00,X
	LDA swsp+&1111,Y
	STA &01,X
	LDA swsp+&1112,Y
	STA &02,X
	LDA #&00
	STA &03,X
	RTS

if sys=224
.sub_97A9
	JSR CheckChannelY
	LDA &00,X
	STA swsp+&1114,Y
	LDA &01,X
	STA swsp+&1115,Y
	LDA &02,x
	STA swsp+&1116,y
	RTS
endif

.IsHndlinUseY
{
	PHA 				;Save A
	STX swsp+&10C5			;Save X
	TYA 
	AND #&E0
	STA swsp+&10C2			;Save intch
	BEQ hndlinuse_notused_C_1

	JSR Alsr5			;ch.1-7
	TAY 				;creat bit mask
	LDA #&00			;1=1000 0000
	SEC 				;2=0100 0000 etc

.hndlinsue_loop
	ROR A
	DEY 
	BNE hndlinsue_loop

	LDY swsp+&10C2			;Y=intch
	BIT swsp+&10C0			;Test if open
	BNE hndlinuse_used_C_0

.hndlinuse_notused_C_1
	PLA 
	SEC 
	RTS

.hndlinuse_used_C_0
	PLA 
	CLC 
	RTS
}
 
.conv_Xhndl_intch
	PHA 
	TXA 
	JMP conv_hndl_X_entry

.conv_Yhndl_intch
	PHA 				;&10 to &17 are valid
	TYA

.conv_hndl_X_entry
{
	CMP #&10
	BCC conv_hndl_10

	CMP #&18
	BCC conv_hndl_18

.conv_hndl_10
	LDA #&08			;intch=0

.conv_hndl_18
	JSR Aasl5			;if Y<&10 or >&18
	TAY 				;ch0=&00, ch1=&20, ch2=&40
	PLA 				;ch3=&60…ch7=&E0
	RTS 				;c=1 if not valid
}

.ClearEXECSPOOLFileHandle
{
	LDA #&C6
	JSR osbyteX00YFF		;X = *EXEC file handle
	TXA 
	BEQ ClearSpoolhandle		;branch if no handle allocated

	JSR ConvertXhndl
	BNE ClearSpoolhandle		;If Y<>?10C2

	LDA #&C6			;Clear *EXEC file handle
	BNE osbyteX00Y00

.ClearSpoolhandle
	LDA #&C7			;X = *SPOOL handle
	JSR osbyteX00YFF
	JSR ConvertXhndl
	BNE clrsplhndl_exit		;If Y<>?10C2

	LDA #&C7			;Clear *SPOOL file handle

.osbyteX00Y00
	LDX #&00
	LDY #&00
	JMP OSBYTE

.ConvertXhndl
	TXA 
	TAY 
	JSR conv_Yhndl_intch
	CPY swsp+&10C2			;Owner?

.clrsplhndl_exit
	RTS
}

.fscv1_EOF
{
	PHA 
	TYA 
	PHA 
	TXA 
	TAY 
	JSR CheckChannelY
	TYA 
	JSR CmpPTRy_EXTa		;X=Y
	BNE eof_NOTEND

	LDX #&FF			;exit with X=FF
	BNE eof_exit

.eof_NOTEND
	LDX #&00			;exit with X=00

.eof_exit
	PLA 
	TAY 
	PLA 
}
.checkchannel_okexit
	RTS

.CheckChannelY
	JSR conv_Yhndl_intch
	JSR IsHndlinUseY
	BCC checkchannel_okexit

	JSR ClearEXECSPOOLFileHandle	;Next sub routine also calls this!

.errCHANNEL
	JSR Brk100_Err_ChkIfBuf
	EQUS &DE, "Channel", 0

.errEOF
	JSR Brk100_Err_ChkIfBuf
	EQUS &DF, "EOF", 0

.BGETV_ENTRY
{
	JSR rememberXYonly
	JSR CheckChannelY
	TYA 				;A=Y
	JSR CmpPTRy_EXTa
	BNE bg_notEOF			;If PTR<>EXT

	LDA swsp+&1117,Y		;Already at EOF?
	AND #&10
	BNE errEOF			;IF bit 4 set

	LDA #&10
	JSR ChannelFlags_SetBits	;Set bit 4
	LDX swsp+&10C5
	LDA #&FE
	SEC 
	RTS 				;C=1=EOF

.bg_notEOF
	LDA swsp+&1117,Y
	BMI bg_samesector1		;If buffer ok

	JSR Channel_SetDirDriveY
	JSR ChannelBufferToDiskY2	;Save buffer
	SEC 
	JSR ChannelBufferRWY		;Load buffer

.bg_samesector1
	LDA swsp+&1110,Y		;Seq.Ptr low byte
	STA &BA
	LDA swsp+&1113,Y		;Buffer address
	STA &BB
	LDY #&00
	LDA (&BA),Y			;Byte from buffer
	PHA 
	LDY swsp+&10C2			;Y=intch
	LDX &BA
	INX 
	TXA
	STA swsp+&1110,Y		;Seq.Ptr+=1
	BNE bg_samesector2

	CLC 
	LDA swsp+&1111,Y
	ADC #&01
	STA swsp+&1111,Y
	LDA swsp+&1112,Y
	ADC #&00
	STA swsp+&1112,Y
	JSR ChannelFlags_ClearBit7	;PTR in new sector!

.bg_samesector2
	CLC 
	PLA 
	RTS 				;C=0=NOT EOF
}

.CalcBufferSectorForPTR
	CLC 
	LDA swsp+&110F,Y		;Start Sector + Seq Ptr
	ADC swsp+&1111,Y
	STA &C3
	STA swsp+&111C,Y		;Buffer sector
	LDA swsp+&110D,Y
	AND #&03
	ADC swsp+&1112,Y
	STA &C2
	STA swsp+&111D,Y

.ChannelFlags_SetBit7
	LDA #&80			;Set/Clear flags (C=0 on exit)

.ChannelFlags_SetBits
	ORA swsp+&1117,Y
	BNE chnflg_save

.ChannelFlags_ClearBit7
	LDA #&7F

.ChannelFlags_ClearBits
	AND swsp+&1117,Y

.chnflg_save
	STA swsp+&1117,Y
	CLC 
	RTS 

.ChannelBufferToDiskY2
	LDA swsp+&1117,Y
	AND #&40			;Bit 6 set?
	BEQ chnbuf_exit2		;If no exit

	CLC 				;C=0=write buffer

.ChannelBufferRWY
{
	PHP 				;Save C
	INC swsp+&10DD			;Remember in case of error?
if sys=120
	JSR FDC_SetToCurrentDrv		; (&10C2)=intch
endif
	LDY swsp+&10C2			;Setup NMI vars
	LDA swsp+&1113,Y		;Buffer page
	STA &BD				;Data ptr
if sys=120
	JSR SetLoadAddrToHost
else
	LDA #&FF			;\ Set load address to host
	STA swsp+&1074			;\
	STA swsp+&1075			;\

endif
	LDA #&00
	STA &BC
	STA &C0				;Sector
	LDA #&01
	STA &C1
	PLP 
	BCS chnbuf_read			;IF c=1 load buffer else save Buffer sector

	LDA swsp+&111C,Y
	STA &C3				;Start sec. b0-b7
	LDA swsp+&111D,Y
	STA &C2				;"mixed byte"
	JSR SaveMemBlock
	LDY swsp+&10C2			;Y=intch
	LDA #&BF			;Clear bit 6
	JSR ChannelFlags_ClearBits
	BCC chnbuf_exit			;always

.chnbuf_read
	JSR CalcBufferSectorForPTR	;sets NMI data ptr
	JSR LoadMemBlock		;Load buffer

.chnbuf_exit
	DEC swsp+&10DD
	LDY swsp+&10C2			;Y=intch
}
.chnbuf_exit2
	RTS

.errFILELOCKED2
	JMP errFILELOCKED

.errFILEREADONLY
	JSR Brk100_Err_ChkIfBuf
	EQUS &C1, "Read only", 0

.bput_Y_intchan
	JSR rememberAXY
	JMP bp_entry

.BPUTV_ENTRY
	JSR rememberAXY
	JSR CheckChannelY

.bp_entry
{
	PHA 
	LDA swsp+&110C,Y
	BMI errFILEREADONLY

	LDA swsp+&110E,Y
	BMI errFILELOCKED2

	JSR Channel_SetDirDriveY
	TYA 
	CLC 
	ADC #&04
	JSR CmpPTRy_EXTa
	BNE bp_noextend			;If PTR<>Sector Count, i.e Ptr<sc

	JSR Channel_GetCatEntryY	;Enough space in gap?
	LDX swsp+&10C3			;X=cat file offset
	SEC 				;Calc size of gap
	LDA swsp+&0F07,X		;Next file start sector
	SBC swsp+&0F0F,X		;This file start
	PHA 				;lo byte
	LDA swsp+&0F06,X
	SBC swsp+&0F0E,X		;Mixed byte
	AND #&03			;hi byte
	CMP swsp+&111A,Y		;File size in sectors
	BNE bp_extendby100		;If must be <gap size

	PLA 
	CMP swsp+&1119,Y
	BNE bp_extendtogap		;If must be <gap size

	STY &B4				;Error, save intch handle
	STY swsp+&10C2			;for clean up
	JSR ClearEXECSPOOLFileHandle

.errCAN_TEXTEND
	JSR Brk100_Err_ChkIfBuf
	EQUS &BF, "Can't extend", 0

.bp_extendby100
	LDA swsp+&111A,Y		;Add maximum of &100
	CLC 				;to sector count
	ADC #&01			;(i.e. 64K)
	STA swsp+&111A,Y		;[else set to size of gap]
	ASL A				;Update cat entry
	ASL A
	ASL A
	ASL A
	EOR swsp+&0F0E,X		;Mixed byte
	AND #&30
	EOR swsp+&0F0E,X
	STA swsp+&0F0E,X		;File len 2
	PLA 
	LDA #&00

.bp_extendtogap
	STA swsp+&0F0D,X		;File len 1
	STA swsp+&1119,Y
	LDA #&00
	STA swsp+&0F0C,X		;File len 0
	JSR SaveCatToDisk
if sys=120
	JSR NMI_RELEASE_WaitFDCbusy
endif
	LDY swsp+&10C2			;Y=intch
.bp_noextend
	LDA swsp+&1117,Y
	BMI bp_savebyte			;If PTR in buffer

	JSR ChannelBufferToDiskY2	;Save buffer
	LDA swsp+&1114,Y		;EXT byte 0
	BNE bp_loadbuf			;IF <>0 load buffer

	TYA 
	JSR CmpPTRy_EXTa		;A=Y
	BNE bp_loadbuf			;If PTR<>EXT, i.e. PTR<EXT

	JSR CalcBufferSectorForPTR	;new sector!
	BNE bp_savebyte			;always

.bp_loadbuf
	SEC 				;Load buffer
	JSR ChannelBufferRWY

.bp_savebyte
	LDA swsp+&1110,Y		;Seq.Ptr
	STA &BA
	LDA swsp+&1113,Y		;Buffer page
	STA &BB
	PLA 
	LDY #&00
	STA (&BA),Y			;Byte to buffer
	LDY swsp+&10C2
	LDA #&40			;Bit 6 set = new data
	JSR ChannelFlags_SetBits
	INC &BA				;PTR=PTR+1
	LDA &BA
	STA swsp+&1110,Y
	BNE bp_samesecnextbyte

	JSR ChannelFlags_ClearBit7	;PTR in next sector
	LDA swsp+&1111,Y
	ADC #&01
	STA swsp+&1111,Y
	LDA swsp+&1112,Y
	ADC #&00
	STA swsp+&1112,Y

.bp_samesecnextbyte
	TYA 
	JSR CmpPTRy_EXTa
	BCC bp_exit			;If PTR<EXT

	LDA #&20			;Update cat file len when closed
	JSR ChannelFlags_SetBits	;Set bit 5
	LDX #&02			;EXT=PTR

.bp_setextloop
	LDA swsp+&1110,Y
	STA swsp+&1114,Y
	INY 
	DEX 
	BPL bp_setextloop
}
.bp_exit
	RTS

.argsv_WriteSeqPointerY
{
	JSR rememberAXY			;Write Sequential Pointer
	JSR CheckChannelY		;(new ptr @ 00+X)
	LDY swsp+&10C2

.wsploop
	JSR CmpNewPTRwithEXT
	BCS SetSeqPointerY		;If EXT >= new PTR

	LDA swsp+&1114,Y		;else new PTR>EXT so pad with a 0
	STA swsp+&1110,Y
	LDA swsp+&1115,Y		;first, actual PTR=EXT
	STA swsp+&1111,Y
	LDA swsp+&1116,Y
	STA swsp+&1112,Y
	JSR IsSeqPointerInBufferY	;Update flags
	LDA &B6
	PHA 				;Save &B6,&B7,&B8
	LDA &B7
	PHA 
	LDA &B8
	PHA 
	LDA #&00
	JSR bput_Y_intchan		;Pad
	PLA 				;Restore &B6,&B7,&B8
	STA &B8
	PLA 
	STA &B7
	PLA 
	STA &B6
	JMP wsploop			;Loop
}

.SetSeqPointerY
	LDA &00,X			;Set Sequential Pointer
	STA swsp+&1110,Y
	LDA &01,X
	STA swsp+&1111,Y
	LDA &02,X
	STA swsp+&1112,Y

.IsSeqPointerInBufferY
	LDA #&6F			;Clear bits 7 & 4 of 1017+Y
	JSR ChannelFlags_ClearBits
	LDA swsp+&110F,Y		;Start sector
	ADC swsp+&1111,Y		;Add sequ.ptr
	STA swsp+&10C4
	LDA swsp+&110D,Y		;Mixed byte
	AND #&03			;Start sector bits 8&9
	ADC swsp+&1112,Y
	CMP swsp+&111D,Y
	BNE bp_exit

	LDA swsp+&10C4
	CMP swsp+&111C,Y
	BNE bp_exit

	JMP ChannelFlags_SetBit7	;Seq.Ptr in buffered sector

.CmpPTRy_EXTa
{
	TAX 
	LDA swsp+&1112,Y
	CMP swsp+&1116,X
	BNE cmpPE_exit

	LDA swsp+&1111,Y
	CMP swsp+&1115,X
	BNE cmpPE_exit

	LDA swsp+&1110,Y
	CMP swsp+&1114,X

.cmpPE_exit
	RTS
}

.CmpNewPTRwithEXT
	LDA swsp+&1114,Y		;Compare ctl blk ptr
	CMP &00,X			;to existing
	LDA swsp+&1115,Y		;Z=1 if same
	SBC &01,X			;(ch.1=&1138)
	LDA swsp+&1116,Y
	SBC &02,X
	RTS 				;C=p>=n

\\\ end cut here
