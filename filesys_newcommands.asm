	\\ Acorn DFS
	\\ filesys_newcommands.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

	\ Contains the new commands:
	\ *VERIFY
	\ *FORM
	\ *ROMS

.CMD_VERIFY
	LDA #&00			;\\\\\ *VERIFY
	BEQ Label_A5C4_v

.CMD_FORM
	LDA #&FF			;\\\\\ *FORM
	STA swsp+&1082

.Label_A5C4_v
{
	STA &C9
	STA swsp+&1090
	BPL Label_A5E5			;If verifying

	JSR InitParam			;Get number of tracks (40/80)
	JSR Decimal_TxtPtrToBinary
	STA swsp+&109F
	BCS Label_A5E2

	CMP #&23
	BEQ Label_A5E5			;If =45

	CMP #&28
	BEQ Label_A5E5			;If =40

	CMP #&50
	BEQ Label_A5E5			;If =80

.Label_A5E2
	JMP errSYNTAX

.Label_A5E5
	JSR GoGSINIT
	STY &CA

	BNE Label_A637_driveloop
	BIT &C9
	BMI Label_A5FB			;IF formatting

	JSR PrtString
	EQUS "Verify"
	BCC Label_A605			;always

.Label_A5FB
	JSR PrtString
	EQUS "Format"
	NOP 

.Label_A605
	JSR PrtString
	EQUS " which drive ? "
	NOP 
	JSR OSRDCH
	BCS jmp_reportEscape

	CMP #&20
	BCC jmp_errBadDrive

	JSR PrtChrA
	SEC 
	SBC #&30
	BCC jmp_errBadDrive

	CMP #&04
	BCS jmp_errBadDrive		;If >=4

	STA CurrentDrv
	JSR prtNewLine
	LDY &CA
	JMP Label_A63A

.Label_A637_driveloop
	JSR GetDriveNo

.Label_A63A
	STY &CA
	BIT &C9
	BPL Label_A647			;If verifying

	LDX CurrentDrv
	LDA #&00
	STA swsp+&10DE,X		;Reset drive mode

.Label_A647
	BIT swsp+&1090
	BPL Label_A652			;If verifying or already done

	JSR IsEnabledOrGo
	JSR CalcRAM

.Label_A652
	JSR SUB_A663
	LDY &CA
	JSR GoGSINIT
	BNE Label_A637_driveloop	;More drives?

	RTS
}

.jmp_reportEscape
	JMP reportESCAPE

.jmp_errBadDrive
	JMP errBADDRIVE

.SUB_A663
{
	BIT &C9
	BMI Label_A675			;If formatting

	JSR PrtString
	EQUS "Verifying"
	BCC Label_A68A			;always

.Label_A675
	JSR SUB_A76C_Clear_E00_FFF
	JSR PrtString
	EQUS "Formatting"
	LDX CurrentDrv
	STX swsp+&1090

.Label_A68A
	JSR PrtString
	EQUS " drive "
	LDA CurrentDrv
	JSR prthexLoNib
	JSR PrtString
	EQUS " track   "
	NOP 
	BIT &C9
	BMI Label_A6B6			;If formatting

	JSR CalcTracksOnDisk
	TXA 
	BNE Label_A6B3
	JMP prtNewLine

.Label_A6B3
	STA swsp+&109F			;number of tracks

.Label_A6B6
	LDA #&00
	STA swsp+&1097			;track

.Label_A6BB_trackloop
	LDA #&08
	JSR PrtChrA
	JSR PrtChrA
	LDA swsp+&1097
	JSR PrtHexA			;print track
	LDA #&06
	STA swsp+&109D			;try up to 5 times!

.Label_A6CE_tryloop
	JSR SUB_A73D_setupOs7F_paramblock	;defaults to "format"
	BIT &C9
	BPL Label_A6E2			;If verifying

	JSR SUB_A788_build_sector_table
	LDX #LO(swsp+&1090)
	LDY #HI(swsp+&1090)
	JSR Osword7F_8271_Emulation
	TAX 				;A=result
	BNE Label_A70A

.Label_A6E2
	LDA #&00			;Modify param block
	STA swsp+&1098			;sector
	LDA swsp+&1099
	AND #&1F			;no.of sectors only
	STA swsp+&1099
	LDA #&03
	STA swsp+&1095			;no.of parameters
	LDA #&5F
	STA swsp+&1096			;8271 command 5F:
	JSR CheckESCAPE			;"Read data & deleted data"
	LDX #LO(swsp+&1090)
	LDY #HI(swsp+&1090)
	JSR Osword7F_8271_Emulation
	BEQ Label_A712			;If ok

	DEC swsp+&109D
	BNE Label_A6CE_tryloop		;Try again!

.Label_A70A
	JSR PrtString
	EQUS "?"
	NOP 
	JMP FDC_ReportDiskFault_A_fault	;Fatal

.Label_A712
	LDA swsp+&109D
	CMP #&06
	BEQ Label_A721			;If no error(s) occurred

	JSR PrtString
	EQUS "?   " 
	NOP 

.Label_A721
	BIT &C9
	BPL Label_A728			;If verifying
	JSR SUB_A779_seccount__10

.Label_A728
	INC swsp+&1097			;track
	LDA swsp+&1097
	CMP swsp+&109F			;more tracks?
	BNE Label_A6BB_trackloop

	BIT &C9
	BPL Label_A73A			;If verifying

	JSR SaveCatToDisk_DontIncCycleNo	;Write new catalogue

.Label_A73A
	JMP prtNewLine
}

.SUB_A73D_setupOs7F_paramblock
	LDX #&00			;Setup parameter block
	STX swsp+&1091			;Load address = &FFFFxx00
	STX swsp+&109A			;Gap 5 size (bytes)
	DEX 
	STX swsp+&1093
	STX swsp+&1094
	LDA PAGE
	STA swsp+&1092			;xx=PAGE
	LDA #&05
	STA swsp+&1095			;5 parameters
	LDA #&63
	STA swsp+&1096			;8271 command &63 = format track
	LDA #&2A			;001-01010  1-10
	STA swsp+&1099			;Size/No.of sectors (1=256,10/trk)
	LDX #&10
	LDY #&13
	STX swsp+&109B			;Gap 1 size (bytes)
	STY swsp+&1098			;Gap 3 size (bytes)
	RTS

.SUB_A76C_Clear_E00_FFF
{
	LDA #&00			;Clear Cat
	TAY 

.Label_A76F
	STA swsp+&0E00,Y
	STA swsp+&0F00,Y
	INY 
	BNE Label_A76F

	RTS
}

.SUB_A779_seccount__10
{
	LDA #&0A			;Cat Sector Count +=10
	CLC 
	ADC swsp+&0F07
	STA swsp+&0F07
	BCC Label_A787

	INC swsp+&0F06

.Label_A787
	RTS
}

.SUB_A788_build_sector_table
{
	LDA #&00			;SECTOR IDs for Formatting
	STA &B0				;B0 -> PAGE
	LDA PAGE
	STA &B1
	LDA #&0A
	STA &B2				;B2 = loop counter
	LDA swsp+&1097			;track
	BEQ Label_A7A4			;IF track=0

	LDY #&02
	LDA (&B0),Y
	CLC 
	ADC #&07			;stagger sector?
	JSR SUB_A7C5_X_A_MOD_10

.Label_A7A4
	TAX 				;X=sector
	LDY #&00

.Label_A7A7_loop
	LDA swsp+&1097			;track
	STA (&B0),Y
	INY 
	LDA #&00			;0
	STA (&B0),Y
	INY 
	TXA 				;sector
	STA (&B0),Y
	INY 
	LDA #&01			;1
	STA (&B0),Y
	INY 
	INX 
	JSR SUB_A7C4_X_X_MOD_10
	DEC &B2
	BNE Label_A7A7_loop

	RTS
}

.SUB_A7C4_X_X_MOD_10
	TXA 

.SUB_A7C5_X_A_MOD_10
{
	SEC 

.Label_A7C6_loop
	SBC #&0A
	BCS Label_A7C6_loop

	ADC #&0A
	TAX 
	RTS
}

.CalcTracksOnDisk
{
	JSR SUB_93F5_rdCatalogue_81	;Load catalogue
	LDA swsp+&0F06			;Size of disk
	AND #&03
	TAX 
	LDA swsp+&0F07
	LDY #&0A			;10 sectors/track
	STY &B0
	LDY #&FF			;Calc number of tracks

.Label_A7E0_loop
	SEC 

.Label_A7E1_loop
	INY 
	SBC &B0
	BCS Label_A7E1_loop

	DEX 
	BPL Label_A7E0_loop
	ADC &B0
	PHA 
	TYA 
	TAX 
	PLA 
	BEQ Label_A7F2

	INX 

.Label_A7F2
	RTS
}

.CMD_FREE
	SEC 				;\\\\\\\\\ *FREE
	BCS Label_A7F7

.CMD_MAP
	CLC	 			;\\\\\\\\\ *MAP

.Label_A7F7
{
	ROR &C6
	JSR GetDriveNo_withInit
	JSR LoadCurDrvCat
	BIT &C6
	BMI Label_A818_free		;If *FREE

	JSR PrintStringP
	EQUS "Address :  Length", 13

.Label_A818_free
	LDA swsp+&0F06
	AND #&03
	STA &C5
	STA &C2
	LDA swsp+&0F07
	STA &C4				;wC4=sector count
	SEC 
	SBC #&02			;wC1=sector count - 2 (map length)
	STA &C1
	BCS Label_A82F

	DEC &C2

.Label_A82F
	LDA #&02
	STA &BB				;wBB = 0002 (map address)
	LDA #&00			;wBF = 0000
	STA &BC
	STA &BF
	STA &C0
	LDA FilesX8
	AND #&F8
	TAY 
	BEQ Label_A86B_nofiles		;If no files
	BNE Label_A856_fileloop_entry	;always

.Label_A845_fileloop
	JSR Sub_A8E2_nextblock
	JSR Yless8			;Y -> next file
	LDA &C4
	SEC 
	SBC &BB
	LDA &C5
	SBC &BC
	BCC Label_A86B_nofiles

.Label_A856_fileloop_entry
	LDA swsp+&0F07,Y		;wC1 = File Start Sec - Map addr
	SEC 
	SBC &BB
	STA &C1
	PHP 
	LDA swsp+&0F06,Y
	AND #&03
	PLP 
	SBC &BC
	STA &C2
	BCC Label_A845_fileloop

.Label_A86B_nofiles
	STY &BD
	BIT &C6
	BMI Label_A87A_free		;If *FREE

	LDA &C1				;MAP only
	ORA &C2
	BEQ Label_A87A_free		;If wC1=0

	JSR Map_Address_Length

.Label_A87A_free
	LDA &C1
	CLC 
	ADC &BF
	STA &BF
	LDA &C2
	ADC &C0
	STA &C0
	LDY &BD
	BNE Label_A845_fileloop

	BIT &C6
	BPL Label_A8BD_rst		;If *MAP

	TAY 
	LDX &BF
	LDA #&F8
	SEC 
	SBC FilesX8
	JSR Sub_A90D_freeinfo
	JSR PrintStringP
	EQUS "Free", 13
	LDA &C4
	SEC 
	SBC &BF
	TAX 
	LDA &C5
	SBC &C0
	TAY 
	LDA FilesX8
	JSR Sub_A90D_freeinfo
	JSR PrintStringP
	EQUS "Used", 13
	NOP 

.Label_A8BD_rst
	RTS
}

.Map_Address_Length
	LDA &BC				;Print address (3 dig hex)
	JSR PrintNibble			;(*MAP only)
	LDA &BB
	JSR PrintHex2
	JSR PrintStringP
	EQUS "     :  "
	LDA &C2				;Print length (3 dig hex)
	JSR PrintNibble
	LDA &C1
	JSR PrintHex2
	LDA #&0D
	JSR OSASCI

.Sub_A8E2_nextblock
{
	LDA swsp+&0F06,Y		;wBB = start sec + len
	PHA 
	JSR Alsr4and3
	STA &BC
	PLA 
	AND #&03
	CLC 
	ADC &BC
	STA &BC
	LDA swsp+&0F04,Y
	BEQ Label_A8FA

	LDA #&01

.Label_A8FA
	CLC 
	ADC FilesX8,Y
	BCC Label_A902

	INC &BC

.Label_A902
	CLC 
	ADC swsp+&0F07,Y
	STA &BB
	BCC Label_A90C

	INC &BC

.Label_A90C
	RTS
}

.Sub_A90D_freeinfo
{
	JSR Alsr3			;*FREE line
	JSR PrintBCD2			;A = Number of files
	JSR PrintStringP
	EQUS " Files "
	STX &BC				;YX = Number of sectors
	STY &BD
	TYA 
	JSR PrintNibble
	TXA 
	JSR PrintHex2
	JSR PrintStringP
	EQUS " Sectors " 
	LDA #&00
	STA &BB
	STA &BE				;!BB = number of sectors * 256
	LDX #&1F			;i.e. !BB = number of bytes
	STX &C1				;Convert to decimal string
	LDX #&09

.Label_A941_loop1
	STA swsp+&1000,X		;?1000 - ?1009 = 0
	DEX 
	BPL Label_A941_loop1

.Label_A947_loop2
	ASL &BB				;!BB = !BB * 2
	ROL &BC
	ROL &BD
	ROL &BE
	LDX #&00
	LDY #&09			;A=0

.Label_A953_loop3
	LDA swsp+&1000,X
	ROL A
	CMP #&0A
	BCC Label_A95D			;If <10

	SBC #&0A

.Label_A95D
	STA swsp+&1000,X
	INX 
	DEY 
	BPL Label_A953_loop3

	DEC &C1
	BPL Label_A947_loop2

	LDY #&20			;Print decimal string
	LDX #&05

.Label_A96C_loop4
	BNE Label_A970

	LDY #&2C

.Label_A970
	LDA swsp+&1000,X
	BNE Label_A97D

	CPY #&2C
	BEQ Label_A97D

	LDA #&20
	BNE Label_A982			;always

.Label_A97D
	LDY #&2C
	CLC 
	ADC #&30

.Label_A982
	JSR OSASCI
	CPX #&03
	BNE Label_A98D

	TYA 
	JSR OSASCI			;Print " " or ","

.Label_A98D
	DEX 
	BPL Label_A96C_loop4

	JSR PrintStringP
	EQUS " Bytes "
	NOP 
	RTS
}

.PrintStringP
{
	STA &B3				;Save A
	PLA 				;Pull calling address
	STA &AE
	PLA 
	STA &AF
	LDA &B3				;Save A & Y
	PHA 
	TYA 
	PHA 
	LDY #&00

.Label_A9AB_loop
	JSR inc_word_AE
	LDA (&AE),Y
	BMI Label_A9B8_exloop

	JSR OSASCI
	JMP Label_A9AB_loop

.Label_A9B8_exloop
	PLA 
	TAY 
	PLA 
	CLC 
	JMP (&00AE)
}

.PrintBCD2
	JSR BinaryToBCD

.PrintHex2
	PHA 
	JSR Alsr4
	JSR PrintNibble
	PLA 

.PrintNibble
	JSR prthexnibcalc
	JMP OSASCI

if sys<>224
.CMD_ROMS
{
	LDA #&00			; *ROMS (<rom>)
	STA &A8
	JSR Sub_AAEA_StackAZero		;Change value of A in stack to 0?
	LDA #&0F
	STA &AA
	JSR RomTablePtrBA
	SEC 
	JSR GSINIT
	STY &AB
	CLC 
	BEQ Label_A9FF_notnum		;If null str (no parameter)

.Label_A9E7_loop
	JSR Decimal_TxtPtrToBinary
	BCS Label_A9FF_notnum		;If not number

	STY &AB				;Save Y
	AND #&0F
	STA &AA				;Rom Nr
	JSR Label_AA53_RomInfo
	LDY &AB
	JSR GoGSINIT
	STY &AB				;Restore Y
	BNE Label_A9E7_loop		;Another rom id?

	RTS

.Label_A9FF_notnum
	ROR &A8				;Loop through roms

.Label_AA01_loop
	BIT &A8
	BPL Label_AA0A

	JSR ROMtitlecompare		;Match title with parameter
	BCC Label_AA0D_nomatch

.Label_AA0A
	JSR Label_AA53_RomInfo

.Label_AA0D_nomatch
	DEC &AA
	BPL Label_AA01_loop

	RTS

.ROMtitlecompare
	LDA #&09			;wF6=&8009 = title
	STA &F6
	LDA #&80
	STA &F7
	LDY &AB

.Label_AA1C_loop
	LDA (TextPointer),Y
	CMP #&0D			;If end of str
	BEQ Label_AA44

	CMP #&22
	BEQ Label_AA44			;If ="."

	INY 
	CMP #&2A
	BEQ Label_AA51_match		;If ="*"

	JSR UcaseA
	STA &AE
	JSR ReadRomByte
	BEQ Label_AA42_nomatch

	LDX &AE
	CPX #&23			;"#"
	BEQ Label_AA1C_loop

	JSR UcaseA
	CMP &AE
	BEQ Label_AA1C_loop

.Label_AA42_nomatch
	CLC 
	RTS

.Label_AA44
	JSR ReadRomByte
	BEQ Label_AA51_match

	CMP #&20
	BEQ Label_AA44			;If =" "   skip spaces

	CMP #&0D
	BNE Label_AA42_nomatch		;If <>CR

.Label_AA51_match
	SEC 
	RTS

.Label_AA53_RomInfo
	LDY &AA				;Y=Rom nr
	LDA (&B4),Y
	BEQ Label_AA42_nomatch		;If RomTable(Y)=0

	PHA 
	JSR PrtString
	EQUS "Rom "
	TYA 
	JSR PrtBcdA			;Print ROM nr
	JSR PrtString
	EQUS " : "
	LDA #&28			;A="("
	JSR PrtChrA
	PLA 
	PHA 
	BMI Label_AA78			;Bit 7 set = Service Entry

	LDY #&20			;Y=" "
	BNE Label_AA7A			;always

.Label_AA78
	LDY #&53			;Y="S"

.Label_AA7A
	TYA 
	JSR PrtChrA
	PLA 
	LDY #&20			;Y=" "
	ASL A
	BPL Label_AA86			;Bit 6 set = Language Entry

	LDY #&4C			;Y="L"

.Label_AA86
	TYA 
	JSR PrtChrA
	LDA #&29			;A=")"
	JSR PrtChrA
	JSR Prtspace
	JSR PrtRomTitle
	JSR prtNewLine
	SEC 
	RTS

.PrtRomTitle
	LDA #&07			;Print ROM title
	STA &F6
	LDA #&80
	STA &F7				;wF6=&8007
	JSR ReadRomByte
	STA &AE				;Copyright offset
	INC &F6				;wF6=&8009
	LDY #&1E
	JSR PrintRomStr
	BCS Label_AAB7_rts		;If reached copyright offset

	JSR Prtspace
	DEY 
	JSR PrintRomStr

.Label_AAB7_rts
	RTS

.Label_AAB8_loop
	CMP #&20
	BCS Label_AABE			;If >=" "

	LDA #&20

.Label_AABE
	JSR PrtChrA
	DEY

.PrintRomStr
	LDA &F6
	CMP &AE
	BCS Label_AACE_rts		;If >=

	JSR ReadRomByte
	BNE Label_AAB8_loop
	CLC		 		;C=0=Terminator

.Label_AACE_rts
	RTS

.ReadRomByte
	TYA 				;Read byte from ROM
	PHA 
	LDY &AA
	JSR OSRDRM			;Address in wF6
	INC &F6
	TAX 
	PLA 
	TAY 
	TXA 
	RTS

.RomTablePtrBA
	JSR rememberXYonly
	LDA #&AA			;ROM information table @ XY
	JSR osbyteX00YFF
	STX &B4
	STY &B5
	RTS

.Sub_AAEA_StackAZero
	TSX 				;Change value of A to 0?
	LDA #&00
	STA swsp+&0107,X
	RTS
}
endif

\\ End of file

