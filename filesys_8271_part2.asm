	\\ Acorn DFS
	\\ filesys_8271_part2.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

.FDC_Reset
	LDA #&01			; Reset FDC
	STA FDC_WRRESET
	TAX
	DEX				; X=0
	STX FDC_WRRESET
	JSR FDC_Initialise		; Reinitialise
	JMP NMI_TUBE_RELEASE

.fdc_sidedriveselect_table
	EQUB  &48, &88, &68, &A8

.SetLoadAddrToHost
	PHA				; Set load address high bytes
	LDA #&FF			; to FFFF
	STA &1074
	STA &1075
	PLA
.Label_ABA2_RTS
	RTS

.LoadNMI1Read_TubeInit
	LDA #&01			; init write to Tube
	JSR TubeRoutineA

.LoadNMI1Read
	LDA #&01
	JSR NMI_CLAIMA
	LDA #&53			; FDC cmd=READ DATA
	BNE InitNMIvarsA		; always

.LoadNMI0Write_TubeInit
	LDA #&00			; init read from Tube
	JSR TubeRoutineA

.LoadNMI0Write
	LDA #&00
	JSR NMI_CLAIMA
	LDA #&4B			; FDC cmd=WRITE DATA

.InitNMIvarsA
	STA NMI_FDCcmd
	LDA #&00
	STA NMI_DataPointer		; Default is &0E00
	LDA #&0E
	STA NMI_DataPointer+1

.SetRW_Attempts
	LDA #&0A
	STA NMI_RW_attempts
	RTS

.TubeRoutine
	LDA TubeOpCode			; get op
	PHA				; see AUG pg.345
	LDA NotTUBEOpIf0
	JMP tuberoutineinited

.TubeRoutineA
	PHA
	STA TubeOpCode
	LDA &BC				; Load address
	STA &1072
	LDA &BD
	STA &1073
	LDA &1074
	AND &1075			; =FF if load to Host
	ORA TubePresentIf0
	EOR #&FF			; -> =0 if load to host
	STA NotTUBEOpIf0
.tuberoutineinited
{
	SEC
	BEQ notTUBEtransfer
	JSR TUBE_CLAIM
	LDX #&72			; control block @ 1072
	LDY #&10
	PLA
	PHA				; A=0 | 1
	JSR TubeCode
	CLC
.notTUBEtransfer
	PLA
	RTS
}

.TUBE_CLAIM
{
	PHA
.tclaim_loop
	LDA #&C1			; magic number!
	JSR TubeCode
	BCC tclaim_loop			; try again
	PLA
	RTS
}

.FDCIntRequest
{
	JSR FDC_Wait			; Called by NMI routine
	BNE fdcintrequest_error		; If Result<>0 try again?
	JSR SetRW_Attempts
	INC Track			; trk=trk+1
	LDA #&00
	STA Sector			; sec=0
	LDA &107C			; secs read last trk
	CLC				; wd 1073+=byte 107C
	ADC &1073
	STA &1073
	BCC FDC_SetupRW
	INC &1074
	BNE FDC_SetupRW
	INC &1075
	BCS FDC_SetupRW
.fdcintrequest_error
	DEC NMI_RW_attempts		; dec RW_attempts
	BPL retryFDCoperation
	JMP FDC_ERROR
}

.retryFDCoperation
{
	LDY #&04
.restoreNMIvars_loop
	LDA &1040,Y
	STA NMI_Counter1,Y
	DEY
	BPL restoreNMIvars_loop
	TXA
	PHA
	JSR TubeRoutine
	PLA
	TAX
}

.FDC_SetupRW
{
	LDA NMI_Counter3		; Anything to do?
	BEQ fdrw_exit
	LDY #&04			; Save NMI vars
.fdrw_loop
	LDA NMI_Counter1,Y
	STA &1040,Y
	DEY
	BPL fdrw_loop
	LDA NMI_FDCcmd
	JSR FDC_WriteCmdA		; Command
	LDA Track
	JSR FDC_WriteParamA		; Track
	LDA Sector
	JSR FDC_WriteParamA		; Sector
	LDA #&0A			; 10 sectors/track
	SEC
	SBC Sector
	STA &107C			; 10 - first sector
	LDA NMI_Counter3
	CMP #&01
	BNE fdrw_lab1
	LDA NMI_Counter2
	BEQ fdrw_lab1
	CMP &107C
	BCC fdrw_lab2
.fdrw_lab1
	LDA &107C
.fdrw_lab2
	ORA #&20			; A=secsize=256/noofsecs
}

.FDC_WriteParamA
{
	PHA
.fdc_writeparam_wait
	LDA FDC_WRCMD_RDSTATUS
	AND #&20
	BNE fdc_writeparam_wait
	PLA
	STA FDC_WRPARA_RDRESULT
}
.fdrw_exit
	RTS

.FDC_WriteCmdA
	PHA				; A=DRIVE0 + CMD
	LDA CurrentDrv
	ROR A				; C = drive 0 or 1
	PLA
	BCC FDC_WriteCmdA2		; C=drive (S=01XXXXXX)
	EOR #&C0			; S=10XXXXXX

.FDC_WriteCmdA2
	BIT FDC_WRCMD_RDSTATUS
	BMI FDC_WriteCmdA2		; while FDC command busy
	STA FDC_WRCMD_RDSTATUS
	RTS

.FDC_cmdfromtableY1
	JSR FDC_cmdfromtableY2

.FDC_Wait
	JSR FDC_WaitIfBusy
	LDA FDC_WRPARA_RDRESULT
	RTS

.FDC_WaitIfBusy
	BIT FDC_WRCMD_RDSTATUS
	BMI FDC_WaitIfBusy
	BIT FDC_WRCMD_RDSTATUS
	BMI FDC_WaitIfBusy
	RTS

.FDC_Initialise
{
	JSR osbyteFF_startupopts
	TXA
	AND #&30			; Disk drive timings
	LSR A				; Calc value for Y such that:
	LSR A				; 00 = &00
	STA &B0				; 01 = &06
	LSR A				; 10 = &0C
	ADC &B0				; 11 = &12
	TAY
	JSR FDC_cmdfromtableY2
	LDY #&18			; Load surfaces 0 & 1 bad tracks
	LDX #&03			; & Write mode spec reg
.fdcinit_loop
	JSR FDC_cmdfromtableY2
	DEX
	BNE fdcinit_loop
	STX &1085			; X=0
	RTS
}

.fdccmdtable0
	EQUB  &35, &0D, &02, &08, &C0, &EA	; Initialise drive timings 00
.fdccmdtable6
	EQUB  &35, &0D, &03, &08, &C0, &EA	; Initialise drive timings 01
.fdccmdtableC
	EQUB  &35, &0D, &03, &08, &C7, &EA	; Initialise drive timings 10
.fdccmdtable12
	EQUB  &35, &0D, &0C, &0A, &C8, &EA	; Initialise drive timings 11
.fdccmdtable18
	EQUB  &35, &10, &FF, &FF, &00, &EA	; Load surface 0 bad tracks
.fdccmdtable1E
	EQUB  &35, &18, &FF, &FF, &00, &EA	; Load surface 1 bad tracks
.fdccmdtable24
	EQUB  &3A, &17, &C1, &EA		; Write mode reg. (Select Non-DMA mode)
.fdccmdtable28
	EQUB  &69, &00, &EA			; seek track 0
.fdccmdtable2B
	EQUB  &5F, &00, &08, &22, &EA		; verify data & deleted data
.fdccmdtable30
	EQUB  &3D, &06, &EA			; Read Scan Sector Number
.fdccmdtable33
	EQUB  &7D, &23, &EA			; Read Drv Ctrl Output Port

	\\ NMI ROUTINES
.NMI0_snip
	LDY NotTUBEOpIf0		; \\ 19 bytes WRITE
	BEQ nmi0_fromhost		; \\
	LDA TUBE_R3_DATA		; \\
	STA FDC_DATA			; \\
	JMP &0D23			; \\
.nmi0_fromhost
	LDA (NMI_DataPointer),Y		; \\
	STA FDC_DATA
.NMI01_READWRITE
	PHA				; Save A&Y
	TYA
	PHA
	LDA FDC_WRCMD_RDSTATUS
	AND #&04			; 4=non DMA data request
	BEQ nmi01_notNonDMArequest
	LDA FDC_DATA			; \\ 19 bytes READ
	LDY NMI_Counter3		; \\
	BEQ nmi1_lab2			; \\
	LDY NotTUBEOpIf0		; \\
	BEQ nmi1_tohost			; \\IF to host
	STA TUBE_R3_DATA		; \\To TUBE
	BNE nmi1_lab1			; \\always????????
.nmi1_tohost
	STA (NMI_DataPointer),Y		; \\To HOST
	INC NMI_DataPointer		; Inc data ptr
	BNE nmi1_lab1
	INC NMI_DataPointer+1
.nmi1_lab1
	DEC NMI_Counter1		; Dec data counter
	BNE nmi1_lab2
	DEC NMI_Counter2
	BNE nmi1_lab2
	DEC NMI_Counter3
.nmi1_lab2
	PLA				; Restore A&Y
	TAY
	PLA
.NMI2_DONOTHING
	RTI
.nmi01_notNonDMArequest
	LDA FDC_WRCMD_RDSTATUS
	AND #&08			; 8 = Interrupt request
	BEQ nmi1_lab2
	LDA PagedRomSelector_RAMCopy	; Select DFS rom
	PHA
	LDA #&00			; DFS rom no.
	STA PagedRomSelector_RAMCopy
	STA PagedRomSelector
	JSR FDCIntRequest
	PLA
	STA PagedRomSelector_RAMCopy
	STA PagedRomSelector
	SEC
	BCS nmi1_lab2			; Always
.NMI6_READ_toMem
	PHA
	TYA
	PHA
	LDA FDC_WRCMD_RDSTATUS
	AND #&04			; 4=non DMA data request
	BEQ nmi6_lab
	LDA FDC_DATA
	LDY #&00
	STA (NMI_DataPointer),Y
	INC NMI_DataPointer
	BNE nmi6_lab
	INC NMI_DataPointer+1
.nmi6_lab
	PLA
	TAY
	PLA
	RTI
.NMI4_WRITE_fromMem
	PHA
	TYA
	PHA
	LDA FDC_WRCMD_RDSTATUS
	AND #&04			; 4=non DMA data request
	BEQ nmi4_lab
	LDY #&00
	LDA (NMI_DataPointer),Y
	STA FDC_DATA
	INC NMI_DataPointer
	BNE nmi4_lab
	INC NMI_DataPointer+1
.nmi4_lab
	PLA
	TAY
	PLA
	RTI
.NMI5_READ_toTube
	PHA
	LDA FDC_WRCMD_RDSTATUS
	AND #&04			; 4=non DMA data request
	BEQ nmi5_lab
	LDA FDC_DATA
	STA TUBE_R3_DATA
.nmi5_lab
	PLA
	RTI
.NMI3_WRITE_fromTube
	PHA
	LDA FDC_WRCMD_RDSTATUS
	AND #&04			; 4=non DMA data request
	BEQ nmi3_lab
	LDA TUBE_R3_DATA
	STA FDC_DATA
.nmi3_lab
	PLA
	RTI

; **** END OF NMI ROUTINES ****
