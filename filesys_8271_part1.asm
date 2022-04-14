	\\ Acorn DFS
	\\ filesys_8271_part1.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

.FDC_DriveReady
{
	LDA #&6C			; READ DRIVE STATUS
	JSR FDC_WriteCmdA
	JSR FDC_Wait
	BCC fdc_readdrvstatus_drv0	; if drive 0, C=0
	JSR Alsr4
	SEC
.fdc_readdrvstatus_drv0
	AND #&04			; Bit2=Drive Ready
}
.fdc_cmdfromtbl_exitloop
	RTS

.FDC_cmdfromtableY2
{
	LDA fdccmdtable0,Y
	JSR FDC_WriteCmdA
	INY
.fdc_cmdfromtabl_loop
	LDA fdccmdtable0,Y
	INY
	CMP #&EA
	BEQ fdc_cmdfromtbl_exitloop
	JSR FDC_WriteParamA
	JMP fdc_cmdfromtabl_loop
}

.ResetFDCNMI_SetToCurrentDrv
	JSR FDC_SetToCurrentDrv

.ResetFDCNMI
	LDA #&02			; Reset vars/NMI routine and seek trk 0
	JSR NMI_CLAIMA			; D00=RTI!
	LDY #&00
	STY Track
	STY Sector
	STY NMI_Counter1
	INY
	STY NMI_Counter3
	INY
	STY NMI_Counter2
	LDY #&28
	JSR FDC_cmdfromtableY1		; Seek trk 0
	BEQ fdc_cmdfromtbl_exitloop	; Seek OK exit

.FDC_ERROR
	JSR NMI_TUBE_RELEASE
	CMP #&12			; 10 01 = Write protect
	BNE disknotwriteprotected
	JSR errDISK
	EQUS &C9, "read only", 0

.disknotwriteprotected
	PHA
	CMP #&0A			; Late DMA?
	BEQ errDRIVEFAULT
	AND #&0F
	CMP #&08
	BCC errDRIVEFAULT		; If<8 (Comp.Type bit0 not set)
.errDISKFAULT
	JSR errDISK
	BRK
	BCC errFAULTDESC		; always

.errDRIVEFAULT
	JSR FDC_Reset
	JSR Brk100_Err_ChkIfBuf
	EQUS 0, "Drive "
	NOP

.errFAULTDESC
	JSR Brk100_ErrCONTINUE
	EQUS &C7, "fault "
	NOP
	PLA
	JSR FDC_ReportDiskFault_A_fault
	JSR Brk100_ErrCONTINUE		; BREAK
	EQUB  &C7, &00

.WaitForBusyDrive
{
	LDA #&80			; Drive is buy, so wait?
	BIT &1085			; Note: FDC_Initialise resets &1085
	BCS Wait_Drive1
	BMI Idle			; Idle IF bit 7 of &1085 set
.Wait_Exit
	ORA &1085
	STA &1085
	BNE ResetFDCNMI
.Wait_Drive1
	BVS Idle			; Idle IF bit 6 of &1085 set
	LSR A				; A=&60
	BCC Wait_Exit			; always
}

.Idle
{
	JSR rememberAXY
	LDY #&46			; X value doesn't matter
.Idel_LOOP
	DEX
	BNE Idel_LOOP
	DEY
	BNE Idel_LOOP
	RTS
}

.FDC_SetToCurrentDrv
{
	JSR rememberAXY
	LDA CurrentDrv
	TAY
	CMP IsDriveReady
	BNE fdc_driveready2		; If not already done?
	JSR FDC_DriveReady
	BNE Label_ABA2_RTS
.fdc_driveready2
	STY IsDriveReady		; Save Y (drive no.)
	LDA #&3A			; Write special register
	JSR FDC_WriteCmdA2
	LDA #&23			; Drive Control Output Port
	JSR FDC_WriteParamA
	LDA fdc_sidedriveselect_table,Y
	JSR FDC_WriteParamA		; Set drive & side
	TYA				; A=Y=drive   ???
	ROR A				; C=drive     ???
	LDY #&33
	JSR FDC_cmdfromtableY1		; A=DRIVE CONTROL OUTPUT PORT?
	LDY IsDriveReady		; Restore Y (drive no.)
	AND #&08			; "Load head"
	BEQ fdc_driveready2		; If head not loaded
	LDA #&FF
	STA LoadedCatDrive
.fdc_waituntilbusy
	JSR FDC_DriveReady		; Current drive ready, exit C=drive
	BNE WaitForBusyDrive		; Branch if drive not ready
	PHP
	CLI				; Enable interrupts!
	PLP
	BIT &FF				; Check if ESCAPE pressed
	BPL fdc_waituntilbusy		; If NO ESCAPE
	JSR FDC_Reset
}
