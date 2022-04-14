	\\ Acorn NFS 3.60
	\\ netsys.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

	\**** START OF ADLC CODE ****\

	Sub_0D0E_Next_YA = &0D0E	; ADLC NMI labels
	Sub_0D11_Next_A = &0D11
	Sub_0D14_Next = &0D14

	TUBEOP2 = 2 ; Multi pairs transfer Parasite->Host
	TUBEOP3 = 3 ; Multi pairs of bytes Host->Parasite
	TubeOp_D5C = &0D5C

	RXTX_Flags_D4A = &0D4A

	ProtectionMask_D63 = &0D63
	RXTX_Flags_D64 = &0D64
	ProtectionMaskCopy_D65 = &0D65

	TubePresent_D67 = &0D67

.Sub_9630_Transmit_Start		; Start Transmit
	JMP Sub_9B6E_Transmit_Start

.Sub_9633_Listen_Start			; Start Listening
	JMP Sub_967A_Listen_Start

.NFS_SERVICE_0C
	JMP NFS_NMI_CLAIM

.NFS_SERVICE_0B
	JMP NFS_NMI_REALEASED

.NFS_SERVICE_05				; Unrecognised Interrupt
{
	LDA #&04
	BIT SYSVIA_IFR			; System VIA IFR - b2 = Shift Reg 8 shifts
	BNE Label_9646
	LDA #&05			; Restore A
	RTS

.Label_9646
	TXA				; Preserve X, & Y
	PHA
	TYA
	PHA

	LDA SYSVIA_ACR			; ACR
	AND #&E3			; 1110 0011 = Clear Shift Reg Control
	ORA &0D51			; Restore previous value
	STA SYSVIA_ACR

	LDA SYSVIA_SR			; Shift Reg

	LDA #&04
	STA SYSVIA_IFR			; IFR - Clear flag
	STA SYSVIA_IER			; IER - Disable Shift Reg interrupt

	LDY &0D57			; Y = Op
	CPY #&86
	BCS Label_9672			; Y >= &86 i.e. HALT, CONT or MACHINETYPE

	LDA ProtectionMask_D63
	STA ProtectionMaskCopy_D65	; Save current mask
	ORA #&1C			; 0001 1100 = disable further &83 JSR, &84 USER, &85 OS
	STA ProtectionMask_D63

.Label_9672
	LDA #HI(Data_9B20)		; Assume all routines in Page &9B!
	PHA
	LDA Data_9B20-&83,Y
	PHA
	RTS				; 'Return' to routine
}

	\ Set up NMI and start listening.
.Sub_967A_Listen_Start
	BIT _INTOFF_STATIONID		; ADCL interrupt off
	JSR Sub_9F3D_Reset
	LDA #&EA
	LDX #&00
	STX &0D66
	LDY #&FF
	JSR OSBYTE
	STX TubePresent_D67		; If Tube present then X=&FF
	LDA #&8F			; Issue ROM Service request &0C - Claim NMI
	LDX #&0C
	LDY #&FF
	JSR OSBYTE

.NFS_NMI_REALEASED			; NMI has been released by another ROM
{					; Copy NMI code to &D00
	LDY #&20
.Label_969A
	LDA Label_9F7D-1,Y
	STA &0CFF,Y
	DEY
	BNE Label_969A

	LDA PagedRomSelector_RAMCopy
	STA &0D07

	LDA #&80
	STA &0D62
	STA &0D66

	LDA _INTOFF_STATIONID
	STA &0D22			; Source Station
	STY &0D23			; Source Network (= 0)
	STY &98
	BIT _INTON_			; ADCL interrupt on
	RTS
}

	\ Listen for scout
.Sub_96BF_ListenForScout
{					; Default NMI routine - Waits for start of frame 
					; and checks destination.
	LDA #&01
	BIT ADLC_REG1			; SR2
	BEQ Sub_96FE_Reset_ListenForScout	; If not AP (Address Present)

	LDA ADLC_REG2			; Read Rx FIFO to get address
	CMP _INTOFF_STATIONID		; Compare with Station ID
	BEQ Label_96D7			; If this station

	CMP #&FF
	BNE Label_96EA			; If not a broadcast (and not this station)

	LDA #&40
	STA RXTX_Flags_D4A		; Set broadcast flag

.Label_96D7
	LDA #LO(Sub_96DC)		; Assume same page
	JMP Sub_0D11_Next_A

.Sub_96DC				; Check network
	BIT ADLC_REG1			; SR2
	BPL Sub_96FE_Reset_ListenForScout	; If RDA = 0 = No data

	LDA ADLC_REG2			; Read data
	BEQ Label_96F2			; If &00 this network
	EOR #&FF
	BEQ Label_96F5			; If &FF then broadcast?

.Label_96EA				; Ignore frame.
	LDA #&A2			; Tx Reset + Rx Frame Discont. + Rx Interrupt enable
	STA ADLC_REG0			; CR1
	JMP Sub_99EB_ListenForScout

.Label_96F2
	STA RXTX_Flags_D4A		; Always 0 = Not broadcast

.Label_96F5
	STA &A2				; Always 0
	LDA #LO(Sub_970E_RX_Frame)
	LDY #HI(Sub_970E_RX_Frame)
	JMP Sub_0D0E_Next_YA
}

.Sub_96FE_Reset_ListenForScout
{
	LDA ADLC_REG1			; SR2
	AND #&81
	BEQ Label_970B			; If RDA + AP = 0 then no data

	JSR Sub_9F3D_Reset
	JMP Sub_99EB_ListenForScout

.Label_970B
	JMP Sub_99E8_ListenForScout
}

	\* Frame rcvd with station id & net, or broadcast
	\* Rcv control byte, port and up to 8 bytes of data
.Sub_970E_RX_Frame
{
	LDY &A2

	LDA ADLC_REG1			; SR2
.Loop_9713
	BPL Sub_96FE_Reset_ListenForScout	; If RDA = 0 = No data - Ignore frame

	LDA ADLC_REG2			; Read Rx FIFO
	STA &0D3D,Y			; e.g. Source station ID/Control byte/Up to 8 bytes of data.
	INY

	LDA ADLC_REG1			; SR2
	BMI Label_9723			; If RDA = 1 : then more data
	BNE Label_9738			; If end of frame (FV = 1) or error

.Label_9723
	LDA ADLC_REG2			; e.g. Source station network
	STA &0D3D,Y
	INY
	CPY #&0C
	BEQ Label_9738			; Rest of header & 8 bytes of data (6+8-2=12)
	STY &A2

	LDA ADLC_REG1			; SR2
	BNE Loop_9713			; Get more data?!
	JMP Sub_0D14_Next		; RTI - Ignore frame

	\ Read last byte
.Label_9738
	LDA #&00
	STA ADLC_REG0			; CR1 : AC = 0
	LDA #&84
	STA ADLC_REG1			; CR2 : RTS = 1, F/M Idle = 1
	LDA #&02
	BIT ADLC_REG1			; SR2
	BEQ Sub_96FE_Reset_ListenForScout	; If FV = 0 : frame not valid - ignore
	BPL Sub_96FE_Reset_ListenForScout	; If RDA = 0 : no data - ignore

	LDA ADLC_REG2			; Get last byte in header/frame
	STA &0D3D,Y			; e.g. Port

	LDA #&44
	STA ADLC_REG0			; CR1 : TxRS = 0, RxRS = 1, TIE = 1, RIE = 0

	SEC
	ROR &98

	LDA &0D40
	BNE Label_9761			; If Port <> 0

	JMP Sub_9A46_ImmediateOp	; Immediate ops

.Label_9761
	BIT RXTX_Flags_D4A
	BVC Label_976B			; If b6 = 0 : not broadcast

	LDA #&07
	STA ADLC_REG1			; CR2 : F/M Idle = 1, 2/1 Byte = 1, PSE = 1

.Label_976B
	BIT RXTX_Flags_D64
	BPL Label_97AE			; If b7 = 0 : ???

	LDA #&C0			; Y:A = &00C0 (&C0-&CF for current fs)
	LDY #&00
}


	\ Y:A -> 1st Control block
.Sub_9774_RX_FindControlBlock
{					; Find control block
	STA ptrA6L			; ptrA6 = Y:A
	STY ptrA6H

.Loop_9778
	LDY #&00
	LDA (ptrA6L),Y			; 0 Control byte?
	BEQ Label_97AB

	CMP #&7F
	BNE Label_979E			; If Not expecting data, try next block

	INY
	LDA (ptrA6L),Y			; 1
	BEQ Label_978C			; If Port = 0 (any port)

	CMP &0D40			; If Port = Rx_Port
	BNE Label_979E			; Not this port, try next block

.Label_978C
	INY
	LDA (ptrA6L),Y			; 2
	BEQ Sub_97B9_RX_FoundControlBlock			; If Station = 0 (any station)

	CMP &0D3D			; If Station = Rx_SourceStation
	BNE Label_979E			; Not source station, try next

	INY
	LDA (ptrA6L),Y			; 3
	CMP &0D3E			; If Net = RX_SourceNet
	BEQ Sub_97B9_RX_FoundControlBlock			; We have a match!

.Label_979E				; Try next block
	LDA ptrA6H
	BEQ Label_97AE			; If page zero

	LDA ptrA6L			; ptrA6 += &0C
	CLC
	ADC #&0C
	STA ptrA6L
	BCC Loop_9778
}

.Label_97AB
	JMP Sub_9835

.Label_97AE
	BIT RXTX_Flags_D64
	BVC Label_97AB			; If b6 = 0 : ???

	LDA #&00			; Y:A = Page ?&9F
	LDY ptr9EH_PWS1
	BNE Sub_9774_RX_FindControlBlock	; Try again!

	\**** Control block found matching header!
	\ ptrA6 -> buffer control block
.Sub_97B9_RX_FoundControlBlock
	LDA #TUBEOP3
	STA TubeOp_D5C			; Tube Operation 3=Multi pairs of bytes Host->Parasite
	JSR Sub_9ECA_SetupCounters	; Set up counters/Tube
	BCC Sub_9835			; If failed (i.e. Tube failed)

	BIT RXTX_Flags_D4A
	BVC Sub_97CB			; If b6 clear = not broadcast

	JMP Sub_99F2_CopyDataToBuffer	; Copy frame received to buffer

	\ Send ACK then expect reply
.Sub_97CB
{					; Get ready to transmit
	LDA #&44
	STA ADLC_REG0			; CR1 : RxRS = 1, TIE = 1
	LDA #&A7
	STA ADLC_REG1			; CR2 : RTS = 1, Clear RxST = 1, F/M Idle = 1, 2/1 byte = 1, PSE = 1
	LDA #LO(Sub_97DC)
	LDY #HI(Sub_97DC)
	JMP Sub_9907_Send_Data_Or_Ack

.Sub_97DC
	LDA #&82			; Get ready to receive
	STA ADLC_REG0			; CR1 : TxRS = 1, RIE = 1
	LDA #LO(Sub_97E6)
	JMP Sub_0D11_Next_A

.Sub_97E6
	LDA #&01
	BIT ADLC_REG1
	BEQ Sub_9835			; SR2: If No data etc. then error

	LDA ADLC_REG2			; This station?
	CMP _INTOFF_STATIONID
	BNE Sub_9835			; If no then error

	LDA #LO(Sub_97FA)
	JMP Sub_0D11_Next_A

.Sub_97FA
	BIT ADLC_REG1
	BPL Sub_9835			; SR2: If no data then error

	LDA ADLC_REG2
	BNE Sub_9835			; If not Network 0 then error

	LDA #LO(Sub_9810)
	LDY #HI(Sub_9810)
	BIT ADLC_REG0
	BMI Sub_9810			; If IRQ already else wait for interrupt
	JMP Sub_0D0E_Next_YA

.Sub_9810
	BIT ADLC_REG1
	BPL Sub_9835			; If no data

	LDA ADLC_REG2			; Ignore Source Station ID + network
	LDA ADLC_REG2
}

.Sub_981B
{
	LDA #&02
	BIT RXTX_Flags_D4A
	BNE Label_982E			; If Tube operation

	LDA #LO(Sub_9843)
	LDY #HI(Sub_9843)
	BIT ADLC_REG0
	BMI Sub_9843			; If IRQ already
	JMP Sub_0D0E_Next_YA

.Label_982E
	LDA #LO(Sub_98A0)
	LDY #HI(Sub_98A0)
	JMP Sub_0D0E_Next_YA
}

.Sub_9835
{
	LDA RXTX_Flags_D4A
	BPL Label_983D			; If b7 = 0 : ???
	JMP Sub_9EAC_tx_error41

.Label_983D
	JSR Sub_9F3D_Reset
	JMP Sub_99DB_ListenForScout
}

	; HOST receive data
.Sub_9843
{
	LDY &A2
	LDA ADLC_REG1
.Loop_9848
	BPL Label_9877			; SR2 : If RDA=0 no data

	LDA ADLC_REG2
	STA (ptrA4L),Y
	INY
	BNE Label_9858
	INC ptrA4H
	DEC &A3
	BEQ Sub_9835

.Label_9858
	LDA ADLC_REG1
	BMI Label_985F			; SR2 If RDA = 1 (more data)
	BNE Label_9877			; If FV or errors

.Label_985F
	LDA ADLC_REG2			; Get data and store in buffer
	STA (ptrA4L),Y
	INY
	STY &A2
	BNE Label_986F
	INC ptrA4H
	DEC &A3
	BEQ Label_9877			; If buffer full

.Label_986F
	LDA ADLC_REG1
	BNE Loop_9848			; SR2 : If more data or error
	JMP Sub_0D14_Next			; RTI

.Label_9877
	LDA #&84
	STA ADLC_REG1			; CR2 : Ready to send + Mark Idle

	LDA #&00
	STA ADLC_REG0			; CR1
	STY &A2

	LDA #&02
	BIT ADLC_REG1			; SR2
	BEQ Sub_9835			; If FV = 0 (invalid frame)
	BPL Label_989D			; If RDA = 0 (no data)
	LDA &A3
}

.Sub_988E
	BEQ Sub_9835
	LDA ADLC_REG2
	LDY &A2
	STA (ptrA4L),Y
	INC &A2
	BNE Label_989D
	INC ptrA4H

.Label_989D
	JMP Sub_98EE

	\ TUBE
.Sub_98A0
{
	LDA ADLC_REG1
.Loop_98A3
	BPL Label_98C3			; SR2 : If RDA=0 no data

	LDA ADLC_REG2
	JSR Sub_9A37_IncCounter		; !&A2 += 1
	BEQ Sub_988E
	STA TUBE_R3_DATA

	LDA ADLC_REG2
	STA TUBE_R3_DATA
	JSR Sub_9A37_IncCounter		; !&A2 += 1
	BEQ Label_98C3

	LDA ADLC_REG1
	BNE Loop_98A3			; SR2 : If RDA=1 etc.
	JMP Sub_0D14_Next


.Label_98C3
	LDA #&00
	STA ADLC_REG0			; CR1

	LDA #&84
	STA ADLC_REG1			; CR2 : Ready to send + Mark Idle

	LDA #&02
	BIT ADLC_REG1			; SR2
	BEQ Sub_988E			; If FV = 0 (invalid frame)
	BPL Sub_98EE			; If RDA = 0

	LDA &A2
	ORA &A3
	ORA ptrA4L
	ORA ptrA4H
	BEQ Sub_988E			; If !&A2 = 0

	LDA ADLC_REG2
	STA &0D5D

	LDA #&20			; b5
	ORA RXTX_Flags_D4A
	STA RXTX_Flags_D4A
}

.Sub_98EE
{
	LDA RXTX_Flags_D4A
	BPL Label_98F9			; If not expecting reply?

	JSR Sub_994E
	JMP Sub_9EA8_tx_success

.Label_98F9
	LDA #&44
	STA ADLC_REG0			; CR1 Rx reset + enable Tx interrupt
	LDA #&A7
	STA ADLC_REG1			; CR2 Ready to send + etc.

	LDA #LO(Sub_9995)
	LDY #HI(Sub_9995)
}

	\**** Send data / acknowledgement
	\ If b7 of RXTX_Flags set, data is included.
	\ Y:A give next NMI routine (after Sub_9925).
.Sub_9907_Send_Data_Or_Ack
{
	STA &0D4B			; After transmitted - Next NMI at YA
	STY &0D4C

	LDA &0D3D			; Destination ID = RX Source station ID
	BIT ADLC_REG0			; SR1
	BVC Label_994B			; If TDRA = 0 : Error
	STA ADLC_REG2

	LDA &0D3E			; Destination Net = RX Source station Net
	STA ADLC_REG2

	LDA #LO(Sub_9925)		; Next transmit Source
	LDY #HI(Sub_9925)
	JMP Sub_0D0E_Next_YA

.Sub_9925
	LDA _INTOFF_STATIONID		; Station ID
	BIT ADLC_REG0
	BVC Label_994B			; If TDRA = 0 : Error
	STA ADLC_REG2			; Source : My Station ID

	LDA #&00
	STA ADLC_REG2			; Source ; My Station Net

	LDA RXTX_Flags_D4A
	BMI Label_9948			; If b7 = 1 : Transmit data

	LDA #&3F
	STA ADLC_REG1			; CR2 : Clr Tx status = 1, Tx last = 1, Frame complete = 1,
					;       F/M Idle = 1, 1 + 2 byte = 1, PSE = 1

	LDA &0D4B			; Y:A -> next routine
	LDY &0D4C
	JMP Sub_0D0E_Next_YA
}

.Label_9948
	JMP Sub_9DB3

.Label_994B
	JMP Sub_9835

.Sub_994E
{
	LDA #&02
	BIT RXTX_Flags_D4A
	BEQ Label_9994			; Not tube

	CLC
	PHP
	LDY #&08

.Label_9959
	LDA (ptrA6L),Y
	PLP
	ADC &009A,Y
	STA (ptrA6L),Y
	INY
	PHP
	CPY #&0C
	BCC Label_9959
	PLP
	LDA #&20
	BIT RXTX_Flags_D4A
	BEQ Label_9992
	TXA
	PHA
	LDA #&08
	CLC
	ADC ptrA6L
	TAX
	LDY ptrA6H
	LDA #&01
	JSR TubeCode
	LDA &0D5D
	STA TUBE_R3_DATA
	SEC
	LDY #&08

.Label_9987
	LDA #&00
	ADC (ptrA6L),Y
	STA (ptrA6L),Y
	INY
	BCS Label_9987
	PLA
	TAX
.Label_9992
	LDA #&FF
.Label_9994
	RTS
}

.Sub_9995
	LDA &0D40
	BNE Sub_99A4			; If Port <> 0 : Not immediate operation

	LDY &0D3F			; Y = Ctrl byte
	CPY #&82
	BEQ Sub_99A4			; If Ctrl byte = &82 (POKE)
	JMP Sub_9AE7

.Sub_99A4
{
	JSR Sub_994E
	BNE Label_99BB			; If TUBE

	LDA &A2				; ptrA4 += ?&A2
	CLC
	ADC ptrA4L
	BCC Label_99B2
	INC ptrA4H

.Label_99B2
	LDY #&08
	STA (ptrA6L),Y			; Update Buffer End
	INY
	LDA ptrA4H
	STA (ptrA6L),Y

.Label_99BB
	LDA &0D40
	BEQ Sub_99DB_ListenForScout	; If Port = 0 : Immediate operation

	\ Update receive control block

	LDA &0D3E			; Source station net
	LDY #&03
	STA (ptrA6L),Y
	DEY
	LDA &0D3D			; Source station id
	STA (ptrA6L),Y
	DEY
	LDA &0D40			; Port
	STA (ptrA6L),Y
	DEY
	LDA &0D3F			; Control byte
	ORA #&80			; Message ready!
	STA (ptrA6L),Y
}


.Sub_99DB_ListenForScout
	LDA #&02
	AND TubePresent_D67
	BIT RXTX_Flags_D4A
	BEQ Sub_99E8_ListenForScout	; If not tube
	JSR Release_Tube_Sub_9A2B	; Release tube

.Sub_99E8_ListenForScout
	JSR Sub_9F4C_Reset		; Clear up

.Sub_99EB_ListenForScout		; Listen for next scout
	LDA #LO(Sub_96BF_ListenForScout)
	LDY #HI(Sub_96BF_ListenForScout)
	JMP Sub_0D0E_Next_YA


	\ Copy data (8 bytes) from received frame to buffer
	\ ONLY USED WHEN BROADCAST RECEIVED
.Sub_99F2_CopyDataToBuffer
{					; (Setup by call to Sub_9ECA_SetupCounters)
	TXA
	PHA
	LDX #&04			; Number of bytes = 12 - 4 = 8

	LDA #&02
	BIT RXTX_Flags_D4A
	BNE Label_9A19			; If tube

	LDY &A2				; Copy to ptrA4 + ?A2
.Loop_99FF
	LDA &0D3D,X
	STA (ptrA4L),Y
	INY
	BNE Label_9A0D
	INC ptrA4H
	DEC &A3
	BEQ Label_9A6E			; If no more pages
.Label_9A0D
	INX
	STY &A2
	CPX #&0C
	BNE Loop_99FF
}

.Label_9A14
	PLA
	TAX
	JMP Sub_99A4

.Label_9A19
{
.Loop_9A19
	LDA &0D3D,X
	STA TUBE_R3_DATA
	JSR Sub_9A37_IncCounter		; Increment counter
	BEQ Label_9A70
	INX
	CPX #&0C
	BNE Loop_9A19
	BEQ Label_9A14			; always
}

.Release_Tube_Sub_9A2B
{
	BIT &98
	BMI Label_9A34			; Tube not claimed!
	LDA #&82
	JSR TubeCode			; Release Tube (Low level primitives)
.Label_9A34
	LSR &98
	RTS
}

	\ !&A2 += 1
.Sub_9A37_IncCounter
{
	INC &A2
	BNE Label_9A45
	INC &A3
	BNE Label_9A45
	INC ptrA4L
	BNE Label_9A45
	INC ptrA4H
.Label_9A45
	RTS
}

	\ RCVD Immediate Operation
.Sub_9A46_ImmediateOp
{
	LDY &0D3F			; Control byte received
	CPY #&81
	BCC Label_9A76			; Y < &81
	CPY #&89
	BCS Label_9A76			; Y >= &89

	CPY #&87
	BCS Label_9A63			; Y = &87 CONT or &88 MACHINETYPE

	TYA
	SEC
	SBC #&81			; 0 <= A <= 5
	TAY

	LDA ProtectionMask_D63		; Protection mask
.Loop_9A5D
	ROR A
	DEY
	BPL Loop_9A5D
	BCS Sub_99E8_ListenForScout	; If not allowed : rx reset

.Label_9A63
	LDY &0D3F			; &81 <= A <= &88

	LDA #HI(Sub_9ABC_RX_PEEK-1)	; Assume all same page!
	PHA
	LDA Data_9A79-&81,Y
	PHA
	RTS				; 'Return' to routine
}

.Label_9A6E
	INC &A2
.Label_9A70
	CPX #&0B
	BEQ Label_9A14

	PLA
	TAX

.Label_9A76
	JMP Sub_9835

.Data_9A79
	EQUB LO(Sub_9ABC_RX_PEEK-1)	; 81 PEEK
	EQUB LO(Sub_9A9F_RX_POKE-1)	; 82 POKE
	EQUB LO(Sub_9A81_RX_JSR-1)	; 83 JSR
	EQUB LO(Sub_9A81_RX_JSR-1)	; 84 User
	EQUB LO(Sub_9A81_RX_JSR-1)	; 85 OS
	EQUB LO(Sub_9AD6_RX_HALTCONT-1)	; 86 Halt
	EQUB LO(Sub_9AD6_RX_HALTCONT-1)	; 87 Continue
	EQUB LO(Sub_9AAA_RX_MACHINETYPE-1)	; 88 MACHINETYPE

	\ RCVD 83 JSR/84 User procedure/85 OS procedure
.Sub_9A81_RX_JSR
{
	LDA #&00
	STA ptrA4L			; ptrA4 = ptr9CH * &100
	LDA #&82			; ?&A2 = 0
	STA &A2				; ?&A3 = 1 (page)
	LDA #&01
	STA &A3
	LDA ptr9CH_PWS0
	STA ptrA4H

	LDY #&03			; !&0D58 = !&0D41
.Loop_9A93
	LDA &0D41,Y
	STA &0D58,Y
	DEY
	BPL Loop_9A93
	JMP Sub_97CB
}

	\ RCVD 82 POKE
.Sub_9A9F_RX_POKE
	LDA #&3D			; ptrA6 = &0D3D (buffer control block)
	STA ptrA6L
	LDA #&0D
	STA ptrA6H
	JMP Sub_97B9_RX_FoundControlBlock

	\ RCVD 88 MACHINETYPE
.Sub_9AAA_RX_MACHINETYPE
	LDA #&01			; ptr4A = &7F25    (&7F25 + &FC = &8021)
	STA &A3				; ?&A2 = &FC	   (&100 - &FC = 4 bytes)
	LDA #&FC			; ?&A3 = 1
	STA &A2
	LDA #LO(Data_8021-&FC)
	STA ptrA4L
	LDA #HI(Data_8021-&FC)
	STA ptrA4H
	BNE Label_9ACE			; always

	\ RCVD 81 PEEK
.Sub_9ABC_RX_PEEK
	LDA #&3D			; ptrA6 = &0D3D (buffer control block)
	STA ptrA6L
	LDA #&0D
	STA ptrA6H
	LDA #TUBEOP2
	STA TubeOp_D5C			; Tube op 2 = Multi pairs transfer Parasite->Host
	JSR Sub_9ECA_SetupCounters	; Setup counters/Tube
	BCC Label_9B1D			; If failed (i.e. tube failed)

.Label_9ACE
	LDA RXTX_Flags_D4A		; Transmit data (Sub_9907)
	ORA #&80
	STA RXTX_Flags_D4A

	\ RCVD 86 HALT / 87 CONTINUE
.Sub_9AD6_RX_HALTCONT
	LDA #&44
	STA ADLC_REG0			; TxRS = 0, RxRS = 1, TIE = 1, RIE = 0, AC = 0
	LDA #&A7
	STA ADLC_REG1			; RTS = 1, CLR RxST = 1, F/M Idle = 1, 2/1 Byte = 1, PSE =1
	LDA #LO(Sub_9AFD_StartShiftRegister)
	LDY #HI(Sub_9AFD_StartShiftRegister)
	JMP Sub_9907_Send_Data_Or_Ack

	\ IMMEDIATE OPERATION
.Sub_9AE7
	LDA &A2
	CLC
	ADC #&80
	LDY #&7F
	STA (ptr9CL_PWS0),Y
	LDY #&80
	LDA &0D3D			; Source station
	STA (ptr9CL_PWS0),Y
	INY
	LDA &0D3E			; Source net
	STA (ptr9CL_PWS0),Y

	\ Interrupt generated after approx 20 cycles of 1MHz
.Sub_9AFD_StartShiftRegister
	LDA &0D3F
	STA &0D57			; ?&0D57 = Control byte (Immediate Op)

	LDA #&84
	STA SYSVIA_IER			; Enable Shift Reg interrupt

	LDA SYSVIA_ACR
	AND #&1C			; 00011100
	STA &0D51			; Save Shift Reg Control

	\ Shift in under control of system clock
	LDA SYSVIA_ACR			; Set Shift Reg Control to 010
	AND #&E3			; 11100011
	ORA #&08			; 00001000
	STA SYSVIA_ACR

	BIT SYSVIA_SR			; Start shifting
			
.Label_9B1D
	JMP Sub_99E8_ListenForScout

	\ THESE ROUTINES ARE CALLED BY NFS_SERVICE_05
	\ It assumes they are all in the same page as the table.
	\ #&0D58 = call address
.Data_9B20
{
	EQUB LO(Sub_9B25_JSR-1)		; &83 JSR
	EQUB LO(Sub_9B2E_USER-1)	; &84 USER
	EQUB LO(Sub_9B3C_OS-1)		; &85 OS
	EQUB LO(Sub_9B48_HALT-1)	; &86 HALT
	EQUB LO(Sub_9B5F_CONT-1)	; &87 CONT

.Sub_9B25_JSR				; &83
	LDA #HI(Label_9B67-1)
	PHA
	LDA #LO(Label_9B67-1)
	PHA
	JMP (&0D58)

.Sub_9B2E_USER				; &84
	LDY #&08
	LDX &0D58
	LDA &0D59
	JSR OSEVEN			; Generate event Y=&08 Network Event => JSR EVNTV
	JMP Label_9B67

.Sub_9B3C_OS				; &85
	LDX &0D58
	LDY &0D59
	JSR langentry
	JMP Label_9B67

.Sub_9B48_HALT				; &86 HALT
	LDA #&04
	BIT RXTX_Flags_D64
	BNE Label_9B67			; If b3 = 1 already halted

	ORA RXTX_Flags_D64
	STA RXTX_Flags_D64		; Set b3

	LDA #&04
	CLI				; Enable interrupts
.Loop_9B58
	BIT RXTX_Flags_D64
	BNE Loop_9B58			; Idle until CONT received.
	BEQ Label_9B67			; always

.Sub_9B5F_CONT				; &87 CONTINUE
	LDA RXTX_Flags_D64
	AND #&FB			; Clear b3
	STA RXTX_Flags_D64

.Label_9B67				; Exit routines (exit service call NFS_SERVICE_05)
	PLA				; Restore X & Y
	TAY
	PLA
	TAX
	LDA #&00			; A = 0 = Service done!
	RTS
}

	\ **************** Start Transmit *****************
	\ ************** SENDS SCOUT FRAME ****************
	\ Entry: ptrA0 -> control block (as OSWORD &10)
	\ Exit : X preserved
.Sub_9B6E_Transmit_Start
{
	TXA				; Save X
	PHA

	LDY #&02
	LDA (ptrA0L),Y			; #&D20 = Destination Station
	STA &0D20
	INY
	LDA (ptrA0L),Y
	STA &0D21

	LDY #&00
	LDA (ptrA0L),Y			; A = Control byte
	BMI Label_9B86
	JMP Label_9C11			; Error &44 Malformed block

.Label_9B86
	STA &0D24
	TAX				; X = Control byte (X is >=&80)

	INY
	LDA (ptrA0L),Y			; Destination Port
	STA &0D25
	BNE Label_9BC5			; Port > 0

	\ Port = 0 : Immediate Operation

	CPX #&83
	BCS Label_9BB1			; X >= &83

	\ &81 PEEK and &82 POKE
	SEC				; Let !&D2A = Buffer End - Buffer Start = Buffer Size
	PHP
	LDY #&08
.Loop_9B9A
	LDA (ptrA0L),Y			; Buffer End
	DEY				; Y=Y-4
	DEY
	DEY
	DEY
	PLP
	SBC (ptrA0L),Y			; Buffer Start
	STA &0D26,Y			; &0D26+4=&0D2A
	INY				; Y=Y+5
	INY
	INY
	INY
	INY
	PHP
	CPY #&0C
	BCC Loop_9B9A
	PLP

.Label_9BB1
	CPX #&81
	BCC Label_9C11			; If X < &81 Or
	CPX #&89
	BCS Label_9C11			; If X >= &89 : Error &44 Badly formed control block

	LDY #&0C			; !&D26 = Remote Address
.Loop_9BBB
	LDA (ptrA0L),Y
	STA &0D1A,Y			; &D1A + &C = &D26
	INY
	CPY #&10
	BCC Loop_9BBB

.Label_9BC5
	LDA #&20
	BIT ADLC_REG1
	BNE Label_9C21			; SR2: DCD = 1 = No Clock

	LDA #&FD
	PHA				; Counter 3 - see below

	LDA #&06			; Setup scout packet:
	STA &0D50			; Number of bytes (at &0D20).

	LDA #&00
	STA &0D4F			; Next byte to send (see Sub_9CCC).

	PHA				; Counter 2
	PHA				; Counter 1
	LDY #&E7			; ADCL_REG1

.Loop_9BDD
	LDA #&04
	PHP
	SEI
	BIT _INTOFF_STATIONID
	BIT _INTOFF_STATIONID
	BIT ADLC_REG1
	BEQ Label_9BFB			; SR2: /OVRN = 0
	LDA ADLC_REG0
	LDA #&67			; A = 01100111
	STA ADLC_REG1			; CR2: CLR Tx ST = 1, CLR Rx ST = 1, FC/TDRA Select = 0 (TDRA) F/M Idle = 1, 2/1 Byte = 1, PSE = 1
	LDA #&10
	BIT ADLC_REG0
	BNE Label_9C2F			; SR1: /CTS = 1 = clear :. Ready

.Label_9BFB
	BIT _INTON_			; Wait for interrupt
	PLP
	TSX				; Idle
	INC &0101,X			; Increment counters
	BNE Loop_9BDD
	INC &0102,X
	BNE Loop_9BDD
	INC &0103,X
	BNE Loop_9BDD
	BEQ Label_9C15			; always Error jammed

.Label_9C11
	LDA #&44			; Error &44 Badly formed control block
	BNE Label_9C23			; always

.Label_9C15
	LDA #&07
	STA ADLC_REG1
	PLA				; Pull counters
	PLA
	PLA
	LDA #&40			; Error &40 Network jammed
	BNE Label_9C23			; always

.Label_9C21
	LDA #&43			; Error &43 No clock

.Label_9C23				; Return error in control block
	LDY #&00
	STA (ptrA0L),Y			; Control Byte = 0 (transmission failed to start)
	LDA #&80
	STA &0D62
	PLA
	TAX
	RTS

.Label_9C2F				; *** Ready to transmit ***
					; Y = &E7 = 11100111
	STY ADLC_REG1			; CR2 : RTS = 1, Clear Tx status, Clear Rx status, ...
	LDX #&44			; X = 01000100
	STX ADLC_REG0			; CR1 : TxRS = 0, RxRS = 1, TIE = 1, RIE = 0

	LDX #LO(Sub_9CCC)		; Interrupt exit address
	LDY #HI(Sub_9CCC)
	STX &0D0C
	STY &0D0D

	SEC
	ROR &98

	BIT _INTON_			; Enable ADLC interrupt

	LDA &0D25
	BNE Label_9C8E			; If Destination Port > 0

	\ Destination Port = 0 :. Immediate Operation

	LDY &0D24			; Y = control byte (&81 to &88)

	LDA Data_9EC2-&81,Y
	STA RXTX_Flags_D4A

	LDA Data_9EBA-&81,Y
	STA &0D50			; Number of bytes in scout (for immediate operations).

	LDA #HI(Sub_9C6F-1)		; Assume all routines in same page.
	PHA
	LDA Data_9C63-&81,Y
	PHA
	RTS

.Data_9C63
	EQUB LO(Sub_9C6F-1)		; 81 PEEK
	EQUB LO(Sub_9C73-1)		; 82 POKE
	EQUB LO(Sub_9CB5-1)		; 83 JSR
	EQUB LO(Sub_9CB5-1)		; 84 User
	EQUB LO(Sub_9CB5-1)		; 85 OS
	EQUB LO(Sub_9CC5-1)		; 86 Halt
	EQUB LO(Sub_9CC5-1)		; 87 Continue
	EQUB LO(Sub_9C6B-1)		; 88

.Sub_9C6B				; Control &88
	LDA #TUBEOP3
	BNE Label_9CB7			; always

.Sub_9C6F				; Control &81 PEEK
	LDA #TUBEOP3
	BNE Label_9C75			; always

.Sub_9C73				; Control &82 POKE
	LDA #TUBEOP2

.Label_9C75
	STA TubeOp_D5C
	CLC
	PHP				; Note always Z=0?
	LDY #&0C

.Loop_9C7C
	LDA &0D1E,Y			; &0D1E + &C = &D2A
	PLP
	ADC (ptrA0L),Y			; !&D2A += Remote Station Address
	STA &0D1E,Y
	INY
	PHP
	CPY #&10
	BCC Loop_9C7C
	PLP
	BNE Label_9CBA			; always?

.Label_9C8E				; Destination Port <> 0
	LDA &0D20
	AND &0D21
	CMP #&FF
	BNE Label_9CB0			; If Destination Station <> &FFFF

	\ BROADCAST

	LDA #&0E			; Broadcast
	STA &0D50			; 6 + 8 byte scout

	LDA #&40
	STA RXTX_Flags_D4A

	LDY #&04			; Copy control block broadcast data (8 bytes)
.Loop_9CA4
	LDA (ptrA0L),Y
	STA &0D22,Y
	INY
	CPY #&0C
	BCC Loop_9CA4
	BCS Sub_9CC5			; always

.Label_9CB0
	LDA #&00
	STA RXTX_Flags_D4A

.Sub_9CB5				; Control byte &83 JSR, &84 User, &85 OS
	LDA #TUBEOP2

.Label_9CB7
	STA TubeOp_D5C

.Label_9CBA
	LDA ptrA0L			; ptrA6 = ptrA0
	STA ptrA6L
	LDA ptrA0H
	STA ptrA6H
	JSR Sub_9ECA_SetupCounters

.Sub_9CC5				; Control byte &86 Halt, &87 Continue
	PLP				; Clean up and exit.

	PLA				; Pull counters
	PLA
	PLA

	PLA				; Restore X
	TAX
	RTS

	\ INTERRUPT ROUTINE
.Sub_9CCC				; SEND SCOUT FRAME
	LDY &0D4F
	BIT ADLC_REG0			; SR1 : V = TDRA status (Note CR2 FC/TDRA Select = 0)
.Loop_9CD2
	BVC Sub_9CF6_tx_error41		; If TDRA = 0 then error.
	LDA &0D20,Y			; else first two bytes in FIFO empty.
	STA ADLC_REG2			; Write FIFO (2 BYTES)
	INY
	LDA &0D20,Y
	INY
	STY &0D4F
	STA ADLC_REG2
	CPY &0D50			; Y = frame size?
	BCS Label_9D08			; All bytes sent!
	BIT ADLC_REG0
	BMI Loop_9CD2			; If IRQ = 1 (TIE)
	JMP Sub_0D14_Next			; Exit NMI routine (finish up & RTI).
}

.Sub_9CF2_tx_error42
	LDA #&42			; Error : No scout acknowledged in four-way handshake.
	BNE Sub_9CFD_tx_errorA

.Sub_9CF6_tx_error41
	LDA #&67
	STA ADLC_REG1			; Clear Tx & Rx status, ...
	LDA #&41			; Error : Some part of the four-way handshake lost or damaged.

.Sub_9CFD_tx_errorA
{
	LDY _INTOFF_STATIONID		; Disable ADLC interrupt
.Loop_9D00				; Idle a bit
	PHA
	PLA
	INY
	BNE Loop_9D00
	JMP Sub_9EAE_tx_resultA
}

.Label_9D08				; Last byte of scout sent
	LDA #&3F
	STA ADLC_REG1			; Clear Rx status, Transmit last data, Select Frame Complete
	LDA #LO(Sub_9D14)
	LDY #HI(Sub_9D14)
	JMP Sub_0D0E_Next_YA

.Sub_9D14
{
	LDA #&82
	STA ADLC_REG0			; CR1 : TxRS = 1, RxRS = 0, TIE = 0, RIE = 1
	BIT RXTX_Flags_D4A
	BVC Label_9D21			; Not broadcast
	JMP Sub_9EA8_tx_success		; Exit ok

.Label_9D21
	LDA #&01
	BIT RXTX_Flags_D4A
	BEQ Label_9D2B			; If b0 = 0
	JMP Sub_9E50

.Label_9D2B
	LDA #LO(Sub_9D30)		; Same page!
	JMP Sub_0D11_Next_A
}

.Sub_9D30
{
	LDA #&01
	BIT ADLC_REG1			; SR2
	BEQ Sub_9CF2_tx_error42		; If AP = 0 : Address not present, so ERROR!

	LDA ADLC_REG2
	CMP _INTOFF_STATIONID
	BNE Label_9D58			; Not this station!

	LDA #LO(Sub_9D44)
	JMP Sub_0D11_Next_A

.Sub_9D44
	BIT ADLC_REG1			; SR2
	BPL Label_9D58			; If RDA = 0 : no data

	LDA ADLC_REG2
	BNE Label_9D58			; If Rx_StationNet <> 0

	LDA #LO(Sub_9D5B)

	BIT ADLC_REG0			; SR1
	BMI Sub_9D5B			; If IRQ

	JMP Sub_0D11_Next_A
}

.Label_9D58
	JMP Sub_9EAC_tx_error41

.Sub_9D5B
	BIT ADLC_REG1
	BPL Label_9D58			; SR2: If RDA = 0 : ERROR

	LDA ADLC_REG2
	CMP &0D20
	BNE Label_9D58			; If Dest Station <> Rx_SourceStation : ERROR

	LDA ADLC_REG2
	CMP &0D21
	BNE Label_9D58			; If Dest Net <> Rx_SourceNet : ERROR

	LDA #&02
	BIT ADLC_REG1
	BEQ Label_9D58			; SR2 : If FV = 0 (don't expect more) : ERROR

	\ Scout Acknowedgement received!

	LDA #&A7
	STA ADLC_REG1			; CR2 : RTS = 1, CLR RxST, F/M Idle = 1, 2/1 Byte = 1, PSE = 1

	LDA #&44
	STA ADLC_REG0			; CR1 : TxRS = 0, RxRS = 1, TIE = 1, RIE = 0

	LDA #LO(Sub_9E50)
	LDY #HI(Sub_9E50)

	STA &0D4B			; #&0D4B = &9E50
	STY &0D4C

	\ Send Destination Station & Net
	LDA &0D20			; A = Destination Station
	BIT ADLC_REG0
	BVC Label_9DCD			; SR1 : If TDRA = 0

	STA ADLC_REG2

	LDA &0D21			; A = Destination Net
	STA ADLC_REG2

	LDA #LO(Sub_9DA3)
	LDY #HI(Sub_9DA3)
	JMP Sub_0D0E_Next_YA

	\ Send Source Station & Net
.Sub_9DA3
	LDA _INTOFF_STATIONID		; A = Source Station

	BIT ADLC_REG0
	BVC Label_9DCD			; SR1 : If TDRA = 0
	STA ADLC_REG2

	LDA #&00			; A = Source Next
	STA ADLC_REG2

.Sub_9DB3
{
	LDA #&02
	BIT RXTX_Flags_D4A
	BNE Label_9DC1			; If to tube

	LDA #LO(Sub_9DC8)
	LDY #HI(Sub_9DC8)
	JMP Sub_0D0E_Next_YA

.Label_9DC1
	LDA #LO(Sub_9E0F)
	LDY #HI(Sub_9E0F)
	JMP Sub_0D0E_Next_YA
}

	\ Transmit data from HOST (ptrA4)
.Sub_9DC8
	LDY &A2
	BIT ADLC_REG0
.Label_9DCD
{
.Loop_9DCD
	BVC Sub_9E48			; SR1 : If TDRA = 0
	LDA (ptrA4L),Y
	STA ADLC_REG2
	INY
	BNE Label_9DDD
	DEC &A3
	BEQ Label_9DF5
	INC ptrA4H
.Label_9DDD
	LDA (ptrA4L),Y
	STA ADLC_REG2
	INY
	STY &A2
	BNE Label_9DED
	DEC &A3
	BEQ Label_9DF5
	INC ptrA4H
.Label_9DED
	BIT ADLC_REG0
	BMI Loop_9DCD			; SR1 : If IRQ
	JMP Sub_0D14_Next
}

.Label_9DF5
{
	LDA #&3F
	STA ADLC_REG1
	LDA RXTX_Flags_D4A
	BPL Label_9E06

	LDA #LO(Sub_99DB_ListenForScout)
	LDY #HI(Sub_99DB_ListenForScout)
	JMP Sub_0D0E_Next_YA

.Label_9E06
	LDA &0D4B			; Y:A = #&0D4B
	LDY &0D4C
	JMP Sub_0D0E_Next_YA
}

	\ Transmit data from TUBE
.Sub_9E0F
{
	BIT ADLC_REG0
.Loop_9E12
	BVC Sub_9E48			; SR1 : If TDRA = 0

	LDA TUBE_R3_DATA
	STA ADLC_REG2
	INC &A2
	BNE Label_9E2A
	INC &A3
	BNE Label_9E2A
	INC ptrA4L
	BNE Label_9E2A
	INC ptrA4H
	BEQ Label_9DF5

.Label_9E2A
	LDA TUBE_R3_DATA
	STA ADLC_REG2
	INC &A2
	BNE Label_9E40
	INC &A3
	BNE Label_9E40
	INC ptrA4L
	BNE Label_9E40
	INC ptrA4H
	BEQ Label_9DF5

.Label_9E40
	BIT ADLC_REG0
	BMI Loop_9E12			; SR1 : If IRQ
	JMP Sub_0D14_Next
}

.Sub_9E48
	LDA RXTX_Flags_D4A
	BPL Sub_9EAC_tx_error41			; If b7 = 0, then ERROR &41
	JMP Sub_99DB_ListenForScout

.Sub_9E50
	LDA #&82
	STA ADLC_REG0			; CR1 : TxRS = 1, RxRS = 0, TIE = 0, RIE = 1
	LDA #LO(Sub_9E5C)
	LDY #HI(Sub_9E5C)
	JMP Sub_0D0E_Next_YA

.Sub_9E5C
	LDA #&01
	BIT ADLC_REG1
	BEQ Sub_9EAC_tx_error41			; SR2 : If AP = 0

	LDA ADLC_REG2
	CMP _INTOFF_STATIONID
	BNE Sub_9EAC_tx_error41			; If Station ID <> RX_STATION

	LDA #LO(Sub_9E70)
	JMP Sub_0D11_Next_A

.Sub_9E70
	BIT ADLC_REG1
	BPL Sub_9EAC_tx_error41		; If RDA = 0 (no data) : ERROR

	LDA ADLC_REG2
	BNE Sub_9EAC_tx_error41		; If RX_NET <> 0 : ERROR

	LDA #LO(Sub_9E84)
	BIT ADLC_REG0
	BMI Sub_9E84			; SR1 : If IRQ
	JMP Sub_0D11_Next_A

.Sub_9E84
{
	BIT ADLC_REG1
	BPL Sub_9EAC_tx_error41		; If RDA = 0 : Error

	LDA ADLC_REG2
	CMP &0D20
	BNE Sub_9EAC_tx_error41		; If Dest Station ID <> RX_SOURCE_STATIONID

	LDA ADLC_REG2
	CMP &0D21
	BNE Sub_9EAC_tx_error41		; If Dest Station NET <> RX_SOURCE_STATIONNET

	\ Data Acknowledgement Received!

	LDA RXTX_Flags_D4A
	BPL Label_9EA1			; If b7 = 0
	JMP Sub_981B

.Label_9EA1
	LDA #&02
	BIT ADLC_REG1			; FV = Frame Valid
	BEQ Sub_9EAC_tx_error41		; If SR2.FV = 0 then error &41
}

	\ TX SUCCESSFUL
.Sub_9EA8_tx_success
	LDA #&00
	BEQ Sub_9EAE_tx_resultA		; always

	\ TX ERROR &41
.Sub_9EAC_tx_error41
	LDA #&41			; ERROR &41 = some part of 4-way handshake lost or damaged.

.Sub_9EAE_tx_resultA
	LDY #&00
	STA (ptrA0L),Y			; Return status
	LDA #&80
	STA &0D62
	JMP Sub_99DB_ListenForScout

.Data_9EBA				; Bytes in scout for immediate operations &81 to &88
	EQUB 6+8, 6+8, 6+4, 6+4, 6+4, 6, 6, 6+4

.Data_9EC2
	EQUB &81, &00, &00, &00, &00, &01, &01, &81

	\**** Setup counters, start tube op if applicable
	\ ptrA6 -> buffer control block
	\ ?TubeOp_D5C = tube operation (if applicable)
	\ Exit: C = 1 if ok (else Tube failed)
.Sub_9ECA_SetupCounters
{
	LDY #&06
	LDA (ptrA6L),Y
	INY
	AND (ptrA6L),Y
	CMP #&FF
	BEQ Label_9F19			; If Buffer in host

	LDA TubePresent_D67
	BEQ Label_9F19			; If Tube not present

	\ **** USE TUBE ****
	LDA RXTX_Flags_D4A		; To TUBE
	ORA #&02
	STA RXTX_Flags_D4A

	SEC
	PHP
	LDY #&04			; !&A2 = Buffer Start - Buffer End
.Loop_9EE6
	LDA (ptrA6L),Y
	INY
	INY
	INY
	INY
	PLP
	SBC (ptrA6L),Y
	STA &009A,Y
	DEY
	DEY
	DEY
	PHP
	CPY #&08
	BCC Loop_9EE6
	PLP
	TXA
	PHA

	LDA #&04
	CLC
	ADC ptrA6L
	TAX				; Assumes same page.
	LDY ptrA6H			; YX -> ptrA6!4 = Buffer Start

	LDA #&C2
	JSR TubeCode			; Claim Tube (Low level primitives)
	BCC Label_9F16			; If failed

	LDA TubeOp_D5C			; Operation
	JSR TubeCode

	JSR Release_Tube_Sub_9A2B	; Release Tube
	SEC

.Label_9F16
	PLA
	TAX
	RTS				; C=0 if failed

	\ **** USE HOST ****
	\ ptrA4 = Base address
	\ ?&A2  = start offset (such that ptrA4 + ?&A2 = Buffer Start)
	\ ?&A3  = number of pages
.Label_9F19				; To HOST
	LDY #&04			; Buffer start
	LDA (ptrA6L),Y
	LDY #&08			; Buffer end
	SEC
	SBC (ptrA6L),Y
	STA &A2

	LDY #&05
	LDA (ptrA6L),Y
	SBC #&00
	STA ptrA4H

	LDY #&08
	LDA (ptrA6L),Y
	STA ptrA4L

	LDY #&09
	LDA (ptrA6L),Y

	SEC
	SBC ptrA4H
	STA &A3
	SEC

	RTS				; C=1=Success
}

.Sub_9F3D_Reset
	LDA #&C1			; CR1: TxRS = 1, RxRS = 1, AC = 1
	STA ADLC_REG0
	LDA #&1E			; CR4: Rx WLS = 8 bits + Tx WLS = 8 bits  (WLS=Word Length Select)
	STA ADLC_REG3
	LDA #&00			; CR3: Disable other modes
	STA ADLC_REG1

.Sub_9F4C_Reset
	LDA #&82			; CR1: TxRS = 1, RIE (Rx Interrupt enable) = 1, AC = 0
	STA ADLC_REG0
	LDA #&67			; CR2: Clear TxST = 1, Clear RxST = 1, F/M Idle = 1, 2/1 Byte = 1, PSE = 1
	STA ADLC_REG1
	RTS

.NFS_NMI_CLAIM				; Another ROM is claiming NMI
{
	BIT &0D66
	BPL Label_9F7A
.Loop_9F5C
	LDA &0D0C
	CMP #&BF
	BNE Loop_9F5C
	LDA &0D0D
	CMP #&96
	BNE Loop_9F5C
	BIT _INTOFF_STATIONID
	BIT _INTOFF_STATIONID
	LDA #&00
	STA &0D62
	STA &0D66
	LDY #&05
}

.Label_9F7A
	JMP Sub_9F4C_Reset

	\\ This routine is copied to &0D00
.Label_9F7D
{
.Sub_0D00_NMI
	BIT _INTOFF_STATIONID		; D00 Disable further interrupts from ADLC
	PHA				; D03 Preserve A & Y
	TYA				; D04
	PHA				; D05
	LDA #&00			; D06 Operand replaced by NFS ROM Nr.
	STA PagedRomSelector		; D08
	JMP Sub_96BF_ListenForScout	; D0B Address can change!

	\ Y:A = address of routine to call when interrupt occurs.
.Sub_0D0E_Next_YA
	STY &0D0D			; D0E Change jump address
.Sub_0D11_Next_A
	STA &0D0C			; D11

.Sub_0D14_Next
	LDA PagedRomSelector_RAMCopy	; D14 Restore conditions
	STA PagedRomSelector		; D17
	PLA				; D1A Restore A & Y
	TAY				; D1B
	PLA				; D1C
	BIT _INTON_			; D1D Enable ADLC interrupts
	RTI				; D20
}
