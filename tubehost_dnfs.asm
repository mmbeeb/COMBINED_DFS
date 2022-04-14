	\\ Acorn Tube Host (DNFS version)
	\\ tubehost_dnfs.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather


 	\\ ******** TUBE **********
	\\ START OF TUBE HOST CODE


.TUBE_ZP_CODE_0016
	LDA #&FF			; COPIED TO &0016
	JSR &069E
	LDA TUBE_R2_DATA
	LDA #&00
	JSR &0695
	TAY
	LDA (&FD),Y
	JSR &0695

.tubezp_29
	INY
	LDA (&FD),Y
	JSR &0695
	TAX
	BNE tubezp_29

.tubezp_32
	LDX #&FF
	TXS
	CLI

.tubezp_36
	BIT TUBE_R1_STATUS
	BPL tubezp_41

.tubezp_3B
	LDA TUBE_R1_DATA
	JSR OSWRCH

.tubezp_41
	BIT TUBE_R2_STATUS
	BPL tubezp_36

	BIT TUBE_R1_STATUS
	BMI tubezp_3B

	LDX TUBE_R2_DATA
	STX &51
	JMP (&0500)

.tubezp_53
	EQUB  &00, &80, &00, &00

.TUBE_CODE_400
	JMP &0484			; COPIED TO &0400-&06FF
	JMP &06A7

.tube406
	CMP #&80			; MAIN SUB
	BCC tube435

	CMP #&C0
	BCS tube428

	ORA #&40
	CMP &15
	BNE tube434

	PHP
	SEI
	LDA #&05
	JSR &069E
	LDA &15
	JSR &069E
	PLP

.tube421
	LDA #&80
	STA &15
	STA &14
	RTS

.tube428
	ASL &14
	BCS tube432

	CMP &15
	BEQ tube434

	CLC
	RTS

.tube432
	STA &15

.tube434
	RTS

.tube435
	PHP
	SEI
	STY &13
	STX &12
	JSR &069E
	TAX
	LDY #&03
	LDA &15
	JSR &069E

.tube446
	LDA (&12),Y
	JSR &069E
	DEY
	BPL tube446

	LDY #&18
	STY TUBE_R1_STATUS
	LDA &0518,X
	STA TUBE_R1_STATUS
	LSR A
	LSR A
	BCC tube463
	BIT TUBE_R3_DATA
	BIT TUBE_R3_DATA

.tube463
	JSR &069E

.tube466
	BIT TUBE_R4_STATUS
	BVC tube466
	BCS tube47A

	CPX #&04
	BNE tube482

.tube471
	JSR &0414
	JSR &0695
	JMP &0032

.tube47A
	LSR A
	BCC tube482

	LDY #&88
	STY TUBE_R1_STATUS

.tube482
	PLP
	RTS

.tube484
	CLI	; SUB
	BCS tube498
	BNE tube48C

	JMP &059C

.tube48C
	LDX #&00
	LDY #&FF
	LDA #&FD
	JSR OSBYTE
	TXA
	BEQ tube471

.tube498
	LDA #&FF
	JSR TubeCode
	BCC tube498

	JSR &04D2

.tube4A2
	LDA #&07
	JSR &04CB
	LDY #&00
	STY &00

.tube4AB
	LDA (&00),Y
	STA TUBE_R3_DATA
	NOP
	NOP
	NOP
	INY
	BNE tube4AB

	INC &54
	BNE tube4C0

	INC &55
	BNE tube4C0

	INC &56

.tube4C0
	INC &01
	BIT &01
	BVC tube4A2

	JSR &04D2
	LDA #&04
	LDY #&00
	LDX #&53
	JMP TubeCode
	LDA #&80
	STA &54
	STA &01
	LDA #&20
	AND romtype
	TAY
	STY &53
	BEQ tube4FB

	LDX copywoffset

.tube4E5
	INX
	LDA &8000,X
	BNE tube4E5
	LDA &8001,X
	STA &53
	LDA &8002,X
	STA &54
	LDY &8003,X
	LDA &8004,X

.tube4FB
	STA &56
	STY &55
	RTS

.TUBE_CODE_500
	EQUB  &37, &05, &96, &05, &F2, &05, &07, &06
	EQUB  &27, &06, &68, &06, &5E, &05, &2D, &05
	EQUB  &20, &05, &42, &05, &A9, &05, &D1, &05
	EQUB  &86, &88, &96, &98, &18, &18, &82, &18

.tube520
	JSR &06C5
	TAY
	JSR &06C5
	JSR OSBPUT
	JMP &059C

	JSR &06C5
	TAY
	JSR OSBGET
	JMP &053A
	JSR OSRDCH
	ROR A
	JSR &0695
	ROL A
	JMP &059E
	JSR &06C5
	BEQ tube552

	PHA
	JSR &0582
	PLA
	JSR OSFIND
	JMP &059E

.tube552
	JSR &06C5
	TAY
	LDA #&00
	JSR OSFIND
	JMP &059C

	JSR &06C5
	TAY
	LDX #&04

.tube564
	JSR &06C5
	STA &FF,X
	DEX
	BNE tube564

	JSR &06C5
	JSR OSARGS
	JSR &0695
	LDX #&03

.tube577
	LDA &00,X
	JSR &0695
	DEX
	BPL tube577

	JMP &0036
	LDX #&00
	LDY #&00

.tube586
	JSR &06C5
	STA &0700,Y
	INY
	BEQ tube593

	CMP #&0D
	BNE tube586

.tube593
	LDY #&07
	RTS

	JSR &0582
	JSR OSCLI
	LDA #&7F

.tube59E
	BIT TUBE_R2_STATUS
	BVC tube59E
	STA TUBE_R2_DATA

.tube5A6
	JMP &0036

	LDX #&10

.tube5AB
	JSR &06C5
	STA &01,X
	DEX
	BNE tube5AB

	JSR &0582
	STX &00
	STY &01
	LDY #&00
	JSR &06C5
	JSR OSFILE
	JSR &0695
	LDX #&10

.tube5C7
	LDA &01,X
	JSR &0695
	DEX
	BNE tube5C7
	BEQ tube5A6

	LDX #&0D

.tube5D3
	JSR &06C5
	STA &FF,X
	DEX
	BNE tube5D3

	JSR &06C5
	LDY #&00
	JSR OSGBPB
	PHA
	LDX #&0C

.tube5E6
	LDA &00,X
	JSR &0695
	DEX
	BPL tube5E6

	PLA
	JMP &053A

	JSR &06C5
	TAX
	JSR &06C5
	JSR OSBYTE

.tube5FC
	BIT TUBE_R2_STATUS
	BVC tube5FC

	STX TUBE_R2_DATA

.tube604
	JMP &0036

	JSR &06C5
	TAX
	JSR &06C5
	TAY
	JSR &06C5
	JSR OSBYTE
	EOR #&9D
	BEQ tube604

	ROR A
	JSR &0695

.tube61D
	BIT TUBE_R2_STATUS
	BVC tube61D

	STY TUBE_R2_DATA
	BVS tube5FC

	JSR &06C5
	TAY

.tube62B
	BIT TUBE_R2_STATUS
	BPL tube62B

	LDX TUBE_R2_DATA
	DEX
	BMI tube645

.tube636
	BIT TUBE_R2_STATUS
	BPL tube636

	LDA TUBE_R2_DATA
	STA &0128,X
	DEX
	BPL tube636

	TYA

.tube645
	LDX #&28
	LDY #&01
	JSR OSWORD

.tube64C
	BIT TUBE_R2_STATUS
	BPL tube64C

	LDX TUBE_R2_DATA
	DEX
	BMI tube665

.tube657
	LDY &0128,X

.tube65A
	BIT TUBE_R2_STATUS
	BVC tube65A

	STY TUBE_R2_DATA
	DEX
	BPL tube657

.tube665
	JMP &0036
	LDX #&04

.tube66A
	JSR &06C5
	STA &00,X
	DEX
	BPL tube66A

	INX
	LDY #&00
	TXA
	JSR OSWORD
	BCC tube680

	LDA #&FF
	JMP &059E

.tube680
	LDX #&00
	LDA #&7F
	JSR &0695

.tube687
	LDA &0700,X
	JSR &0695
	INX
	CMP #&0D
	BNE tube687

	JMP &0036

.tube695
	BIT TUBE_R2_STATUS
	BVC tube695

	STA TUBE_R2_DATA
	RTS

.tube69E
	BIT TUBE_R4_STATUS
	BVC tube69E

	STA TUBE_R4_DATA
	RTS

	LDA &FF
	SEC
	ROR A
	BMI tube6BC

.tubeEVENThandler
	PHA
	LDA #&00
	JSR &06BC
	TYA
	JSR &06BC
	TXA
	JSR &06BC
	PLA

.tube6BC
	BIT TUBE_R1_STATUS
	BVC tube6BC

	STA TUBE_R1_DATA
	RTS

.tube6C5
	BIT TUBE_R2_STATUS
	BPL tube6C5

	LDA TUBE_R2_DATA
	RTS

	\\ END OF TUBE HOST CODE
