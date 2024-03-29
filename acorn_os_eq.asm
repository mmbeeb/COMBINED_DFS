	\\ Acorn OS
	\\ acorn_os_eq.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

	TextPointer=&F2
	PagedRomSelector_RAMCopy=&F4

	USERV=&0200
	FSCV=&021E
	TubeCode=&0406
	NMIRoutine=&0D00
	PagedROM_PrivWorkspaces=&0DF0

	PagedRomSelector=&FE30

	ADLC_REG0=&FEA0
	ADLC_REG1=&FEA1
	ADLC_REG2=&FEA2
	ADLC_REG3=&FEA3
	_INTOFF_STATIONID=&FE18
	_INTON_=&FE20

	TUBE_R1_STATUS=&FEE0
	TUBE_R1_DATA=&FEE1
	TUBE_R2_STATUS=&FEE2
	TUBE_R2_DATA=&FEE3
	TUBE_R3_STATUS=&FEE4
	TUBE_R3_DATA=&FEE5
	TUBE_R4_STATUS=&FEE6
	TUBE_R4_DATA=&FEE7

	SYSVIA_SR=&FE4A
	SYSVIA_ACR=&FE4B
	SYSVIA_IFR=&FE4D
	SYSVIA_IER=&FE4E

	OSRDRM=&FFB9
	OSEVEN=&FFBF
	GSINIT=&FFC2
	GSREAD=&FFC5
	NVWRCH=&FFC8
	NVRDCH=&FFCB
	OSFIND=&FFCE
	OSGBPB=&FFD1
	OSBPUT=&FFD4
	OSBGET=&FFD7
	OSARGS=&FFDA
	OSFILE=&FFDD
	OSRDCH=&FFE0
	OSASCI=&FFE3
	OSNEWL=&FFE7
	OSWRCH=&FFEE
	OSWORD=&FFF1
	OSBYTE=&FFF4
	OSCLI=&FFF7

