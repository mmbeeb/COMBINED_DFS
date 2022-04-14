	\\ Acorn DFS
	\\ filesys_eq.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

	NMI_PrevNMIOwner=&A0

if sys<200
	NMI_FDCcmd=&A1
	NMI_RW_attempts=&A2
	NMI_Counter1=&A3
	NMI_Counter2=&A4
	NMI_Counter3=&A5
	NMI_DataPointer=&A6
else
	NMI_Flags=&A1
	NMI_FDCresult=&A2
	NMI_SecsPerTrk=&A3
	NMI_Timings=&A4
	NMI_SecCounter=&A5
	NMI_ByteCounter=&A6
	NMI_FDCcommand=&A7
endif

	\ Static workspace offset
if sys=224
	swsp=&C000-&0E00		;MASTER
else
	swsp=0
endif

	DirectoryParam=&CC
	CurrentDrv=&CD
	Track=&CE
	Sector=&CF

	FilesX8=swsp+&0F05
	TubeOpCode=swsp+&1080
	IsTubeGBPB=swsp+&1081
	LoadedCatDrive=swsp+&1082
	IsDriveReady=swsp+&1083
	FSMessagesOnIfZero=swsp+&10C6
	CMDEnabledIf1=swsp+&10C7
	NMIstatus=swsp+&10C8
	DEFAULT_DIR=swsp+&10C9
	DEFAULT_DRIVE=swsp+&10CA
	LIB_DIR=swsp+&10CB
	LIB_DRIVE=swsp+&10CC
	PAGE=swsp+&10CF
	RAMAVAILABLE=swsp+&10D0
	SRC_DISK=swsp+&10D1
	DEST_DISK=swsp+&10D2
	NotTUBEOpIf0=swsp+&10D5
	TubePresentIf0=swsp+&10D6

if sys<200
	FDC_WRCMD_RDSTATUS=&FE80
	FDC_WRPARA_RDRESULT=&FE81
	FDC_WRRESET=&FE82
	FDC_DATA=&FE84
else
if sys=226
	FDC_DRIVECONTROL=&FE80
	FDC_STATUS_COMMAND=&FE84
	FDC_TRACK=&FE85
	FDC_SECTOR=&FE86
	FDC_DATA=&FE87
else
	FDC_DRIVECONTROL=&FE24
	FDC_STATUS_COMMAND=&FE28
	FDC_TRACK=&FE29
	FDC_SECTOR=&FE2A
	FDC_DATA=&FE2B
endif
endif

\\ End of file
