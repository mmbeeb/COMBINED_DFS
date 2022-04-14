	\\ Acorn NFS 3.60
	\\ netsys_eq.asm
	\\ Compiler: BeebAsm V1.08
	\\ Disassembly by Martin Mather

	ESC_ON = &97			; If bit 7 clear, Escape key is ignored.

	ptr9AL = &9A
	ptr9AH = &9B

	ptr9CL_PWS0 = &9C		; First page of private workspace
	ptr9CH_PWS0 = &9D

	ptr9EL_PWS1 = &9E		; Second page of private workspace
	ptr9EH_PWS1 = &9F

	ptrA0L = &A0
	ptrA0H = &A1
	ptrA4L = &A4
	ptrA4H = &A5
	ptrA6L = &A6
	ptrA6H = &A7
	ptrABL = &AB
	ptrABH = &AC

	ptrBBL = &BB
	ptrBBH = &BC
	ptrBEL = &BE
	ptrBEH = &BF

	ShowInfo = &0E06
