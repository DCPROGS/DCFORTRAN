	function SLOCK()
c Set true if SCROLL LOCK key is on (see TKEY.FOR)
c If SCROLL LOCK ON then NOUT=  1000 (binary)=10(hex)
c If NUM LOCK ON then    NOUT= 10000 (binary)=20(hex)
c If CAPS LOCK ON then   NOUT=100000 (binary)=40(hex)
	logical slock
	integer*2 ntrup
	integer intary(9)
c	data intary(1)/Z'200'/
	data ntrup/Z'16'/
	data nmasks/z'10'/		!masks all but 5th bit (1,2,...,5)
c
c	intary(1)=512	!2h in upper byte (AH) of AX (ie *256)
	intary(1)=4608	!12h in upper byte (AH) of AX (ie *256) for enhanced keyboard
	call intrup(intary,ntrup)
	nout=iand(nmasks,intary(1))
	SLOCK=nout.ne.0		!4th bit is set
	RETURN
	end

