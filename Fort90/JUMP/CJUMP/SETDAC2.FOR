	subroutine SETDAC2(v)
c To set potential on DAC2; v=potential in mV;
c Assumes 1401 already opened. DAC command is permanently loaded.
c	integer*2 i2
c
c Send command to DAC2
	character*30 fmt,str
	ival=ifixr(v*32768./5000.)
	if(ival.gt.32752) ival=32752
	if(ival.lt.-32768) ival=-32768
c	i2=int2(ival)
c	write(10,1000) i2
c	write(10,1000) ival
c1000	FORMAT ('DAC,2,',i6)
	fmt='(a6,i6,a1)'
	write(str,fmt) 'DAC,2,',ival,';'
c	CALL FORSENDSTRING(str,15,IERR)
	CALL FORSENDSTRING(str,IERR)
c	print*,'ierr SETDAC2 =',ierr
	call FLUSH(10)
	RETURN
	end


