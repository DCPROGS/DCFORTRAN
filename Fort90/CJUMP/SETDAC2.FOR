	subroutine SETDAC2(v)
c To set potential on DAC2; v=potential in mV;
c Assumes 1401 already opened. DAC command is permanently loaded.
c Send command to DAC2

 	integer*2 ierr
	character*60 fmt
	character*15 str

	ival=ifixr(v*32768./5000.)
	if(ival.gt.32752) ival=32752
	if(ival.lt.-32768) ival=-32768
	fmt='(a6,i6,a3)'
	write(str,fmt) 'DAC,2,',ival,',2;'
	CALL FORSENDSTRING(str,IERR)
	call FLUSH(10)
	RETURN
	end


