	subroutine SETPOT(v,comfac,ival)
c To set membrane potential via DAC3; v=potential in mV; comfac=amplification
c factor needed to use whole range of DAC output (eg 10. or 20.)
c Returns potential in DAC units, in ival.
c Assumes 1401 already opened. DAC command is permanently loaded.

 	integer*2 ierr
	character*60 fmt
	character*15 str

c
c Send command to DAC3
	v1=comfac*v
	ival=ifixr(v1*32768./5000.)
c	fix IVAL to nearest integer mult of 16
	ival=16*ifixr(float(ival)/16.0)
	if(ival.gt.32752) ival=32752
	if(ival.lt.-32768) ival=-32768
	fmt='(a6,i6,a3)'
	write(str,fmt) 'DAC,3,',ival,',2;'
	CALL FORSENDSTRING(str,IERR)

	call FLUSH(10)
	RETURN
	end


