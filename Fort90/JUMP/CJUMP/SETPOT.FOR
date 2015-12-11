	subroutine SETPOT(v,comfac,ival)
c To set membrane potential via DAC3; v=potential in mV; comfac=amplification
c factor needed to use whole range of DAC output (eg 10. or 20.)
c Returns potential in DAC units, in ival.
c Assumes 1401 already opened. DAC command is permanently loaded.
	integer*2 ival
	character*30 str,fmt
c
cc Open 1401
c	ierr = LabGo()
c	IF (ierr.NE.0) THEN
c	   PRINT *,'1401 open failed',ierr
c	   CALL EXIT(2)
c	ENDIF
c
c Send command to DAC3
	v1=comfac*v
	ival=int2(ifixr(v1*32768./5000.))
c	fix IVAL to nearest integer mult of 16
	ival=16*int2(ifixr(float(ival)/16.0))
	if(ival.gt.32752) ival=32752
	if(ival.lt.-32768) ival=-32768
c	write(10,1000) ival
c1000	FORMAT ('DAC,3,',i6)
	fmt='(a6,i6,a1)'
	write(str,fmt) 'DAC,3,',ival,';'
c	CALL FORSENDSTRING(str,15,IERR)
	CALL FORSENDSTRING(str,IERR)
	call FLUSH(10)
	RETURN
	end


