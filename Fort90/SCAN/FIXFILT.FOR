	program FIXFILT
c To insert value of filtf (-3dB) into filter file after conversion from PDP
c format with CONVFILT (this is done by CONVFALL already)
	real*4 filt(100)
	character ftitle*80,fname*7,filnam*12
c
2	call TITENT('Filter file name (exc .DAT):',fname,7)
	n=nblank(fname)
	filnam=fname(1:n)//'.dat'
	print 1
1	format(' Filter freq (-3dB) kHz = ')
	call INPUTr(filtf)
c
c	OPEN(UNIT=11,file='fda312.dat',status='old',
	OPEN(UNIT=11,file=filnam,status='old',
     & ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
	read(11,rec=1) ftitle,nfilt,dtf,filt
	write(11,rec=1) ftitle,nfilt,dtf,filt,filtf
	CLOSE(unit=11)
	goto 2
	end

