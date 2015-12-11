	subroutine GETEXPT(expdate,defname,tapeID,ipatch,npatch,Emem,temp)
	character expdate*11,defname*6,tapeID*24
	character cnum1*11,cnum2*11,cnum3*11
c
331	print 33
33	format(' Give the date on which the experiment was done:',/,
     &'  Date (1-31) = ')
	call INPUTi(idat)
	if(idat.lt.1.or.idat.gt.31) goto 331
341	print 34
34	format('  Month (1-12) = ')
	call INPUTi(imon)
	if(imon.lt.1.or.imon.gt.12) goto 341
351	print 35
35	format('  Year (1999, 2000, 2001 etc) = ')
	call INPUTi(iyr)
	if(iyr.lt.1990.or.imon.gt.3000) goto 351
	call INTCONV(idat,cnum1)
	if(idat.le.9) then
	   cnum1(2:2)=cnum1(1:1)
	   cnum1(1:1)='0'
	endif
	call INTCONV(imon,cnum2)
	if(imon.le.9) then
	   cnum2(2:2)=cnum2(1:1)
	   cnum2(1:1)='0'
	endif
	call INTCONV(iyr,cnum3)
	defname(1:6)=cnum3(3:4)//cnum2(1:2)//cnum1(1:2)	!for default file name
c Put esperiment date in nicer format into expdate*11
	expdate(1:2)=cnum1(1:2)
	expdate(3:3)='-'
	if(cnum2(1:2).eq.'01') expdate(4:6)='Jan'
	if(cnum2(1:2).eq.'02') expdate(4:6)='Feb'
	if(cnum2(1:2).eq.'03') expdate(4:6)='Mar'
	if(cnum2(1:2).eq.'04') expdate(4:6)='Apr'
	if(cnum2(1:2).eq.'05') expdate(4:6)='May'
	if(cnum2(1:2).eq.'06') expdate(4:6)='Jun'
	if(cnum2(1:2).eq.'07') expdate(4:6)='Jul'
	if(cnum2(1:2).eq.'08') expdate(4:6)='Aug'
	if(cnum2(1:2).eq.'09') expdate(4:6)='Sep'
	if(cnum2(1:2).eq.'10') expdate(4:6)='Oct'
	if(cnum2(1:2).eq.'11') expdate(4:6)='Nov'
	if(cnum2(1:2).eq.'12') expdate(4:6)='Dec'
	expdate(7:7)='-'
	expdate(8:11)=cnum3(1:4)
c
	print 36,expdate
36	format(' For experiment on ',a11,' this is patch number [1] = ') 
	npatch=1
	call INPUTi(npatch)
	call TITENT0('Enter tape details: ',tapeID,24,.true.)
111	print 311
311	format(
     & ' Enter patch type:',/,
     & ' (1) outside-out',/,
     & ' (2) inside-out',/,
     & ' (3) cell-attached',/,
     & ' (4) whole-cell',/,
     & ' (5) simulated data',/,
     & '   Option number = ')
	call INPUTi(ipatch)
	if(ipatch.lt.1.or.ipatch.gt.5) goto 111
c
	print 13
13	format(' Membrane potential (mV) = ')
	call INPUTr(Emem)
c
	print 121
121	format(' Temperature = ')
	call INPUTr(temp)
c
c
	RETURN
	end


