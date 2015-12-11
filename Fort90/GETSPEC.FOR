	subroutine GETSPEC(filespec)
c To get data about the file specified in filespec, and print it
	integer iattr(4)
	character*30 filespec
	character*100 filfound
	character*11 cdate
	character*8 ctime
	character*11 cnum
	logical discprt
	common/dp/discprt
c
	call FINDFRST(filespec,filfound,iattr)	!spin90 routine
	ierr=ISERROR()
	if(ierr.ne.0) then
	   print 7, ierr
7	   format(' error ',i5,' in GETSPEC')
	   RETURN
	endif
c time
	call MVBITS(iattr(2),0,5,isec,0)		!F90 routine
	isec=2*isec	    	!2 sec increments
	call MVBITS(iattr(2),5,6,imin,0)
	call MVBITS(iattr(2),11,5,ihour,0)
	call INTCONV(ihour,cnum)
	ctime(1:2)=cnum(1:2)
	if(nblank(cnum).eq.1) then
	   ctime(1:2)='0'//cnum(1:1)
	endif
	ctime(3:3)=':'
	call INTCONV(imin,cnum)
	ctime(4:5)=cnum(1:2)
	if(nblank(cnum).eq.1) then
	   ctime(4:5)='0'//cnum(1:1)
	endif
	ctime(6:6)=':'
	call INTCONV(isec,cnum)
	ctime(7:8)=cnum(1:2)
	if(nblank(cnum).eq.1) then
	   ctime(7:8)='0'//cnum(1:1)
	endif
c
c date
	call MVBITS(iattr(3),0,5,iday,0)		!F90 routine
	call MVBITS(iattr(3),5,4,imonth,0)
	call MVBITS(iattr(3),9,5,iyear,0)
	iyear=iyear+1980
	call INTCONV(iday,cnum)
	cdate(1:2)=cnum(1:2)
	cdate(3:3)='-'
	cdate(7:7)='-'
	if(imonth.eq.1) cdate(4:6)='Jan'
	if(imonth.eq.2) cdate(4:6)='Feb'
	if(imonth.eq.3) cdate(4:6)='Mar'
	if(imonth.eq.4) cdate(4:6)='Apr'
	if(imonth.eq.5) cdate(4:6)='May'
	if(imonth.eq.6) cdate(4:6)='Jun'
	if(imonth.eq.7) cdate(4:6)='Jul'
	if(imonth.eq.8) cdate(4:6)='Aug'
	if(imonth.eq.9) cdate(4:6)='Sep'
	if(imonth.eq.10) cdate(4:6)='Oct'
	if(imonth.eq.11) cdate(4:6)='Nov'
	if(imonth.eq.12) cdate(4:6)='Dec'
	call INTCONV(iyear,cnum)
	cdate(8:11)=cnum
c
	print 1, filfound,cdate,ctime,iattr(4)
	if(discprt) write(8,1) filfound,cdate,ctime,iattr(4)
1	format(
     & ' Executable file: ',a60,/,
     & ' Last modified on ',a11,' at ',a8,', length (bytes) = ',i9)
c
	RETURN
	end

