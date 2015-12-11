c STANDARD PROGRAM COMPONENTS

	character cdate*11,ctime*11,filnam*32,ans*1,UC*1
	logical pon,slock,discprt
	character*40 mtitle	!machine name
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
	pon()=slock()
c
	OPEN(unit=7,file='PRN',iostat=nerr)
	filnam='ekdist.prt'
	call WINPRINT
c
	if(pon()) write(7,3000)
	if(discprt) write(8,3000)
3000	format(/,' EKDIST: Single channel distributions ')
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 3001,cdate,ctime(1:8),mtitle
	if(pon()) write(7,3001) cdate,ctime(1:8),mtitle
	if(discprt) write(8,3001) cdate,ctime(1:8),mtitle
3001	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     &'   Machine = ',a40)
	print 3002
3002	format(
     & ' SCROLL LOCK on for printing')
====
	call SAVEDP	!save disc printout
====
	call ENDPRINT
=====
