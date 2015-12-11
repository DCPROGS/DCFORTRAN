	program MAKECSAM
c To take any binary file that comtains integer*2 numbers and write it
c in consam format
c
	character*40 datfil,consfil
	character*11 cdate,ctime
	logical discprt,debug,caplock
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM

	debug()=caplock()
c
c
	filnam='MAKECSAM.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(
     &' MAKECSAM: Converts a 16-bit integer binary data file to ',/,
     &' CONSAM.SSD format, by asking for data for header',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
c
	call WRITCSAM(datfil,consfil,cdate,ctime)
c
	end

