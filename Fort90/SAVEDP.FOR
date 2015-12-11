	subroutine SAVEDP
c Subroutine to close the disc print file, and then re-open it (append mode)
c If called periodically this should ensure that not all of the file is
c lost if program crashes while file is open
	character filnam*32
	logical discprt
c	logical open
	common/dp/discprt
	character mtitle*40				!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c	INQUIRE(unit=8,opened=OPEN)
	if(discprt) then
	   CLOSE(unit=8)
         OPEN(unit=8,file=FILNAM,status='UNKNOWN',
     &   access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	endif
	RETURN
	end



