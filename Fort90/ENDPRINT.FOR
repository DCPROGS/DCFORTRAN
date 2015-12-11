	subroutine ENDPRINT()
c To finish print and eject last page on Deskjet and Laserjet
c 01/03/92 02:55pm Form feed not needed on Laserjet (this version)
c Also asks if disc print file to be viewed or printed IF unit=8
c was open and the calling program had COMMON/dpp/filnam, in which
c FILNAM (which should be a character*30 array) has a first character
c which is a valid disc (C to M (or c to m); but standard disc for print
c out is H: at present).  Done this way so that ENDPRT can also be called by
c progs that do not use disc print file (so do not want FILNAM as a
c parameter in call)
c   iwindows=1 for DOS (not from Windows)',/,
c   iwindows=2  Windows 3.1/3.11',/,
c   iwindows=3) Windows 95',/,
c   iwindows=4) Windows NT',/,
	character filnam*32,command*50,cd*1
	logical discprt
	logical open,pon,slock
	common/dp/discprt
	character mtitle*40,prtport*4
      character*1 ans,UC
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c	common/user/student,cluster,win31
	logical cluster,student
	common/user/student,cluster,iwindows
c
	pon()=slock()
c
101   format(a1)
	INQUIRE(unit=7,opened=OPEN)
c
	if(open.and.pon()) then
	   call FLUSH(7)
	   write(7,1)
1	   format('1')	!form feed
	   CLOSE(unit=7)
	endif
c
	INQUIRE(unit=8,opened=OPEN)
	if(.not.open) discprt=.false.
	if(open) then
c	   write(8,1)
	   CLOSE(unit=8)
	endif
c
c NB if ENDPRINT is called by a prog that does not use a disc print
c file then the COMMONS /dp/ and /dpp/ will not be present in main
c prog, so discprt and filnam will be undefined. Therefore first test
c if unit is open, and if not set DISCPRT=false
	if(.not.discprt) goto 999		!no disc file made
	if(student) goto 999
c
c Seems that disc print file was used: check disc (should be first character
c of FILNAM
	cd=filnam(1:1)
	id=ichar(cd)
	if((id.ge.67.and.id.le.77).or.(id.ge.99.and.id.le.109)) then
	   iopt=3		!default
3	   print 4,iopt
4	   format(/,
     & ' (1) Print out the ''disc print-out file'' now',/,
     & ' (2) View the ''disc print-out file'' on screen now',/,
     & ' (3) Exit now and print later',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.0.or.iopt.gt.3) goto 3
	   if(iopt.eq.3) goto 999
	   if(iopt.eq.1) then
c	      if(win31) then
	      if(iwindows.eq.2) then
	         OPEN(unit=7,file='PRN')		!open printer to be sure
		   command='PRINT '//filnam
	         call SYSTEM(command)
		else
c	   	   call COPY(filnam,'lpt1')
	   	   call SYSTEM('copy/b '//charnb(filnam)//' lpt1')
		endif
	   else if(iopt.eq.2) then
		command='Type '//filnam//'|more'
	      call SYSTEM(command)
	   endif
	   if(iopt.lt.0.or.iopt.gt.3) goto 3
	   if(iopt.eq.3) goto 999
	   if(iopt.eq.2) then
	      iopt=1
	      goto 3	!another chance to print, after viewing
	   endif
	else
	   print 20,filnam
20	   format(' Disc print file appears to be open but name ',a30,/,
     &   ' does not start with valid disc letter')
	endif
c
999	continue
	CLOSE(unit=8)
	RETURN
	end



