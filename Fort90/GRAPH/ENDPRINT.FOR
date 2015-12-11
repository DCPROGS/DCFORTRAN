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
	character filnam*32,command*50,cd*1
	logical discprt
	logical open,pon,slock
	common/dp/discprt
	character*40 mtitle	!machine name
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
	pon()=slock()
c
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
c
c Seems that disc print file was used: check disc (should be first character
c of FILNAM
	cd=filnam(1:1)
	id=ichar(cd)
	if((id.ge.67.and.id.le.77).or.(id.ge.99.and.id.le.109)) then
	   iopt=1		!default
3	   print 4,iopt
4	   format(/,
     & ' (1) Print out the ''disc print-out file'' now',/,
     & ' (2) View the ''disc print-out file'' on screen now',/,
     & ' (3) Exit now and print later',/,
     & ' Option number [',i2,'] = ')
	   read 2,i1
2	   format(i8)
	   if(i1.lt.0.or.i1.gt.3) goto 3
	   if(i1.ne.0) iopt=i1
	   if(iopt.eq.3) goto 999
	   if(iopt.eq.1) then
	      OPEN(unit=7,file='PRN')		!open printer to be sure
		command='PRINT '//filnam
	   else if(iopt.eq.2) then
		command='Type '//filnam//'|more'
	   endif
	   call SYSTEM(command)
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
c	CLOSE(unit=7)
	CLOSE(unit=8)
c
	RETURN
	end



