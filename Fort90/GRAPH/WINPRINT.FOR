	subroutine WINPRINT
c To control print file usage (for QGEN1, and...?)
c
c Modified 08/02/94 03:10pm to add option to window to 'setup this computer'
c  and save only the name of this machine in .INI (list of names in mtitles()
c  is now redundant, as is machine number ('machine'))
c Modified 07/10/92 09:22am so .INI always on C:\, and includes title for
c each machine (included in common)
c Modified 06/04/92 10:37am so looks on C:\ for .INI file if it is not present
c in D:\
c Modified 04/23/92 10:36am so keeps ndev=disc for printout, and icol=0 for
c  monochrome, icol=1 for colour also.
c WINPRINT is version of WINPRT with no args -filnam is input from main prog,
c and value of MACHINE is returned, via common/dpp/filnam,machine, as used
c in ENDPRINT too.
c Returns 'discprt' via common
c Modified 02/01/91 08:20am so that also reads computer type from the disc
c and modifies filnam to use appropriate disc. The print file will be
c opened here if needed (no need to put in main prog now, though if this
c done it will be closed again here, for compatibility with older progs)
c
c Modified 01/24/91 04:54pm so that if APPEND is requested then DISCPRT set true
c (see also WNEXT.FOR- this is version for progs that do not use graphics)
c
	integer scrnbuf1(1024),scrnbuf2(1024)
	character*30 text(4)
	character command*50,ndev*2,filnam*32
      character*1 ans,UC
	logical discprt,apfile,open,present
	logical debug,caplock,newvals,setup
c=	character*40 mtitles(20),mtitle		!machine names
	character*40 mtitle				!machine name
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
      INCLUDE '\f77l3\lib\SPINATT.HDR		!modif version of SPINATTR.HDR
c
	debug()=caplock()
	newvals=caplock()
c
	setup=.false.
101   format(a1)
	INQUIRE(unit=8,opened=OPEN)
	if(open) CLOSE(unit=8)		!for older progs
c Set default values (initially displayed)
	INQUIRE(file='C:\WINPRINT.INI',exist=present,flen=len)
	if(present.and.len.gt.0) then
         OPEN(unit=14,file='C:\WINPRINT.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
c=	   read(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
c=     &	nmac,mtitles
	   read(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitle				!should read mtitles(1)?
c=	   mtitle=mtitles(machine)		!name of this machine
	else		!make it!
c=	   mtitles(1)='DC Dell 425E'
c=	   mtitles(2)='DC Dell 433P'
c=	   mtitles(3)='DC Dell 450DE'
c=	   mtitles(4)='DC Dell 325P'
c=	   mtitles(5)='DC Toshiba 5200'
c=	   nmac=5
         OPEN(unit=14,file='C:\WINPRINT.INI',status='NEW',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	endif
c
	if(newvals) then
	   print 12
12	   format(
     & ' You have started the program with CAPS LOCK on',/,
     & '  -do you really want to change machine specification [N] ? ')
	   read 101,ans
	   if(UC(ans).ne.'Y') goto 13
	endif
c
17	continue	!return here if 'setup' requested in window
	if(newvals) then
c=	   do 15 i=1,nmac
c=	    print 16,i,mtitles(i)
c=16	    format(' (',i3,') ',a20)
c=15	   continue
c=	   newm=nmac+1	!next machine number
c=	   print 1,newm
c=1	   format(
c=     & ' Machine number [0=add machine #',i3,' to list] = ')
c=	   read 2,machine
c=2	   format(i8)
c=	   if(machine.le.0) then
	   nlen=40
	   call TITENT0(
     &	'Name of machine (appears on print-out):',mtitle,
     &		nlen,.false.)
c=	   mtitles(newm)=mtitle
c=	   machine=newm
c=	   else
c=		mtitle=mtitles(machine)
c=	   endif
	   ndisc=26	!always default for networked machines
	   print 9,ndisc
9	   format(
     & ' Number of hard disc partitions on this machine [',i2,'] = ')
	   call INPUTi(ndisc)
c=	   read 2,i
c=	   if(i.gt.0) ndisc=i
	   ic=ichar(ndev(1:1))
c   check if ndev=a-z or A-Z
	   if((ic.ge.65.and.ic.le.90).or.(ic.ge.97.and.ic.le.122)) then
	      print 10,ndev
10	      format('&Printout to go on disc partition ',a2,
     &	' O.K. [Y] ? ')
	      read 101,ans
	      if(UC(ans).eq.'N') then
		   call DISCNUM1(id,ndev,0,0)
	      endif
	   else
		if(ndisc.eq.10) ndev='H:'	!default
		if(ndisc.eq.6) ndev='F:'	!default
		if(ndisc.eq.4) ndev='E:'	!default
		if(ndisc.eq.3) ndev='E:'	!default
		if(ndisc.eq.1) ndev='C:'	!default
	      print 110
110	      format('&Specify disc partition for printout:')
		call DISCNUM1(id,ndev,0,1)
	   endif
	   icol=1
	   print 11
11	   format(' Colour screen [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') icol=0
	   write(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitle       !machine,nmac now superfluous
	endif
13	continue
c If 'filnam' includes a disc name then replace it with the appropriate
c disc for machine, otherwise append the disc name to FILNAM (more recent
c progs to not have disc specified in FILNAM)
	if(filnam(2:3).eq.':\') then
	   filnam(1:2)=ndev
	else if (filnam(1:1).eq.'\') then
	   do i=30,1,-1
		j=i+2
		filnam(j:j)=filnam(i:i)
	   enddo
	   filnam(1:2)=ndev
	else
	   do i=29,1,-1
		j=i+3
		filnam(j:j)=filnam(i:i)
	   enddo
	   filnam(1:3)=ndev//'\'
	endif
c
	if(setup) then
	   call DISPSCN(scrnbuf2)		!redisplay screen
	   setup=.false.
	   goto 4	!get another character
	endif
c
c Display the window
	call CAPTSCN(scrnbuf1)			!keep input screen
	call CLS
c NB hitting F2 moves cursor down one line for some reason, so keep
c cursor at top (if at bottom it will make window scroll up the screen)
	call LOCATE(0,0)
	ir1=2
	ic1=10
	ir2=11
	ic2=46
	if(icol.eq.0) then
	 call DEFWIND(2,ir1,ic1,ir2,ic2,singlbd+wrap+scrol,bright+blkbkg)
	else
	 call DEFWIND(2,ir1,ic1,ir2,ic2,singlbd+wrap+scrol,bright+dblubkg)
	endif
	if(discprt) then
	   text(1)='1.Print-out to disc file  [Y]'
	else
	   text(1)='1.Print-out to disc file  [N]'
	endif
	if(apfile) then
	   text(2)='2.Append to present file  [Y]'
	else
	   text(2)='2.Append to present file  [N]'
	endif
	text(3)='3.View current print file '
	text(4)='4.Setup for this computer '
	call openwind(2)
	call clrb(2)
	call BLDB(2,0,0,text(1))
	call BLDB(2,1,0,text(2))
	call BLDB(2,2,0,text(3))
	call BLDB(2,3,0,text(4))
	if(icol.eq.0) then
	   call BLDB(2,4,0,'Type option number (1,2,3,4,enter):',
     &   inverse+brite+flash+blkchr)
c     &   dblubkg+yellochr+brite+flash)
	else
	   call BLDB(2,4,0,'Type option number (1,2,3,4,enter):',
     &   inverse+brite+flash+redchr)
	endif
	call BLDB(2,6,0,'[ENTER/ESC=exit when settings OK]')
	call BLDB(2,7,0,'[F1=help; F2=save settings]')
c
4	i=nkey0()
	call BLDB(2,7,0,'[F1=help; F2=save settings]')
	if(i.eq.16) goto 3	!enter
	if(i.eq.-16) goto 3	!ESC
c
	if(i.eq.-1) then
	   call CAPTSCN(scrnbuf2)	!save window while doing setup
	   ihatt=whitechr+redbkg		!for help menu
	   call DEFWIND(3,6,30,21,72,dblbdr+wrap+scrol,ihatt)
	   call OPENWIND(3)
	   call CLRB(3)
	   n=NBLANK(filnam)
	   call BLDB(3,0,0,
     &	'(1) Keep the print-out on the disk (in   ',ihatt)
	   call BLDB(3,1,0,
     & 	'  file '//filnam(1:n)//').',ihatt)
	   call BLDB(3,2,0,
     &	'(2) Y means that output is appended to   ',ihatt)
	   call BLDB(3,3,0,
     & 	'  existing '//filnam(1:n)//'.',ihatt)
	   call BLDB(3,4,0,
     &	'    N means that existing file is lost   ',ihatt)
	   call BLDB(3,5,0,
     & 	'  and a new one is started.',ihatt)
	   call BLDB(3,6,0,
     &	'(3) View the existing printout file:      ',ihatt)
	   call BLDB(3,7,0,
     & 	'  '//filnam(1:n),ihatt)
	   call BLDB(3,8,0,
     &	'(4) Change the setup for this machine    ',ihatt)
	   call BLDB(3,9,0,
     & 	'   (e.g. location for print file etc).',ihatt)
	   call BLDB(3,10,0,
     & 	'F2: stores the answers to (1) and (2) ',ihatt)
	   call BLDB(3,11,0,
     & 	'  so they appear as defaults next time.',ihatt)
	   call BLDB(3,13,0,'Hit ANY KEY to leave HELP',ihatt)
	   call ANYKEY
	   call DISPSCN(scrnbuf2)
	   goto 4
	endif
c
	if(i.eq.-2) then		!F2 key
	   write(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitle
	   call BELL(1)
	   call BLDB(2,7,0,' SETTINGS SAVED               '//char(0))
c NB need null character at end or trailing blanks not printed
	   call LOCATE(0,0)
	   goto 4
	endif
	if(i.lt.1.or.i.gt.4) goto 4
c
	if(i.eq.4) then
	   call CAPTSCN(scrnbuf2)	!save windows while doing setup
	   call CLS
	   newvals=.true.
	   setup=.true.		!setup requested from here (so screen restored)
	   goto 17
	endif
c
	if(i.ne.3) then
		goto 6
	else
		call CAPTSCN(scrnbuf2)	!save windows while listing file
		call CLS
c		CLOSE(unit=8)
		command='Type '//filnam//'|more'
c		call SYSTEM('Type c:\fortran\scan\EKDIST.PRT|more')
		call SYSTEM(command)
	      OPEN(unit=8,file=FILNAM,status='UNKNOWN',
     &     ACCESS='APPEND',FORM='FORMATTED',CARRIAGE CONTROL='FORTRAN')
		pause
		call DISPSCN(scrnbuf2)
		goto 4
	endif
c Now bit for i=1,2
6	if(i.eq.1) then
	   discprt=.not.discprt
	   if(.not.discprt) apfile=.false.
	endif
	if(i.eq.2) then
	   apfile=.not.apfile
	   if(apfile) discprt=.true.	!cannot append if DISCPRT false!
	endif
c Re-draw Y/N for both questions 1 AND 2 each time
c	i=2		!i=2 first
	i=0		!i=0 first
	ir=ir1+i+1
	call CLS(ir,ic1+28,ir,ic1+28)		!delete 'N'
	call LOCATEW(2,int2(i),27)	!move cursor to 'N'
	if(discprt) call PRINTW(2,'Y')
	if(.not.discprt) call PRINTW(2,'N')
c	i=3
	i=1
	ir=ir1+i+1
	call CLS(ir,ic1+28,ir,ic1+28)		!delete 'N'
	call LOCATEW(2,int2(i),27)	!move cursor to 'N'
	if(apfile) call PRINTW(2,'Y')
	if(.not.apfile) call PRINTW(2,'N')
	goto 4	!any more
c  Finished with window 2
3	continue
c TO OVERWRITE, NOW REWIND FILE
      if(discprt) OPEN(unit=8,file=FILNAM,status='UNKNOWN',
     & access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	if(discprt.and.(.not.apfile)) REWIND(unit=8)
c
	call DISPSCN(scrnbuf1)		!restore screen
      close(unit=14)
	RETURN
	end

