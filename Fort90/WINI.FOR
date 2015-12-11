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
	character command*50,ndev*2
      character*1 ans,UC
	logical discprt,apfile,open,present
	logical debug,caplock,newvals,setup
c=	character*40 mtitles(20),mtitle		!machine names
	character mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
c	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
      INCLUDE '\f77l3\lib\SPINATT.HDR		!modif version of SPINATTR.HDR
c
	debug()=caplock()
	newvals=caplock()
c
c Version number for DCPROGS.INI
	iver=1
c
	setup=.false.
101   format(a1)
	INQUIRE(unit=8,opened=OPEN)
	if(open) CLOSE(unit=8)		!for older progs
c Set default values (initially displayed)
c Check if old .INI is still there; if so read it and copy to the new one
	INQUIRE(file='C:\WINPRINT.INI',exist=present,flen=len)
	if(present.and.len.gt.0) then
         OPEN(unit=13,file='C:\WINPRINT.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	   read(13,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitle			!should read mtitles(1)?
	   INQUIRE(file='C:\DCPROGS.INI',exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
	      prtport='LPT1'		!default
		OPEN(unit=14,file='C:\DCPROGS.INI',status='NEW',
     &   	access='DIRECT',form='UNFORMATTED',recl=1024)
	      write(14,rec=1) iver1,discprt,apfile,ndev,ndisc,icol,
     &	mtitle,prtport
		CLOSE(unit=13)
		CLOSE(unit=14)
		call BELL(3)
		print 14
14		format(
     & ' NEW INITIALISATION FILE, DCPROGS.INI, CREATED.',/,
     & ' NOW DELETE OLD FILE, C:\WINPRINT.INI, AND RUN',/,
     & '  THE PROGRAM AGAIN.')
		PAUSE
		STOP
	   endif
	endif
c
	INQUIRE(file='C:\DCPROGS.INI',exist=present,flen=len)
	if(present.and.len.gt.0) then
	   OPEN(unit=14,file='C:\DCPROGS.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
c=	   read(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
c=     &	nmac,mtitles
	   read(14,rec=1) iver1,discprt,apfile,ndev,ndisc,icol,
     &	mtitle,prtport			!should read mtitles(1)?
c	   if(iver1.lt.iver) then		!write newer version
c		write(14,rec=1) iver,discprt,apfile,ndev,ndisc,icol,
c     &	mtitle,prtport			!should read mtitles(1)?
c	   endif
c=	   mtitle=mtitles(machine)		!name of this machine
	else		!make it!
         OPEN(unit=14,file='C:\DCPROGS.INI',status='NEW',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	endif
c
	goto 1000

1000	end

