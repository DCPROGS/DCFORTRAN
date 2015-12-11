	subroutine DISCNUM1(idisc,ndev,iask,idef)
c
c Call DISCNUM to ask for disc letter and define NDEV on any machine
c
c 05/26/89 08:42pm Lahey version of DISCN1
c 04/23/92 03:34pm machine=0 for 'other' machines in latest WINPRINT and
c ndisc now stored -need ONLY ndisc here so code for machine=1,2,3 now removed
c NB ndisc is number of HARD disc partitions.
c Typing ESC returns IDISC=-1
c If IDEF > 0 it is specified as the default device
c INPUT
c (1) No default device specified (IDEF=0)
c (2) Default device name specified in NDEV input (IDEF=1)
c   (in this case must not overwrite default by value from disc)
c (3) Default device name specified in IDISC input (IDEF=2)
c IASK=0 asks for hard disk # only
c IASK=-1 asks for hard disk # or floppy #
c IASK=-2 asks for floppy # only
c iask=-3,-4,-5 same except carriage return suppressed before question
c printed (eg for when on specified line in graphics)
c
c OUTPUT
c 06/14/90 03:16pm Altered so:
c IDISC=1,2,3,4,5,6,7,8,9,10,11,12  for A:,B:,C:,D:,E:,F:,G:,H:,I:,J:,K:,L:
c Modified 04/06/94 01:32pm so idisk can go up to 24 for networked machines
c IDISC=-1 if ESC typed (and NDEV undefined)
c NDEV='C:' etc
c If ndisc=1 then returns ndev='C:', idisc=3 without asking if iask=0 (hard
c disk only): otherwise gives choice of A:, B:, C:
	character ndev*2
c	character ndev1*2
	character ch,getch,UC,ch1,lastdev
	integer*2 ktype
c	logical discprt,apfile
	character*32 filnam	!for COMMON only
	character*40 mtitle	!machine name
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c Read WINPRINT.INI again? Not really necessary as will normally have ndisc
c already from common/dpp/, from earlier call WINPRINT
c	if(machine.lt.0.or.machine.gt.3) then
c         OPEN(unit=14,file='D:\WINPRT.INI',status='UNKNOWN',
c     &   access='DIRECT',form='UNFORMATTED',recl=32)
c	   read(14,rec=1) discprt,apfile,machine,ndev1,ndisc,icol
c	   CLOSE(unit=14)
c	endif
c
c	if(ndisc.le.0.or.ndisc.gt.10) ndisc=10   !in case .INI not defined yet
	if(ndisc.le.0.or.ndisc.gt.24) ndisc=24   !in case .INI not defined yet
c
	if(iask.eq.0.and.ndisc.eq.1) then	!no choice!
	   ndev='C:'
	   idisc=3
	   RETURN
	endif
c
	ch1=' '
	if(iask.lt.-2) then
	   iask=iask+3		!=0,-1,-2
	   ch1='&'
	endif
	if(idef.eq.2) call CDISC1(ndev,idisc,1) !define ndev from idisc (see below)
	lastdev=char(66+ndisc)	!=D-Z for ndisc=2,...,24
c
1	continue
	if(idef.gt.0) goto 162
	if(iask.eq.0) then
	   print 132,ch1,lastdev
132	   format(a1,
     &   'Disc (type a character, C to ',a1,': ESC to abort) = ')
	else if(iask.eq.-1) then
	   print 133,ch1,lastdev
133	   format(a1,
     &   'Disc (type a character, A to ',a1,': ESC to abort) = ')
	else if(iask.eq.-2) then
	   print 151,ch1
151	   format(a1,
     &   'Floppy disc (type a character, A or B: ESC to abort) = ')
	endif
c No default specified
2	format(i8)
	ch=GETCH(ktype)	!get waiting character and carry on
	if(ktype.eq.3) then		!input was a number
	   idisc=ichar(ch)-48		! -the number
	   call CDISC1(ndev,idisc,1)	!define ndev from idisc (see below)
	   print 25,ndev
25	   format('&',a2)
	   call ANYKEY		!so needs carriage return
	else if(ktype.eq.2) then	!input was letter
	   ndev=UC(ch)//':'		!convert to upper case
	   print 25,ndev
	   call ANYKEY		!so needs carriage return
	   call CDISC1(ndev,idisc,0)	!define idisc from ndev (see below)
	else if(ktype.eq.16.and.ichar(ch).eq.27) then	!ESC typed
	   idisc=-1
	   goto 99
	else
	   goto 1		!any other key (inc 'enter', as their is no default)
	endif
	if(idisc.lt.1.or.idisc.gt.ndisc+2) goto 1
	if(iask.eq.-2.and.(.NOT.(idisc.eq.1.or.idisc.eq.2))) goto 1
	goto 99	!RETURN
c
c Default specified
162	continue
	if(iask.eq.0) then
	   print 134,ch1,lastdev,ndev
134	   format(a1,
     & 'Disc (type a character, C to ',a1,': ESC to abort) [',a2,'] = ')
	else if(iask.eq.-1) then
	   print 135,ch1,lastdev,ndev
135	   format(a1,
     & 'Disc (type a character, A to ',a1,': ESC to abort) [',a2,'] = ')
	else if(iask.eq.-2) then
	   print 136,ch1,ndev
136	   format(a1,
     &'Floppy (type a character, A or B: ESC to abort) [',a2,'] = ')
	endif
	ch=GETCH(ktype)	!get waiting character and carry on
	if(ktype.eq.3) then		!input was a number
	   idisc=ichar(ch)-48		! -the number
	   call CDISC1(ndev,idisc,1)	!define ndev from idisc (see below)
	   print 25,ndev
	   call ANYKEY		!so needs carriage return
	else if(ktype.eq.2) then	!input was letter
	   ndev=UC(ch)//':'		!convert to upper case
	   call CDISC1(ndev,idisc,0)	!define idisc (see below)
	   print 25,ndev
	   call ANYKEY		!so needs carriage return
	else if(ktype.eq.4.and.ichar(ch).eq.13) then  !'enter' typed for default
	   print 25,ndev
	   call CDISC1(ndev,idisc,0)	!define idisc from default ndev
	else if(ktype.eq.16.and.ichar(ch).eq.27) then	!ESC typed
	   idisc=-1
	   goto 99
	else
	   goto 1
	endif
	if(idisc.lt.1.or.idisc.gt.ndisc+2) goto 1
	if(iask.eq.-2.and.(.NOT.(idisc.eq.1.or.idisc.eq.2))) goto 1
c
99	continue
	RETURN
	end


	subroutine CDISC1(ndev,idisc,imode)
c imode=0  -defines idisc from ndev
c imode=1  -defines ndev from idisc
	character*2 ndev
c
	if(imode.eq.0) then
	   idisc=0		!if no valid NDEV
	   if(NDEV.eq.'A:') idisc=1
	   if(NDEV.eq.'B:') idisc=2
	   if(NDEV.eq.'C:') idisc=3
	   if(NDEV.eq.'D:') idisc=4
	   if(NDEV.eq.'E:') idisc=5
	   if(NDEV.eq.'F:') idisc=6
	   if(NDEV.eq.'G:') idisc=7
	   if(NDEV.eq.'H:') idisc=8
	   if(NDEV.eq.'I:') idisc=9
	   if(NDEV.eq.'J:') idisc=10
	   if(NDEV.eq.'K:') idisc=11
	   if(NDEV.eq.'L:') idisc=12
	   if(NDEV.eq.'M:') idisc=13
	   if(NDEV.eq.'N:') idisc=14
	   if(NDEV.eq.'O:') idisc=15
	   if(NDEV.eq.'P:') idisc=16
	   if(NDEV.eq.'Q:') idisc=17
	   if(NDEV.eq.'R:') idisc=18
	   if(NDEV.eq.'S:') idisc=19
	   if(NDEV.eq.'T:') idisc=20
	   if(NDEV.eq.'U:') idisc=21
	   if(NDEV.eq.'V:') idisc=22
	   if(NDEV.eq.'W:') idisc=23
	   if(NDEV.eq.'X:') idisc=24
	   if(NDEV.eq.'Y:') idisc=25
	   if(NDEV.eq.'Z:') idisc=26
	   RETURN
	endif
	if(imode.eq.1) then
	   if(idisc.lt.1.or.idisc.gt.26) then
		call BELL(1)
		idisc=-1
		RETURN
	   endif
	   if(idisc.eq.1) ndev='A:'
	   if(idisc.eq.2) ndev='B:'
	   if(idisc.eq.3) ndev='C:'
	   if(idisc.eq.4) ndev='D:'
	   if(idisc.eq.5) ndev='E:'
	   if(idisc.eq.6) ndev='F:'
	   if(idisc.eq.7) ndev='G:'
	   if(idisc.eq.8) ndev='H:'
	   if(idisc.eq.9) ndev='I:'
	   if(idisc.eq.10) ndev='J:'
	   if(idisc.eq.11) ndev='K:'
	   if(idisc.eq.12) ndev='L:'
	   if(idisc.eq.13) ndev='M:'
	   if(idisc.eq.14) ndev='N:'
	   if(idisc.eq.15) ndev='O:'
	   if(idisc.eq.16) ndev='P:'
	   if(idisc.eq.17) ndev='Q:'
	   if(idisc.eq.18) ndev='R:'
	   if(idisc.eq.19) ndev='S:'
	   if(idisc.eq.20) ndev='T:'
	   if(idisc.eq.21) ndev='U:'
	   if(idisc.eq.22) ndev='V:'
	   if(idisc.eq.23) ndev='W:'
	   if(idisc.eq.24) ndev='X:'
	   if(idisc.eq.25) ndev='W:'
	   if(idisc.eq.26) ndev='Z:'
	   RETURN
	endif
	idisc=-1    !if imode not 0 or 1!!
	call BELL(4)
	RETURN
	end

