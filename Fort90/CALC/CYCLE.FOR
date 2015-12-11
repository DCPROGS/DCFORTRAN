	program CYCLE
c To enumerate routes round a simple cycle (MWC n=1) by simulation
c
	real*8 QT(100,100),QD(100,100),qdum(100,100)
	real*8 Peq(100)	!use Peq as arg for EQOCC
	character*2 charmod(25,40)
	real*4 PI(100,100)
	integer SCRNBUF0(1024)
	integer IC(2,200)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100),IL(100)
	integer IQ(100,100)
	character*10 titlep(200)
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/dimblk/K,KMAX,ir,nmod
	COMMON/KBLK/kA,kB,kC,kD
c  For simulations
	logical KBHIT		!Utility lib
	logical repeat		!for randsk
	character*1 ch,getch
	real*4 pisim(100,100),tsim(100),picum(100,100)
	integer nsim(100)
	real*4 amean(100)
	real*4 pcum(101)		!for INSTAT
	integer ist(100)		!for INSTAT
	integer icur(100),ilast
	logical debon,via1,via3
c	allocatable::tint,iamp,istate
	allocatable::istate
c	real tint(:)
c	integer iamp(:),istate(:)
	integer istate(:)
	common/rand/ix1,iy1,iz1

c Routine stuff
	character*1 ans,UC
	logical discprt
	logical pon,slock,debug,caplock,mono
	character*11 cdate,ctime
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
C
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='CYCLE.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1     format(' CYCLE: Program to enumerate routes round a cycle.',/,
     &' MWC, n=1: R=4, AR=3, AR*=2, R*=2',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3       format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
101	format(a1)
c Specify rate constants (could also be read from QGEN?)
	k=4		!number of states
	kmax=10
	qt(4,3)=1.e8
	qt(4,1)=1.d0
	qt(3,2)=1000.d0
	qt(3,4)=100.d0
	qt(2,3)=100.d0
	qt(1,2)=1.e8
	qt(2,1)=0.1d0
	qt(1,4)=10.d0
c
	titlep(1)="alpha0    "
	titlep(2)="beta0     "
	titlep(3)="k(+1)*    "
	titlep(4)="k(-1)*    "
	titlep(5)="alpha1    "
	titlep(6)="beta1     "
	titlep(7)="k(-1)     "
	titlep(8)="k(+1)     "
c Amplitudes
	icur(1)=40
	icur(2)=50
	icur(3)=0
	icur(4)=0
c
c Define conc dep
	ncdep=2
	ix(1)=4
	ix(2)=1
	jx(1)=3
	jx(2)=2
	do i=1,10
	   il(i)=1
	enddo
c Define model (see modwind for character defs)
	ilast=3
	jlast=3
	charmod(1,1)="C4"
	charmod(2,1)=char(32)//char(179)      !vertical
	charmod(3,1)="C3"
	charmod(1,2)=char(196)//char(196)      !horizontal
	charmod(2,2)="  "
	charmod(3,2)=char(196)//char(196)      !horizontal
	charmod(1,3)="O1"
	charmod(2,3)=char(32)//char(179)      !vertical
	charmod(3,3)="O2"
c
c Call CHARQ to get connections (in IC(2,20)) and kA,kF
	call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
	kA=2
	kF=2
	kB=1
	kC=1
	kD=0
	do i=1,ilast
	   print 67,(charmod(i,j),j=1,jlast)
	   if(pon()) write(7,67) (charmod(i,j),j=1,jlast)
	   if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
67	   format(4x,10a2)
	enddo
c Define ncyc etc, as in QGEN, and display for modification
	ncyc=1
	nsc(1)=4
	im(1,1)=1
	im(1,2)=4
	im(1,3)=3
	im(1,4)=2
	jm(1,1)=4
	jm(1,2)=3
	jm(1,3)=2
	jm(1,4)=1
	call GETCYC(3)	!get micro rev rate only
c
c
c Now show model + QWIND
	ni=2*ncon	!number of rows
	npar=ni
	nidisp=ni
	nr1=1		!posn of QWIND on screen
	nc1=2*jlast+7
	iflag=3			!show all values in qwind
	call CAPTSCN(scrnbuf0)	!before putting up model
	call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)	!display model
	call QWIND2(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     & nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,itry)
	call DISPSCN(scrnbuf0)  !restore orig screen
c
	print 16
16	format( ' Concentration (micromolar)= ')
	call INPUTr(xA)
	if(pon()) write(7,163)1,xa
      if(discprt) write(8,163)1,xa
163	FORMAT(/,' Ligand #',i2,': concentration (micromolar)= ',g13.6)
	XA=XA*1.0E-6		!=MOLAR
c calc QD to get init occs
	v1=0.0
	call QZERO(QD,k)
	call QSETD(xA,xB,IL,V1,QT,QD,k,.false.)
c Print rates
	call QZERO(Qdum,k)
	call QSETD(1.0,1.0,IL,V1,QT,Qdum,k,.true.)
      if(pon()) write(7,131)
      if(discprt) write(8,131)
131	format(/,' Final values of rate constants')
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   r=qdum(i,j)
         if(pon()) write(7,12)m,i,j,titlep(m),r
         if(discprt) write(8,12)m,i,j,titlep(m),r
12	   format(i3,3x,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	enddo
c
	CALL EQOCCd(QD,K,K-1,KMAX,Peq)		!CALC EQUILIB OCCS
c
C CALC CURRENT FOR THIS (or these) conc (now all in SLIFED1)
	call SLIFEd1(QD,Peq,KMAX,KMAX)
c Print Q,PI
	DO 20 I=1,K
	DO 20 J=1,K
	   pi(i,j)=-1.		!in case denom=0
	   if(dabs(qd(i,i)).lt.1.d-25) goto 20
	   PI(I,J)=SNGL(QD(I,J)/(-QD(I,I)))
20	continue
	print 400
400	FORMAT( ' Type Q and PI matrices [Y]? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 45
	CALL DATYP(QD,'    QD  ',pon(),K,K,KMAX,KMAX)		!TYPE Q
	CALL ATYPE2(PI,'    PI  ',pon(),K,K,KMAX,KMAX)
45	continue
	call flush(7)
c
c End of definition of rates.
c
c Now do simulations.
c In this case there is no need to add lifetimes of contiguous openings
c of same conductance, since we are interested only in tranistions/routes
c between individual states.
c
c Define cumulative pi matrix
	do i=1,k
	 do j=1,k
	  picum(i,j)=0.
	  do n=1,j
	   if(n.ne.i) picum(i,j)=picum(i,j)+pi(i,n)
	  enddo
	 enddo
	enddo
	if(debug())call ATYP(picum,' PI(cum)',pon(),k,k,Kmax,kmax)
c calc mean lifetimes
	do i=1,k
	   amean(i)=-sngl(1.d3/qd(i,i))
	enddo
c
	call RANDSK(ix1,iy1,iz1,0,repeat)		!read IX,IY,IZ
	print 30
30	format(' Number of transitions to be simulated = ')
	call INPUTi(ntmax)
c	ALLOCATE(stat=ierr,tint(ntmax),iamp(ntmax),istate(ntmax))
	ALLOCATE(stat=ierr,istate(ntmax))
c Define initial state
	call INSTAT(peq,k,i1,pcum,ist,k1,fixed)
	is=i1			!present state
c
	if(pon()) write(7,341)is
      if(discprt) write(8,341)is
c	print 341,is
341	format(' Initial state = #',i3)
	call CLS
	call LOCATE(0,0)
	print 34
34	format(' Simulating (abort with F2; Caplock on to see values.',/)
c Start loop
	nt=0
	do while(nt.lt.ntmax)
	  if(mod(nt,1000).eq.0) then	!check for F2 every 1000
c NB key F2 has ktype=0, scan code=60 (see \fortran\tkey.for)
		if(KBHIT()) then
		   ch=GETCH(ktype)
		   if(ktype.eq.0.and.ichar(ch).eq.60) goto 91	!end run
		endif
	   endif
	   nt=nt+1
	   u=random()		!uniform(0,1)
	   t=-amean(is)*alog(u)	!in ms
c	   tint(nt)=t
c	   iamp(nt)=icur(is)
	   istate(nt)=is
	   nsim(is)=nsim(is)+1
	   tsim(is)=tsim(is)+t
c  - which state next?
	   u=random()		!uniform(0,1)
	   do j=1,k
		if(j.ne.is) then
		   n=j
		   bot=0.
		   if(j.gt.1) bot=picum(is,j-1)
		   if(u.gt.bot.and.u.le.picum(is,j)) goto 13	  !out of loop
		endif
	   enddo
13	   j=n		!next state
c test freqs
	   pisim(is,j)=pisim(is,j)+1.
	   is=j	!state # for next cycle
	enddo
91	continue
c END of simulation loop
c
c Define and calculate routes to be enumerated
c NB Problem with earlier version. With particular numbers used this was
c esp a problem for iopt=2 -with long 3-2-3 oscillation got. eg
c 121=2,122=3,123=2,124=3,...,135=2,136=3,137=4 so 121-137 was counted as iseq=1,
c and 123-137 was counted as another iseq=1, up to 135-137.  Want to count
c this only once, so once 137=4 is reached, increment i so start at 138
c to look for start (state 2) of next sequence.
	iopt=1
40	print 31,iopt
31	format(' List one of the following:',/,
     & ' (1) Number of routes from 4 to 2 that',/,
     & '    (a) Go through 3 but not 1',/,
     & '    (b) Go through 1 but not 3',/,
     & '    (c) Go through both 1 and 3 at least once',/,
     & ' (2) Number of routes from 2 to 4 that',/,
     & '    (a) Go through 3 but not 1',/,
     & '    (b) Go through 1 but not 3',/,
     & '    (c) Go through both 1 and 3 at least once',/,
     & ' (3) Number of routes from 4 to 2 that',/,
     & '    (a) Go through 3 more recently than 1',/,
     & '    (b) Go through 1 more recently than 3',/,
     & ' (4) Number of routes from 2 to 4 that',/,
     & '    (a) Go through 3 more recently than 1',/,
     & '    (b) Go through 1 more recently than 3',/,
     & ' (5) Number of simple cycles:',/,
     & '    (a) 4-3-2-1-4',/,
     & '    (b) 4-1-2-3-4',/,
     & ' (6) Finished',/,
     & ' Option number [',i2,'] = ')
c     & '',/,
	call INPUTi(iopt)
	if(iopt.eq.6) goto 99
c
	if(iopt.eq.1) then
	   n1=0
	   n2=0
	   n3=0
	   debon=debug()
c	   do i=1,nt
	   i=1
100	   continue		!start of loop
		if(mod(i,100).eq.0) then	!check debug every 100
		   debon=debug()
		endif
		if(i.gt.nt) goto 9
		is=istate(i)
		if(debon) then
		   print 351,i,is
351		   format(' #',i7,' = ',i3)
		endif
c Find first sojourn in 4
		if(istate(i).eq.4) then	!sequence started
		   if(debon) then
			print 35,i
35			format('   #',i7,' = state 4')
		   endif
		   j=i+1
		   if(j.gt.nt) goto 9
		   js=istate(j)
		   if(debon) then
			print 36,j,js
36			format('   #',i7,' = state ',i3)
		   endif
		   via1=.false.
		   via3=.false.
		   do while(js.ne.2)
			if(js.eq.1) via1=.true.
			if(js.eq.3) via3=.true.
			j=j+1
			if(j.gt.nt) goto 9
			js=istate(j)
			if(debon) then
			   print 36,j,js
		      endif
		   enddo
		   if(via3.and.(.not.via1)) then
			iseq=1
			n1=n1+1
		   else if(via1.and.(.not.via3)) then
			iseq=2
			n2=n2+1
		   else if(via3.and.via1) then
			iseq=3
			n3=n3+1
		   endif
		   if(debon) then
			print 37,iseq
37			format(' sequence = type ',i3)
			if(iseq.eq.3) then
			   pause
			endif
		   endif
		   i=j+1	!start looking for next seq after end of this one
		   goto 100
		else		!istate not 4
		   i=i+1
		   goto 100
		endif		!end if 'if(istate=4)'
c	   enddo
9	   continue
         print 33,n1,n2,n3
         if(pon()) write(7,33) n1,n2,n3
         if(discprt) write(8,33) n1,n2,n3
33	   format(/,
     & ' Number of times that routes from 4 to 2:',/,
     & '    (a) goes through 3 but not 1 = ',i6,/,
     & '    (a) goes through 1 but not 3 = ',i6,/,
     & '    (a) goes through both 1 and 3 at least once = ',i6,/)
c
	else if(iopt.eq.2) then
	   n1=0
	   n2=0
	   n3=0
	   debon=debug()
c	   do i=1,nt
	   i=1
200	   continue		!start of loop
		if(mod(i,100).eq.0) then	!check debug every 100
		   debon=debug()
		endif
		if(i.gt.nt) goto 9
		is=istate(i)
		if(debon) then
		   print 351,i,is
c351		   format(' #',i7,' = ',i3)
		endif
c Find first sojourn in 2
		if(istate(i).eq.2) then	!sequence started
		   if(debon) then
			print 352,i
352			format('   #',i7,' = state 2')
		   endif
		   j=i+1
		   if(j.gt.nt) goto 92
		   js=istate(j)
		   if(debon) then
			print 36,j,js
c36			format('   #',i7,' = state ',i3)
		   endif
		   via1=.false.
		   via3=.false.
		   do while(js.ne.4)
			if(js.eq.1) via1=.true.
			if(js.eq.3) via3=.true.
			j=j+1
			if(j.gt.nt) goto 92
			js=istate(j)
			if(debon) then
			   print 36,j,js
		   endif
		   enddo
		   if(via3.and.(.not.via1)) then
			iseq=1
			n1=n1+1
		   else if(via1.and.(.not.via3)) then
			iseq=2
			n2=n2+1
		   else if(via3.and.via1) then
			iseq=3
			n3=n3+1
		   endif
		   if(debon) then
			print 37,iseq
c37			format(' sequence = type ',i3)
			if(iseq.eq.3) then
			   pause
			endif
		   endif
		   i=j+1	!start looking for next seq after end of this one
		   goto 200
		else		!istate not 4
		   i=i+1
		   goto 200
		endif		!end if 'if(istate=4)'
c	   enddo
92	   continue
         print 332,n1,n2,n3
         if(pon()) write(7,332) n1,n2,n3
         if(discprt) write(8,332) n1,n2,n3
332	   format(/,
     & ' Number of times that routes from 2 to 4:',/,
     & '    (a) goes through 3 but not 1 = ',i6,/,
     & '    (a) goes through 1 but not 3 = ',i6,/,
     & '    (a) goes through both 1 and 3 at least once = ',i6,/)

	else if(iopt.eq.3) then
	   n1=0
	   n2=0
	   debon=debug()
c	   do i=1,nt
	   i=1
103	   continue		!start of loop
		if(mod(i,100).eq.0) then	!check debug every 100
		   debon=debug()
		endif
		if(i.gt.nt) goto 93
		is=istate(i)
		if(debon) then
		   print 351,i,is
c351		   format(' #',i7,' = ',i3)
		endif
c Find first sojourn in 4
		if(istate(i).eq.4) then	!sequence started
		   if(debon) then
			print 35,i
c35			format('   #',i7,' = state 4')
		   endif
		   j=i+1
		   if(j.gt.nt) goto 93
		   js=istate(j)
		   if(debon) then
			print 36,j,js
c36			format('   #',i7,' = state ',i3)
		   endif
		   via1=.false.
		   via3=.false.
		   do while(js.ne.2)
			if(js.eq.1) then
			   via1=.true.
			   via3=.false.
			else if(js.eq.3) then
			   via1=.false.
			   via3=.true.
			endif
			j=j+1
			if(j.gt.nt) goto 93
			js=istate(j)
			if(debon) then
			   print 36,j,js
		      endif
		   enddo
		   if(via3.and.(.not.via1)) then
			iseq=1
			n1=n1+1
		   else if(via1.and.(.not.via3)) then
			iseq=2
			n2=n2+1
		   endif
		   i=j+1	!start looking for next seq after end of this one
		   goto 103
		else		!istate not 4
		   i=i+1
		   goto 103
		endif		!end if 'if(istate=4)'
c	   enddo
93	   continue
         print 333,n1,n2
         if(pon()) write(7,333) n1,n2
         if(discprt) write(8,333) n1,n2
333	   format(/,
     & '  Number of routes from 4 to 2 that',/,
     & '    (a) Go through 3 more recently than 1 = ',i7,/,
     & '    (b) Go through 1 more recently than 3 = ',i7,/)

	else if(iopt.eq.4) then
	   n1=0
	   n2=0
	   debon=debug()
c	   do i=1,nt
	   i=1
104	   continue		!start of loop
		if(mod(i,100).eq.0) then	!check debug every 100
		   debon=debug()
		endif
		if(i.gt.nt) goto 93
		is=istate(i)
		if(debon) then
		   print 351,i,is
c351		   format(' #',i7,' = ',i3)
		endif
c Find first sojourn in 2
		if(istate(i).eq.2) then	!sequence started
		   if(debon) then
			print 35,i
c352			format('   #',i7,' = state 2')
		   endif
		   j=i+1
		   if(j.gt.nt) goto 94
		   js=istate(j)
		   if(debon) then
			print 36,j,js
c36			format('   #',i7,' = state ',i3)
		   endif
		   via1=.false.
		   via3=.false.
		   do while(js.ne.4)
			if(js.eq.1) then
			   via1=.true.
			   via3=.false.
			else if(js.eq.3) then
			   via1=.false.
			   via3=.true.
			endif
			j=j+1
			if(j.gt.nt) goto 94
			js=istate(j)
			if(debon) then
			   print 36,j,js
		      endif
		   enddo
		   if(via3.and.(.not.via1)) then
			iseq=1
			n1=n1+1
		   else if(via1.and.(.not.via3)) then
			iseq=2
			n2=n2+1
		   endif
		   i=j+1	!start looking for next seq after end of this one
		   goto 104
		else		!istate not 4
		   i=i+1
		   goto 104
		endif		!end if 'if(istate=4)'
c	   enddo
94	   continue
         print 334,n1,n2
         if(pon()) write(7,334) n1,n2
         if(discprt) write(8,334) n1,n2
334	   format(/,
     & '  Number of routes from 2 to 4 that',/,
     & '    (a) Go through 3 more recently than 1 = ',i7,/,
     & '    (b) Go through 1 more recently than 3 = ',i7,/)

	else if(iopt.eq.5) then
	   n1=0
	   n2=0
	   debon=debug()
	   i=1
c	   do i=1,nt
300	   continue		!start of loop
		if(mod(i,100).eq.0) then	!check debug every 100
		   debon=debug()
		endif
		if(i.gt.nt) goto 97
		is=istate(i)
		if(debon) then
		   print 351,i,is
c351		   format(' #',i7,' = ',i3)
		endif
c Find first sojourn in 4
		if(istate(i).eq.4) then	!sequence started
		   if(i+4.gt.nt) goto 97
		   is1=istate(i+1)	!must be 1 or 3
		   is2=istate(i+2)
		   is3=istate(i+3)
		   is4=istate(i+4)
		   if(is1.eq.3.and.is2.eq.2.and.is3.eq.1.and.is4.eq.4) then
			n1=n1+1
			if(debon) then
			   print 353
353			   format(/,' anticlockwise loop:')
			   do j=i,i+4
				print 41,j,istate(j)
41				format('   #',i7,i4)
			   enddo
			   pause
			endif
			i=i+4		!now in state 4=start of next potential seq
			goto 300
		   endif
		   if(is1.eq.1.and.is2.eq.2.and.is3.eq.3.and.is4.eq.4) then
			n2=n2+1
			if(debon) then
			   print 354
354			   format(/,' clockwise loop:')
			   do j=i,i+4
				print 41,j,istate(j)
c41				format('   #',i7,i4)
			   enddo
			   pause
			endif
			i=i+4		!now in state 4=start of next potential seq
			goto 300
		   endif
		endif
c         if no loop found
		i=i+1
		if(i.gt.nt) goto 97
		goto 300
c
97	   continue
         print 337,n1,n2
         if(pon()) write(7,337) n1,n2
         if(discprt) write(8,337) n1,n2
337	   format(/,
     & ' Number of anticlockwise loops = ',i6,/,
     & ' Number of clockwise loops     = ',i6,/)
c===
	endif
	iopt=iopt+1
	goto 40
c
99	continue
c	DEALLOCATE(tint,iamp,istate)
	DEALLOCATE(istate)
c
c Print checks
c
c calc simulated Popen
	to=0.
	do 165 i=1,ka
165	to=to + tsim(i)
	ts=0.
	do 166 i=ka+1,k
166	ts=ts + tsim(i)
	Ttot=to+ts
	if(pon()) write(7,167)to,ts,to+ts,to/Ttot
      if(discprt) write(8,167)to,ts,to+ts,to/Ttot
	print 167,to,ts,to+ts,to/Ttot
167	format(' Simulated: total time, open, shut, length= ',
     & 3g13.6,/,' Popen= ',g13.6)
c
c Calc PI matrix from simulated transition freqs
	do 24 i=1,k
	s=0.		!for row sum
	do 25 j=1,k
	if(i.eq.j) goto 25
	s=s+pisim(i,j)
25	continue
	do 26 j=1,k
	if(s.gt.0.) pisim(i,j)=pisim(i,j)/s
26	continue
24	continue
	call ATYP(pisim,' PI(sim)',pon(),k,k,Kmax,kmax)
c	print 160,k,ka
c	if(pon()) write(7,160)k,ka
c     if(discprt) write(8,160)k,ka
c160	format(' k,ka= ',2i8)
	print 164
	if(pon()) write(7,164)
	if(discprt) write(8,164)
164	format(/,
     & ' State  # of sojourns       Mean life',14x,' Occupancy',/,
     & 21x,'    simulated    true        simulated     true')
	do i=1,k
	 if(nsim(i).gt.0) then
	  print 162,i,nsim(i),tsim(i)/float(nsim(i))
     & ,amean(i),tsim(i)/Ttot,peq(i)
	  if(pon()) write(7,162)i,nsim(i),tsim(i)/float(nsim(i))
     & ,amean(i),tsim(i)/Ttot,peq(i)
        if(discprt) write(8,162)i,nsim(i),tsim(i)/float(nsim(i))
     & ,amean(i),tsim(i)/Ttot,peq(i)
162	 format(i6,2x,i10,4x,4g13.6)
	 else if(nsim(i).eq.0) then
	  print 1621,i,nsim(i),
     &  amean(i),tsim(i)/Ttot,peq(i)		!if no sojourns
	  if(pon()) write(7,1621)i,nsim(i),
     &  amean(i),tsim(i)/Ttot,peq(i)
        if(discprt) write(8,1621)i,nsim(i),
     &  amean(i),tsim(i)/Ttot,peq(i)
1621	  format(i6,2x,i10,4x,'       ?     ',3g13.6)
	 endif
	enddo
	if(pon()) write(7,1632) nt
      if(discprt) write(8,1632) nt
	print 1632, nt
1632	format(
     & ' Total number of sojourns in individual states= ',i12)
c
	call RANDSK(ix1,iy1,iz1,1,repeat)		!write IX,IY,IZ
	call ENDPRINT
	end

	subroutine GETCYC(iopt)
	integer icyc(50),IM(50,100),JM(50,100)
c	integer ICYC(20),NSC(8),IM(8,15),JM(8,15)
      character*1 ans,UC
	COMMON/MPAR/NCYC,NSC,IM,JM
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
c iopt=3 asks for rates to be calc by micro rev only
c
101   format(a1)
	ans='N'
c
	if(iopt.eq.2) goto 5
	if(iopt.eq.3) goto 6
132	if(ncyc.eq.0) then
	   print 1
1	   format(' No cycles in the model',/)
	else
	   do 125 L=1,ncyc
	   print 126,L
126	   format(' Cycle # ',i3)
	   print 1311,im(L,1),jm(L,1)
1311	   format(2i3,'  (calc by micro rev)')
	   print 127,(IM(L,M),JM(L,M),m=2,nsc(L))
127	   format(2(5(2i3,4x),/))
125	   continue
	endif
	if(iopt.eq.1) RETURN
c
	print 2
2	format('& O.K. [Y] ? ')
      read 101,ans
5     if(UC(ans).eq.'N') then
	print 21
21	format(
     & ' Alter ONLY the rates to be calc by microscopic rev. [Y] ? ')
      read 101,ans
      if(UC(ans).ne.'N') goto 6
	   print 61
61	   FORMAT( ' No of cycles (up to 8) = ')
	   call INPUTi(L)
	   if(L.lt.0) goto 66
	   ncyc=L
	   IF(NCYC.EQ.0) GOTO 66
	   DO 62 L=1,NCYC
	   print 63,L
63	   FORMAT( ' Number of states in cycle #',I2,' = ')
	   call INPUTi(nsc(L))
	   print 67
	   call INPUT2i(icyc(1),icyc(2))
	   print 65,ICYC(1),ICYC(2)
65	   FORMAT(' List states in cycle in correct order:',/,
     &    ' state # 1= ',i2,/,
     &    ' state # 2= ',i2)	!start with micro-rev route
	   DO 64 M=3,NSC(L)
	   print 651,M
651	   FORMAT( ' state #',I2,' = ')  !EG, 1,3,4,7 IN CORRECT ORDER
	   call INPUTi(icyc(m))
64	   CONTINUE
C SET IM,JM. IM(L,1)=I,JM(L,1)=J for Q(i,j) to be calc by micro-rev
c in the Lth cycle. The rest of the q(i,j) in the cycle, going round
c in the same direction, have i,j in the other IM,JM
	   DO 641 M=1,NSC(L)-1
	   IM(L,M)=ICYC(M)
	   JM(L,M)=ICYC(M+1)
641	   CONTINUE
C LAST IM,JM
	   IM(L,NSC(L))=ICYC(NSC(L))
	   JM(L,NSC(L))=ICYC(1)		!COMPLETE THE CYCLE
62	   CONTINUE	!REPEAT FOR NEXT CYCLE
	endif
c
66	continue
	RETURN
c
c Special section for iopt=3, to get only the route to be calc
c by micro rev, for each cycle (when cycles have already been defined
c in CHARQ- values already in im,jm
6	continue
	if(ncyc.eq.0) RETURN
	do 120 i=1,ncyc
17	   print 123,i
123	   format(' Cycle #',i3)
	   do 121 j=1,nsc(i)
121	   icyc(j)=im(i,j)	!states for current cycle
	   print 13,(icyc(j),j=1,nsc(i))
13	   format('   states: ',20i3)
	   print 67
67	   FORMAT( '  route, q(i,j), to be calc by micro rev: i,j= ')
	   call INPUT2i(i1,j1)
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
	   do 124 m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
124	   continue
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	   call IVECREV(icyc,nsc(i),50)
	   do 16 m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
16	   continue
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c if reach here the specified i,j are not found in the cycle
	   goto 17
15	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1-m2,20)
	   goto 18
151	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1,20)
c and redefine im,jm in correct order
18	   continue
	   do 131 m=1,nsc(i)-1
	   im(i,m)=icyc(m)
	   jm(i,m)=icyc(m+1)
131	   continue
c and the last im,jm
	   im(i,nsc(i))=icyc(nsc(i))
	   jm(i,nsc(i))=icyc(1)		!complete the cycle
120	continue		!next cycle
c
	RETURN	!from GETCYC
	end

