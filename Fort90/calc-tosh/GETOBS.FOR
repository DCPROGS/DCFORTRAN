	subroutine GETOBS(titlef,titled,titlex,titley,ifile,idtype,
     &    nfile,prt,readini,readat,ndisc,infil,ifile1,
     &    titype,tau,area,ncomp,iver)
c To get observations in MECHFIT
c Get nset different sorts of obs; for each read time constants and areas/amps
c as in INWIND, as in CVFIT? -or as for input of param in CVFIT/EKDIST?
c NB Burst/cluster distributions are not very suitable since, e.g., may need
c different kA,kB,kC,kD for runs at different conc.  Anyway info is really
c mostly in the shut time pdf. NO -shut time pdf gives info about number of
c openings per burst, and pdf of gaps within burst, but NO info about
c burst length which is critical info!
c
c    (1) For macroscopic jumps, scaling is a problem -for now FIT the scale
c factor, as Nchan=number of channels, as a parameter to be estimated.
c Apart from time constants and amplitudes, we need other info in the data
c	(2) Also, scale the slowest time constants of shut-time distribution
c by Nchan (not same value as for jumps!)
c                                                Additional data needed:
c
c  idtype=1  Pulse response (macroscopic): conc before,during,after + length
c							  =xA0,xA,xA1,t0=tpuls.
c  idtype=2  Step response (macroscopic)    conc before, after=xA0,xA
c   [for macroscopic also need Y(inf), gamma(i) and (V-Vrev) + Nchan1=parameter]
c  idtype=3  First latency pdf for pulse    as for pulse response (1 channel)
c  idtype=4  First latency pdf for step     as for step response (1 channel)
c  idtype=5  Shut time pdf                  conc=xA  (+nchan2=parameter)
c  idtype=6  Open time pdf                  conc=xA
c  idtype=7  Burst length pdf               conc=xA
c=======others not yet done
c  idtype=8	 Cluster length pdf                "
c  idtype=9	 Super-cluster length pdf          "
c  idtype=10 No of openings/burst              "
c  idtype=11 No of openings/cluster            "
c  idtype=12 No of openings/super-cluster      "
c
c Declarations for MECHFIT
	character*50 titype(20)		!data types
	character infil*33
	integer*4 idtype(20)
	real*4 xA0(20),xA(20),xA1(20),tpulse(20),Yinf(20)
	integer ncomp(20),nstau(20)
	real*4 Vmem(20)   !for (V-Vrev)
	real*4 tau(10,20),area(10,20)		!nc=component #; np=idtype
	real*4 times(1000,20)
	real*4 Yobs(1000,20)
	real*4 weight(1000,20)
	integer npt(20)
	integer ntime(20)
	ALLOCATABLE:: data			!for INWIND
	real data(:,:)
	logical allocated
	character*79 heading,title
	logical discprt,slock,fopen,pon,prt,printed,newconc
	logical debug,caplock
	logical readat,readini
	integer juse(20),jmiss(20)
	character*40 titlex,titley	!axis labels
	character*60 titlef	!file name
	character*60 titled(20)
	character*11 cnum
	character*1 ans,UC,ans1
	common/data1/times,Yobs,weight,
     &	ntime,xA0,xA,xA1,tpulse,Vmem,nstau
	common/data2/nset,nsfit,juse,jmiss
	common/dp/discprt
c
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	prt=pon()		!to reduce number of pon() calls
c
	printed=.false.	!so data not printed twice
	newconc=.false.
c
c	if(idest.eq.0) goto 60		!first time
c	print 61
c61	format(' Use same data and weighting [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') then
c	   if(hdisp) RETURN	!no fitting so use current Xobs or yval1
c	   goto 114			!fit same data
c	else
c	   idest=0              !diff data
c	endif
c Define data types
	ntype=8
	titype(1)='Pulse response (macroscopic)'
	titype(2)='Step response (macroscopic) '
	titype(3)='First latency pdf for pulse '
	titype(4)='First latency pdf for step  '
	titype(5)='Shut time pdf               '
	titype(6)='Open time pdf               '
	titype(7)='Burst length pdf            '
	titype(8)='Open time per burst pdf     '
c ======omit rest for now (see above)
c	titype(9)='No of openings/burst        '
cc	titype(8)='Cluster length pdf          '
cc	titype(9)='Super-cluster length pdf    '
cc	titype(10)='No of openings/burst        '
cc	titype(11)='No of openings/cluster      '
cc	titype(12)='No of openings/super-cluster'
c  idtype=3  First latency pdf for pulse    as for pulse response
c  idtype=4  First latency pdf for step     as for step response
c  idtype=5  Shut time pdf                  conc=xA
c  idtype=6  Open time pdf                  conc=xA
c  idtype=7  burst length pdf               conc=xA
c  idtype=8  total open time/burst pdf      conc=xA
c
60	continue
c
	print 20
20	format(
     & ' (1) Read data from MFDAT.DAT data file',/,
     & ' (2) Input new data from keyboard',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.eq.2) then
	   readat=.false.
	   goto 102
	else if(iopt.eq.1) then
	   ifile=1		!so MFDISK asks for IFILE and reads it
c
	   call MFDISK(nset,ilabel,titlef,titled,titlex,titley,
     &    ifile,nfile,prt,readini,iesc,readat,ndisc,infil,ifile1,
     &    idtype,xA0,xA,xA1,tpulse,nstau,Yinf,ncomp,Vmem,tau,area,
     &    npt,fopen,iver)
c
	   if(.not.readat) goto 102
	   if(iesc.eq.98) RETURN		!reallocate arrays
	   if(iesc.eq.99) RETURN		!discnum abort
	   goto 71		!display without asking
	endif
c
	ans1='n'
	call DCASK('View/alter the data',ans1,ans)
	if(UC(ans).ne.'Y') GOTO 114
c Loop to view each set
71	continue
	print 322,titlef
	if(pon()) write(7,322) titlef
	if(discprt) write(8,322) titlef
322	format(' File: ',a60)
	do 113 j=1,nset
	   ncol=2
	   if(idtype(j).gt.2) then
		heading='    tau (ms)       area    '
	   else
		heading='    tau (ms)     amplitude'
	   endif
	   nd1=ncomp(j)+3		!add 3 in case more components added
	   ALLOCATE(data(nd1,ncol))
	   iflag=ncol	!so input data displayed in all cols of window
	   call INTCONV(j,cnum)
	   title=' SET '//charnb(cnum)//': '//charnb(titled(j))
	   do i=1,ncomp(j)
		data(i,1)=tau(i,j)
		data(i,2)=area(i,j)		!=amp for relaxations
	   enddo
	   nidisp=ncomp(j)
	   nrow=ncomp(j)
	   if(nidisp.gt.20) nidisp=20		!best value- up to 20?
	   iconst=0	!no constraint
	   if(idtype(j).gt.2) iconst=-1	!so last area filled in automatically
	   nr1=-1		!so window initially in default position
	   nrowsav=nrow
c
	   call INWINDv(data,nrow,ncol,nidisp,title,heading,
     &		 nr1,nc1,iflag,iconst,nd1,ncol)
c  Reassign data (if it has been altered)
	   if(nrow.ne.nrowsav) then		!line(s) added/deleted
		ncomp(j)=nrow
	   endif
	   if(iflag.eq.1) then
	    do i=1,ncomp(j)
		tau(i,j)=data(i,1)
		area(i,j)=data(i,2)		!=amp for relaxations
	    enddo
	   endif
	   DEALLOCATE(data)
113	continue		!end of jth set loop
c
	do j=1,nset
c Check npt() OK
	   id=idtype(j)
	   print 1092,j,titype(id)
	   if(pon()) write(7,1092) j,titype(id)
         if(discprt) write(8,1092) j,titype(id)
1092	   FORMAT(/,'  Data set # ',i3,3x,a50)
	   npt(j)=npt(1)		!default
	   print 21,npt(j)
c21	   format(' Number of time points per time constant [',i3,'] = ')
	   call INPUTi(npt(j))
	enddo
c
	do j=1,nset
c Print out values for set #j
	   call PRTJ(j,titype,pon(),idtype,xA0,xA,xA1,
     &	 tpulse,nstau,Yinf,ncomp,Vmem,tau,area,npt,iver)
	   printed=.true.	!so data not printed again below
	enddo
c
c ALTERATIONS
c
74	continue
c
	print 72
72	format(' Options to alter data:',/,
     & ' [1] No changes ',/,
     & ' (2) Add another data set',/,
     & ' (3) Remove LAST data set',/,
     & ' (4) Alter a set title',/,
     & ' (5) Add another component',/,
     & ' (6) Remove a component',/,
     & ' (7) Alter file title',/,
     & ' (8) Alter concentration',/,
     & ' Option number [1] = ')
	ialt=1
	call INPUTi(ialt)
c	if(ialt.eq.1) goto 1101		!print and store on disk?
	if(ialt.lt.1.or.ialt.gt.8) goto 74
	goto (1101,82,83,84,85,85,88,89) ialt
c Add set
82	continue
	print 621
621	format(' (1) Type in data',/,
     & ' (2) Duplicate an existing data set',/,
     & ' Option number = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   nset=nset+1
	   j1=nset		!so only new set read in below
	   goto 203
	else if(iopt.eq.2) then
	   print 90
90	   format(' Duplicate set number = ')
	   call INPUTi(j)
	   nset=nset+1
	   ncomp(nset)=ncomp(j)
	   titled(nset)=titled(j)
	   do 91 i=1,ncomp(j)
		tau(i,nset)=tau(i,j)
		area(i,nset)=area(i,j)
91	   continue
	endif
	goto 74	!more changes?
c
c Remove last set
83	continue
	nset=nset-1
	goto 74	!more changes?
c option to change set title
84	continue
	print 501
501	format(' Alter title for set number = ')
	call INPUTi(j)
	nlen=60
	call TITENT0('Enter title for set:',titled(j),nlen,.false.)
	goto 74	!more changes?
c
85	continue
	j=1
	if(nset.gt.1) then
	   print 512
512	   format(' Alter set number = ')
	   call INPUTi(j)
	endif
	if(ialt.eq.5) then
	   ncomp(j)=ncomp(j)+1
	   print 515,ncomp(j),j,ncomp(j),j
515      format('&tau(',2i3,'), area(',2i3,') = ')
	   call INPUT2r(tau(i,j),area(i,j))
	else if(ialt.eq.6) then
520	   print 519,ncomp(j)
519	   format('  -remove component # (up to',i3,') = ')
	   call INPUTi(i)
	   if(i.lt.1.or.i.gt.ncomp(j)) goto 520
         if(prt) write(7,513) i,tau(i,j)
         if(discprt) write(8,513)  i,tau(i,j)
513	   format(' Observation #',i4,' omitted: tau= ',g13.6)
	   n=0
	   do 514 k=1,ncomp(j)
		if(k.eq.i) goto 514
		n=n+1
		tau(n,j)=tau(k,j)
		area(n,j)=area(k,j)
514	   continue
	   ncomp(j)=ncomp(j)-1
	endif
	goto 74 		!more changes?
c
88	continue
	if(iver.eq.1001) goto 74	!has no file title!
	nlen=60		!default as read from .INI (last titlef used)
	call TITENT0('Enter title for whole file:',titlef,nlen,.false.)
	goto 74	!more changes?
c
89	continue
	newconc=.true.
	j1=1
	goto 70	!get altered conc (then goto 74)
c change conc
c	goto 74	!more changes?
c
c End of alterations
c
c Type in observations from VDU
102	continue
	nlen=60		!default = previous title
	call TITENT0('Enter title for whole file:',titlef,nlen,.true.)
	print 103
103	FORMAT(' Number of data sets in this file = ')
	call INPUTi(nset)
	j1=1		!get all sets from 1 to NSET
c jump to 203 to add a set
203	continue
c
	newconc=.false.
70	continue	!return here with newconc=T to correct concentrations
	do j=j1,nset
	   if(newconc) then
		print 701,j,titled(j)
701		format(/,' SET # ',i3,2x,a60,/)
		goto 73
	   endif
107	   print 10,j
10	   format(' What data type is in set #',i3,' ?')
	   do i=1,ntype
		print 106,i,titype(i)
106		format(' (',i2,') ',a50)
	   enddo
	   PRINT 23
23	   format(' Type number = ')
	   call INPUTi(i)
	   if(i.lt.1.or.i.gt.ntype) goto 107
	   idtype(j)=i
	   titled(j)=titype(i)
	   call TITENT0('Title for this set:',titled(j),60,.false.)
c Get ancillary values needed for this set
	   print 11
11	   format(' Number of exponential components = ')
	   call INPUTi(i)
	   ncomp(j)=i
c All data so far are exponentials; for pdf need 2*ncomp-1 values
c For relaxations need
c Y(t)=Y(inf) + SUM[w(i)*exp(-r(i)*t)]
c -where SUM[(w(i)]=Y(0) - Y(inf)
c
c but need to normalise this somehow, so only relative amplitudes matter
c e.g. Y'(t)=Y(t)/w(max) = Y'(inf) - SUM[w(i)'*exp(-r(i)*t)]
c so w(max)'=1 always.  Thus have ncomp time constants, ncomp-1 values of
c w', and Y'(inf) i.e. 2*ncomp values.
	   call INTCONV(j,cnum)
	   title=' SET '//charnb(cnum)//': '//charnb(titype(idtype(j)))
	   if(idtype(j).le.2) then
		heading='    tau (ms)     amplitude'
	   else if(idtype(j).gt.2.and.idtype(j).le.9) then
		heading='    tau (ms)       area    '
c=	   else if(idtype(j).gt.9.and.idtype(j).le.ntype) then
c=		heading='   mean #          area    '
	   endif
	   iconst=0		!cols not constrained
	   if(idtype(j).gt.2) iconst=-1	!so last area filled in automatically
	   nr1=-1		!so window initially in default position
	   ncol=2		!defined above
	   nrow=ncomp(j)
	   nidisp=ncomp(j)
	   if(allocated(data)) DEALLOCATE(data)
	   nd1=ncomp(j)+3		!add 3 in case more components added
	   ALLOCATE(data(nd1,ncol))
	   iflag=0	!display initially blank
	   nrowsav=nrow
	   call INWINDv(data,nrow,ncol,nidisp,title,heading,
     &		 nr1,nc1,iflag,iconst,nd1,ncol)
	   if(nrow.ne.nrowsav) then		!line(s) added/deleted
		ncomp(j)=nrow
	   endif
	   do i=1,ncomp(j)
		tau(i,j)=data(i,1)
		area(i,j)=data(i,2)		!=amp for relaxations
	   enddo
	   DEALLOCATE(data)
c  Get ancillary variables
73	   continue
	   if(idtype(j).eq.1.or.idtype(j).eq.3) then
	      if(idtype(j).eq.1) then
		  print 18
18		  format(
     &      ' Equilibrium current, Y(inf) (pA) = ')
		  call INPUTr(Yinf(j))
		  print 19
19		  format(
     &    '&Driving force, (V - Vrev) (mV) = ')
		  call INPUTr(Vmem(j))
	      endif
		print 14
14		format(
     &    '&agonist concentration (micromolar) before pulse [0.] = ')
		xA0(j)=0.0
		call INPUTr(xA0(j))
		print 15,xA(j)
15		format(
     & '&agonist concentration (micromolar) during pulse [',f8.3,'] = ')
		call INPUTr(xA(j))
		print 16
16		format(
     &    '&agonist concentration (micromolar) after pulse [0.] = ')
		xA1(j)=0.0
		call INPUTr(xA1(j))
		print 17
17		format('&duration of pulse (ms) = ')
		call INPUTr(tpulse(j))
	   else if(idtype(j).eq.2.or.idtype(j).eq.4) then
	      if(idtype(j).eq.2) then
		   print 18
		   call INPUTr(Yinf(j))
		   print 19
		   call INPUTr(Vmem(j))
		endif
		print 14
		xA0(j)=0.0
		call INPUTr(xA0(j))
		print 16
		call INPUTr(xA(j))
	   else if(idtype(j).ge.5.and.idtype(j).le.ntype) then
		print 12,xA(j)
12		format('&agonist concentration (micromolar) [',f8.3,'] = ')
		call INPUTr(xA(j))
		if(idtype(j).eq.5) then
		   print 26
26		format(
     &	'&Multiply the n slowest time constants by Nchan: n [1] = ')
		nstau(j)=1
		call INPUTi(nstau(j))
		endif
c		if(idtype(j).ge.7) then
c		   print 13
c13		   format('&T(crit) for definition of bursts (ms) = ')
c		   call INPUTr(tcrit(j))
c		endif
	   endif
c For all data types, now set the time points.  For each set specify
c the number of points per time constants (e.g. 10 or 20?) in npt(j)
c so have ntpnt(j)=ncomp(j)*ntpnt(j) time values (these values will overlap at
c low t but this should not matter -or start values for slower tau only
c after end of those for faster tau?)
c -could space these equally between t=0 and t=5*tau?
	   if(newconc) goto 75
22	   print 21,npt(j)
21	   format(' Number of time points per time constant [',i3,'] = ')
	   call INPUTi(npt(j))
	   if(npt(j).le.3) then
		npt(j)=20	!default
		goto 22
	   endif
c
c Print out values
75	   call PRTJ(j,titype,pon(),idtype,xA0,xA,xA1,
     &	 tpulse,nstau,Yinf,ncomp,Vmem,tau,area,npt,iver)
	   printed=.true.	!so data not printed again below
	enddo	!end of jth set loop
	if(newconc) then
	   newconc=.false.
	   goto 74	!any more changes?
	endif
c
1101	continue
	if(readat) then
	   print 110,ifile
110	   FORMAT(
     &  ' Data are from file ',i3,':')
	   call DCASK('  re-write data to disc','n',ans)
	else
	   call DCASK(' Store these data on disc','y',ans)
	endif
	if(ans.eq.'Y') then
	   iesc=0
	   ifile=0		!so MFDISK asks for IFILE and writes it
	   call MFDISK(nset,ilabel,titlef,titled,titlex,titley,
     &    ifile,nfile,prt,readini,iesc,readat,ndisc,infil,ifile1,
     &    idtype,xA0,xA,xA1,tpulse,nstau,Yinf,ncomp,Vmem,tau,area,
     &    npt,fopen,iver)
	   if(iesc.eq.99) RETURN		!discnum abort
	   close(unit=12)
	   fopen=.false.
	endif
c
C NOW HAVE DATA. START FITTING
114	CONTINUE
c
c Initially all sets used
	do i=1,nset
	   jmiss(i)=1	!miss all
	enddo
	do i=1,nset
	   juse(i)=i	!use all
	enddo
	print 126,nset
126	format(/,' Fit all',i3,' sets [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
130	   print 127
127	   format('&Number of sets to be fitted = ')
	   call INPUTi(if)
	   if(if.lt.0.or.if.gt.nset) goto 130
	   nmiss=nset-if
	   if(nmiss.gt.0) then
		do j=1,if
		   print 129,j
129		   format('&  (',i2,')  fit set number = ')
	         call INPUTi(j1)
		   if(j1.lt.1.or.j1.gt.nset) goto 130
		   jmiss(j1)=0		!fit set j1
		enddo
	   endif
	else		!fit all
	   nmiss=0
	   do i=1,nset
		jmiss(i)=0	!fit all
	   enddo
	endif
c Define nsfit and juse()
	nsfit=nset-nmiss
	j=0
	do i=1,nset
	   if(jmiss(i).eq.0) then
		j=j+1
		juse(j)=i
	   endif
	enddo
c
c If data not printed above then print it here for the fitted sets
c (jump to here if there is only one set)
98	continue
c Calculate the arrays of times
	nmax=0
	do j=1,nset
	   n=ncomp(j)*npt(j)
	   if(n.gt.nmax) nmax=n
	enddo
c ===need to return to main prog to allocate time() -just declare time(1000)
c for now!
c	ALLOCATE(times(nmax,10))	!ith time for jth set
c Define these arrays.
c==use 5tau or better use 4tau-to avoid too many heavily weighted small values?
	taumul=5.0	!go up to 4 or 5*tau for each time constant
	print 28,taumul
28	format(' Use times out to n time constants: n [',f5.1,'] = ')
	call INPUTr(taumul)
	if(pon()) write(7,29) taumul
	if(discprt) write(8,29) taumul
29	format(' Use times out to ',f5.1,' time constants')
c
	do j=1,nset
	   ntime(j)=0
	   n=npt(j)
	   en=float(n)
	   i1=0
	   do m=1,ncomp(j)
		delt=tau(m,j)*taumul/en
		if(m.eq.1) then
		   ic=1
		else
		   ic=2 + ifix((en-1.)*tau(m-1,j)/tau(m,j))
		endif
		do i=ic,n
		   i1=i1+1
		   times(i1,j)=float(i-1)*delt
		enddo
	   enddo
	   ntime(j)=i1
c
c Calculate the 'observations' -Yobs(1000,20)=pdf/jump at these times
c calc from the observed tau(),area() -straightforward except for pulse
c which has rising and falling phase -so far only decay has been entered!
c need to add to 'data' the rising phase, or at least tpeak, for
c the pulse response (idtype=1)
	   if(idtype(j).eq.1) then	!pulse -macroscopic
		do i=1,ntime(j)
		   s=0.0
		   t=times(i,j)
		   do m=1,ncomp(j)
			amp=area(m,j)                       !'area'=amp in pA
c			s=s + amp*exp(-t/tau(m,j))	!'area'=amp in pA
c			s=s + amp*exp1(-t/tau(m,j))	!'area'=amp in pA -underflows!
			s=s + AEXPS1(amp,-t/tau(m,j))	!avoid underflow
		   enddo
		   Yobs(i,j)=s + Yinf(j)
c========rising phase to be added for pulse!!!
		enddo
	   else if(idtype(j).eq.2) then	!step -macroscopic
		do i=1,ntime(j)
		   s=0.0
		   t=times(i,j)
		   do m=1,ncomp(j)
			amp=area(m,j)                       !'area'=amp in pA
c			s=s + amp*exp1(-t/tau(m,j))
			s=s + AEXPS1(amp,-t/tau(m,j))	!avoid underflow
		   enddo
		   Yobs(i,j)=s + Yinf(j)
		enddo
	   else if(idtype(j).ge.3.and.idtype(j).le.ntype) then	!pdf
		do i=1,ntime(j)
		   s=0.0
		   t=times(i,j)
		   do m=1,ncomp(j)
		      a=area(m,j)/tau(m,j)
			s=s + a*exp1(-t/tau(m,j))
		   enddo
		   Yobs(i,j)=s
		enddo
c	   else if(idtype(j).ge.10.and.idtype(j).le.12) then	!geometric
c          In this case times(i,j) contains NOT times but, for
c	     example 'number of ops/bst' and tau(m,j) are 'means' of components
c		do i=1,ntime(j)
c		   s=0.0
c		   x=times(i,j)         !ops/bst etc
c		   do m=1,ncomp(j)
c		      a=area(m,j)/tau(m,j)          !tau='mean'
c			z=1.0 - (1.0/tau(m,j))
c			s=s + a*(z**(x-1.0))
c		   enddo
c		enddo
	   endif
c
	enddo		!end of j=1,nsfit loop
c
	if(.not.printed) then
	   print 322,titlef
	   if(pon()) write(7,322) titlef
	   if(discprt) write(8,322) titlef
c322	   format(' File: ',a60)
	   do j=1,nset
		if(jmiss(j).eq.0) then
		   call PRTJ(j,titype,pon(),idtype,xA0,xA,xA1,
     &	    tpulse,nstau,Yinf,ncomp,Vmem,tau,area,npt,iver)
		endif
	   enddo
	endif
c
c Define weights (weighting method printed below, outside loop)
c Try modif of 1/yobs**2 method -use this only if yobs>1.e-3; when
c yobs<1.e-3 set weight =10**6 (value for yobs=1.e-3)
	do j1=1,nsfit
	   j=juse(j1)
	   do i=1,ntime(j)
c==	      weight(i,j)=1000.
c==	      weight(i,j)=1.0/(Yobs(i,j)*Yobs(i,j))
		y2=Yobs(i,j)*Yobs(i,j)
c==		if(y2.gt.1.e-6) then
		if(y2.gt.1.e-12) then
	         weight(i,j)=1.0/y2
		else
c==	         weight(i,j)=1.0e6
	         weight(i,j)=1.0e12
		endif

	   enddo
	   if(debug()) then
	      print 25,j
      	if(discprt) write(8,25) j
25		format(' Set ',i3,/,
     &      '    i       time(i)       Yobs(i)    weight(i)')
		do i=1,ntime(j)
		   print 24,i,times(i,j),yobs(i,j),weight(i,j)
      	   if(discprt) write(8,24) i,times(i,j),yobs(i,j),
     &		weight(i,j)
24		   format(1x,i8,3x,3g13.6)
		enddo
	   endif
	enddo
	print 27
      if(discprt) write(8,27)
c==27	format(/,' Weights = constant',/)
c==27	format(/,' Weights = 1/(Yobs*Yobs)',/)
c==27	format(/,' Weights = 1/(Yobs*Yobs), but max weight = 1.e6',/)
27	format(/,' Weights = 1/(Yobs*Yobs), but max weight = 1.e12',/)
c
c For all subsequent calculations need to convert conc to molar
	do i=1,20
	   xA0(i)=1.e-6*xA0(i)
	   xA(i)=1.e-6*xA(i)
	   xA1(i)=1.e-6*xA1(i)
	enddo
	call flush(7)
	RETURN
	END

