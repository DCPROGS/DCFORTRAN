	subroutine SIMJUMP(tint,iampl,iprops,tjmax,pisim,nsim,tsim,
     & QT,icur,ndim,nt1,kt,ktmax,vhold,vref,ligname,
     & concb,vkin,ilast,icalc,cmax1,tpulse,tdec,trise,njump)
c==	subroutine SIMJUMP(tint,iampl,iprops,tjmax,pisim,nsim,tsim,
c==     & cA1,cB1,QD,icur,ndim,nt1,kt,ktmax,
c==     & xa11,vkin,ilast,icalc,cmax1,tpulse,tdec,trise,njump)

c===TO DO
c==(7) better still would be to write like CJUMP.DAT file, as series
c==    of separate jumps, so sigle channel jumps could be averaged in CJFIT
c==(8) put nsoj() etc into SIMSC for steady state -and graphics too, inc
c==    state transition diagram
c
c To do simulations of jump experiments in SCSIM
c
c Modif for 100 state/10 ligand 11/14/01 11:32am
c 	common/cconc/ split into 2 so conc0 etc can be common with main prog
c	common/cconc1/conc0(10),conc1(10),conc2(10),nljump !for funcd etc and main
c	common/cconc2/step,pulse,varconc,trise2,tdec2,tpulse2,
c     & cmax,nshape,u,arat,brat,tabs		!for funcd etc
c    NB does not matter which conc() array is used for time dep conc
c    returned by CALCONC, because at present
c    conc of all ligands other than #nljump are same throughout
c    (No need for this -CALCONC could be fixed to return an array, conc(10),
c    that contains all conc at time t)
c
c Don't know how many intervals per jump, so cannot easily specify
c  the number of jumps in advance -therefore simulate transitions up to
c  the max number specified, and throw out the last (incomplete) jump
c Each jump ends when eiher (a) get trapped in a shut state with infinite
c lifetime, or (b) reach the maximum duration specified for the jump (tjmax)
c
c    Simplest method would be to have 1 channel only but average jumps; can
c see effect of having n-channel patch by having a separate stage at the
c end in which the simulated 1-channel jumps are added 2, 3,.. at a time
c before writing to scandat.dat.  Could also average ALL the jumps and queue
c the record for fitting in CJFIT
c
c No need to ask about resolution -this can be done in ekdist.  The intervals
c should completely fill a jump (t=0 to tjmax), and total length is unaffected
c by resolution so must get enough transitions to fill each jump regardless
c of resolution
c
c Outermost loop= next jump (goto 400)
c middle loop-next channel for current jump (goto 200)
c innermost loop= next interval (goto 14)
c
c  Mark the first interval in each sweep by setting bit 5 ='32' in iprops
c   and mark the last interval in each sweep by setting bit 6 ='64',
c   i.e. iprops(kt)=IBSET(iprops(kt),6)
c
c New addition:
c For each observable sojourn, keep
c (a) the number of times each individual state is visited, in nsoj(kt,k)
c     kt=1,...,kt, k=1,...,k=number of states
c (b) the total time spent in each individual state, in tsoj(kt,k)
c
c NOTES from SIMSC
c Modif 12/23/89 08:57pm. TINT, iAMPL enlarged, and tint1,tint2 at
c present only same size as tint (rather
c than 5 times bigger to allow for elim of short events when resolution
c imposed). See def of ndim,ndim1 below. And TINSIM,ISTATE removed (used
c before to display indiv state transitions at end).
c*Modif Sept 87 to
c	(a) add N independent channels (N=nchan)
c	(b) impose specified resolution to each channel while
c		going along so get Ktmax USABLE intervals (otherwise
c		models that generate many very short intervals will
c		lose most of data when resolution imposed in EKDIST)
c
	real*4 tint(ndim)
	integer*2 iampl(ndim)		!allocated in main
	integer*1 iprops(ndim)
	allocatable:: Amat,eigen0
	real*8 Amat(:,:,:),eigen0(:)
	real*4 conc00(10),conc0(10),conc1(10),conc2(10),concb(10)
c   conc00=conc with which equilibrated before xA0 when not at equilib at t=0
c   conc0=concentrations before t=0
c   conc1=conc after t=0 (during pulse, when pulse=true)
c   conc2=conc after end of pulse (when pulse=true)
c
	allocatable::nsoj,tsoj
	integer nsoj(:,:),nsoj0(100),nj(100)
	real*4 tsoj(:,:),tsoj0(100),tj(100)
c to hold duration and state # for all trans in 1 jump for state trans diagram:
	allocatable::itsoj,ttsoj
	integer itsoj(:)
	real*4 ttsoj(:)
	logical disptrans,alpha,help,calcurv
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*11 cnum1,cnum2
	common/hlp/help		!for QDIALOG
c
	allocatable:: tval,totcur
	allocatable:: tins,iamps
	allocatable:: tint1,iamp1
	allocatable:: kjumps,nij
	real*4 tval(:),totcur(:),tins(:),tint1(:)
	integer*2 iamps(:),iamp1(:)
	integer*4 kjumps(:),nij(:)
	character*20 ligname(10)
c
	real*4 arate(100),brate(100)	!conc-dep and conc-indep rates
	logical cdep(100,100),allcdep(100),notcdep(100),infin(100)		!make allocatable?
	logical pulse,step,varconc,infinite,endjump,equal
	logical open,shut,notend,add,null,btest
	real*4 pisim(100,100),tsim(100)
	real*4 amean(100)
	real*4 Q(100,100),pi(100,100),picum(100,100)
	REAL*8 QT(100,100),Q1(100,100),p0(100)
	real*8 tt,bm,p00(100),pinf(100),pt(100)
C QM IS TO HOLD MINORS OF Q IN EQOCC. SEEMS THAT IT MUST BE
C DECLARED IN MAIN PROG FOR CORRECT CALCS (SEE COMMENTS IN EQOCC)
c
	real*4 pcum(101),p0s(100)		!for INSTAT
	integer ist(100)		!for INSTAT
	integer*2 icur(100,100),ilast
	integer nsim(100)
	character*1 ans
	logical KBHIT		!Utility lib
	character*1 ch,getch
	LOGICAL debug,debon,deb
	logical caplock,fixed
	logical discprt
	common/dp/discprt
	COMMON/KBLK/KA,KB,KC,KD
	COMMON/dimblk/K,KMAX,ir,nmod
	integer*2 iacrit
	common/RBLCK/treso,tresg,avamp,iacrit
	common/rand/ix1,iy1,iz1
c Commons for getqd
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/LIG/nlig,IL(100)
c
	common/cmax/trise1,tdec1				!for func1
c	common/cconc/step,pulse,varconc,trise2,tdec2,tpulse2,
c     & cmax,xa0,xa1,xa2,nshape,u,arat,brat,tabs	!for funcd
c 	common/cconc/ split into 2 so conc0 etc can be common with main prog
	common/cconc1/conc0,conc1,conc2,nljump !for funcd etc and main
	common/cconc2/step,pulse,varconc,trise2,tdec2,tpulse2,
     & cmax,nshape,u,arat,brat,tabs				!for funcd etc
	common/rts/itb,itn		!for rtsafe
c
	EXTERNAL func,func1,funcd
C
	debug()=caplock()
c
	km=100		!dimension of QD etc
	alpha=.true.
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
c	nchan=1		!throughout
	ichan=1		!throughout
c
	ALLOCATE(nsoj(ndim,k),tsoj(ndim,k))
	do i=1,ndim
	   do j=1,k
		nsoj(i,j)=-1	!initialise (for debug)
	   enddo
	enddo
c
c Define concentrations as follows (not same as scalcs)
c   conc00=conc with which equilibrated before xA0 when not at equilib at t=0
c   conc0=concentrations before t=0
c   conc1=conc after t=0 (during pulse, when pulse=true)
c   conc2=conc after end of pulse (when pulse=true)
c       (needed only for calc of p(0))
c
33	continue

	nljump=1
	if(nlig.gt.1) then	!show the ligands
	   do i=1,nlig
		print 651,i,ligname(i)
651		format(' Ligand (',i2,'): ',a10)
	   enddo
	endif
	print 601,nljump
601	format(
     &     ' Which ligand conc to be jumped: ligand # [',i2,'] = ')
	call INPUTi(nljump)
c     Now get fixed conc of other ligands, if any
	if(nlig.gt.1) then
	   do i=1,nlig
		if(i.ne.nljump) then	!all the rest
		   x=1.e6*conc0(i)
		   print 669,i,ligname(i),x
669		   format(' (Fixed) conc of ligand ',i2,' (',
     &	     a10,') throughout (muM) [',g10.4,'] = ')
		   call INPUTr(x)
		   conc0(i)=1.e-6*x
		   conc1(i)=1.e-6*x	!same throughout
		   conc2(i)=1.e-6*x	!same throughout
		endif
	   enddo
	endif
c
	if(icalc.lt.1.or.icalc.gt.3) icalc=1	!if not defined in scsim.ini
	print 29,vkin,xa1*1.e6,icalc
29	format(/,
     & '  V = ',f8.1,' mV;  c=',g13.6,' microMolar',/,
     & ' Simulate channels following:',/,
     &' (1) step change in concentration',/,
     &' (2) rectangular concentration pulse',/,
     &' (3) time-dependent (exponential) concentration change',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(icalc)
	step=.false.
	pulse=.false.
	varconc=.false.
	xA0=0.0	!default
	nlig=1
	if(icalc.eq.1) then
	   step=.true.
	   xA0=conc0(nljump)*1.e6
	   print 291,ligname(nljump),xA0
291      format(
     & '&Concentration of ',a10,' before the step (muM) [',f8.3,'] = ')
	   call INPUTr(xA0)
	   conc0(nljump)=xA0*1.e6
c
	   xA1=conc1(nljump)*1.e6
	   print 292,xA1
292      format(
     & 	'&Concentration after the step (muM) [',f8.3,'] = ')
	   call INPUTr(xA1)
	   conc1(nljump)=xA1*1.e6
	   print 36,xa0,xa1
	   if(discprt) write(8,36) xa0,xa1
36	   format(/,' Step at t=0 from ',g13.6,' to ',g13.6,' micromolar')
	else if(icalc.eq.2) then
	   pulse=.true.
	   xA0=conc0(nljump)*1.e6
	   print 312,ligname(nljump),xA0
312      format(
     & '&Concentration of ',a10,
     &	' before the pulse (muM) [',g10.4,'] = ')
	   call INPUTr(xA0)
	   conc0(nljump)=xA0*1.e6
c
	   xA1=conc1(nljump)*1.e6
	   print 313,xa1
313	   format(
     &    '& Concentration during pulse (micromolar) [',f8.3,'] = ')
	   call INPUTr(xA1)
	   conc1(nljump)=xA1*1.e6
c
	   xA2=conc2(nljump)*1.e6
	   print 314,xA2
314	   format(
     &   '& Concentration after pulse (micromolar) [',f8.3,'] = ')
	   call INPUTr(xA2)
	   conc2(nljump)=xA2*1.e6
	   if(tpulse.lt.0..or.tpulse.gt.1.e6) tpulse=1.	!if not defined in scsim.ini
	   print 315,tpulse
315	   format(
     &	'& Duration of the pulse (ms) [',f7.2,'] = ')
	   call INPUTr(tpulse)
	   tpulse2=tpulse		!for common
	   print 316,xa0,tpulse,xa1,xa2
	   if(discprt) write(8,316) xa0,tpulse,xa1,xa2
316	   format(/,
     &	' Concentration pulse:',/,
     &	'   start at ',g13.6,' micromolar ',/,
     &	'   then ',g13.6,' ms at ',g13.6,' micromolar ',/,
     &	'   before jumping to ',g13.6,' micromolar ')
c
	else if(icalc.eq.3) then
	   varconc=.true.
c	   xA0=conc0(nljump)*1.e6
c	   xA2=conc2(nljump)*1.e6
	   xA0=0.
	   xA2=0.
	   conc0(nljump)=xA0*1.e-6
	   conc2(nljump)=xA2*1.e-6
251	   nshape=1
	   print 249,nshape
249	   format(
     & ' At present concentration starts from, and decays to, zero',/,
     & ' Transmitter concentration time-course:',/,
     & ' (1) Exponential rise and fall: exp(-t/tdec)-exp(t/trise)',/,
     & ' (2) Instantaneous rise, exponential fall',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(nshape)
	   if(nshape.lt.1.or.nshape.gt.2) goto 251
	   if(nshape.eq.2) then
		pause 'not yet done'
		goto 251
	   endif
	   if(nshape.eq.1) then
	      if(trise.lt.0..or.trise.gt.1.e6) trise=0.1  !if not defined in scsim.ini
		print 31,trise
31		format(
     &'  Time constant for rise of transmitter conc (ms)  [',f9.2,']= ')
		call INPUTr(trise)
	   endif
	   if(tdec.lt.0..or.tdec.gt.1.e6) tdec=1.	!if not defined in scsim.ini
	   print 32,tdec
32	   format(
     &    '& Time constant for decay, tau1, (ms) [',f9.2,'] = ')
	   call INPUTr(tdec)
	   if(nshape.eq.1.and.tdec.le.trise) then
		call BELL(1)
		print 35
35		format(
     &	' Must have tau(decay) > tau(rise) to calculate',/,
     &	'    exp[-t/tau(dec)] - exp[-t/tau(rise)] -try again')
		goto 33
	   endif
	   tdec1=tdec	!copy for common
	   trise1=trise	!copy for common
	   tdec2=tdec	!copy for common
	   trise2=trise	!copy for common
	   if(nshape.eq.1) then
		t1=0.0001
		t2=4*tdec
		y=0.0		!1st deriv in func=0 at min/max
		epsx=0.001		!error=0.001 ms if tau in ms
		epsy=-1.		!ignore
		ndisp=-2		!silent
		call BISEC0(FUNC1,t1,t2,y,tmax,Yout,epsx,epsy,
     &	 nerr1,ndisp,.false.)
		if(nerr1.eq.0) then
		   ftmax=FUNC(tmax)
		   if(debon) then
c		      f0=FUNC(0.0)
			print 1,ftmax,tmax
		      if(discprt) write(8,1) ftmax,tmax
1			format(
     &		' f(tmax) = ',g13.6,' at tmax = ',g13.6,/)
		   endif
		else
		   nerr=nerr1
		   if(debon) then
      		print 5,nerr1
			if(discprt) write(8,5) nerr1
5			format(' ERROR in EXPMAX: nerr1 = ',i5)
		   endif
		endif
		print 42,cmax1
42		format(' Peak concentration (micromolar) [',g13.6,'] = ')
		call INPUTr(cmax1)
		cmax=cmax1/ftmax
		print 43, trise,tdec,cmax1,cmax
	      if(discprt) write(8,43) trise,tdec,cmax1,cmax
43		format(/,
     &	'   conc= cm*(exp[-t/tau(dec)] - exp[-t/tau(rise)])',/,
     &	' tau(rise), tau(dec) (ms) = ',f10.6,3x,f10.6,/,
     &	' peak conc = ',g13.6,' micromolar ( cm = ',g13.6,')')
	   else if(nshape.eq.2) then
		print 42
c42		format(' Peak concentration (micromolar) = ')
		call INPUTr(cmax)
		print 431, tdec,cmax
	      if(discprt) write(8,431) tdec,cmax
431		format(
     &	'   conc= cmax*(exp[-t/tau(dec)]) ',/,
     &	' tau(dec) (ms) = ',f9.6,
     &	' cmax = ',g11.4,' micromolar')
	   endif
c
	   cmax=cmax*1.e-6	!molar
	endif
c
c Now check initial condition -get initial occupancies in p0(j)
	print 293,conc0(nljump)*1.e6
293	format(
     &  ' Is system at equilibrium with ',f10.4,' muM at t=0 [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'Y') then	!at equilibrium
c=	   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   call EQOCCd(Q1,k,k-1,km,p0)	!calc init occs at xA0
c	   if(qprt) then
      	print 50,conc0(nljump)*1.e6,ligname(nljump)
      	if(discprt) write(8,50) conc0(nljump)*1.e6,ligname(nljump)
50		format(/,
     &    ' Q matrix before t=0, at concentration = ',g13.6,' of ',a10)
		call ATYPd(Q1,'  Q(0)  ' ,k,k,km,km)
c	   endif
         print 51,conc0(nljump)*1.e6,ligname(nljump)
         if(discprt) write(8,51) conc0(nljump)*1.e6,ligname(nljump)
51	   format(/,
     & ' Equilib occupancies before t=0, at concentration = ',g13.6,
     &	' of ',a10)
	   do j=1,k
		print 53,j,p0(j)
      	if(discprt) write(8,53) j,p0(j)
53		format(' p0(',i2,') = ',g13.6)
	   enddo
	else		!not at equilib at t=0
	   print 21,conc0(nljump)*1.e6
21	   format(
     &    ' Length of exposure to ',g13.6,' muM before t=0 (ms) = ')
	   call INPUTr(tpre)
	   print 22
22	   format('&Concentration before this (muM) [0.0] = ')
	   xA00=conc00(nljump)*1.e6
	   call INPUTr(xA00)
	   conc00(nljump)=xA00*1.e-6
         if(discprt) write(8,23) xa0*1.e6,tpre,xA00
23	   format(
     &    ' Initial condition for conc jump not at equilibrium:',/,
     &    ' at ',g13.6,' muM for ',g13.6,' ms, starting from ',g13.6,
     &    ' muM')
c      Now calculate p0(j) for these values
	   call QSETD(conc00,IL,V1,QT,Q1,k,.false.)
	   call EQOCCd(Q1,k,k-1,km,p00)	!calc init occs at xA00
c       p00(j) contains initial occ before tpre ms at xA0
	   call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   ALLOCATE(Amat(km,km,km),eigen0(km))	!km=100 now
	   call QMAT5(Q1,Amat,k,eigen0,ibad,km,km,km)	!at conc=xA0
	   call EQOCCd(Q1,k,k-1,km,pinf)	!calc init occs at xA00
	   tt=dble(tpre)
	   do j=1,k
		pt(j)=pinf(j)
		do m=1,k-1
		   bm=0.0d0
		   do i=1,k
			bm=bm + p00(i)*Amat(i,j,m)
		   enddo
		   pt(j)=pt(j) + bm*dexp(eigen0(m)*tt)
		enddo
	   enddo
c      copy p(tpre) to p0(j)
	   do j=1,k
		p0(j)=pt(j)
	   enddo
	   DEALLOCATE(Amat,eigen0)
	   print 294,tpre,conc0(nljump)*1.e6
	   if(discprt) write(8,294) tpre,conc0(nljump)*1.e6
294	   format(
     &  ' Occupancies at t=0, after ',f9.4,' ms at ',g10.4,' muM')
	   do j=1,k
		p0s(j)=sngl(p0(j))	!for INSTAT
		print 53,j,p0(j)
      	if(discprt) write(8,53) j,p0(j)
c53		format(' p0(',i2,') = ',g13.6)
	   enddo
	endif
c
c
c  Define arrays for faster definition of which q(i,j) are c-dependent
c Define logical array cdep(i,j), for neatness
c NB if we have more than one ligand we need to define arate(i) as sum
c of the rate constants that depend on (are assoc rate constants for) the
c ligand whose conc varies with time (this will be multiplied by conc
c later so should not have conc in it). Similarly define brate(i) as sum
c of rate constants that do NOT vary with time -this will include dissoc
c and isomerisation rates AND assoc rates (mult by conc) for ligands other
c than #nljump, that are present at constant conc.
	call QSETD(conc0,IL,V1,QT,Q1,k,.false.)	!put constant conc into q1()
	do i=1,k	!loop over rows
	   allcdep(i)=.true.	!all rates out of state i are conc dep
	   arate(i)=0.0
	   brate(i)=0.0
	   do j=1,k
		if(i.ne.j) then
		   cdep(i,j)=.false.
		   do L=1,ncdep
c			if(i.eq.ix(L).and.j.eq.jx(L)) then
			if(i.eq.ix(L).and.j.eq.jx(L).and.IL(L).eq.nljump) then
			   cdep(i,j)=.true.
			   goto 410	!conc dep
			endif
		   enddo
410		   continue
c NB now work with QT not QD
		   if(cdep(i,j)) then
			arate(i)=arate(i) + sngl(qt(i,j))	!NB QT does not include conc!
		   else
			brate(i)=brate(i) + sngl(q1(i,j))	!Q1 has constant conc in it
		   endif
		endif
	   enddo
c====is 1.e-10 suitable substitute for zero?
	   if(brate(i).gt.1.e-10) allcdep(i)=.false.  !at least one rate of state i is not c-dep
	   if(arate(i).lt.1.e-10) notcdep(i)=.true.  !life of state i is not c-dep
	   xA2=conc2(nljump)
	   infin(i)=allcdep(i).and.xA2.lt.1.e-12	!life infinite for event that starts after pulse
	enddo
c
c
c NOW DO SIMULATION
c
	print 331
331	format(' F1 key to END RUN')
	call CLRKB		!Remove any waiting keyboard char: Utility lib
c Start loop to generate transitions
	do i=1,k
	   do j=1,k
		pisim(i,j)=0.	!checks simulated frequencies
	   enddo
	enddo
c
	nds=1000		!up to nds sojourns per jump
	ALLOCATE(itsoj(nds),ttsoj(nds))
c
c Note: KT counts no of open and shut times i.e. no of periods spent
c in states of equal conductance, rather than no of transitions between
c states (counted by NT1) many of which will have equal conductance
	nt1=0		!no of transitions done (whole run)
	ntj=0		!no of transitions done (current jump) (exc infinite/last)
	k1=-1		!so INSTAT asks how to choose init state
	fixed=.false.	!unless set true by INSTAT when start state fixed
c
	call INSTAT(p0s,k,i1,pcum,ist,k1,fixed)
	is=i1			!present state
c
	ans='N'
	call DCASK('Display transition diagram',ans,ans)
	disptrans=ans.eq.'Y'
	if(alpha.and.(.not.disptrans)) call CLS
c
	print 34
34	format(
     & ' Abort run with F2; Caplock on to see values.',/,
     & ' Hit any key to start',/)
	call ANYKEY
c record min and max length of all state sojourns
	omax=0.
	gmax=0.
	omin=1.e37
	gmin=1.e37
	njump=0
	kt=1		!no of intervals done for all jumps (cf ktj, below)
c
c
c START of loop for next jump
400	continue
c   record values in case incomplete jump has to be removed
	if(njump.gt.0) then
	   ktlast=kt-1		!already updated
	   ntlast=nt1
	   njlast=njump
	   if(disptrans) then
		imode=1	!state transition diagram for present jump
		nval=1
	      ALLOCATE(tval(nval),totcur(nval))	!must allocate for call
		ntj=ntj+1	!so last (truncated) value included
		ttsoj(ntj)=tlast	!set last (truncated) sojourn
		itsoj(ntj)=is
		call SIMDISP(ntj,ttsoj,itsoj,nds,njump,
     & 	 kt1,kt2,tint,iampl,ndim,icur,tval,totcur,nval,
     &	 calcurv,imode,alpha,QT,p0,vhold,vref,km)
	      DEALLOCATE(tval,totcur)
		if(debon) then	!simdisp leaves graph on screen
		   alpha=.true.	!so can see debug messages
		   call MODE(3)
		endif
		ans='N'
		if(alpha) then
		   call DCASK('Stop displaying transition diagram',ans,ans)
		else
	 	   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,'Stop displaying transition diagram',
     &		defolt,11,cans)
		   call GETINPa(cans,ans)
		endif
		disptrans=ans.eq.'N'
	   endif
	endif
c Now start new jump
	tabs=0.0	!abs time from start of jump (real*8??)
	ntj=0		!no of transitions done (current jump) (exc infinite/last)
	njump=njump+1
	tint(kt)=0.0	!initialise first interval
c debug
	if(njump.gt.1) then
	   tc0=tjmax*1.e-6	!total time agrees to 1 in a million
	   if(.not.EQUAL(tot1,tjmax,tc0)) then	!debug
		print 73,tot1,tot2,tjmax,tot1-tjmax
		if(discprt) write(8,73) tot1,tot2,tjmax,tot1-tjmax
73		format(' ********ERROR: tot1,tot2,tjmax = ',4g13.6)
	   endif
	   if(.not.EQUAL(tot2,tjmax,tc0)) then	!debug
		print 73,tot1,tot2,tjmax,tot2-tjmax
		if(discprt) write(8,73) tot1,tot2,tjmax,tot2-tjmax
	   endif
	endif
	tot1=0.0		!check sojourn time/jump
	tot2=0.0		!check interval time/jump
c
	if(step) then
	else
	   conc=CALCONC(tabs)	!conc at time=tabs=0.0 here
	   conc1(nljump)=conc	!conc1() is argument for qsetd

	endif
c Need mean lifetimes at current conc -for this case need the conc dep rates
c and the conc-indep rates separately
c     calculate Q at current conc -not needed until picum() is recalculated
c     because already have the rate constants in arate(), brate()
	call QSETD(conc1,IL,V1,QT,Q1,k,.false.)
	do i=1,k	!loop over rows
	   r=abs(q(i,i))
	   if(r.lt.1.e-10) then
		   infin(i)=.true.
	   else
		   infin(i)=.false.
		   amean(i)=1000./r	!mean lives at c=conc(tabs)
	   endif
	enddo
c	do i=1,k	!loop over rows
c	   if(notcdep(i)) then
c		amean(i)=1000./brate(i)		!mean lifetime
c	   else
c		r=arate(i)*conc + brate(i)
c		if(r.lt.1.e-10) then
c		   infin(i)=.true.
c		else
c		   infin(i)=.false.
c		   amean(i)=1000./r	!mean lives at c=conc(tabs)
c		endif
c	   endif
c	enddo
c
c START of loop for next channel in current jump
c200	continue	!return here for 2nd... channels if req
	ktj=1		!no of intervals done on current jump -reset for each jump
	kt1=kt	!record index of 1st interval in this jump
c     initialise for the next interval
	do i=1,k
	   nsoj0(i)=0	!count # of sojourns in each state for next interval
	   tsoj0(i)=0.	!count length of sojourns in each state for next interval
	enddo
	call INSTAT(p0s,k,i1,pcum,ist,k1,fixed)	!for 1st in jump only
	is=i1			!present state
	debon=debug()
	if(mod(njump,50).eq.0) then
	 if(debon) then	!if debon then in alpha mode
         if(discprt) write(8,342) njump,is,kt
	   print 342,njump,is,kt
342	   format(/,
     & ' Jump # ',i5,' (init state = ',i3,'): ',i9,' intervals ....',/)
	 else
	   if(alpha) then
	      if(njump.eq.1) call CLS
	      call LOCATE(0,0)
	      print 342,njump,is,kt
	   else
	      call INTCONV(njump,cnum1)
	      call INTCONV(kt,cnum2)
		call WDIALOG(1,'Jump #'//charnb(cnum1)//': '
     &	 //charnb(cnum2)//' intervals',-11)		!-11 so overwrites
	   endif
	 endif
	endif
	ilast=icur(is,ichan)	!initialise to initial level
	if(debon) then
	   print 70,tabs,conc*1.e6
	   if(discprt) write(8,70) tabs,conc*1.e6
70	   format(' c(',f8.3,') = ',g11.4)
	endif
c
c START OF LOOP

14	continue		!return here for next
	if(mod(nt1,100).eq.0) then	!check if debug on every 100th transition
c NB key F2 has ktype=0, scan code=60 (see \fortran\tkey.for)
	   if(KBHIT()) then
		ch=GETCH(ktype)
		if(ktype.eq.0.and.ichar(ch).eq.60) goto 15	!end run
	   endif
	   deb=debug()
	   if(debon.and.(.not.deb)) then
		call CLS	!clear when debug switched off
	   endif
	   debon=deb
	   if(debon.and.(.not.alpha)) then
		call MODE(3)
		alpha=.true.
	   endif
	endif
	if(mod(nt1,1000).eq.0) then
c	   if(.not.debon) call LOCATE(3,0)
	   if(debon) print 351,nt1,ktj,njump
351	   format(' ',i8,' transitions;',i8,' intervals in jump ',i4)
	endif
c
c  Get lifetime for current state
c  This now depends on conc at the present time
c Pulse is from 0 to tpulse where tpulse is in ms
	nt1=nt1+1
	u=random()		!uniform(0,1)
	if(notcdep(is)) then
	   au=-alog(u)
	   t=amean(is)*au		!no problem when lifetime not cdep
	   goto 252
	endif
	infinite=.false.
	if(step) then
	   au=-alog(u)
	   t=amean(is)*au		!mean is for c=conc(tabs)
	else if(pulse) then
	   if(tabs.gt.tpulse) then	!event starts after end of pulse
		if(infin(is)) then
		   infinite=.true.
		else
		   au=-alog(u)
		   t=amean(is)*au		!mean is for c=conc(tabs)
		endif
	   else				!event starts before end of pulse
	      au=-alog(u)
		t=amean(is)*au		!mean is for c=conc(tabs)
		if(tabs+t.gt.tpulse) then		!event ends AFTER tpulse
		   if(infin(is)) then
			infinite=.true.
		   else
			t1=tpulse-tabs	!time from start of event to end of pulse
			au=-alog(u)
			t=amean(is)*(au - t1*arate(is)*(xA0-xA1))	!see simulat.doc
		   endif
		endif
	   endif
	else if(varconc) then
c        Rather than using umin for infinite lifetime, calculate
c        the value (from eq.11 in simulat.doc) for tmax=1.e7 ms (or
c	   whatever.  Similarly calc umax for t=tmin=1.e-5 ms. If u
c        is outside these limits can set life=infinte or =tmin without
c	   doing RTSAFE (and can omit 1st two func evaluations in RTSAFE_
	   tmin=0.00001	!ms =0.001 mus
	   tmax=1.e7	!ms
	   if(nshape.ne.1) then
		pause 'Only nshape=1 defined so far'
		stop
	   endif
c       NB if brate(is) is not zero then life is never infinite
	   arat=0.001*arate(is)	!for common -MUST BE IN 1/ms
	   brat=0.001*brate(is)	!for common -ditto
	   if(allcdep(is)) then
	      x1=arat*cmax*(tdec*exp(-tabs/tdec)*(1.-exp1(-tmin/tdec)) -
     &	  trise*exp(-tabs/trise)*(1.-exp1(-tmin/trise)))
	      umax=exp1(-x1)		!for life -> tmin
	      x2=arat*cmax*(tdec*exp(-tabs/tdec)*(1.-exp1(-tmax/tdec)) -
     &	trise*exp(-tabs/trise)*(1.-exp1(-tmax/trise)))
	        umin=exp1(-x2)		!for life -> tmax
	   else
		umin=0.
		umax=1.
	   endif
c	   x0=arate(is)*cmax*(tdec*exp(-t/tdec) - trise*exp(-t/trise))
c	   if(x0.gt.87.) then
c		umin=0.0
c	   else
c	      umin=exp(-x0)	!for life -> infinity
c	   endif
	   if(u.ge.umax) then
		t=tmin
	   else if(u.le.umin) then
		infinite=.true.
	   else		!solve for lifetime using u (in common) from above
		if(infin(is)) then
		   x0=1.0		!1 ms
		else
		   x0=amean(is)
		endif
		xstep=1.		!initial step
		xacc=0.001		!accuracy = 1 microsec
		x1=tmin		!outer limits
		x2=tmax		!outer limits
c          set omit=true because should not need initial check now
	      t=RTSAFE(funcd,x1,x2,xacc,nerr,x0,xstep,.true.)
c	      t=RTSAFE(funcd,x1,x2,xacc,nerr,x0,xstep,.false.)
		if(nerr.ne.0) then
		   print 82,nerr
		   if(discprt) write(8,82) nerr
82		   format(' ****ERROR ',i3,' in RTSAFE')
		endif
		if(debon) then
		   print 83,itb+itn,itb,itn
		   if(discprt) write(8,83) itb+itn,itb,itn
83		format(1x,i4,' iterations: ',i4,' bisection, ',i4,' Newton')
		endif
	   endif
	endif
252	continue	!jump here if lifetime not c-dep
c
c  Update time elapsed from t=0, so it is now time at the END of above sojourn
c   NB t not defined if infinite, so don't use to updatt tabs!
c Don't update tabs until decide whether this sojourn (length=t) is part of the
c current (#kt) interval, or is the first sojourn of a new interval
c If the last sojourn (length=t) is part of the present interval #kt), and its
c end (at tabs) extends beyond tjmax, then the present interval is the last in
c the jump.
c If it is the first sojourn of the next interval and tabs>tjmax then
c next interval is the last in the jump, so before going on must count it
c as the next (truncated) interval
c   Reached end of jump?
	if(infinite.or.tabs+t.ge.tjmax) then	!end so no need to define tabs
	   endjump=.true.
	   tsav=t
	   t=tjmax-tabs	!so last interval goes up to tjmax
	   tabs=tabs + t		!update absolute time (if t defined)
	else
	   tabs=tabs + t		!update absolute time (if t defined)
	   endjump=.false.
	   ntj=ntj+1   !no of transitions done (current jump) (exc infinite/last)
	   itsoj(ntj)=is
	   ttsoj(ntj)=t
	endif
	tot1=tot1+t		!check sojourn time/jump
c
c  Check if level has changed
c   -if not, accumulate times for equal currents but do not increment kt
c (for first transition always accumulates -ilast set to initial state)
c141	continue
	if(icur(is,ichan).eq.ilast) then
	   tint(kt)=tint(kt)+t		!continue accumulating tint(kt)
c
	   if(.not.endjump) then	!exclude truncated values
		nsoj0(is)=nsoj0(is)+1
		tsoj0(is)=tsoj0(is)+t
		if(is.le.kA) then
		   if(t.gt.omax) omax=t
		   if(t.lt.omin) omin=t
		else
		   if(t.gt.gmax) gmax=t
		   if(t.lt.gmin) gmin=t
		endif
	   endif
c Type the individual sojourn just defined
c Note that ACAL now set to 1.0 and 'amplitude' stored in ICUR(i,j) (see SCSIM)
c   Print the last (truncated) sojourn
	   if(debon) then
		if(infinite) then
		   print 172,nt1,is,t,icur(is,ichan)
	         if(discprt) write(8,172)nt1,is,t,icur(is,ichan)
172		   format(i10,2x,i6,2x,g13.6,i5)
		   if(endjump) then
			print 1721
			if(discprt) write(8,1721)
1721		      format('& (truncated from infinity)')
		   endif
		else
		   print 173,nt1,is,t,icur(is,ichan)
	         if(discprt) write(8,173)nt1,is,t,icur(is,ichan)
173		   format(i10,2x,i6,2x,g13.6,i5)
		   if(endjump) then
			print 1731,tsav
			if(discprt) write(8,1731) tsav
1731		      format('& (truncated from ',g13.6,')')
		   endif
		endif
c		if(infinite) then
c		   print 71,nt1,is,icur(is,ichan)
c		   if(discprt) write(8,71) nt1,is,icur(is,ichan)
c71		   format(i10,2x,'infinite life in ',i3,2x,i5)
c		else
c		   print 17,nt1,is,t,icur(is,ichan)
c	         if(discprt) write(8,17)nt1,is,t,icur(is,ichan)
c17		   format(i10,2x,i6,2x,g13.6,i5)
c		endif
	   endif
	   tlast=t
	   if(endjump) then	!set amp, props
		iampl(kt)=icur(is,ichan)
		tot2=tot2+tint(kt)	!check interval time/jump
c NB if null, both bits 5 and 6 set here
		iprops(kt)=0
		if(ktj.eq.1) then	!mark first interval in jump
		   iprops(kt)=IBSET(iprops(kt),5)	!set bit 5 ='32'
		endif
		iprops(kt)=IBSET(iprops(kt),6)	!set bit 6='64'
		ktjlast=ktj	!transition at end of latest jump
		kt2=kt	!transition at end of latest jump
c         print the last (truncated) sojourn, and last (truncated) interval
		if(debon) then
		   t2=tabs		!time for end of interval (for debug printout)=tjmax
		   t1=t2-tint(kt)	!time for start of interval (for debug printout)=tjmax
		   print 171,kt,ktj,tint(kt),iampl(kt),
     &		iprops(kt),tabs-tint(kt),tabs
      	   if(discprt) write(8,171) kt,ktj,tint(kt),iampl(kt),
     &		iprops(kt),t1,t2
		endif
c   record the individual transitions for the last interval
		do i=1,k
		   if(nsoj(kt,i).ne.-1) then		!already defined!
			pause
		   endif
		   nsoj(kt,i)=nsoj0(i)
		   tsoj(kt,i)=tsoj0(i)
		enddo
		if(kt.ge.ktmax) then
		   if(endjump) iprops(kt)=IBSET(iprops(kt),6)
		   goto 15		!END RUN
		endif
		kt=kt+1		!ready for the next interval
		tint(kt)=0.0
		if(debon) then
		   print 74
		   if(discprt) write(8,74)
74		   format(' Jump ends after >1 transition same level ')
		endif
		goto 400	!start next jump
	   endif
	else
c Last interval completed -record it and start the next
c	   iampl(kt)=icur(is,ichan)
	   iampl(kt)=ilast
	   iprops(kt)=0
	   ilast=icur(is,ichan)
	   tot2=tot2+tint(kt)	!check interval time/jump
	   if(ktj.eq.1) then	!mark first interval in jump
		iprops(kt)=IBSET(iprops(kt),5)	!set bit 5 ='32'
	   endif
c   record the individual transitions for the last interval
	   do i=1,k
	      if(nsoj(kt,i).ne.-1) then		!already defined!
		   pause
		endif
		nsoj(kt,i)=nsoj0(i)
		tsoj(kt,i)=tsoj0(i)
	   enddo
c         type the interval just completed
	   if(debon) then
		print 171,kt,ktj,tint(kt),iampl(kt),
     &		iprops(kt),tabs-t-tint(kt),tabs-t
      	if(discprt) write(8,171) kt,ktj,tint(kt),iampl(kt),
     &		iprops(kt),tabs-t-tint(kt),tabs-t
171		format(
     &	 ' Interval #',i6,'(',i5,')',2x,g13.6,i6,2x,i3,
     &	 ' t = ',g10.3,' - ',g10.3,/)
	   endif
c
c Type the individual sojourn just defined -this is part of next interval,
c so type it after interval above
c Note that ACAL now set to 1.0 and 'amplitude' stored in ICUR(i,j) (see SCSIM)
	   if(debon) then
		if(infinite) then
		   print 172,nt1,is,t,icur(is,ichan)
	         if(discprt) write(8,172)nt1,is,t,icur(is,ichan)
c172		   format(i10,2x,i6,2x,g13.6,i5)
		   if(endjump) then
			print 1721
			if(discprt) write(8,1721)
c1721		      format('& (truncated from infinity)')
		   endif
		else
		   print 173,nt1,is,t,icur(is,ichan)
	         if(discprt) write(8,173)nt1,is,t,icur(is,ichan)
c173		   format(i10,2x,i6,2x,g13.6,i5)
		   if(endjump) then
			print 1731,tsav
			if(discprt) write(8,1731) tsav
c1731		      format('& (truncated from ',g13.6,')')
		   endif
		endif
	   endif
	   tlast=t
	   if(.not.endjump) then	!exclude truncated values
		nsoj0(is)=nsoj0(is)+1
		tsoj0(is)=tsoj0(is)+t
		if(is.le.kA) then
		   if(t.gt.omax) omax=t
		   if(t.lt.omin) omin=t
		else
		   if(t.gt.gmax) gmax=t
		   if(t.lt.gmin) gmin=t
		endif
	   endif
c
c If tjmax exceeded (or infinite) then the 1st sojourn at the new level
c counts as a new (truncated) interval that extends up to tjmax
c NB if kt already equal to ktmax so skip to 15 below, then must reset
c endjump to false, because the next interval (to which it applies)
c does not materialise
	   if(endjump) then
	      if(kt.ge.ktmax) then
		   endjump=.false.
		   goto 15		!END RUN
		endif
		kt=kt+1
		ktj=ktj+1
		tint(kt)=t
		iampl(kt)=icur(is,ichan)
		if(ktj.ne.1) iprops(kt)=0	!clear iprops
		iprops(kt)=IBSET(iprops(kt),6)	!set bit 6='64'
		if(debon) then
		   print 171,kt,ktj,tint(kt),iampl(kt),
     &		iprops(kt),tabs-tint(kt),tabs
      	   if(discprt) write(8,171) kt,ktj,tint(kt),iampl(kt),
     &		iprops(kt),tabs-tint(kt),tabs
		endif
c   record the individual transitions for the last interval
		do i=1,k
	   	   if(nsoj(kt,i).ne.-1) then		!already defined!
			pause
		   endif
		   nsoj(kt,i)=nsoj0(i)
		   tsoj(kt,i)=tsoj0(i)
		enddo
		tot2=tot2+tint(kt)	!check interval time/jump
		ktjlast=ktj	!transition at end of latest jump
		kt2=kt	!transition at end of latest jump
	   endif
c	   print 108
c	   if(discprt) write(8,108)
c108	   format(/)
c      Now re-initialise for the next interval
	   if(kt.ge.ktmax) then
		if(endjump) iprops(kt)=IBSET(iprops(kt),6)
		goto 15		!END RUN
	   endif
	   kt=kt+1
	   if(endjump) then
		if(debon) then
		   print 75,is
		   if(discprt) write(8,75) is
75		   format(' Jump ends after transition to ',i3)
		endif
		goto 400		!next jump (after incrementing kt)
	   endif
c Get here if last transition ended an interval, but did not end the jump
c so t must be not infinite, or trencated, and forms 1st sojourn of next interval
	   ktj=ktj+1
	   tint(kt)=t	!initialise next interval
	   do i=1,k
	     nsoj0(i)=0	!count # of sojourns in each state for next interval
	     tsoj0(i)=0.	!count length of sojourns in each state for next interval
	   enddo
	   nsoj0(is)=nsoj0(is)+1
	   tsoj0(is)=tsoj0(is)+t
	   if(is.le.kA) then
		if(t.gt.omax) omax=t
		if(t.lt.omin) omin=t
	   else
		if(t.gt.gmax) gmax=t
		if(t.lt.gmin) gmin=t
	   endif
	endif
c NB skip to 400 for next jump, so last interval in each jump excluded form
c nsim,tsim
	nsim(is)=nsim(is)+1
	tsim(is)=tsim(is)+t
c
c  Which state next?
c  First recalculate picum() at the time, tabs, just defined
c  NB does not matter which conc() array is used here because at present
c conc of all ligands other than #nljump are same throughout
c (No need for this -CALCONC could be fixed to return an array, conc(10),
c  that contains all conc at time t)
	conc=CALCONC(tabs)	!conc at time=tabs
	conc1(nljump)=conc
	if(debon) then
	   print 70,tabs,conc*1.e6
	   if(discprt) write(8,70) tabs,conc*1.e6
c70	   format(' c(',f8.3,') = ',g11.4)
	endif
c     calculate Q at current conc; needed to calculate pi(), picum()
c=	call QNEWC(QD,cA1,cB1,Q1,conc,xB0,ncdep,nlig,IL,IX,JX,k,kmax)
	call QSETD(conc1,IL,V1,QT,Q1,k,.false.)
	do i=1,k
	   do j=1,k
		q(i,j)=sngl(q1(i,j))
	   enddo
	enddo
	do i=1,k	!loop over rows
	   if(notcdep(i)) then
		amean(i)=1000./brate(i)		!mean lifetime
	   else
		r=arate(i)*conc + brate(i)
		if(r.lt.1.e-10) then
		   infin(i)=.true.
		else
		   infin(i)=.false.
		   amean(i)=1000./r	!mean lives at c=conc(tabs)
		endif
	   endif
	enddo
c Calc PI
c NB if state i has infinite lifetime then pi(i,j) not defined, but infin(i)
c was set true above
	do i=1,k
	   if(.not.infin(i)) then
		do j=1,k
	 	   pi(i,j)=-q(i,j)/q(i,i)
		enddo
	   endif
	enddo
c
c Calc cumulative PI matrix
	do i=1,k
	 if(.not.infin(i)) then
	   do j=1,k
		picum(i,j)=0.
		do n=1,j
		   if(n.ne.i) picum(i,j)=picum(i,j)+pi(i,n)
		enddo
	   enddo
	 endif
	enddo
c  now use current picum() to decide which state next
c if current state (is) has infinite lifetime then must stay in state #is
c (on return to 14 the fact that infin(is)=true causes jump to end at once)
	if(.not.infin(is)) then
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
	   is=j
	endif
	goto 14
c END OF LOOP
c
15	continue
c
	DEALLOCATE(itsoj,ttsoj)
c
c NOW ALL DONE
c Unless we have jumped out at exactly the end of a jump, then remove
c the partially-completed jump
	ktsav=kt
	if(.not.endjump) then
	   ktj=ktjlast
	   kt=ktlast
	   nt1=ntlast
	   njump=njlast
	   if((.not.BTEST(iprops(kt),6)).and.
     &   	(.not.BTEST(iprops(kt),5))) then	!NB iprops index=kt, not ktj
		call BELL(3)
		print 296,kt,ktj,njump
296		format(
     &	' Last interval #',i7,' (#',i5,' in jump ',i4,
     &	') should have props=32 or 64!')
	   endif
	endif
c Simplified version for jumps -one channel, no resolution
	n0=0
	n1=0
	do i=1,kt
	   if(BTEST(iprops(i),5).and.BTEST(iprops(i),6)) then
		if(iampl(i).eq.0) then
		   n0=n0+1
		else
		   n1=n1+1
		endif
	   endif
	enddo
      if(debon) print 39,nt1,kt,njump,n0+n1,n0,n1
      if(discprt) write(8,39) nt1,kt,njump,n0+n1,n0,n1
39	format(/,' Simulation of ',i8,' transitions, giving',/,
     & '  ',i8,' intervals in ',i6,' jumps',/,
     & ' Number of nulls = ',i5,', of which ',i5,
     & ' shut throughout and ',i4, ' open throughout')
	if(.not.endjump) then
         if(debon) print 391,ktsav
         if(discprt) write(8,391) ktsav
391	   format(' (',i8,
     &	'intervals altogether, but incomplete jump discarded)')
	endif
c
c calc simulated Popen
	to=0.
	do 165 i=1,ka
165	to=to + tsim(i)
	ts=0.
	do 166 i=ka+1,k
166	ts=ts + tsim(i)
	Ttot=to+ts
      if(discprt) write(8,167)to,ts,to+ts,to/Ttot
	if(debon) print 167,to,ts,to+ts,to/Ttot
167	format(/,' Simulated: total time, open, shut, (ms) = ',
     & 3g13.6,/,'           Popen= ',g13.6)
c
	n=0
	do i=1,k
	   n=n+nsim(i)
	enddo
	call EQOCCd(Q1,k,k-1,km,pinf)	!calc init occs at xA0
	if(debon) print 164,n
	if(discprt) write(8,164) n
164	format(/,
     & '  SUMMARY ',/,
     & ' No of state sojourns = ',i9,/,
     &' (exc last, truncated, sojourn of last interval in each jump)',/,
     &'    (for conc-dep states, ''true'' values are for final conc)',/,
     & ' State  # of sojourns    Mean life',14x,' Occupancy',/,
     & 17x,'    simulated    true        simulated     true')
	do i=1,k
	   if(nsim(i).gt.0) then
		if(debon) print 162,i,nsim(i),tsim(i)/float(nsim(i))
     &	 ,amean(i),tsim(i)/Ttot,pinf(i)
	      if(discprt) write(8,162)i,nsim(i),tsim(i)/float(nsim(i))
     &	 ,amean(i),tsim(i)/Ttot,pinf(i)
162		format(i3,1x,i10,4x,4g13.6)
	   else
		if(debon) print 1621,i,nsim(i),
     &	 amean(i),tsim(i)/Ttot,pinf(i)		!if no sojourns
	      if(discprt) write(8,1621)i,nsim(i),
     &	 amean(i),tsim(i)/Ttot,pinf(i)
1621		format(i3,1x,i10,4x,'       ?     ',3g13.6)
	   endif
	   if(.not.notcdep(i)) then
      	if(discprt) write(8,1633)
		if(debon) print 1633
1633		format('& (*conc-dep')
	   endif
	enddo
      if(discprt) write(8,1632) nt1
	if(debon) print 1632, nt1
1632	format(
     & ' Total number of sojourns in individual states= ',i12)
c
c Analysis of structure of intervals
	if(debon) print 84, omin,omax,gmin,gmax
      if(discprt) write(8,84) omin,omax,gmin,gmax
84	format(/,
     & ' Open sojourns (ms): min = ',g13.6,' max = ',g13.6,/,
     & ' Shut sojourns (ms): min = ',g13.6,' max = ',g13.6,/)
	omax=0.
	gmax=0.
	omin=1.e37
	gmin=1.e37
	omax1=0.
	gmax1=0.
	omin1=1.e37
	gmin1=1.e37
c Use ktsav rather than kt here, so intervals in the last, incomplete,
c jump are used too
	do i=1,ktsav
	   open=iampl(i).ne.0
	   if(open) then
	      if(tint(i).gt.omax) omax=tint(i)
	      if(tint(i).lt.omin) omin=tint(i)
	   else
		if(tint(i).gt.gmax) gmax=tint(i)
		if(tint(i).lt.gmin) gmin=tint(i)
	   endif
	   notend=.not.BTEST(iprops(i),6)	!omit last (truncated) values
	   if(notend) then
		if(open) then
		   if(tint(i).gt.omax1) omax1=tint(i)
		   if(tint(i).lt.omin1) omin1=tint(i)
		else
		   if(tint(i).gt.gmax1) gmax1=tint(i)
		   if(tint(i).lt.gmin1) gmin1=tint(i)
		endif
	   endif
	enddo
	if(debon) print 841, omin,omax,gmin,gmax
      if(discprt) write(8,841) omin,omax,gmin,gmax
841	format(/,' Open and shut times (including last in the jump)',/,
     & ' Open intervals (ms): min = ',g13.6,' max = ',g13.6,/,
     & ' Shut intervals (ms): min = ',g13.6,' max = ',g13.6,/)
	if(debon) print 842, omin1,omax1,gmin1,gmax1
      if(discprt) write(8,842) omin1,omax1,gmin1,gmax1
842	format(/,' Open and shut times (excluding last in the jump)',/,
     & ' Open intervals (ms): min = ',g13.6,' max = ',g13.6,/,
     & ' Shut intervals (ms): min = ',g13.6,' max = ',g13.6,/)
c    Auto ranges -divide into n log-ranges between min and max
c Must upper limit of ranges on values that include the last interval
c in jump, otherwise sojourns in the last interval will be excluded
c (the last sojourn was not counted in nsoj(), tsoj() anyway but sojourns
c th last interval, other thaan the last sojourn, are included.
	omin=0.99*omin    !so smallest value not missed
	gmin=0.99*gmin    !so smallest value not missed
	omax=1.1*omax
	gmax=1.1*gmax
	if(omin.lt.1.e-4) omin=1.e-4		!ms
	if(gmin.lt.1.e-4) gmin=1.e-4
	n=6
	dyo=10**((alog10(omax)-alog10(omin))/float(n-1))
	do ir=1,n-1
	   op1=omin*(dyo**(ir-1))
	   op2=op1*dyo
	   do j=1,kA
		nj(j)=0
		tj(j)=0.
	   enddo
	   do i=1,ktsav
		open=iampl(i).ne.0
c             (last sojourn in jump already excluded -floolowing line
c		   excludes all sojourns in the last interval ina jump)
c=		notend=.not.BTEST(iprops(i),6)	!omit last (truncated) values
		t=tint(i)
c=		if(open.and.notend.and.t.gt.op1.and.t.le.op2) then
		if(open.and.t.gt.op1.and.t.le.op2) then
		   do j=1,kA		!loop for states
			nj(j)=nj(j)+nsoj(i,j)
			tj(j)=tj(j)+tsoj(i,j)
		   enddo
		endif
	   enddo	!end of i=1,ktsav
	   if(debon) print 44,op1,op2
	   if(discprt) write(8,44) op1,op2
44	   format(/,
     & ' Open time range from ',g13.6,' to ',g13.6,' ms',/,
     & ' state   # of sojourns  total time ')
	   do j=1,kA
		if(debon) print 45,j,nj(j),tj(j)
		if(discprt) write(8,45) j,nj(j),tj(j)
45		format(i6,4x,i9,4x,g13.6)
	   enddo
	   op1=op2		!for next range
	enddo		!end of ranges
c    Repeat for shut times
	dyg=10**((alog10(gmax)-alog10(gmin))/float(n-1))
	do ir=1,n-1
	   gp1=gmin*(dyg**(ir-1))
	   gp2=gp1*dyg
	   do j=kA+1,k
		nj(j)=0
		tj(j)=0.
	   enddo
	   do i=1,ktsav
		shut=iampl(i).eq.0
c=		notend=.not.BTEST(iprops(i),6)	!omit last (truncated) values
		t=tint(i)
c=		if(shut.and.notend.and.t.gt.gp1.and.t.le.gp2) then
		if(shut.and.t.gt.gp1.and.t.le.gp2) then
		   do j=kA+1,k		!loop for states
			nj(j)=nj(j)+nsoj(i,j)
			tj(j)=tj(j)+tsoj(i,j)
		   enddo
		endif
	   enddo	!end of i=1,ktsav
	   if(debon) print 441,gp1,gp2
	   if(discprt) write(8,441) gp1,gp2
441	   format(/,/,
     & ' Shut time range from ',g13.6,' to ',g13.6,' ms',/,
     & ' state   # of sojourns  total time ')
	   do j=kA+1,k
		if(debon) print 45,j,nj(j),tj(j)
		if(discprt) write(8,45) j,nj(j),tj(j)
c45		format(i6,4x,i9,4x,g13.6)
	   enddo
	   gp1=gp2		!for next range
	enddo		!end of ranges

	DEALLOCATE(nsoj,tsoj)
c
c Options to sum records
c  Number of values in summed trace is potentially the total number of channels
c   in all the summed records =kt for all jumps.
c   The time, ie cumulative tint() for the sum, goes into tval() and the
c   amplitude of the summed trace goes into totamp()
c   Easiest if we first find the number of intervals in each jump, and allocate
c   an arrays big enough to hold the max number
	ALLOCATE(kjumps(njump),nij(njump))
	nj1=0
	do i=1,kt
	   if(BTEST(iprops(i),5)) then	!first in jump
		nj1=nj1+1
		kjumps(nj1)=i
	   endif
	enddo
	if(nj1.ne.njump) then
	   call BELL(3)
	   print 76,nj1,njump
76	   format(' ERROR: nj1,njump = ',2i8)
	endif
	do i=1,njump-1
	  nij(i)=kjumps(i+1)-kjumps(i)
	enddo
	nij(njump)=kt-kjumps(njump)+1
	nmax=0
	do i=1,njump
	   if(nij(i).gt.nmax) nmax=nij(i)
	enddo
c Calc mean latency
	nlat=0
	n0=0
	n1=0
	tlat=0.0
	do i=1,njump
	   i1=kjumps(i)
	   shut=iampl(i1).eq.0
	   null=nij(i).eq.1
	   if((.not.null).and.shut) then
		nlat=nlat+1
		tlat=tlat+tint(i1)
	   else if(null) then
		if(shut) then
		   n0=n0+1
		else
		   n1=n1+1
		endif
	   endif
	enddo
	if(nlat.ne.0) then
	   tlat=tlat/float(nlat)
	else
	   tlat=0.
	endif
	if(debon) print 85,njump,nlat,tlat,n0+n1,n0,n1
	if(discprt) write(8,85) njump,nlat,tlat,n0+n1,n0,n1
85	format(/,' ',i6,' jumps: ',/,i6,
     & ' initially shut and non-null with mean latency =  ',
     & g13.6, 'ms',/,
     & ' Number of nulls = ',i5,', of which ',i5,
     & ' shut throughout and ',i4, ' open throughout',/)
c
	if(njump.eq.1) goto 99
c===take out for now =but if want to sum in 2s, 3s etc need question
c	ans='N'
c	call DCASK('Sum two or more jumps',ans,ans)
c	if(ans.eq.'N') goto 99
c461	print 46
c46	format(
c     & ' Number to be summed (0=all, -1=skip) [0] = ')
	iopt=0
c	call INPUTi(iopt)
	if(iopt.lt.0) goto 99
c	if(iopt.gt.1) goto 461
c Now make 'digitised' trace in totamp().  Arrays can be allocated here
c as long as the subroutine to write them as a cjump.dat file
c is called from here
	srate=20000.
	if(alpha) then
	   print 78,srate
78	   format(
     & ' ''Digitisation'' rate for total current (Hz) [',f9.1,'] = ')
	   call INPUTr(srate)
	   ans='Y'
	   call DCASK('Show the theoretical curve too',ans,ans)
	else
 	   call DEFOLTr(srate,defolt)
	   call QDIALOG(1,'Digitisation rate for total current (Hz)',
     &	defolt,11,cans)
	   call GETINPr(cans,srate)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Show the theoretical curve too',
     &	defolt,11,cans)
	   call GETINPa(cans,ans)
	endif
	calcurv=ans.eq.'Y'
	if(alpha) call CLS
	dt=1000./srate				!interval between points (ms)
	nval=ifixr(tjmax/dt)		!tjmax is in ms
c
	if(iopt.eq.0) then
	   goto 100		!skip slow method
	   ALLOCATE(tins(ndim),iamps(ndim))	!for grand total
	   ALLOCATE(tint1(nmax),iamp1(nmax))	!for current jump
c      copy first jump into tins
	   do i=1,nij(1)
		tins(i)=tint(i)
		iamps(I)=IAMPL(i)
	   enddo
	   n1=nij(1)
	   do j=2,njump
c   copy jth jump into tint1
		i1=0
		m1=kjumps(j)
		m2=m1+nij(j)-1
		do m=m1,m2
		   i1=i1+1
		   tint1(i1)=tint(m)
		   iamp1(i1)=IAMPL(m)
		enddo
		n2=nij(j)
		ndim1=ndim
		ndim2=nmax
		nomit=0
		n1sav=n1
	      call SUMSC(tins,iamps,n1,tint1,iamp1,n2,
     &		nomit,ndim1,ndim2,.false.)
c          NB the tins,iamps,n1 output here are input for next cycle
		print 81,n1,(n1sav-1)+(n2-1)+1
81		format(' output ',i8,' values; expect ',i8)
	   enddo	!end of loop for all jumps
	   nt=n1	!number of intervals in the sum
	   DEALLOCATE(tint1,iamp1)	!for current jump
c
c    Now make tval(),totamp -'digitise' the total current in tins, iamps
c   (check total time!)
c  First need cumulative version of tins -tins(1) unchanged
	   do i=2,nt
		tins(i)=tins(i-1)+tins(i)
	   enddo
	   print 77,nt,tins(nt),tjmax
77	   format(
     &	' nt = ',i8,' tins(nt) = ',g13.6,' tjmax = ',g13.6)
c
	   ALLOCATE(tval(nval),totcur(nval))
c first make tval() array (once only)
	   do i=1,nval
		tval(i)=float(i-1)*dt	!what is amplitude at this time
	   enddo
c
	   j1=1
	   do i=1,nval
		t=tval(i)	!what is amplitude at this time
		if(t.le.tins(1)) then
		   totcur(i)=float(iamps(1))
		   goto 79	!jump out
		endif
		do j=j1,nt-1
		   if(tins(j).le.t.and.tins(j+1).gt.t) then
			totcur(i)=float(iamps(j+1))
			j1=j		!for next cycle
			goto 79	!jump out
		   endif
		enddo
c	      call BELL(3)
	      print 80,i
80	      format(' ERROR: totcur(',i5,') should not reach here')
79	      continue
	   enddo
	   DEALLOCATE(tins,iamps)
c Now display tval(), totcur() and queue for cjfit, (and/or write
c as cjump file -not yet done)
	   imode=2	!summed channels
	   nds=1
	   ALLOCATE(itsoj(nds),ttsoj(nds))		!must allocate for call
	   call SIMDISP(ntj,ttsoj,itsoj,nds,njump,
     & 	 kt1,kt2,tint,iampl,ndim,icur,tval,totcur,nval,
     &	 calcurv,imode,alpha,QT,p0,vhold,vref,km)
	   DEALLOCATE(itsoj,ttsoj)
	   DEALLOCATE(tval,totcur)
	endif
c
	pause
c
100	continue	!skip slow method
c  Alternative (faster?) method of summing
c -'digitise' each channel first, then add
c Don't need any array of size ndim for this, just nmax for the jumps and
c nval for the 'digitised' output, so allocate arrays appropriately
	if(iopt.eq.0) then
	   ALLOCATE(tint1(nmax),iamp1(nmax))	!for current jump
	   ALLOCATE(tval(nval),totcur(nval))
c first make tval() array (once only)
	   do i=1,nval
		tval(i)=float(i-1)*dt	!what is amplitude at this time
	   enddo
c
c      copy first jump into tint1,iamp1
	   do i=1,nij(1)
		tint1(i)=tint(i)
		iamp1(I)=IAMPL(i)
	   enddo
c  now digitise the first jump -need cumulative version
	   n1=nij(1)
	   do i=2,n1
		tint1(i)=tint1(i-1)+tint1(i)
	   enddo
	   if(alpha) then
	      print 772,njump
772	      format(' Adding ',i6,' one-channel jumps . . .')
	   else
	      call INTCONV(njump,cnum1)
		call WDIALOG(1,
     &	' Adding '//charnb(cnum1)//' one-channel jumps . . .',11)
	   endif

	   if(debon) then
	      print 771,n1,tint1(n1),tjmax
771	      format(
     &	' n1 = ',i8,' tint1(n1) = ',g13.6,' tjmax = ',g13.6)
	   endif
c   digitise tint1 and allocate output to totcur() to initialise it
	   add=.false.
	   call DIGITISE(tint1,iamp1,n1,nmax,nval,tval,totcur,nval,add)
c   now add in the rest of the jumps
	   do j=2,njump
c   copy jth jump into tint1
		n1=nij(j)
		i1=0
		m1=kjumps(j)
		m2=m1+n1-1
		do m=m1,m2
		   i1=i1+1
		   tint1(i1)=tint(m)
		   iamp1(i1)=IAMPL(m)
		enddo
		do i=2,n1
		   tint1(i)=tint1(i-1)+tint1(i)	!make cumulative form
		enddo
		if(debon) print 771,n1,tint1(n1),tjmax
		add=.true.
		call DIGITISE(tint1,iamp1,n1,nmax,nval,tval,totcur,nval,add)
		if(mod(j,50).eq.0) then
		   if(alpha) then
		      if(j.eq.50) call CLS
		      call LOCATE(0,0)
		      print 343,j
343			format(' Jump #',i8)
		   else
		      call INTCONV(j,cnum1)
			call WDIALOG(1,'Jump #'//charnb(cnum1),-11)	!-11 so overwrites
		   endif
		endif
	   enddo
	   DEALLOCATE(tint1,iamp1)	!for current jump
c Now display tval(), totcur() and queue for cjfit, (and/or write
c as cjump file -not yet done)
	   imode=2	!summed channels
	   nds=1
	   ALLOCATE(itsoj(nds),ttsoj(nds))		!must allocate for call
	   call SIMDISP(ntj,ttsoj,itsoj,nds,njump,
     & 	 kt1,kt2,tint,iampl,ndim,icur,tval,totcur,nval,
     &	 calcurv,imode,alpha,QT,p0,vhold,vref,km)
	   DEALLOCATE(itsoj,ttsoj)
	   DEALLOCATE(tval,totcur)
	endif
c
99	DEALLOCATE(kjumps,nij)
	if(.not.alpha) then
	   call MODE(3)
	   alpha=.true.
	endif
c
	RETURN
	end


	function CALCONC(t)
c To calculate conc at time t IN SIMJUMP (in SCSIM)
c
	logical step,pulse,varconc
c	common/cconc/step,pulse,varconc,trise,tdec,tpulse,
c     & cmax,xa0,xa1,xa2,nshape,u,arat,brat,tabs	!for funcd
c 	common/cconc/ split into 2 so conc0 etc can be common with main prog
	common/cconc1/conc0(10),conc1(10),conc2(10),nljump !for funcd etc and main
	common/cconc2/step,pulse,varconc,trise2,tdec2,tpulse2,
     & cmax,nshape,u,arat,brat,tabs				!for funcd etc
c
	if(step) then
	   if(t.gt.0.) then
		c=conc1(nljump)
	   else
		c=conc0(nljump)
	   endif
	else if(pulse) then
	   tp=tpulse	!in ms
	   if(t.lt.0.0) then
		c=conc0(nljump)
	   else if(t.gt.tp) then
		c=conc2(nljump)
	   else
		c=conc1(nljump)		!during pulse
	   endif
	else if(varconc) then
	   if(nshape.eq.1) then
		c=cmax*(exp(-t/tdec) - exp(-t/trise))
	   else
		c=cmax*exp(-t/tdec)
	   endif
	endif
	calconc=c
c
	RETURN
	end

	subroutine FUNCD(t,ft,df)
c For RTSAFE in simpjump (scsim)
c Function is as in simulat.doc: t0=tabs=time when event starts, and
c t=lifetime (=z in simulat.doc)
c Need to use expi(x) to prevent underflow
	logical step,pulse,varconc
c 	common/cconc/ split into 2 so conc0 etc can be common with main prog
c=	common/cconc1/conc0(10),conc1(10),conc2(10),nljump !for funcd etc and main
	common/cconc2/step,pulse,varconc,trise2,tdec2,tpulse2,
     & cmax,nshape,u,arat,brat,tabs				!for funcd etc
c=	common/cconc/step,pulse,varconc,trise,tdec,tpulse,
c=     & cmax,xa0,xa1,xa2,nshape,u,arat,brat,tabs
c
	au=-alog(u)
	t0=tabs
	if(nshape.eq.1) then
	   td0=exp1(-t0/tdec)
	   td=exp1(-t/tdec)
	   tr0=exp1(-t0/trise)
	   tr=exp1(-t/trise)
	   ac=arat*cmax
	   ft=ac*(tdec*td0*(1.0-td) - trise*tr0*(1.0-tr)) + brat*t
	   ft=ft - au	!so solves for ft=-log(u)
c  with derivative
	   df=ac*(td0*td - tr0*tr) + brat
	endif
	RETURN
	end

	function FUNC(t)
	common/cmax/trise,tdec
	FUNC=exp1(-t/tdec) - exp1(-t/trise)
	return
	end

	function FUNC1(t)
	common/cmax/trise,tdec
c first deriv
	FUNC1= -exp1(-t/tdec)/tdec + exp1(-t/trise)/trise
	return
	end

	logical function EQUAL(r1,r2,tc0)
	equal=abs(r1-r2).lt.tc0
	return
	end

	subroutine DIGITISE(tcum,iamps,n1,nd1,nval,tval,totcur,nd2,add)
c Input is cumulative array of times in tcum, and amplitudes in iamps
c This version assumes output time already defined in tval()
c add=F   allocates output to totcur()
c add=T   adds output to the input totcur()
c
	real*4 tcum(nd1)
	integer*2 iamps(nd1)
	real*4 tval(nd2),totcur(nd2)
	logical add
c
	j1=1
	do i=1,nval
	   t=tval(i)	!what is amplitude at this time
	   if(t.le.tcum(1)) then
		   ampout=float(iamps(1))
		   goto 79	!jump out
	   endif
	   do j=j1,n1-1
		   if(tcum(j).le.t.and.tcum(j+1).gt.t) then
			ampout=float(iamps(j+1))
			j1=j		!for next cycle
			goto 79	!jump out
		   endif
	   enddo
c	   call BELL(3)
	   print 80,i
80	   format(' ERROR: totcur(',i5,') should not reach here')
79	   continue
	   if(add) then
		totcur(i)=totcur(i) + ampout
	   else
		totcur(i)=ampout
	   endif
	enddo
c
	RETURN
	end
