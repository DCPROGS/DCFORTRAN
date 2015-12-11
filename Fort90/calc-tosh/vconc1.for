	subroutine VCONC1(QT,conc0,nljump,ligname,
     &  k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc)
c
c To calc relaxations with time-varying conc by R-K method in SCALCS
c NB number of calculated values, kount, may not be exactly that
c requested (because time intervals are not exactly as requested -they
c depend on step lengths) so set ntime=kount to allow correct display
c
c Modif 03/06/01 03:29pm for 100 states/10 ligands.
c  nlig, IL now in common (so can go into DERIVS)
c  ligand number to be varied, #nljump, added to common/qm
c  conc of all ligands (and initial conc of #nljump) in conc0()
c
	REAL*8 QT(100,100),p0(100),Q1(100,100)
	real*4 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
c	character*1 ans,UC
	LOGICAL pon,debug,qprt,discprt
	logical slock,caplock
c for ODEINT (double precision version)
	real*8 ystart(100),XP(1024),YP(100,1024)
	real*8 t0,t1,eps,h1,hmin,dxsav
	real*4 conc0(10)
c	real*4 conc00(10),conc(10)	!not needed?
	character*20 ligname(10)
c
	common/dp/discprt
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp,tpuls,xb
	COMMON /PATH/KMAX,KOUNT,DXSAV,XP,YP		!for ODEINT
c	PARAMETER(NVAR=4)
c	DIMENSION YSTART(NVAR)
	common/qm/Q1,kvar,nljump1		   !for DERIVS (only) Q1=QT
	integer IX(100),JX(100),IL(100)		!NB 'ix' is used by RANDOM
	COMMON/LIG/nlig,IL
	COMMON/CPAR/NCDEP,IX,JX,X
	real*4 conc01(10)		!copy of conc0(10) for common
	common/c0/conc01		!common with derivs.for
	EXTERNAL DERIVS,RKQC	!for ODEINT
	external  tmaxfunc
c
c define function
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c
	kvar=k		!copy for common
c Copy of conc0 for common (in derivs.for)
	nljump1=nljump	!copy for common
	do i=1,10
	   conc01(i)=conc0(i)
	enddo
c
c Calculate the initial condition, p(0), before specified conc time-course starts
	xA0=0.0	!default
      print 22,nljump,ligname(nljump)
22    format(
     & '&Conc of ligand #',i1,'(',a10,
     & ') at which initially equilibrated (muM) [0.0] = ')
	call INPUTr(xA0)
	conc0(nljump)=xA0*1.e-6		!molar
	xb=xA0	!default
      print 23,nljump,ligname(nljump),xb
23    format(
     & '&Background conc of ligand #',i1,'(',a10,
     & ') during and after pulse (muM) [',f6.2,'] = ')
	call INPUTr(xb)
c  Leave xb in micromolar for conctt() (and for print)
c	xb=xb*1.e-6		!molar
c
	call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
c==	call QNEWC(QD,cA1,cB1,Q1,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q1,k,k-1,km,p0)	!calc init occs at xA00
	if(qprt) then
         print 50,xa0*1.e6
         if(pon()) write(7,50) xa0*1.e6
         if(discprt) write(8,50) xa0*1.e6
50	   format(/,' Q matrix before t=0, at concentration = ',g13.6)
	   call ATYPD(Q1,'  Q(0)  ',k,k,km,km)
	endif
c    print equilib occs
      print 51,xa0*1.e6
      if(pon()) write(7,51) xa0*1.e6
      if(discprt) write(8,51) xa0*1.e6
51	format(/,
     & ' Equilib occupancies before t=0, at concentration = ',g13.6)
	do j=1,k
	   print 53,j,p0(j)
         if(pon()) write(7,53) j,p0(j)
         if(discprt) write(8,53) j,p0(j)
53	   format(' p0(',i2,') = ',g13.6)
	enddo
c
c
c Ask what conc time course is (as in EPSCSIM)
33	continue
	print 25
25	format(/,' Transmitter concentration time-course:',/,
     & ' (1) Rectangular pulse from t=0 to t=t',/,
     & ' (2) ''Rectangular'' pulse with exponential rise and fall',/,
     & ' (3) Instantaneous rise, exponential fall',/,
     & ' (4) Exponential rise and fall:c+exp(-t/tdec)-exp(t/trise)',/,
     & '    OR c+w*exp(-t/tdec1) + (1-w)*exp(-t/tdec2) - exp(t/trise) ',
     & /,' Option number = ')
	call INPUTi(nshape)
	if(nshape.lt.1.or.nshape.gt.4) goto 33
c
	if(nshape.eq.2.or.nshape.eq.4) then
	   print 31
31	   format(
     & ' Time constant for rise of transmitter concentration (ms) = ')
	   call INPUTr(trise)
c	   input 9,trise
	endif
	if(nshape.ge.2) then
	   ncomp=1
	   print 311
311	   format(' Number of components for exponential decay [1] = ')
	   call INPUTi(ncomp)
	   print 32
32	   format(
     & '&(1) Time constant for decay, tau1, (ms) = ')
	   call INPUTr(tdec)
	   if(ncomp.eq.2) then
	      print 321
321	      format(
     &     '&(2) Second time constant for decay, tau2, (ms) = ')
	      call INPUTr(tdec2)
	      print 322
322	      format(
     & '&Relative amplitude (at t=0) of tau1/tau2 component = ')
	      call INPUTr(relamp)
	      wamp=relamp/(1+relamp)
	   endif
	endif
	if(nshape.eq.1.or.nshape.eq.2) then
	   print 323
323	   format(
     & '&Length of pulse of transmitter concentration (ms) = ')
	   call INPUTr(tpuls)
	endif
	if(nshape.eq.4.and.tdec.le.trise) then
	   call BELL(1)
	   print 35
35	   format(
     &' Must have tau(decay) > tau(rise) to calculate',/,
     &'    exp[-t/tau(dec)] - exp[-t/tau(rise)] -try again')
	   goto 33
	endif
c
	print 34
34	format(
     & '&Peak transmitter concentration (micromolar) = ')
	call INPUTr(cpeak)
	if(nshape.eq.1) then
	   cmax=cpeak
	   if(discprt) write(8,341) cmax,tpuls,xb
341	   format(/,
     &' Rectangular pulse to ',g13.6,' micromolar, from t=0 to t=',
     & g13.6,'ms',/,
     & ' Concentration after the pulse = ',g13.6)
	else if(nshape.eq.2) then
c response gets up to max defined by rising exponential
	   cmax=cpeak/(1.0 - exp(-tpuls/trise))	!=cpeak for long pulse
	   print 372,tpuls,tpuls,cpeak,cmax,trise,xb
	   if(discprt) write(8,372) tpuls,tpuls,cpeak,cmax,trise,xb
372	   format(/,
     &	' Pulse with exponential rise and fall',/,
     &	'   Pulse length (ms) = ',g13.6,/,
     &	'   Max concentration (at t = ',f9.3,') = ',g13.6,
     &	' (Cmax = ',g13.6,')',/,
     &	'   Time constant for rise of conc (ms) = ',g13.6,/,
     & ' Background conc during and after the pulse = ',g13.6)
	   if(ncomp.eq.1) then
       	print 373,tdec
        	if(pon()) write(7,373) tdec
       	if(discprt) write(8,373) tdec
373		format('   Time constant for decay of conc (ms) = ',g13.6)
	   else if(ncomp.eq.2) then
       	print 374,tdec,tdec2,relamp
        	if(pon()) write(7,374) tdec,tdec2,relamp
       	if(discprt) write(8,374) tdec,tdec2,relamp
374		format(/,2x,
     &' tau1 (decay) = ',g13.6,' ms.',' tau2 (decay) = ',g13.6,' ms.',/,
     &' Relative amplitude (at t=0) of tau1 component /tau2 component ='
     &  	,g13.6)
	   endif
	else if(nshape.eq.3) then
	   cmax=cpeak
	   if(ncomp.eq.1) then
	      print 342,cmax,tdec
	      if(pon()) write(7,342) cmax,tdec
	      if(discprt) write(8,342) cmax,tdec
342	      format(
     &   ' Exponential decline from ',g13.6,' micromolar; tau = ',
     &     g13.6,'ms',/,
     & ' Background conc during and after the pulse = ',g13.6)
	   else if(ncomp.eq.2) then	!must solve for tpeak numerically!
	      print 3421,cmax,tdec,tdec2,relamp,xb
	      if(discprt) write(8,3421) cmax,tdec,tdec2,relamp,xb
3421	      format(
     &   ' Exponential decline from ',g13.6,' micromolar;',/,
     &' tau1 (decay) = ',g13.6,' ms.',' tau2 (decay) = ',g13.6,' ms.',/,
     &' Relative amplitude (at t=0) of tau1 component /tau2 component ='
     &  	,g13.6,/,
     & ' Background conc during and after the pulse = ',g13.6)
	   endif
	else if(nshape.eq.4) then
c Calc concentration as c(t)=cmax*[exp(-t/tdec) - exp(-t/trise)] where
c cmax is calculated to give the required peak concentration (cmax is
c conc that would be achieved if rise was not cut short by decay)
	   if(ncomp.eq.1) then
	     tpeak=-alog(trise/tdec)/(1.0/trise - 1.0/tdec)	!time for max
	     ctmax=exp(-tpeak/tdec) - exp(-tpeak/trise)	!as fraction of 1.0
	     cmax=cpeak/ctmax
           print 37,trise,tdec,cpeak,tpeak,cmax,xb
           if(discprt) write(8,37) trise,tdec,cpeak,tpeak,cmax,xb
37	     format(/,
     &' tau(rise) = ',g13.6,' ms ;  tau (decay) = ',g13.6,' ms.',/,
     &' Peak concentration of ',g13.6,' micromolar attained at t = ',
     &  g13.6,' ms',/,' ( cmax = ',g13.6,')',/,
     & ' +Background conc during and after the pulse = ',g13.6)
	   else if(ncomp.eq.2) then	!must solve for tpeak numerically!
		epsx=0.01		!max error (ms)
		epsy=-1.
		tm=10.*tdec2		!upper guess for tpeak
		x1=0.
		x2=tm
		y=0.
		call BISEC0(tmaxfunc,x1,x2,y,tpeak,yout,epsx,epsy,
     &		nerr,-2,.false.)
c		subroutine BISEC0(FUNC,x1,x2,y,Xout,Yout,epsx,epsy,
c     &	 nerr,ndisp,omit)
	      ctmax=wamp*exp(-tpeak/tdec)+(1.-wamp)*exp(-tpeak/tdec2)
     &	 - exp(-tpeak/trise)	!as fraction
	      cmax=cpeak/ctmax
           print 371,trise,tdec,tdec2,relamp,cpeak,tpeak,cmax,xb
           if(discprt) write(8,371) trise,tdec,tdec2,relamp,cpeak,
     &	tpeak,cmax,xb
371	     format(/,
     &' tau(rise) = ',g13.6,' ms;',/,
     &' tau1 (decay) = ',g13.6,' ms.',' tau2 (decay) = ',g13.6,' ms.',/,
     &' Relative amplitude (at t=0) of tau1 component /tau2 component ='
     &  ,g13.6,/,
     &' Peak concentration of ',g13.6,' micromolar attained at t = ',
     &  g13.6,' ms',/,' ( cmax = ',g13.6,')',/,
     & ' +Background conc during and after the pulse = ',g13.6)
	   endif
	endif
c	print conc of other ligands if any
	do i=1,nlig
	   if(i.ne.nljump) then	!all the rest
		x=1.e6*conc0(i)
		if(discprt) write(8,6641) i,ligname(i),x
6641		format(/,' (Fixed) conc of ligand ',i2,' (',
     &	  a10,') throughout (micromolar) = ',g13.6)
	   endif
	enddo
c
c Conc time course now defined.  Which time points to be used?
	print 10
10	format(' Calculate occupancies up to tmax (ms) = ')
	call INPUTr(tmax)
	print 24
24	format('&Number of time points to be calculated = ')
	call INPUTi(ntime)
	dt=tmax/float(ntime-1)		!in ms
c
      print 241
241	format(/,' Calculating the integral . . .')
c
c Now use QNEWC to keep, in Q1, the Q matrix with concentrations removed
c (for use in XTCALC), but divide by cA1*10e6 so conc-dep elements
c are in 1/(micromolar*sec) to get better scaling (assume nlig=1)
c In revised 100-state version, already have QT
c==	call QNEWC(QD,cA1*1.e6,1.,QT,1.00,1.00,ncdep,1,IL,IX,JX,k,km)
c To get numbers im better range, convert Q1 to reciprocal milliseconds
c NB copy to Q1(in common/qm/ for DERIVS (used by odeint)) so QT not changed
c
	do i=1,k
	 do j=1,k
	   Q1(i,j)=QT(i,j)*1.d-3
	 enddo
	enddo
c
	t0=0.d0
	t1=dble(tmax)	!milliseconds
	do j=1,k
	  ystart(j)=p0(j)
	enddo
	eps=1.0d-4
	h1=0.01d0          !first step size (ms)
c	hmin=0.0d0        !min step size
c	hmin=0.0000001d0     !min step size (ms)
	hmin=1.d-20		   !min step size (ms)
c	h1=h1*1.d-3		!seconds
c	hmin=hmin*1.d-3		!seconds
c	t1=dble(tmax)*1.d-3	!seconds
	kmax=1024         !max number of values (time points) to be saved
	dxsav=dble(dt)    !interval at which to keep values (as close as poss)
c	CALL ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,DERIVS,RKQC)
	call ODEINT(YSTART,k,t0,t1,eps,h1,hmin,nOK,Nbad,DERIVS,RKQC)
c Store result in suitable form for display
	do i=1,kount
	   do j=1,k
		xcal(i,j)=sngl(xp(i))		!from common/path -in ms still
		ycal(i,j)=sngl(yp(j,i))		!ditto
	   enddo
cc Also keep conc profile in xcal(i,15), ycal(i,15)
c Also keep conc profile in xcal(i,k+2), ycal(i,k+2)
	   xcal(i,k+2)=xcal(i,1)
	   ycal(i,k+2)=CONCt(xcal(i,k+2))
	enddo
	ntime=kount		!number of values actually calculated
      print 242, ntime,tmax,dt
      if(pon()) write(7,242) ntime,tmax,dt
      if(discprt) write(8,242) ntime,tmax,dt
242	format(
     & ' Calculated ',i6,' points up to ',g13.6,' ms,',/,
     & ' (at intervals of at least = ',g13.6,' ms)')
c
	RETURN
	end



	function CONCt(t)
c Calc concentration as c(t)=cmax*[exp(-t/tdec) - exp(-t/trise)]
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp,tpuls,xb
c t,tdec,trise all in ms
c Returns conc in micromolar
	goto (1,2,3,4) nshape
c
c (1) Square conc pulse from t=0 to t=tpuls
1	continue
	if(t.le.tpuls) then
	   conct=cmax
	else
	   conct=0.0
	   conct=xb
	endif
	RETURN
c (2) Pulse with exponential rise and fall
2	continue
	if(t.le.tpuls) then
	   x=1.0
	else
	   t1=t-tpuls
	   if(ncomp.eq.1) then
		x=exp(-(t1)/tdec)
	   else if(ncomp.eq.2) then
		x=(wamp*exp(-t1/tdec) + (1.-wamp)*exp(-t1/tdec2))
	   endif
	endif
	conct=cmax*(x - exp(-t/trise))
	conct=conct+xb
	RETURN
c
c (3) Exponential decline from cmax
3	continue
	conct=cmax*exp(-t/tdec)
	if(ncomp.eq.1) then
	   conct=cmax*exp(-t/tdec)
	else if(ncomp.eq.2) then
	   conct=cmax*(wamp*exp(-t/tdec) + (1.-wamp)*exp(-t/tdec2))
	endif
	conct=conct+xb
	RETURN
c
c (4) Exponential rise and fall
4	continue
	if(ncomp.eq.1) then
	   conct=cmax*(exp(-t/tdec) - exp(-t/trise))
	else if(ncomp.eq.2) then
	   conct=cmax*(wamp*exp(-t/tdec) + (1.-wamp)*exp(-t/tdec2)
     &	 - exp(-t/trise))
	endif
	conct=conct+xb
	RETURN
	END


	real*4 function TMAXFUNC(t)
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp,tpuls,xb
c for BISEC0 (called only when ncomp=2,nshape=4)
c
	x=(wamp/tdec)*exp(-t/tdec) + ((1.-wamp)/tdec2)*exp(-t/tdec2)
     &  -  exp(-t/trise)/trise
	tmaxfunc=x+xb
	RETURN
	end

