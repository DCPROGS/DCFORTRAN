	subroutine VCONC(QD,cA1,cB1,ncdep,nlig,IL,IX,JX,
     &  k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc)
c
c To calc relaxations with time-varying conc by AGH method in SCALCS
c
	REAL*8 QD(10,10),p0(10)
	real*8 Amat(10,10,10),eigen(10)
	real*8 Q1(10,10),pt(10)
	real*8 Xtdec(10,10),Xt1(10,10),Q2(10,10),p10(10)    !for debug
	real*8 expXtd(10,10),expXt1(10,10)			    !for debug
	real*8 Xt(10,10),expXt(10,10)
	real*4 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
c	real*4 xcal(2048,10),ycal(2048,10)
	character*1 ans,UC
	integer IX(10),JX(10),IL(10)		!NB 'ix' is used by RANDOM
	LOGICAL pon,debug,qprt,discprt
	logical slock,caplock
c
	common/dp/discprt
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp
c
c define function
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c
c Calculate the initial condition, p(0), before specified conc time-course starts
	xA00=0.0	!default
	nlig=1
      print 22
22    format(
     & '&Concentration at which initially equilibrated (muM) [0.0] = ')
	call INPUTr(xA00)
	xa00=xA00*1.e-6		!molar
	call QNEWC(QD,cA1,cB1,Q1,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q1,k,k-1,km,p0)	!calc init occs at xA00
	if(qprt) then
         print 50,xa00*1.e6
         if(pon()) write(7,50) xa00*1.e6
         if(discprt) write(8,50) xa00*1.e6
50	   format(/,' Q matrix before t=0, at concentration = ',g13.6)
	   call DATYP(Q1,'  Q(0)  ' ,pon(),k,k,km,km)
	endif
      print 51,xa00*1.e6
      if(pon()) write(7,51) xa00*1.e6
      if(discprt) write(8,51) xa00*1.e6
51	format(/,
     & ' Equilib occupancies before t=0, at concentration = ',g13.6)
	do j=1,k
	   print 53,j,p0(j)
         if(pon()) write(7,53) j,p0(j)
         if(discprt) write(8,53) j,p0(j)
53	   format(' p0(',i2,') = ',g13.6)
	end do
c
c
c Ask what conc time course is (as in EPSCSIM)
33	continue
	print 25
25	format(' Transmitter concentration time-course:',/,
     & ' (1) Rectangular pulse from t=0 to t=t',/,
     & ' (2) Instantaneous rise, exponential fall',/,
     & ' (3) Exponential rise, exponential fall',/,
     & ' Option number = ')
	call INPUTi(nshape)
	if(nshape.lt.1.or.nshape.gt.3) goto 33
	if(nshape.eq.3) then
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
	else
	   print 323
323	   format(
     & '&Length of pulse of transmitter concentration (ms) = ')
	   call INPUTr(tdec)
	endif
	if(nshape.eq.3.and.tdec.le.trise) goto 33
	print 34
34	format(
     & '&Peak transmitter concentration (micromolar) = ')
	call INPUTr(cpeak)
	if(nshape.eq.1) then
	   cmax=cpeak
	   if(pon()) write(7,341) cmax,tdec
	   if(discprt) write(8,341) cmax,tdec
341	   format(
     &' Rectangular pulse to ',g13.6,' micromolar, from t=0 to t=',
     & g13.6,'ms')
	else if(nshape.eq.2) then
	   cmax=cpeak
	   if(ncomp.eq.1) then
	      print 342,cmax,tdec
	      if(pon()) write(7,342) cmax,tdec
	      if(discprt) write(8,342) cmax,tdec
342	      format(
     &   ' Exponential decline from ',g13.6,' micromolar; tau = ',
     &     g13.6,'ms')
	   else if(ncomp.eq.2) then	!must solve for tpeak numerically!
	      print 3421,cmax,tdec,tdec2,relamp
	      if(pon()) write(7,3421) cmax,tdec,tdec2,relamp
	      if(discprt) write(8,3421) cmax,tdec,tdec2,relamp
3421	      format(
     &   ' Exponential decline from ',g13.6,' micromolar;',/,
     &' tau1 (decay) = ',g13.6,' ms.',' tau2 (decay) = ',g13.6,' ms.',/,
     &' Relative amplitude (at t=0) of tau1 component /tau2 component ='
     &  	,g13.6)
	   endif
	else if(nshape.eq.3) then
c Calc concentration as c(t)=cmax*[exp(-t/tdec) - exp(-t/trise)] where
c cmax is calculated to give the required peak concentration (cmax is
c conc that would be achieved if rise was not cut short by decay)
	   if(ncomp.eq.1) then
	     tpeak=-alog(trise/tdec)/(1.0/trise - 1.0/tdec)	!time for max
	     ctmax=exp(-tpeak/tdec) - exp(-tpeak/trise)	!as fraction of 1.0
	     cmax=cpeak/ctmax
           print 37,trise,tdec,cpeak,tpeak,cmax
           if(pon()) write(7,37) trise,tdec,cpeak,tpeak,cmax
           if(discprt) write(8,37) trise,tdec,cpeak,tpeak,cmax
37	     format(
     &' tau(rise) = ',g13.6,' ms ;  tau (decay) = ',g13.6,' ms.',/,
     &' Peak concentration of ',g13.6,' micromolar attained at t = ',
     &  g13.6,' ms',/,' ( cmax = ',g13.6,')')
	   else if(ncomp.eq.2) then	!must solve for tpeak numerically!
		epsx=0.01		!max error (ms)
		tm=10.*trise		!upper guess for tpeak
		call BISEC0(tmaxfunc,0.,tm,0.,tpeak,yout,epsx,-1.,
     &		nerr,-2,.false.)
	      ctmax=wamp*exp(-tpeak/tdec)+(1.-wamp)*exp(-tpeak/tdec2)
     &	 - exp(-tpeak/trise)	!as fraction
	      cmax=cpeak/ctmax
c		SUBROUTINE BISEC0(FUNC,X1,X2,Y,XOUT,YOUT,EPSx,epsy,
c	     & NERR,NDISP,OMIT)
           print 371,trise,tdec,tdec2,relamp,cpeak,tpeak,cmax
           if(pon()) write(7,371) trise,tdec,tdec2,relamp,cpeak,
     &	tpeak,cmax
           if(discprt) write(8,371) trise,tdec,tdec2,relamp,cpeak,
     &	tpeak,cmax
371	     format(
     &' tau(rise) = ',g13.6,' ms;',/,
     &' tau1 (decay) = ',g13.6,' ms.',' tau2 (decay) = ',g13.6,' ms.',/,
     &' Relative amplitude (at t=0) of tau1 component /tau2 component ='
     &  ,g13.6,/,
     &' Peak concentration of ',g13.6,' micromolar attained at t = ',
     &  g13.6,' ms',/,' ( cmax = ',g13.6,')')
	   endif
	endif
c
c Conc time course now defined.  Which time points to be used?
	print 10
10	format(' Calculate occupancies up to tmax (ms) = ')
	call INPUTr(tmax)
242	print 24
24	format(
     & '&Number of time points to be calculated (=<2048) [512] = ')
	ntime=512
	call INPUTi(ntime)
	if(ntime.gt.2048) goto 242
	dt=tmax/float(ntime-1)		!in ms
c
      print 241, ntime,tmax,dt
      if(pon()) write(7,241) ntime,tmax,dt
      if(discprt) write(8,241) ntime,tmax,dt
241	format(
     & ' Calculate ',i6,' points up to ',g13.6,' ms: interval = ',
     & g13.6,' ms')
c
c Now use QNEWC to keep, in Q1, the Q matrix with concentrations removed
c (for use in XTCALC)
	call QNEWC(QD,cA1,cB1,Q1,1.00,1.00,ncdep,nlig,IL,IX,JX,k,km)
c
c Check: for nshape=1, calculate here the initial vector for the
c 'off-jump', p'(0)=p(tdec)=p(0)exp(X(tdec)) -can check this vs
c value found by conventional calculation
	if(nshape.eq.1) then
	   call XTCALC(Q1,Xtdec,tdec,ncdep,IX,JX,k,km)
	   call QMAT5(Xtdec,Amat,k,eigen,ibad,km,km,km)
	   do i=1,k
	   do j=1,k
		EXPXTd(i,j)=0.d0
	      do m=1,k
		   expxtd(i,j)=expxtd(i,j) + Amat(i,j,m)*dexp(eigen(m))
		end do
	   end do
	   end do
c and premultiply by p(0), to get p(tdec)=p'(0)
         call MATMUL(p0,EXPXTd,p10,1,k,k,1.d0,1,km,km,km,1,km)
         print 511
         if(pon()) write(7,511)
         if(discprt) write(8,511)
511	   format(/,
     & ' Initial vector for off-jump p''(0)=p(tdec)=p(0) exp(X(tdec))')
	   call DATYP(Xtdec,'X(tdec) ' ,pon(),k,k,km,km)
	   call DATYP(expxtd,'exp(Xtd)' ,pon(),k,k,km,km)
	   do j=1,k
		print 531,j,p10(j)
      	if(pon()) write(7,531) j,p10(j)
      	if(discprt) write(8,531) j,p10(j)
531	 	format(' p''0(',i2,') = ',g13.6)
	   end do
	endif
c
c When time=0 then X(t)=0 so have problems in calc of eigenvalues
c so just skip time=0, at which solution is simply p(t)=p(0) -set
c these values here, and start loop at itime=2
	do j=1,k
	   xcal(1,j)=0.0
	   ycal(1,j)=sngl(p0(j))     !occ of state j at time t
	enddo
c
c NOW START THE TIME LOOP
	do itime=2,ntime
	   time=float(itime-1)*dt
c
c Calc matrix X(t) at this time where X(t) = integral (from 0 to t)
c of Q(t) (i.e. simply Q*t when Q constant).  To do this, each
c conc-dep element of Q is multiplied by integral (0 to t) of c(t)
c (rather than by c(t))
c t,tdec,trise all in ms, conc in muM.  Result of CONCINT has dimensions of
c conc* time. Multiply result by 1.e-9 to get in units of (Molar.sec).
c	   c1=concint(time)*1.e-9	!molar*sec  (Calcs now in XTCALC)
	   call XTCALC(Q1,Xt,time,ncdep,IX,JX,k,km)
c
c and now calculate p(t)=p(0)*exp(X(t)). First get spectral expansion of X
	   call QMAT5(Xt,Amat,k,eigen,ibad,km,km,km)
	   if(ibad.ne.0) print 14,ibad
14	   format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
c
c Calculate exp(X(t))=SUM[A(m).exp(eigen(m))]
	   do i=1,k
	   do j=1,k
		EXPXT(i,j)=0.d0
	      do m=1,k
		   expxt(i,j)=expxt(i,j) + Amat(i,j,m)*dexp(eigen(m))
		end do
	   end do
	   end do
c and premultiply by p(0), to get p(t)
         call MATMUL(p0,EXPXT,pt,1,k,k,1.d0,1,km,km,km,1,km)
c Store result in suitable form for display
	   do j=1,k
		xcal(itime,j)=time
		ycal(itime,j)=sngl(pt(j))     !occ of state j at time t
	   enddo
c
	   if(qprt) then
		c=conc(time)
      	print 54,time,c
      	if(pon()) write(7,54) time,c
      	if(discprt) write(8,54) time,c
54		format(/,' X(t) matrix at t = ',g13.6,
     &	' ms; concentration (muM) = ',g13.6)
	      call DATYP(Xt,'  X(t)  ' ,pon(),k,k,km,km)
	      call DATYP(expxt,'exp(Xt) ' ,pon(),k,k,km,km)
		do i=1,k
		   x=sngl(eigen(i)/(time*1.e-3))
		   tau=-1000./x
      	   print 55,i,eigen(i),x,tau
      	   if(pon()) write(7,55) i,eigen(i),x,tau
      	   if(discprt) write(8,55) i,eigen(i),x,tau
55		   format(1x,i3,': eig(Xt) = ',g13.6,' eig/t, t/eig = ',
     &		2g13.6)
		end do
	   endif
c
	   if(debug()) then
		print 700
700		format(' Print spectral expansion matrices [N] ? ')
		read 101,ans
		if(UC(ans).eq.'Y') then
		   do 701 m=1,k
701		   call DATYP3(amat,m,'A matrix',pon(),k,k,km,km)
		endif
	   endif
c
	   if(qprt) then
            print 514
            if(pon()) write(7,514)
            if(discprt) write(8,514)
514	      format(/,' p(t)=p(0) exp(X(t))')
		do j=1,k
		   print 534,j,pt(j)
      	   if(pon()) write(7,534) j,pt(j)
      	   if(discprt) write(8,534) j,pt(j)
534	 	   format(' pt(',i2,') = ',g13.6)
	      end do
	   endif
c Further checks (rect pulse case) for off jump.  Calc X(t-tdec) in XT1
c (XT1/(t-tdec) should be Q(conc=0);
c and add to Xtdec already calc -sum should equal X(t)
	   if(qprt.and.nshape.eq.1.and.time.gt.tdec) then
		time1=time-tdec
		call XT1CALC(Q1,Xt1,time1,ncdep,IX,JX,k,km)
	      call MATADD(Xtdec,Xt1,Q2,k,k,
     &	    km,km,km,km,km,km)
      	print 56,time1
      	if(pon()) write(7,56) time1
      	if(discprt) write(8,56) time1
56		format(/,' X(t'') matrix at t'' = ',g13.6,
     &	  ' ms after off-jump')
	         call DATYP(Xt1,'  X(t'')  ' ,pon(),k,k,km,km)
	      call DATYP(Q2,'Xt''+Xtd ' ,pon(),k,k,km,km)
		do i=1,k
		do j=1,k
			Q2(i,j)=Xt1(i,j)/(time1*1.e-3)
		end do
		end do
	      call DATYP(Q2,'X(t'')/t''' ,pon(),k,k,km,km) !=Q(conc=0)??
c alternative calc of occs via p'(0).exp(X(t')) (ie p10*exp(Xt1))
	      call QMAT5(Xt1,Amat,k,eigen,ibad,km,km,km)
	      if(ibad.ne.0) print 14,ibad
c14	      format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
	      do i=1,k
	      do j=1,k
		 EXPXT1(i,j)=0.d0
	       do m=1,k
		   expxt1(i,j)=expxt1(i,j) + Amat(i,j,m)*dexp(eigen(m))
		 end do
	      end do
	      end do
	      call DATYP(expxt1,'exp(Xt'')' ,pon(),k,k,km,km)
       	call MATMUL(expxtd,expxt1,Q2,k,k,k,1.d0,
     &	 km,km,km,km,km,km)
            call MATMUL(p10,EXPXT1,pt,1,k,k,1.d0,1,km,km,km,1,km)
            print 5141
            if(pon()) write(7,5141)
            if(discprt) write(8,5141)
5141	      format(/,
     &	' Q2=exp(X(tdec))*exp(X(t'')) [=exp(Xtd+Xt'')=exp(X(t))?]',
     &	/,' Alternative calc of p(t)=p''(0) exp(X(t''))')
	      call DATYP(Q2,'  Q2    ' ,pon(),k,k,km,km)
		do j=1,k
		   print 5341,j,pt(j)
      	   if(pon()) write(7,5341) j,pt(j)
      	   if(discprt) write(8,5341) j,pt(j)
5341	 	   format(' pt(',i2,') = ',g13.6)
	      end do
c temp debug -overwrite 1st calc============
		do j=1,k
		   xcal(itime,j)=time
		   ycal(itime,j)=sngl(pt(j))     !occ of state j at time t
		enddo
	   endif
	end do	!END OF TIME LOOP
c
	RETURN
	end



	function CONC(t)
c Calc concentration as c(t)=cmax*[exp(-t/tdec) - exp(-t/trise)]
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp
c t,tdec,trise all in ms
	goto (1,2,3) nshape
c
c (1) Square conc pulse from t=0 to t=tdec
1	continue
	if(t.le.tdec) then
	   conc=cmax
	else
	   conc=0.0
	endif
	RETURN
c
c (2) Exponential decline from cmax
2	continue
	conc=cmax*exp(-t/tdec)
	if(ncomp.eq.1) then
	   conc=cmax*exp(-t/tdec)
	else if(ncomp.eq.2) then
	   conc=cmax*(wamp*exp(-t/tdec) + (1.-wamp)*exp(-t/tdec2))
	endif
	RETURN
c
c (3) Exponential rise and fall
3	continue
	if(ncomp.eq.1) then
	   conc=cmax*(exp(-t/tdec) - exp(-t/trise))
	else if(ncomp.eq.2) then
	   conc=cmax*(wamp*exp(-t/tdec) + (1.-wamp)*exp(-t/tdec2)
     &	 - exp(-t/trise))
	endif
	RETURN
	END



	function CONCINT(t)
c Calc integral (from 0 to t) of c(t).dt
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp
c t,tdec,trise all in ms, conc in muM.  Result has dimensions of
c conc* time. Multiply result by 1.e-9 to get in units of (Molar.sec).
	goto (1,2,3) nshape
c
c (1) Square conc pulse from t=0 to t=tdec
1	continue
	if(t.le.tdec) then
	   concint=cmax*t
	else
	   concint=cmax*tdec
	endif
	RETURN
c
c (2) Exponential decline from cmax
2	continue
	if(ncomp.eq.1) then
	   concint=cmax*tdec*(1. - exp(-t/tdec))
	else if(ncomp.eq.2) then
	   concint=cmax*(wamp*tdec*(1.-exp(-t/tdec)) +
     &	(1.-wamp)*tdec2*(1.-exp(-t/tdec2)))
	endif
	RETURN
c
c (3) Exponential rise and fall
3	continue
	if(ncomp.eq.1) then
	   concint=cmax*(tdec*(1.-exp(-t/tdec)) -
     &	 trise*(1.-exp(-t/trise)))
	else if(ncomp.eq.2) then
	   concint=cmax*(wamp*tdec*(1.-exp(-t/tdec)) +
     &	(1.-wamp)*tdec2*(1.-exp(-t/tdec2)) -
     &	  trise*(1.-exp(-t/trise)))
	endif
	RETURN
	END

	function TMAXFUNC(t)
	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp
c for BISEC0 (called only when ncomp=2,nshape=3)
c
	x=(wamp/tdec)*exp(-t/tdec) + ((1.-wamp)/tdec2)*exp(-t/tdec2)
     &  -  exp(-t/trise)/trise
	tmaxfunc=x
	RETURN
	end

