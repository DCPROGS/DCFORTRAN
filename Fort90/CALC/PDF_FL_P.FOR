	subroutine pdf_FL_p(Q0,Q1,p0,t,ft,tpulse,Prns,tau0,area0,
     &	tau1,area1,bad,km)
c  Subroutine to calc pdf of first latency after pulse of conc from zero to xA
c and back to zero.
c  Call once with t<0 to calculate matrices; then call with t=>0 to
c calculate f(t) (assumes matrices already calculated) -needs t in msec
c
c
c Input if t<0:
c	tpulse = pulse duration
c 	Q1 = Q matrix during pulse -conc=xA
c 	Q0 = Q matrix before and after pulse -conc=0
c	p0 = occupancies p(i) (i=1,...,k) at t=0 (before pulse)
c
c Output if t<0:
c     tau0,area0=kF components for calc of f(t), 0<t<tpulse
c     tau1,area1=kB components for calc of f(t), t>tpulse
c     Prns = P(r=>1) (single precision)
c  NB area0, area1 are divided by P(r=>1) before output
c
c Input if t>=0:
c     t (t=>0)
c	tpulse = pulse duration
c     tau0,area0=kF components for calc of f(t), 0<t<tpulse
c     tau1,area1=kB components for calc of f(t), t>tpulse
c
c
c Output if t>=0:
c	ft=f(t)
c
c The pdf is
c
c   f(t)=pF(0)*GFA(t)*uA/P(r=>1),
c
c where for 0<t<tpulse:
c	GFA(t) = G1FA(t) = exp(Q1FF*t)*Q1FA
c and for t>tpulse
c	GFA(t) = exp(Q1FF*tpulse)*G0FA(t-tpulse)
c	       = exp(Q1FF*tpulse)*exp[Q0FF*(t-tpulse)]*Q0FA
c=========not obvious that these are the same at t=tpulse!!!
c=========is Q0FA=Q1FA in general?
c NB Q0FF is singular -can still do by spectral expansion of Q0FF (as done in
c SCJUMP) but this gives kF components, some zero area, so better use the
c method that gives the kB components directly (see PULSE.CHI):
c f(t)=pF(0)*[exp(Q1FF*tpulse)]FB*exp(Q0BB*(t-tpulse))*Q0BA*uA,  t>tpulse
c so
c Initial vector is pF(0)*[exp(Q1FF*tpulse)]FB
c and
c	P(r=>1) = pF(0)*{inv(Q1FF)*[exp(Q1FF*tpulse) - I]*Q1FA -
c   				[exp(Q1FF*tpulse)]FB* inv(Q0BB)*Q0BA}*uA
c
c  (c.f. f(t)=phiF0*exp(QFF*t)(-QFF)uF for step)
c
c For 0<t<tpulse,
c	the pre-exponential row vector is pF(0)
c	the post-exponential column vector is Q1FA*uA=-Q1FF*uF
c	the denominator is P(r>=1)
c For t>tpulse,
c	the pre-exponential row vector is pF(0)*exp(Q1FF*tpulse)
c	the post-exponential column vector is Q0FA*uA=-Q0FF*uF
c	the denominator is P(r>=1)
c
c Part of family of subroutines to do single channel calcs via
c set of discrete subroutines that can be called as req.
c Params/commom
	real*8 Q0(km,km),Q1(10,10),p0(km),x
	real*4 area0(km),tau0(km)	!for 0<t<tpulse
	real*4 area1(km),tau1(km)	!for t>tpulse
	real*4 Prns
	logical bad
c Local
	real*8 Q2(10,10),Q3(10,10),Q4(10,10)
	real*8 eigen(10),amat(10,10,10)
	real*8 w1(10),ucol(10,1),col1(10,1),pF0(1,10),row1(1,10)
	real*8 Prn,one,tt
c	integer AF,FF
	integer FF,FA,BA,BB
	common/KBLK/kA,kB,kC,kD
c
	kF=kB+kC+kD       !known as kT in some progs
	bad=.false.
c
c Calculation of f(t) from previously calculated tau(), area(), in single
c precision
	if(t.ge.0.0) then		!tau, area already calc -get f(t)
	   s=0.0
	   if(t.le.tpulse) then
		do m=1,kF
		   a=area0(m)/tau0(m)
		   s=s + a*exp(-t/tau0(m))
		enddo
	   else
		do m=1,kB
		   t1=t-tpulse
		   a=area1(m)/tau1(m)
		   s=s + a*exp(-t1/tau1(m))
		enddo
	   endif
	   ft=s
	   RETURN
	endif
c
c Calculation of area, tau etc for 1st latency pdf
c	AF=17
	FA=71
	FF=77
	BA=21
	BB=22
	one=1.0d0
	do i=1,km
	   ucol(i,1)=one
	enddo
	do i=1,kF
	   pF0(1,i)=p0(i+kA)
	enddo
c
c SECTION (1): Calculations of tau0, area0 for 0<t<tpulse
c   Initial vector
c    Final vector Q1FA*uA=-Q1FF*uF
	call SUBMAT(Q1,FF,Q2,km,km,km,km)	!Q1FF in Q2
	call MATMUL(Q2,ucol,col1,kF,kF,1,-one,km,km,km,1,km,1)  !-Q1FF*uF in col1
c Calc stuff for exponential
	call QMAT5(Q2,Amat,kF,eigen,ibad,km,km,km)
	if(ibad.eq.0) then
	   do m=1,kF		!calc w1(m)
		call MATSCL3(pF0,Amat,m,col1,kF,w1(m),km,km,km)
	   enddo
	else
	  call BELL(1)
	  print*,' Error in QMAT5 for Q1FF: ibad = ',ibad
	endif
	ncomp=kF
	mbad=0
	do m=1,ncomp
	  x=dabs(eigen(m))
	  if(x.lt.1.0d-35) then
		tau0(m)=1.e35
		area0(m)=1.0
		mbad=m
	  else
		tau0(m)=1000./sngl(x)
		area0(m)=sngl(w1(m)/x)
	  endif
	enddo
c area set to 1.0 for bad component, so set others to zero
	if(mbad.ne.0) then
	   do m=1,ncomp
		if(m.ne.mbad) area0(m)=0.0		!area(mbad)=1 (above)
	   enddo
	endif
c
c SECTION (2): Calculations for Prn = P(r=>1)
c   Calc Q3 = exp(Q1FF*tpulse) =SUM[amat(i,j,m)*exp(eigen(m)*tpulse]
c   -amat,eigen are values for Q1FF at this point, and Q2 contains Q1FF
c	PRn=pF(0)*XFA*uA where
c	XFA=inv(Q1FF)[exp(Q1FF*tpulse) - I]*Q1FA + [exp(Q1FF*tpulse)]FB*G0BA]
	tt=dble(tpulse)*1.d-3		!in seconds for expmat
	call EXPMAT(Q3,amat,eigen,tt,kF,km)
	call MATINV(Q2,kF,km,Q2,km)		!inv(Q1FF) in Q2
c   Calc Q4=[exp(Q1FF*tpulse) - I]
	do i=1,kF
	   do j=1,kF
		if(i.eq.j) then
		   Q4(i,j)=Q3(i,j) - one
		else
		   Q4(i,j)=Q3(i,j)
		endif
	   enddo
	enddo
c   Calc Q4=inv(Q1FF)[exp(Q1FF*tpulse) - I]
	call MATMUL(Q2,Q4,Q4,kF,kF,kF,one,km,km,km,km,km,km)
	call SUBMAT(Q1,FA,Q2,km,km,km,km)	!Q1FA in Q2
c   Calc Q2=inv(Q1FF)[exp(Q1FF*tpulse) - I]*Q1FA= first term in XFA
	call MATMUL(Q4,Q2,Q2,kF,kF,kA,one,km,km,km,km,km,km)
c   Now 2nd term of XFA -first get G0BA
	call GMAT1(BA,Q0,Q4,km,km)    !G0BA in Q4
c   Calc [exp(Q1FF*tpulse)]FB*G0BA] = [Q3]FB*Q4 in Q4
	call MATMUL(Q3,Q4,Q4,kF,kB,kA,one,km,km,km,km,km,km)
	do i=1,kF
	   do j=1,kA
		Q2(i,j)=Q2(i,j)+Q4(i,j)		!XFA in Q2
	   enddo
	enddo
c    Finally get Prn=pF(0)*XFA*uA
	call MATSCL2(pF0,Q2,ucol,kF,kA,Prn,km,km,km)
	Prns=sngl(Prn)	!for return to caling prog
c
c
c SECTION (3): Calculations of tau1, area1 for t>tpulse
C
c    Final vector Q0FA*uA=-Q0FF*uF -but
c NB Q0FF is singular -can still do by spectral expansion of Q0FF (as done in SCJUMP)
c but this gives kF components, some zero area, so better use the method that
c gives the kB components directly (see PULSE.CHI):
c f(t)=pF(0)*[exp(Q1FF*tpulse)]FB*exp(Q0BB*(t-tpulse))*Q0BA*uA,  t>tpulse
c so
c Initial vector is pF(0)*[exp(Q1FF*tpulse)]FB
c Final vector is Q0BA*uA
	call SUBMAT(Q0,BA,Q2,km,km,km,km)	!Q0BA in Q2
	call MATMUL(Q2,ucol,col1,kB,kA,1,one,km,km,km,1,km,1)  !Q0BA*uA in col1
c    Initial vector pB(0)*[exp(Q1FF*tpulse)]BB; Q3 contains [exp(Q1FF*tpulse)]
c next multiplication should select FB part of Q3
      call MATMUL(pF0,Q3,row1,1,kF,kB,one,1,km,km,km,1,km)	!init vec in row1
c Calc stuff for exponential
	call SUBMAT(Q0,BB,Q2,km,km,km,km)	!Q0BA in Q2
	call QMAT5(Q2,Amat,kB,eigen,ibad,km,km,km)
	if(ibad.eq.0) then
	   do m=1,kB		!calc w1(m)
		call MATSCL3(row1,Amat,m,col1,kB,w1(m),km,km,km)
	   enddo
	else
	  call BELL(1)
	  print*,' Error in QMAT5 for Q0BA: ibad = ',ibad
	endif
	ncomp=kB
	mbad=0
	do m=1,ncomp
	  x=dabs(eigen(m))
	  if(x.lt.1.0d-35) then
		tau1(m)=1.e35
		area1(m)=1.0
		mbad=m
	  else
		tau1(m)=1000./sngl(x)
		area1(m)=sngl(w1(m)/x)
	  endif
	enddo
c area set to 1.0 for bad component, so set others to zero
	if(mbad.ne.0) then
	   do m=1,ncomp
		if(m.ne.mbad) area1(m)=0.0		!area(mbad)=1 (above)
	   enddo
	endif
c Normalise the areas
	if(Prns.gt.1.e-25) then
	   do m=1,kF
		area0(m)=area0(m)/Prns
	   enddo
	   do m=1,kB
		area1(m)=area1(m)/Prns
	   enddo
	else
	   bad=.true.
	endif
c
	RETURN
	end

