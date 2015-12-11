	subroutine CALCVAL(xcal,ycal,jcal,ndc1,ndimc,tmin,tmax,
     & njump,ncalc,icur,QT,p0,km,ncalj,vhold,vref)
c
c To calculate the theoretical (mean) curv in SCSIM, for superimposition on
c the simulation (as in SCALCS)
c To get signal that agrees with simulated curve (sum of njump channels)
c must include njump in the calibration factor
c Put values directly into xcal, ycal(i,jcal)
c
	real*4 xcal(ndc1,ndimc),ycal(ndc1,ndimc)
	integer*2 icur(100,100)
	logical step,pulse,varconc
c 	common/cconc/ split into 2 so conc0 etc can be common with main prog
c=	common/cconc/step,pulse,varconc,trise,tdec,tpulse,
c=     & cmax,xa0,xa1,xa2,nshape,u,arat,brat,tabs	!for funcd
	common/cconc1/conc0(10),conc1(10),conc2(10),nljump !for funcd etc and main
	common/cconc2/step,pulse,varconc,trise,tdec,tpulse,
     & cmax,nshape,u,arat,brat,tabs				!for funcd etc
	COMMON/KBLK/KA,KB,KC,KD
c
	real*8 p0(100),QT(100,100)
c
	allocatable::amat,eigen,Q1,pinf,pt
	real*8 amat(:,:,:),eigen(:),Q1(:,:),pinf(:),pt(:)
	allocatable::amp
	real*4 amp(:)
c	allocatable::bmj
c	real*8 bmj(:,:)
c
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/LIG/nlig,IL(100)
c
c for ODEINT1 (double precision version)
	allocatable::xp,yp
	real*8 XP(:),YP(:,:)
	real*8 tt0,tt1,eps,h1,hmin,dxsav
c==	COMMON /PATH/KMAX,KOUNT,DXSAV,XP,YP		!for ODEINT
	COMMON /PATH/KMAX,KOUNT,DXSAV			!for ODEINT1
	EXTERNAL derivsim,RKQC	!for ODEINT
c for expmaxd
c	allocatable::w,tau
c	real*8 w(:),tau(:)
	real*8 w(100),tau(100)
	real*8 ypeak,tpeak,texp,yinf,yinf1,y0,a0,thigh,atot
	common/expd/ w,tau,yinf1,ncomp1	!for expmaxd, expfunc,expfunc1
	EXTERNAL EXPFUNC,EXPFUNC1
c
	logical discprt
	common/dp/discprt
c
	k=kA+kB+kC+kD
c
	ALLOCATE(amat(km,km,km),eigen(km),Q1(km,km),pinf(km),pt(km))
	ALLOCATE(amp(km))
c
c Use initial vector p0 as calculated in SIMJUMP (in common)
	if(step) then
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc1,IL,V1,QT,Q1,k,.false.)
	   xA1=conc1(nljump)	!for print
c=	   call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
	   call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at conc1
	   call QMAT(Q1,Amat,k,eigen,ibad)
c
	   do j=1,kA
		amp(j)=float(njump)*float(icur(j,1))
	   enddo
c
	   curinf=0.0
	   do j=1,kA
		curinf=curinf+amp(j)*sngl(pinf(j))
	   enddo
c
	   dt=(tmax-tmin)/float(ncalc-1)
	   cur=0.0
	   do i=1,ncalc
		t=tmin+float(i-1)*dt
		xcal(i,jcal)=t
c=	      call PTCALC(pt,0.001*t,bmj,p0,pinf,eigen,amat,k,km)
	      call PTCAL1(pt,0.001*t,p0,pinf,eigen,amat,k,km)
		cur=0.0
		do j=1,kA
		   cur=amp(j)*sngl(pt(j))
		enddo
		ycal(i,jcal)=cur
	   enddo
	else if(pulse) then
c Need to get a point exactly at tpulse
	   nc1=ifix(float(ncalc)*tpulse/tmax)
	   if(nc1.lt.20) nc1=20
	   if(nc1.gt.ncalc) nc1=ncalc
c  First the on jump
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   xA1=conc1(nljump)	!for print
	   call QSETD(conc1,IL,V1,QT,Q1,k,.false.)
	   call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at conc1
	   call QMAT(Q1,Amat,k,eigen,ibad)
c
	   do j=1,kA
		amp(j)=float(njump)*float(icur(j,1))
	   enddo
	   y0=0.0d0
	   do j=1,kA
		y0=y0 + dble(amp(j))*p0(j)			!for expmaxd
	   enddo
	   yinf=0.0d0
	   do j=1,kA
		yinf=yinf + dble(amp(j))*pinf(j)			!for expmaxd
	   enddo
c Need the bmj() for expmaxd, to calulate w(m)=coeff for each exponential
c component in whatever is being plotted (total current in this case)
	   call GETAMP(w,p0,amat,amp,k,kA,km)
c
	   dt=(tpulse-tmin)/float(nc1-1)
	   cur=0.0
	   do i=1,nc1
		t=tmin+float(i-1)*dt
		xcal(i,jcal)=t
	      call PTCAL1(pt,0.001*t,p0,pinf,eigen,amat,k,km)
		cur=0.0
		do j=1,kA
		   cur=amp(j)*sngl(pt(j))
		enddo
		ycal(i,jcal)=cur
	   enddo
	   print 331
	   if(discprt) write(8,331)
331	   format(/,' Calculated ON-relaxation')
	   call CALOUT(w,eigen,tau,y0,yinf,atot,k,km)
c calculate area (including asymptote term) from t=0 to t=tpulse
	   a0=0.d0
	   do m=1,k-1
		a0=a0 + w(m)*tau(m)*(1.d0 - dexp1(-tpulse/tau(m)))
	   enddo
	   a0=a0 + yinf*tpulse	!add asymptote term
c
	   thigh=dble(tpulse)	!look for max between t=0.1 and t=tpulse
	   ncomp1=k-1	!in common for expmaxd
	   call EXPMAXd(tpeak,ypeak,yinf,thigh,texp,
     &	.false.,discprt,.false.,km,nerr,nerr1,nerr1,0)
	   if(nerr.eq.-5) then
	      print 12,xA1*1.e6,ypeak
	      if(discprt) write(8,12) xA1*1.e6,ypeak
12		format(
     & 	' At conc = ',g13.6,' muM: monotonic, asymptote = ',g13.6)
	   else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
	      print 13,nerr,nerr1,nerr2,xA1*1.e6
	      if(discprt) write(8,13) nerr,nerr1,nerr2,xA1*1.e6
13		format(' Error: nerr,nerr1,nerr2 = ',3i3,
     &	    ' at conc = ',g13.6,' micromolar')
	   else if(nerr.eq.0) then
	      print 131,xA1*1.e6,ypeak,tpeak
	      if(discprt) write(8,131) xA1*1.e6,ypeak,tpeak
131		format(' At conc = ',g13.6,' muM, peak current = ',g13.6,
     &	 ' pA at t = ',f8.2,' ms')
	   endif
c
c     Now do off-jump in points nc1+1 to ncalc
	   call PTCAL1(pt,0.001*tpulse,p0,pinf,eigen,amat,k,km)
c       Occs at tpulse make p0() for the off jump
	   do i=1,k
		p0(i)=pt(i)
	   enddo
c       now at post-jump conc=xA2
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   xA2=conc2(nljump)	!for print
	   call QSETD(conc2,IL,V1,QT,Q1,k,.false.)
	   call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at xA1
	   call QMAT(Q1,Amat,k,eigen,ibad)
c
	   do j=1,kA
		amp(j)=float(njump)*float(icur(j,1))
	   enddo
	   y0=0.0d0
	   do j=1,kA
		y0=y0 + dble(amp(j))*p0(j)			!for expmaxd
	   enddo
	   yinf=0.0d0
	   do j=1,kA
		yinf=yinf + dble(amp(j))*pinf(j)			!for expmaxd
	   enddo
	   call GETAMP(w,p0,amat,amp,k,kA,km)
c
c Use log spacing of x (time) values for off jump
	   nc2=ncalc-nc1
c==	   dt=(tmax-tpulse)/float(nc2-1)
c=	   dlt=alog10((tmax/tpulse)/float(nc2-1)	!step in log(t)
c Make an x array which starts at zero (despite log scale), and ends at tmax
c so we get a point at t=0 (plotted at t=tpulse)
	   tmin1=0.01
	   tmax1=tmax-tpulse
	   dlt=alog10(((tmax1+tmin1)/tmin1))/float(nc2-1)	!step in log(t)
	   if(nc2.ge.1) then
		cur=0.0
		i1=0
		do i=nc1+1,ncalc
		   i1=i1+1
c===		   t=tmin+float(i1-1)*dt
		   t=tmin1*((10.0**(float(i1-1)*dlt)) - 1.)   !starts at tmin=0.0
		   xcal(i,jcal)=t+tpulse	!plot starts at tpulse
	         call PTCAL1(pt,0.001*t,p0,pinf,eigen,amat,k,km)
		   cur=0.0
		   do j=1,kA
			cur=amp(j)*sngl(pt(j))
		   enddo
		   ycal(i,jcal)=cur
		enddo
	   endif
	   print 332
	   if(discprt) write(8,332)
332	   format(/,' Calculated OFF-relaxation')
	   call CALOUT(w,eigen,tau,y0,yinf,atot,k,km)
	   print 10,tpulse,a0,tpulse,atot,a0+atot
	   if(discprt) write(8,10) tpulse,a0,tpulse,atot,a0+atot
10	   format(' Area (pC):',/,
     & '  from t=0 to t = ',f8.2,' ms: = ',g13.6,/,
     & '  after t = ',f8.2,' ms: = ',g13.6,/,
     & '  total area = ',g13.6,/)
c
	   thigh=-1.d0	!so set internally
	   ncomp1=k-1	!in common for expmaxd
	   call EXPMAXd(tpeak,ypeak,yinf,thigh,texp,
     &	.false.,discprt,.false.,km,nerr,nerr1,nerr2,0)
	   if(nerr.eq.-5) then
	      print 121,xA2*1.e6,ypeak
	      if(discprt) write(8,121) xA2*1.e6,ypeak
121		format(' At conc = ',g13.6,
     &	' micromolar: monotonic, asymptote = ',g13.6)
	   else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
	      print 13,nerr,nerr1,nerr2,xA1*1.e6
	      if(discprt) write(8,13) nerr,nerr1,nerr2,xA1*1.e6
c13		format(' Error: nerr,nerr1,nerr2 = ',3i3,
c     &	    ' at conc = ',g13.6,' micromolar')
	   else if(nerr.eq.0) then
	      print 131,xA2*1.e6,ypeak,tpeak
	      if(discprt) write(8,131) xA2*1.e6,ypeak,tpeak
c131		format(' At conc = ',g13.6,' muM, peak current = ',g13.6,
c     &	 ' pA at t = ',f8.2,' ms')
	   endif
c
	else if(varconc) then
	   do j=1,kA
		amp(j)=float(njump)*float(icur(j,1))
	   enddo

	   kp1=1024
	   ALLOCATE(xp(kp1),yp(km,kp1))	!km=100 at present
	   tt0=0.d0
	   tt1=dble(tmax)	!milliseconds
	   eps=1.0d-4
	   h1=0.1d0          !first step size (ms)
c	   hmin=0.0d0        !min step size
c	   hmin=0.0000001d0     !min step size (ms)
	   hmin=1.d-20		   !min step size (ms)
	   kmax=1024         !max number of values (time points) to be saved
	   dxsav=dble(dt)    !interval at which to keep values (as close as poss)
c=	   CALL ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,DERIVS,RKQC)
c=	   call ODEINT(p0,k,tt0,tt1,eps,h1,hmin,nOK,Nbad,derivsim,RKQC)
c use special version of odeint, so allocatable arrays, xp,yp (which cannot
c	         be in common) are in the call
	   call ODEINT1(p0,k,tt0,tt1,eps,h1,hmin,nOK,Nbad,derivsim,RKQC,
     &	nshort,xp,yp,km,kp1)
c       Next bit stores fraction in each state at time xp(i)
c	   do i=1,kount
c		do j=1,k
c		   xcal(i,j)=sngl(xp(i))		!from common/path -in ms still
c		   ycal(i,j)=sngl(yp(j,i))		!ditto
c		enddo
c	   enddo
c  For now just keep total current
	   ncalj=kount	!to return number of values calculated
	   do i=1,kount
		xcal(i,jcal)=sngl(xp(i))		!from common/path -in ms still
	   enddo
	   do i=1,kount
		cur=0.0
		do j=1,kA
		   cur=amp(j)*sngl(yp(j,i))
		enddo
		ycal(i,jcal)=cur
	   enddo
	   DEALLOCATE(xp,yp)
c	   ntime=kount		!number of values actually calculated
c         print 242, ntime,tmax,dt
c         if(discprt) write(8,242) ntime,tmax,dt
c242	   format(
c     & ' Calculated ',i6,' points up to ',g13.6,' ms,',/,
c     & ' (at intervals of at least = ',g13.6,' ms)')
	endif
c
	DEALLOCATE(amat,eigen,Q1,pinf,pt)
c=	DEALLOCATE(w,tau)
	DEALLOCATE(amp)
c=	DEALLOCATE(bmj)
	RETURN
	end

	subroutine CALOUT(w,eigen,tau,y0,yinf,atot,k,km)
	real*8 w(km),eigen(km),tau(km),area,atot,yinf,y0
	logical discprt
	common/dp/discprt
c
c also returns atot
	print 36
      if(discprt) write(8,36)
36	format(
     & ' Component  eigenvalue        tau (ms)      amp at t=0 (pA)',
     & '    Area (pC)   ')
c
	atot=0.0d0
	do m=1,k-1
	   if(dabs(eigen(m)).gt.1.d-30) then
		tau(m)=-1.0d3/eigen(m)
	   else
		tau(m)=0.0d0
	   endif
	   area=w(m)*tau(m)
	   atot=atot+area
c
	   print 39,m,sngl(eigen(m)),tau(m),w(m),area
         if(discprt) write(8,39) m,sngl(eigen(m)),tau(m),w(m),area
39	   format(i8,3x,g13.6,3x,g13.6,3x,g13.6,4x,g11.4)
	enddo
	print 391,y0,yinf,atot
      if(discprt) write(8,391) y0,yinf,atot
391	format(/,
     & ' Total current at t=0 (pA) = ',g13.6,/,
     & ' Total current at equilibrium (pA) = ',g13.6,/,
     & ' Total area (pC) excluding asymptote = ',g11.4)
c
	RETURN
	end

	subroutine GETAMP(w,p0,amat,amp,k,kA,km)
	real*8 w(km),bm,amat(km,km,km),p0(km)
	real*4 amp(km)
	allocatable::bmj
	real*8 bmj(:,:)
c
	ALLOCATE(bmj(km,km))
	do j=1,k
	   do m=1,k-1
		bm=0.0d0
		do i=1,k
		   bm=bm + p0(i)*Amat(i,j,m)
		enddo
		bmj(m,j)=bm
	   enddo
	enddo
	do m=1,k-1
	   w(m)=0.0d0
	   do j=1,kA
		w(m)=w(m)+bmj(m,j)*dble(amp(j))
	   enddo
	enddo
	DEALLOCATE(bmj)
	RETURN
	end

c Functions for expmaxd:

	real*8 function EXPFUNC(t)
c Evaluate exponentials
	real*8 s,t,w,tau,yinf
	common/expd/ w(100),tau(100),yinf,ncomp
c
	s=0.0d0
	do i=1,ncomp
	   s=s + w(i)*dexp1(-t/tau(i))
	enddo
	EXPFUNC=s+yinf
	RETURN
	end

	real*8 function EXPFUNC1(t)
c First derivative of exponentials
	real*8 s,t,w,tau,yinf
	common/expd/ w(100),tau(100),yinf,ncomp
c
	s=0.0d0
	do i=1,ncomp
	   s=s - (w(i)/tau(i))*dexp1(-t/tau(i))
	enddo
	EXPFUNC1=s
	RETURN
	end


