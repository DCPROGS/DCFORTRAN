	subroutine DERIVSIM(x,y,dydx)
c Subroutine for ODEINT for use in SCALCS
c DERIVSIM is version of DERIVS for use in SCSIM
c
c x=time, y=p(j), dydx=dp(t)/dt
c
c Problem is that QD needed here, and cannot be in common if either (a) a
c parameter of calling routine, or (b) allocated in calling routine. Therefore
c copy of QD made in SCSIM for common
c NB QD1 is in reciprocal milliseconds, because time throughout is in ms
c slope =dp(t)/dt = p(t)*QD1 is in recip milliseconds
c
	real*8 x,y(1),dydx(1)
	real*8 QT(100,100),Q1(100,100)
	common/qblk1/QT,conc(10),v1
	COMMON/dimblk/K,KMAX,ir,nmod
	COMMON/CPAR/NCDEP,IX(100),JX(100),dum
	COMMON/LIG/nlig,IL(100)
c
	time=sngl(x)				!in ms for conc(t)
	conc=CALCONC(time)	!molar
c=	call QNEWC(QD1,cA1,cB1,Q1,conc,xB0,ncdep,nlig,IL,IX,JX,k,kmax)
	call QSETD(conc,IL,V1,QT,Q1,k,.false.)
c Calculate the derivatives
	do j=1,k
	   d=0.0d0
	   do i=1,k
		d=d + y(i)*q1(i,j)
	   enddo
	   dydx(j)=d
	enddo
c
	RETURN
	end

