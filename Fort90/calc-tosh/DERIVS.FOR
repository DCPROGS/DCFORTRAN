	subroutine DERIVS(x,y,dydx)
c x=time, y=p(j), dydx=dp(t)/dt
	real*8 x,y(1),dydx(1),s
	real*8 QT(100,100),Q1(100,100),ct
	common/qm/QT,k,nljump	!common with vconc1
	integer IX(100),JX(100),IL(100)		!NB 'ix' is used by RANDOM
	COMMON/LIG/nlig,IL
	COMMON/CPAR/NCDEP,IX,JX,x1
	real*4 conc0(10)
	common/c0/conc0		!common with vconc1.for
c
c Pass QT=Q matrix without concentrations in COMMON with units
c of (1/ms) or 1/(muM*ms).
c Modif 03/06/01 02:21pm for 100 states/10 ligands.  The variable conc is
c for ligand #nljump. If nlig>1 then other conc may not be zero but they
c are constant throughout.
c
	do i=1,k
	   do j=1,k
		q1(i,j)=qt(i,j)
	   enddo
	enddo
c
c Set the concentrations (variable for ligand #nljump, fixed for others)
c code for Function conct(t) is in VCONC1.for
	time=sngl(x)				!in ms for conc(t)
	ct=dble(conct(time)*1.e-6)		!in molar for Q matrix
	if(nlig.eq.1) then
	   do L=1,ncdep
		i=IX(L)
		j=JX(L)
		q1(i,j)=q1(i,j)*ct  !multiply appropriate values by c(t)
	   enddo
	else
	   do L=1,ncdep
		i=IX(L)
		j=JX(L)
		ilig=IL(L)
		if(ilig.eq.nljump) then
		   q1(i,j)=q1(i,j)*ct  !multiply appropriate values by c(t)
		else
		   x1=conc0(IL(L))
		   q1(i,j)=q1(i,j)*dble(x1)  !multiply by fixed conc in conc0()
		endif
	   enddo
	endif
c
c Recalculate the diagonals of Q1
	do i=1,k
	   s=0.d0
	   do j=1,k
		if(i.ne.j) then
		   s=s+q1(i,j)
		endif
	   end do
	   q1(i,i)=-s
	end do
c Calculate the derivatives
	do j=1,k
	   d=0.0d0
	   do i=1,k
		d=d + y(i)*q1(i,j)
	   end do
	   dydx(j)=d
	end do
c
	RETURN
	end

c	SUBROUTINE DERIVS(X,Y,DYDX)
c	DIMENSION Y(1),DYDX(1)
c	DYDX(1)=-Y(2)
c	DYDX(2)=Y(1)-(1.0/X)*Y(2)
c	DYDX(3)=Y(2)-(2.0/X)*Y(3)
c	DYDX(4)=Y(3)-(3.0/X)*Y(4)
c	RETURN
c	END
