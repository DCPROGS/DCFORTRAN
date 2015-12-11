	subroutine CALCMR(QD,k)
c Subroutine to find values of rate constants that obey
c microscopic reversibility in cubic models -or any for which there are
c cycles in which all rate constants are part of previously set MR cycles
c and therefore cannot be set in the usual way.
c npar, irate, jrate in commons
c
c This routine uses dfpmin to solve the whole set of MR equations for all cycles
c that are set to obey MR, for values of all the rate constants that appear
c in them (see MICROREV.MCD).

c Starting guesses are current parameters with MR
c imposed in the usual way in all cycles for which it is possible to do so.
c
c Care needed with fixed and constrained parameters!
c
c In theory progs MR is set in QSETD which has QT,QD as arguments but in HJCFIT
c MR is set in QSET_HJC (calls SETMR) which have theta as argument.  DFPMIN
c needs arguments as vector like theta().
c
c In case of theory programs, calculate array theta() before entry, that
c contains all the rate constants, together with indexing arrays IQ(), and
c irate(), jrate().  In HJCFIT call with theta0() which has all rates
c including fixed and indexing arrays as above
c
c npar=total number of rates in mechanism (inc fixed or constrained)
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QD(100,100)
	real*8 theta(200)		!internal for dfpmin
	integer jfix(200)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	logical obeymr(50),mrcalc
c Indexing arrays
c	integer IQ(100,100)
	integer isetmr(50)
	common/mr/obeymr
	common/mr1/isetmr,mrcalc
c commons for FUNCMR, DFUNCMR
c  Common for calcmr/dfpmin
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
	logical discprt
	common/dp/discprt
	EXTERNAL FUNCMR,DFUNCMR		!for dfpmin
c
c Make sure IQ is defined (using irate, jrate from model definition)
c	do i=1,100		!ditto
c	   do j=1,100
c		IQ(i,j)=0
c	   enddo
c	enddo
	do i=1,200
	   jfix(i)=0
	enddo
c Rate constants: initial values before global imposition of microscopic reversiblity
         print 13
	   if(discprt) write(8,13)
13	   format(
     &' Initial values of rates before global imposition of mic rev')
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
c=	   IQ(i,j)=m		!define IQ()
	   theta(m)=QD(i,j)	!define theta to hold all rates in mech
         print 12,m,i,j,theta(m)
	   if(discprt) write(8,12) m,i,j,theta(m)
12	   format(i3,2x,' q(',i2,',',i2,')=',2x,g13.6)
	enddo
c Now call dfpmin
	gtol=1.e-4
	stpmx=0.1
	ndisps=1
c	ndisps=0
c	ndisps=-1000	!silent
	nt=npar
     	call DFPMINd(theta,nt,gtol,iter,smin1,funcMR,dfuncMR,
     &  stpmx,jfix,ndisps)
	print 3,iter,smin1
	if(discprt) write(8,3) iter,smin1
3	format(
     & ' After ',i3,' iterations, s = ',g13.6,' (0 = exact solution)')
	smin1=FUNCmr(theta)		!check for debug

c Now put values in theta back into QD
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   QD(i,j)=theta(m)
	enddo
c	call CHECKMR(QD)	!now in QSETD etc
	RETURN
	end



	real*8 function FUNCMR(theta)
c To calculate function to be minimised in CALCMR() S=SUM[d*d] where
c d=difference between the two sides of the MR constraint, and sum is over
c the number of such constraints. For example
c d = k12*k23*k34*k41 - k21*k14*k43*k32
c
c theta()= array of all rates (Q elements)
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 theta(200)
	real*8 QD(100,100),r1,r2,d
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	logical obeymr(50),mrcalc
	integer isetmr(50)
	common/mr/obeymr
	common/mr1/isetmr,mrcalc
c commons for FUNCMR, DFUNCMR
c  Common for calcmr/dfpmin
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
c
c====convert theta to QD and get fixed values from QD, OR make some way to
c====index which values in theta constitute each cycle
c=== from qset_hjc
c  Fixed parameters are already in QT
c  First insert the kfit estimated param from theta()
c  Copy fixed values to QD (other values overwritten later)
c	do i=1,k
c	   do j=1,k
c		QD(i,j)=QT(i,j)
c	   enddo
c	enddo
c
c Copy current parameter estimates from theta (still without conc) into QD
c	do n=1,kfit
c	   i=irate1(n)
c	   j=jrate1(n)
c	   QT(i,j)=theta(n)	!define QT too
c	   QD(i,j)=theta(n)
c	enddo
c
c======
c Make QD()  from theta
	do n=1,npar
	   i=irate(n)
	   j=jrate(n)
c	   QT(i,j)=theta(n)	!define QT too
	   QD(i,j)=theta(n)
	enddo
c
	smr=0.0	!=SUM[d^2]
	do m=1,ncyc
	   if(obeymr(m)) then
		r1=1.d0
		r2=1.d0
		do j=1,nsc(m)
		   i1=im(m,j)
		   j1=jm(m,j)
		   r1=r1*QD(i1,j1)	!product one way round
		   r2=r2*QD(j1,i1)	!product other way round
		enddo
		d=r1-r2
		smr=smr + sngl(d*d)
	   endif
	enddo
	FUNCMR=smr
	RETURN
	end


	subroutine DFUNCMR(theta,gval)
c Returns oartial first derivative of MR relations calculated in FUNCMR()
c with respect to each parameter (rate constant)
c For example when
c d1 = k12*k23*k34*k41 - k21*k14*k43*k32
c d(d1)/d(k21) = -k14*k43*k32
c and since S=SUM[d*d]
c d(S)/d(k21) = SUM[2*di*d(di)/d(k21)]
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 theta(200),gval(200)
	real*8 QD(100,100),r1,r2
	logical obeymr(50),mrcalc
	integer isetmr(50)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	common/mr/obeymr
	common/mr1/isetmr,mrcalc
c commons for FUNCMR, DFUNCMR
c  Common for calcmr/dfpmin
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
c
c Make QD()  from theta
	do n=1,npar
	   i=irate(n)
	   j=jrate(n)
	   QD(i,j)=theta(n)
	enddo

	do n=1,npar		!differentiate wrt each of the npar rates in theta
	   i=irate(n)
	   j=jrate(n)
	   gval(n)=0.d0
c QD(i,j) is the current rate constant -need to locate ALL the cycles that
c contain it
	   do m=1,ncyc
		if(obeymr(m)) then
		   r1=1.d0
		   r2=1.d0
		   ifound=0
		   do j2=1,nsc(m)
			i1=im(m,j2)
			j1=jm(m,j2)
			r1=r1*QD(i1,j1)	!product one way round
			r2=r2*QD(j1,i1)	!product other way round
			if(i1.eq.i.and.j1.eq.j) then
			   ifound=1		!q(i,j) in r1
			else if(j1.eq.i.and.i1.eq.j) then
			   ifound=2		!q(i,j) in r2
			endif
		   enddo
		   d=r1-r2
		   if(ifound.eq.1) then 	!get product of all rates in r1 except q(i,j)
			gval(n)=gval(n) + d*r1/QD(i,j)
		   else if(ifound.eq.2) then 	!get product of all rates in r2 except q(i,j)
			gval(n)=gval(n) - d*r2/QD(i,j)
		   endif
		endif
	   enddo	!end of m=1,ncyc
	   gval(n)=2.d0*gval(n)		!include the factor of 2
	enddo		!end of n=1,npar
c
	RETURN
	end

