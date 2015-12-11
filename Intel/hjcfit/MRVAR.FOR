	subroutine MRVAR(i,j,covar,jcov,kgood,obeymr,IQ,cvmr,npar)
	use menu_f90
	CHARACTER*11 cnum1,cnum2
c===at present this fails if any of the rates from which MR value
c===is calculated are values that are constrained -but can insert
c===errors for constrained values as var=efac^2*var(value from which
c===constrained val is calc)


c To calculate approx CV for rates found by micro rev in hjcfit
c Output: cvmr= approx CV for MR rate q(i,j) calculated as
c   approx SD of log(q(i,j))
c   cvmr=-1. if any of the rates needed in the MR calculation have
c		 not got valid entries in covar matrix
c Input:
c COVAR=covar matrix for log(rate) (size=kgood)
c jcov(m), m=1,npar, such that jcov(i)=n where n=row/col number
c in covar matrix that corresponds to parameter #i, and jcov(n)=-1 if
c parameter #i does not appear in the covar matrix
c
c For cycle #L the MR rate is calculated thus
c		fac=1.0d0
cc calc factor
c		do m=2,nsc(L)
c		   i=IM(L,m)
c		   j=JM(L,m)
c		   fac=fac*QD(j,i)/QD(i,j)
c		enddo
c		i=im(L,1)	!value to be calc for cycle #L
c		j=jm(L,1)
c		QD(i,j)=QD(j,i)*fac
c
c	real*8 covar(ktd,ktd)
	real*8 covar(npar,npar)
	real*8 vlf
	real*4 cvmr
	integer*4 IQ(100,100),jcov(200)
	logical obeymr(50), allocated
	allocatable::a,cov,irc
	real*8 a(:),cov(:,:)
	integer*4 irc(:)
C
	integer NSC(50),IM(50,100),JM(50,100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	integer isetmr(50)
	common/mr1/isetmr
	common/tty/ittypanel,itty

c
c Check which cycle is used to calculate rate(i,j)
	do n=1,ncyc
	   if(i.eq.im(n,1).and.j.eq.jm(n,1).and.obeymr(n)) then
		nc=n
		goto 10
	   endif
	enddo
	
	call intconv(i,cnum1)
	call intconv(j,cnum2)
	CALL GMSETTEXTSETTING(ITTY,' MR route '//cnum1(1:3)//'-'//
     &cnum2(1:3)//' not found in MRVAR')
	cvmr=-1.
	
	goto 99
c
c cycle #nc is used to calculate q(i,j)
c
c  Example nsc(1) =4
c    im()   jm()
c 1   1      2
c 2   2      3
c 3   3      4
c 4   4      1
c  q(1,2)=q(2,1)*q(1,4)*q(4,3)*q(3,2)
c                ___________________
c		     q(4,1)*q(3,4)*q(2,3)
c
c or for lq(i,j)=log(q(i,j))
c   lq(1,2)=lq(2,1)+lq(1,4)+lq(4,3)+lq(3,2) - (lq(4,1)+lq(3,4)+lq(2,3))
c This is a linear function with 2n - 1 terms (n=nsc(nc)), so make an
c array of coefficients, a(i)=+1. or -1. in this case, and make
c a matrix of this size that contains the variances and covariances
c of the terms on the RHS. Then use general formula for variance of
c a linear function.
10	continue
	n=nsc(nc)		!number of routes in the cycle
	nd=2*n-1		!number of variables in linear function
	ALLOCATE(a(nd),cov(nd,nd),irc(kgood))
c First the n terms before the minus sign
	j1=0
	do i1=1,n
	   j1=j1+1
	   a(j1)=1.d0
	   i=jm(nc,i1)
	   j=im(nc,i1)
	   m=IQ(i,j)	!param number for this rate
	   if(jcov(m).eq.-1) then
		cvmr=-1.
		goto 99
	   endif
	   irc(j1)=jcov(m)	!row/col for param #m in covar()
	enddo
c Now the n-1 terms after the minus sign
	do i1=2,n
	   j1=j1+1
	   a(j1)=-1.d0
	   i=im(nc,i1)
	   j=jm(nc,i1)
	   m=IQ(i,j)	!param number for this rate
	   if(jcov(m).eq.-1) then
		cvmr=-1.
		goto 99
	   endif
	   irc(j1)=jcov(m)	!row/col for param #m in covar()
	enddo
c
	if(j1.ne.nd) then
	   CALL GMSETTEXTSETTING(ITTY,' Error in MRVAR') 
	   
	endif
c
c Copy the relevant row/col from COVAR to COV
	ii=0	!index for cov()
	jj=0
	kk=1	!index for irc()
	do i=1,kgood	!go through all rows/cols of COVAR
	   if(i.eq.irc(kk)) then
		ii=ii+1
		do j=1,kgood
		   if(j.eq.irc(kk)) then
			jj=jj+1
			cov(ii,jj)=covar(i,j)
		   endif
		enddo
	   endif
	   kk=kk+1
	enddo
c
	call VARLINF(a,cov,nd,vlf,nd)

c
	if(vlf.ge.0.0d0) then
	   cvmr=sngl(dsqrt(vlf))
	else
	   cvmr=-1.0
	endif
c
99	continue
	if(allocated(cov)) DEALLOCATE(a,cov,irc)
	RETURN
	end

	subroutine VARLINF(a,covar,n,vlf,kdim)
c To calculate variance of a linear function f=SUM[a(i)*x(i)], where covar()
c is the covariance matrix of x, i=1,..,n
	real*8 a(kdim),covar(kdim,kdim),vlf
c
	vlf=0.d0
	do i=1,n
	   vlf=vlf + a(i)*a(i)*covar(i,i)
	enddo
	do i=1,n
	   do j=1,n
		if(i.ne.j) then
		   vlf=vlf + a(i)*a(j)*covar(i,j)
		endif
	   enddo
	enddo
	RETURN
	end

