	program TOCC
c to test ec50 routines for hjcfit
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(100,100), QD(100,100), theta0(200), peq(100)
c For ec50_hjc
	real*4 xA,curmax,curinfs,concmax
	real*4 conc(10,10),vhold,vref,V1
	real*8 dgamma(100)
	integer irate(200),jrate(200)
	logical monot
	COMMON/LIG/nlig,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(100),jx(100),x
c For qset_hjc
	integer NSC(50),IM(50,100),JM(50,100)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	common/CBLK/nset,conc,jsetlast		!for HJCLIK,DISP etc
	integer IQf(100,100),irate1(200),jrate1(200)
	common/QDBLK/QT,QD,npar,IQf,irate1,jrate1,nlig1
	COMMON/MPAR/NCYC,NSC,IM,JM
c	COMMON/indblk/nqset,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	logical fixec50,prtec50
	real*8 ec50,xqlo,xqhi
	common/ec/fixec50,imod0,ec50,i50,j50,m50,prtec50,xqlo,xqhi
c
	one=1.0d0
	zero=0.0d0
	two=2.0d0
	four=4.0d0
	half=one/two
c
c Milone 97 without block (model 29 in DC qmodel.dat)
c 1     q( 1, 4)=   alpha2        1500.00
c 2     q( 4, 1)=   beta2         50000.0
c 3     q( 2, 5)=   alpha1a       13000.0
c 4     q( 5, 2)=   beta1a        9.00000
c 5     q( 3, 6)=   alpha1b       15000.0
c 6     q( 6, 3)=   beta1b        7.00000
c 7     q( 4, 6)=   k(-2)b        6000.00
c 8  *  q( 6, 4)=   k(+2)b       0.100000E+09
c 9     q( 4, 5)=   k(-2)a        5000.00
c10  *  q( 5, 4)=   k(+2)a       0.100000E+09
c11     q( 5, 7)=   k(-1)a        6000.00
c12  *� q( 7, 5)=   k(+1)a       0.100000E+09
c13     q( 6, 7)=   k(-1)b        5000.00
c14  *  q( 7, 6)=   k(+1)b       0.100000E+09
c
	THETA0(1)=1846.55004882812
	THETA0(2)=51119.0000000000
	THETA0(3)=6690.00000000000
	THETA0(4)=83.0000000000000
	THETA0(5)=38809.0000000000
	THETA0(6)=78.8090057373047
	THETA0(7)=1246.19995117188
	THETA0(8)=7693823.24218750
	THETA0(9)=14653.0000000000
	THETA0(10)=1070000000.00000
	THETA0(11)=1246.19995117188
	THETA0(12)=7693823.24218750
	THETA0(13)=14653.0000000000
	THETA0(14)=1070000000.00000
	alpha2=theta0(1)
	beta2=theta0(2)
	alpha1a=theta0(3)
	beta1a=theta0(4)
	alpha1b=theta0(5)
	beta1b=theta0(6)
	ak22b=theta0(7)	!k(-2)b
	ak12b=theta0(8)	!k(+2)b
	ak22a=theta0(9)	!k(-2)a
	ak12a=theta0(10)	!k(+2)a
	ak21a=theta0(11)	!k(-1)a
	ak11a=theta0(12)	!k(+1)a
	ak21b=theta0(13)	!k(-1)b
	ak11b=theta0(14)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c
	b1=two*E1a/aK1a + two*E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=two*(one+E1a)/aK1a + two*(one+E1b)/aK1b
	a2=(one+E2)/(aK1a*aK2a)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	   call BELL(1)
	   print 1,disc
1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif
c======== Debug
	x=16.81*1.d-6
	popen=(b1*x+a1*x*x)/(1+b2*x+a2*x*x)
	prel=popen/pmax
	x=ec50
	popen=(b1*x+a1*x*x)/(1+b2*x+a2*x*x)
	prel=popen/pmax
c Now ec50_hjc
	kA=3
	kB=1
	kC=3
	kD=0
	NLVAR=1
	VHOLD=-80.0000
	VREF=-80.0000
	CFACD=-80000000000.0000
	DGAMMA(1)=0.600000000000000D-10
	DGAMMA(2)=0.600000000000000D-10
	DGAMMA(3)=0.600000000000000D-10
	K=7
	KDIM=100
	nlig=1
	nlig1=1
	ncdep=4
	IL(1)=1
	IL(2)=1
	IL(3)=1
	IL(4)=1
	IX(1)=7
	IX(2)=6
	IX(3)=7
	IX(4)=5
	JX(1)=6
	JX(2)=4
	JX(3)=5
	JX(4)=4
c
	IRATE(1)=1
	IRATE(2)=4
	IRATE(3)=2
	IRATE(4)=5
	IRATE(5)=3
	IRATE(6)=6
	IRATE(7)=4
	IRATE(8)=6
	IRATE(9)=4
	IRATE(10)=5
	IRATE(11)=5
	IRATE(12)=7
	IRATE(13)=6
	IRATE(14)=7
	JRATE(1)=4
	JRATE(2)=1
	JRATE(3)=5
	JRATE(4)=2
	JRATE(5)=6
	JRATE(6)=3
	JRATE(7)=6
	JRATE(8)=4
	JRATE(9)=5
	JRATE(10)=4
	JRATE(11)=7
	JRATE(12)=5
	JRATE(13)=7
	JRATE(14)=6
	do i=1,k
	   do j=1,k
		QT(i,j)=zero
	   enddo
	enddo
	npar=14
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   QT(i,j)=theta0(m)
	enddo
c
	ncyc=1
	IM(1,1)=7
	IM(1,2)=5
	IM(1,3)=4
	IM(1,4)=6
	JM(1,1)=5
	JM(1,2)=4
	JM(1,3)=6
	JM(1,4)=7
c
	NEQ=3
	IE(1)=4
	IE(2)=4
	IE(3)=5
	JE(1)=6
	JE(2)=5
	JE(3)=4
	IF(1)=5
	IF(2)=6
	IF(3)=7
	JF(1)=7
	JF(2)=7
	JF(3)=6
	EFAC(1)=1.000
	EFAC(2)=1.000
	EFAC(3)=1.000
c
	IRATE1(1)=1
	IRATE1(2)=4
	IRATE1(3)=2
	IRATE1(4)=5
	IRATE1(5)=3
	IRATE1(6)=6
	IRATE1(7)=5
	IRATE1(8)=6
	IRATE1(9)=7
	JRATE1(1)=4
	JRATE1(2)=1
	JRATE1(3)=5
	JRATE1(4)=2
	JRATE1(5)=6
	JRATE1(6)=3
	JRATE1(7)=7
	JRATE1(8)=7
	JRATE1(9)=6
c
	nqset=0
	imod0=29
	fixec50=.true.
	I50=6
	J50=4
	M50=8
	ec50=16.81d-6
c
	call EC50_HJC(EC50,curinfs,monot,curmax,concmax,QT,
     & conc,nlvar,vhold,vref,cfacd,dgamma,k,kdim)

	end













