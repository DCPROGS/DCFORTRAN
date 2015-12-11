	real*8 function DETWF(s)
	USE MENU_F90
c To calculate det(W(S)) for HJC distributions.  Also returns, via COMMON
c the matrix WF(s) via common
c DET(W(s)) for SHUT times
c For open times W(s) is kA*kA matrix
c For shut times W(s) is kF*kF matrix
c
c Modified 07/10/03 02:21pm to use determ2 and to treat overflow and underflow
c better -this is increasing problem in mechs with many states. Determ2 returns
c ndscale = number of factors of 1.d-10 that have been applied to keep det
c from overflowing (negative ndscale = factors of 1.d10 to prevent underflow)
c common/det4/ndscale,idsign
c added so ndscale, idsign available elsewhere
c idsign=1 if det>0, idsign=-1 if det<0, idsign=0 if det=0
c
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c Keep commons /detw1 and /detw3 so make QAF etc fixed size (use PARAMETER so
c easy to change max kA)
	PARAMETER kAx=20 	!max number of open states fir following
	real*8 QAF(kAx,100),QFA(100,kAx),QAA(kAx,kAx),QFF(100,100)
	real*8 EXPQA(kAx,kAx),EXPQF(100,100)
	real*8 WA(kAx,kAx),WF(100,100)
c=	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA   !for DETWA,DETWF,dARSdS -now param
c=	common/detw3/WA,WF			  !for DETWA,DETWF -now params
c=	real*8 QAF(10,10),QFA(10,10),QAA(10,10),QFF(10,10)
c=	real*8 EXPQF(10,10),EXPQA(10,10),WA(10,10),WF(10,10)
c Local
	real*8 W(100,100),Q1(100,100)
c	integer lwork(100),mwork(100)	!not needed for determ2
	common/KBLK/kA,kB,kC,kD
	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF
	common/detw2/tres,km,nerr			!for HJCLIK,DETWA,DETWF
	common/detw3/WA,WF					   !for DETWA,DETWF
	common/det4/ndscale,idsign
	real*8 dexp1
		common/tty/ittypanel,itty
c
	kF=kB+kC+kD
	if(km.ne.100) then
	CALL GMSETTEXTSETTING(ITTY,	' km is not equal to 100 in DETWF')
	
	
	   STOP
	endif
c
c exp[-(sI-QAA)tres]= exp(-s*tres)*exp(QAA*tres)=exp(-s*tres)*EXPQA
c Calc I-exp[-(sI-QAA)tres] in this loop, in W. Also get (sI-QAA) in Q1 in
c same loop
	one=1.0d0
	nerr=0
	s1=-s*tres
	if(dabs(s1).gt.709.d0) then
	   nerr=4
	   RETURN
	endif
	est=dexp1(s1)
	do i=1,kA
	   do j=1,kA
		if(i.ne.j) then
		   W(i,j)=-est*EXPQA(i,j)
		   Q1(i,j)=-QAA(i,j)
		else
		W(i,j)=one-est*EXPQA(i,j)
		Q1(i,j)=s-QAA(i,j)
		endif
	   enddo
	enddo
c
c Calc inv(sI-QAA) in Q1
      call MATINV(Q1,kA,km,Q1,km)
c Postmult by I-exp[-(sI-QAA)tres]
      call MATMUL(Q1,W,W,kA,kA,kA,one,
     & km,km,km,km,km,km)
c Postmult by QAF
      call MATMUL(W,QAF,W,kA,kA,kF,one,
     & km,km,kAx,km,km,km)
c Premult by QFA
	call MATMUL(QFA,W,W,kF,kA,kF,one,
     & km,kAx,km,km,km,km)
c and add QFF to complete H(s), then calc W(s)=sI-H(s)
	do i=1,kF
	   do j=1,kF
		if(i.ne.j) then
		   W(i,j)=-(QFF(i,j)+W(i,j))
		else
		   W(i,j)=s-(QFF(i,j)+W(i,j))
		endif
	   enddo
	enddo
c Copy to WF for return via COMMON
	do i=1,kF
	   do j=1,kF
		WF(i,j)=W(i,j)
	   enddo
	enddo
c Now calc the determinant of W(s)
c
c DETV altered to prevent overflow: value of DET
c which is output must be mult by 10**ndexp to get correct determinant
c
c 01/21/02 05:11pm Still get occasional overflow in DETV
c e.g. with 5x5 matrix which is near diagonal and elements all
c around 1.d174. Can do rough check for this by looking at
c product of diagonals first
c 07/10/03 12:55pm can remove this bit because DETERM2 should never
c overflow!
c	sumdiag=0.0d0
c	do i=1,kF
c	   sumdiag=sumdiag+dlog10(dabs(W(i,i)))
c	enddo
c	if(sumdiag.gt.234.d0) then
c	   nerr=10		!overflow of det() likely
c	   RETURN
c	endif
c
c	call ARRAYD(2,kF,kF,km,km,W,W)	!convert to 1-D storage
cc	call DETV(W,kF,det,lwork,mwork,ndexp)	!DETV is in EQOCCD
c	call DETERM1(W,kF,det,lwork,mwork,ndexp,nerr)
c	print 4,det,ndexp
c4	format(' det, ndexp = ',g13.6,i10)
c	DETWF=det*(10.d0**ndexp)
c Try determinant routine from numerical recipes
c	subroutine DETERM2(A,n,ndim,det,ndscale)
c ndscale=number of factors of 10^-10 that det was multiplied by prevent
c overflow/underflow
c For most calcs need only the sign of det, or sign of slope
c dsign(a,b) transfers sign of b to sign of a
c dsign(1.d0,det) gives +1 or -1
c	subroutine DETERM2(A,n,ndim,det,ndscale)
	ndim=100	!declared size of W
	call DETERM2(W,kF,ndim,det,ndscale)
	detsign=dsign(1.d0,det)		!+1 or -1 (real*8)
	idsign=int4(detsign)
	if(det.eq.0.d0) idsign=0
c Correct det for scaling (and leave with ndscale=0) if possible
c Otherwise return with scaled det and ndscale not zero
	if(ndscale.ne.0) then
c	   dn=dlog(10.d0**(10*ndscale))	!arg can overflow!
	   dn=dfloat(10*ndscale)*dlog(10.d0)	!this form OK
	   dd=dlog(dabs(det))
	   if(dn+dd.lt.308.d0) then	!det can be corrected before leaving
		det=det*10.d0**(10*ndscale)
		ndscale=0	!DET has correct value now
	   endif
	endif
c
	DETWF=det
c
	RETURN
	end
