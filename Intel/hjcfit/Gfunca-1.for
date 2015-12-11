	subroutine GFUNCA(s,ng,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
	USE MENU_F90
c Subroutine to return Frank Ball's g(s)=number of eigenvalues of H(s) that
c  are equal to or less than s, for open times (GFUNCF is for shut times).
c  H(s) is calculated as in DETWA.
C MODIFIED 20-3-07 to save memory by making H,eigHAA and HAmat allocatable
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c Keep commons /detw1 and /detw3 so make QAF etc fixed size (use PARAMETER so
c easy to change max kA)
c	PARAMETER kAx=60 	!max number of open states fir following
!	PARAMETER kAx=20 	!max number of open states fir following

	real*8 QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100)
	real*8 EXPQA(20,20),EXPQF(100,100)
	real*8 dexp1,tres
	common/KBLK/kA,kB,kC,kD
c	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF
	common/detw2/tres,km,nerr				   !for DETWA,DETWF
c
c	real*8 Q1(100,100)
	allocatable::Q1,H,HAmat,eigHAA
	real*8 Q1(:,:)
	real*8 H(:,:),HAmat(:,:,:),eigHAA(:)
c	real*8 H(100,100),HAmat(100,100,100),eigHAA(100)
	logical discprt
	common/dp/discprt
		common/tty/ittypanel,itty
c

	kF=kB+kC+kD
	if(km.ne.100) then
	CALL GMSETTEXTSETTING(ITTY,	' km is not equal to 100 in GFUNCA')
	   STOP
	endif
      nd=max(kf,ka)
	allocate(Q1(nd,nd),H(nd,nd),HAmat(nd,nd,nd),eigHAA(nd))
c
c exp[-(sI-QFF)tres]= exp(-s*tres)*exp(QFF*tres)=exp(-s*tres)*EXPQF
c Calc I-exp[-(sI-QFF)tres] in this loop, in H. Also get (sI-QFF) in Q1 in
c same loop
	kax=20
	one=1.0d0
	s1=-s*tres
	nerr=0
	if(dabs(s1).gt.709.d0) then
	   nerr=4
	   RETURN
	endif
	est=dexp1(s1)
	do i=1,nd
	   do j=1,nd
		if(i.ne.j) then
	 	   H(i,j)=-est*EXPQF(i,j)
	 	   Q1(i,j)=-QFF(i,j)
		else
		   H(i,j)=one-est*EXPQF(i,j)
		   Q1(i,j)=s-QFF(i,j)
		endif
	   enddo
	enddo
c
c Calc inv(sI-QFF) in Q1
      call MATINV2(Q1,nd,nd,Q1,nd,.false.,det,ndscale)
c      call MATINV2(Q1,kF,km,Q1,km,.false.,det,ndscale)
c Postmult by I-exp[-(sI-QFF)tres]
      call MATMUL(Q1,H,H,nd,nd,nd,one,
     & nd,nd,nd,nd,nd,nd)
c     & km,km,km,km,km,km)
c Postmult by QFA
      call MATMUL(H,QFA,H,nd,nd,nd,one,
     & nd,nd,km,kAx,nd,nd)
c     & km,km,km,kAx,km,km)
c Premult by QAF
	call MATMUL(QAF,H,H,nd,nd,nd,one,
     & kAx,km,nd,nd,nd,nd)
c     & kAx,km,km,km,km,km)
c Add QAA to complete H(s)
c Calculate H(s) explicitly for Ball method
	do i=1,nd
	   do j=1,nd
	      H(i,j)=QAA(i,j)+H(i,j)
	   enddo
	enddo
c Calculate eigenvalues of H(s)
c	call QMAT5(H,HAmat,kA,eigHAA,ibad,km,km,km)
	call QMAT5(H,HAmat,nd,eigHAA,ibad,nd,nd,nd)

c   Find g(s) = number of eigenvalues of H(s) that are equal to or less than s
	ng=0
	do i=1,nd
	   if(eigHAA(i).le.s) ng=ng+1
	enddo
	
c
	deallocate(Q1,H,HAmat,eigHAA)
	RETURN
	end

