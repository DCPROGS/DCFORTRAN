	subroutine GfuncF(s,ng,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
	USE MENU_F90
c Subroutine to return Frank Ball's g(s)=ng=number of eigenvalues of H(s) that
c  are equal to or less than s, for SHUT times (GFUNCA is for open times).
c  H(s) is calculated as in DETWF.
c Input = s
c Output = ng
C MODIFIED 20-3-07 to save memory by making H,eigHAA and HAmat allocatable

c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c Keep commons /detw1 and /detw3 so make QAF etc fixed size (use PARAMETER so
c easy to change max kA)
c	PARAMETER kAx=60 	!max number of open states for following
c		!max number of open states fir following

	real*8 QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100)
	real*8 EXPQA(20,20),EXPQF(100,100)
	common/KBLK/kA,kB,kC,kD
c	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF
	
	real*8 dexp1,tres
	common/detw2/tres,km,nerr			!for HJCLIK,DETWA,DETWF
c
c	real*8 Q1(100,100)
	allocatable::Q1,H,HFmat,eigHFF
	real*8 Q1(:,:)
	real*8 H(:,:),HFmat(:,:,:),eigHFF(:)
c	real*8 H(100,100),HFmat(100,100,100),eigHFF(100)
	logical discprt
	common/dp/discprt
	common/tty/ittypanel,itty
	
	kAx=20 
c
	kF=kB+kC+kD
	if(km.ne.100) then
	CALL GMSETTEXTSETTING(ITTY,	' km is not equal to 100 in GFUNCF')	   
	   STOP
	endif
c
      nd=max(kf,ka)
	allocate(Q1(nd,nd),H(nd,nd),HFmat(nd,nd,nd),eigHFF(nd))

c exp[-(sI-QAA)tres]= exp(-s*tres)*exp(QAA*tres)=exp(-s*tres)*EXPQA
c Calc I-exp[-(sI-QAA)tres] in this loop, in H. Also get (sI-QAA) in Q1 in
c same loop
	one=1.0d0
	nerr=0
	s1=-s*tres
	if(dabs(s1).gt.709.d0) then
	   nerr=4
	   RETURN
	endif
	est=dexp1(s1)
	do i=1,nd
	   do j=1,nd
		if(i.ne.j) then
		   H(i,j)=-est*EXPQA(i,j)
		   Q1(i,j)=-QAA(i,j)
		else
		   H(i,j)=one-est*EXPQA(i,j)
		   Q1(i,j)=s-QAA(i,j)
		endif
	   enddo
	enddo
c
c Calc inv(sI-QAA) in Q1
c      call MATINV2(Q1,kA,km,Q1,km,.false.,det,ndscale)
      call MATINV2(Q1,nd,nd,Q1,nd,.false.,det,ndscale)
c Postmult by I-exp[-(sI-QAA)tres]
      call MATMUL(Q1,H,H,nd,nd,nd,one,
     & nd,nd,nd,nd,nd,nd)
c     & km,km,km,km,km,km)
c Postmult by QAF
      call MATMUL(H,QAF,H,nd,nd,nd,one,
     & nd,nd,kAx,km,nd,nd)
c     & km,km,kAx,km,km,km)
c Premult by QFA
	call MATMUL(QFA,H,H,nd,nd,nd,one,
     & km,kAx,nd,nd,nd,nd)
c     & km,kAx,km,km,km,km)
c Calculate H(s) explicitly for Ball method
	do i=1,nd
	   do j=1,nd
	      H(i,j)=QFF(i,j)+H(i,j)
	   enddo
	enddo
c Calculate eigenvalues of H(s)
c	call QMAT5(H,HFmat,kF,eigHFF,ibad,km,km,km)
	call QMAT5(H,HFmat,nd,eigHFF,ibad,nd,nd,nd)
c   Find g(s) = number of eigenvalues of H(s) that are equal to or less than s
	ng=0
	do i=1,nd
	   if(eigHFF(i).le.s) ng=ng+1
	enddo

	deallocate(Q1,H,HFmat,eigHFF)

	RETURN
	end

