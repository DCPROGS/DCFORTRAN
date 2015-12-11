	SUBROUTINE QMAT1(Q,EM,EN,k,eigen,ibad,kQ,kN,kE)
C
c QMAT1 is a version of QMAT5 that returns EM, EN rather than AMAT.  More
c convenient for diagonalisation and called by QUNC to get uncoupled model
c
c Modif 03/22/01 03:20pm by tidying code (qmat.for is streamlined version with
c fewer checks)
c
c Modif 06/21/99 10:42am by adding common/materr/ifail,ibad
c
c###Modif 08/07/90 07:02pm For time pdfs (negative eigenvalues) want
c fastest tau (=largest neg eigenvalue) first, so sort into ascending
c order. For geometric distns want smallest mean first; eigenvalues are pos
c so again sort into ascending order
C***7-OCT-81 MODIF OF QMAT2 THAT (1)USES EM,EN AS OUTPUT
C RATHER THAN AMAT TO SAVE STORAGE- CALC AMAT(I,J,M)=EM(I,M)*EN(M,J)
C IN MAIN PROG. (2)DIMENSIONS OF Q (=KQ), OF EM AND EN (=KN),
C AND OF EIGEN (=KE) GIVEN AS PARAMETERS
C 22-MAY-81. QMAT2 IS QMAT1 BUT A,Q,EIGEN ALL REAL*8.
C **** SEEMS THAT Q MATRIX MUST BE 8*8 IN CALL PROG FOR F02AGF
C TO FUNCTION PROPERLY!! EVEN THOUGH ONLY TOP K*K ELEMENTS
C USED. MUST ALSO HAVE A(8,8,8) AND EIGEN(8) IN CALLING
C PROG IN THIS VERSION.
C 30-APR-81. QMAT1 IS VERSION OF QMAT WITH FEWER PARAMETERS FOR USE
C WITH SCGEN1. NEED ONLY OUTPUT AMAT AND EIGENVALUES. INTERNAL CHECK
C INSERTED FOR (1) ZERO IMAGINARY PARTS, (2) QUALITY OF UNIT. IF
C THESE FAILED,IBAD RETURNED AS NON ZERO,OTHERWISE 0. ALL I/O IS SINGLE
C PRECISION IN THIS VERSION AND INPUT Q MATRIX NOT DESTROYED (STILL
C DOUBLE PRECISION INTERNALLY).
C TO CALCULATE EIGENVALUES AND A MATRICES OF SPECTRAL EXPANSION OF
C ANY GIVEN Q MATRIX. Q IS KXK. A(I,J,M) IS IJ'TH ELEMENT
C OF M'TH A MATRIX).EIGRL AND EIGIM ARE REAL AND IMAG PARTS OF
C EIGENVALUES. EMRL AND EMIM ARE SAME FOR M MATRIX (COLS=EIGENVECTORS)
C THIS PROG GIVES EIGENVALUES IN ASCENDING ORDER. UNIT=EMRL*EN.
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 Q(kq,kq),eigen(ke)
	real*8 EM(KN,KN),EN(KN,KN)
	allocatable eigen1,unit,mwork
	allocatable QD,eigrl,eigim,emrl,emim,iwork
	real*8 eigen1(:),unit(:,:)
	real*8 QD(:,:),eigrl(:),eigim(:),emrl(:,:),emim(:,:)
	integer mwork(:),iwork(:)
c
	logical calcdet
	real*8 det
c
	logical discprt
	common/dp/discprt
	common/materr/ifail,ibad1
c
	if(k.eq.1) then
	   eigen(1)=Q(1,1)
	   em(1,1)=1.d0
	   en(1,1)=1.d0
	   ibad=0
	   ifail=0
	endif
c
c===	km=k			!MAX K DEFINED BY input
c allocate exact size needed (may be kA, kF etc in call)
	ALLOCATE(eigen1(k),unit(k,k))
	ALLOCATE(QD(k,k),eigrl(k),eigim(k),emrl(k,k),emim(k,k))
	ALLOCATE(mwork(k),iwork(k))
	IBAD=0
	IFAIL=0
c
C MAKE COPY OF INPUT FOR F02AGF SO Q NOT LOST
	do i=1,k
	   do j=1,k
		QD(i,j)=Q(i,j)
	   enddo
	enddo
c
c all arrays allocated size k here
	CALL F02AGF(QD,k,k,EIGRL,EIGIM,EMRL,k,EMIM,k,IWORK,IFAIL)
c	TYPE *,' PASSED F02AGF'
C N.B. THIS DESTROYS QD
C M IS NORMALISED. CONVERT SO THAT ALL M(1,J)=1.0 (ALL DOUBLE PREC)
C IMAG PART (EMIM) SHOULD BE ZERO SO OMITTED FROM THIS STEP
	do j=1,k
	   if(dabs(emrl(1,j)).gt.1.0D-32) then
C I.E. OMIT ANY COLUMN OF WHICH TOP ELEMENT IS ZERO
		b=1.0d0/emrl(1,j)
		do i=1,k
		   emrl(i,j)=emrl(i,j)*b
		enddo
	   endif
	enddo
C I'TH COLUMN OF EMRL CONTAINS REAL PART OF EIGENVECTOR
C CORRESPONDING WITH I'TH EIGENVALUE,I.E. IT IS THE M MATRIX.
C
c Modif 08/07/90 07:02pm For time pdfs (negative eigenvalues) want
c fastest tau (=largest neg eigenvalue) first, so sort into ascending
c order. For geometric distns want smallest mean first; eigenvalues are pos
c so again sort into ascending order
c
	do i=1,k
	   mwork(i)=i	!KEEPS TRACK OF SORTED ORDER
	   eigen1(i)=eigrl(i)	!ascending order
	enddo
c In call to SORTD eigen1 and mwork must have same declared size
	call SORTD(eigen1,mwork,k,k)		!SORTS EIGENVALUES
	do j=1,k
         m=mwork(j)
	   do i=1,k
		QD(i,j)=emrl(i,m)	!EMRL WITH COLS REORDERED IN QD
	   enddo
	enddo
	do i=1,k
	   do j=1,k
		emrl(i,j)=QD(i,j)	!COPY SORTED M MATRIX BACK TO EMRL
	   enddo
	enddo

C END OF REORDERING
c
c Now invert EM; result in EN.
	calcdet=.false.
	call MATINV2(EMrl,k,k,EN,kN,calcdet,det,ndscale)
c check inversion
	call MATMUL(EMrl,EN,UNIT,k,k,k,1.d0,
     & k,k,kN,kN,k,k)
c
C DO CHECKS
c	zero=1.0d-10	!FOR CHECKS ON IMAG PARTS AND ON UNIT MATRIX
	umin=1.0d-7	!FOR CHECKS ON IMAG PARTS AND ON UNIT MATRIX
	one=1.0d0
	errmax1=0.d0	!max error in diagonal of unit matrix
	errmax2=0.d0	!max error in off-diagonal of unit matrix
	do i=1,k
	   if(dabs(eigim(i)).gt.umin) ibad=1
	   do j=1,k
		IF(DABS(EMIM(I,J)).GT.umin) IBAD=2
		if(i.eq.j) then
		   err=dabs(UNIT(i,j))-one
		   if(err.gt.errmax1) errmax1=err
		   if(err.gt.umin) then
			ibad=4
		   endif
		else
		   err=dabs(UNIT(i,j))
		   if(err.gt.errmax2) errmax2=err
		   if(err.gt.umin) then
			IBAD=3
		   endif
		endif
	   enddo
	enddo
	IF(IFAIL.NE.0) IBAD=5
	IF(IBAD.EQ.0) GOTO 999
	print 22,IFAIL,IBAD
	if(discprt) write(8,22) IFAIL,IBAD
22	FORMAT( ' ERROR IN QMAT1: ifail,ibad= ',2i8)
	if(ibad.eq.1) then
	   print 23
	   if(discprt) write(8,23)
23       format(' An imaginary eigenvalue element is >1.d-7')
	else if(ibad.eq.2) then
	   print 24
	   if(discprt) write(8,24)
24       format(' An imaginary eigenvector element is >1.d-7')
	else if(ibad.eq.3) then
	   print 25,errmax2
	   if(discprt) write(8,25) errmax2
25       format(' An off-diagonal of UNIT is >1.d-7: max error = '
     &	,g13.6)
	else if(ibad.eq.4) then
	   print 26,errmax1
	   if(discprt) write(8,26) errmax1
26       format(
     & ' A diagonal of UNIT differs from 1 by >1.d-7: max error = '
     &	,g13.6)
	else if(ibad.eq.5) then
	   print 27,ifail
	   if(discprt) write(8,27) ifail
27       format(' Error in FO2AGF: ifail = ',i5)
	endif
999	continue
c
c Copy to output arrays
	do i=1,k
	   eigen(i)=eigen1(i)
	   do j=1,k
		EM(i,j)=EMrl(i,j)
	   enddo
	enddo
c
	deallocate(eigen1,unit,mwork)
	deallocate(QD,eigrl,eigim,emrl,emim,iwork)
	RETURN
	END


