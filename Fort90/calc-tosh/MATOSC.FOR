	subroutine MATOSC(AB,kA,kB,QM,km,GOSC,ko)
c Calculates directly from Q, the matrix GOSC=inv(I - GAB*GBA), which
c represents any number of A-B-A transitions (and similarly for other
c subscripts represented by the 1st argument)
c
C SUBROUTINE TO CALCULATE PROB TRANSITION MATRICES ,GAB ETC, FROM Q
C	GAB=-INV(QAA)*QAB (UP TO 8 X 8 IN SIZE)
C	AB=INTEGER CODE FOR REQ MATRIX (kA,kB=numbers in corresp subsets)
C	QM=DOUBLE PREC Q
C	km=DIM OF QM IN MAIN PROG (SQUARE)
C	ko=DIM OF Gosc IN MAIN PROG (SQUARE)
C
	allocatable::GAB,GBA,Q1
	real*8 GAB(:,:),GBA(:,:),Q1(:,:)
	real*8 QM(km,km),Gosc(ko,ko)
	real*8 one
	INTEGER AB
C
c	kd=10		!dimension of allocatable arrays
	kd=km		!dimension of allocatable arrays
	ALLOCATE(GAB(km,km),GBA(km,km),Q1(km,km))
	one=1.0d0
	call GMAT1(AB,QM,GAB,Km,Km)	!GAB in GAB
C Separate the digits of 'AB' as in SUBMAT so AB='ij'
	i=AB/10	!LEFT HAND DIGIT
	j=mod(AB,10)	!RIGHT HAND DIGIT
	ji=j*10+i         !integer that represents 'BA'
	call GMAT1(ji,QM,GBA,Km,Km)	!GBA in GBA
	call MATMUL(GAB,GBA,Q1,kA,kB,kA,one,
     & Kd,Kd,Kd,Kd,Kd,Kd)			!GAB*GBA in Q1
c Get I-Q1 in Gosc
	do 96 i=1,kA
	do 96 j=1,kA
	Gosc(i,j)=-Q1(i,j)
	if(i.eq.j) Gosc(i,j)=one-Q1(i,j)
96	continue
	call MATINV(Gosc,kA,ko,Gosc,ko)	!inv(I-GAB*GBA) in Gosc
c
	DEALLOCATE(GAB,GBA,Q1)
	RETURN
	END

