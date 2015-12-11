	subroutine MATOSC2(AB,kA,kB,QM,km,GOSC,ko,QAAI,QBBI,GAB,GBA,k1)
c Like MATOSC but returns also GAB,QAAI=inv(QAA),GBA,QBBI=inv(QBB)
c Calculates directly from Q, the matrix GOSC=inv(I - GAB*GBA), which
c represents any number of A-B-A transitions (and similarly for other
c subscripts represented by the 1st argument)
c
C SUBROUTINE TO CALCULATE PROB TRANSITION MATRICES ,GAB ETC, FROM Q
C	GAB=-INV(QAA)*QAB (UP TO 8 X 8 IN SIZE)
C	AB=INTEGER CODE FOR REQ MATRIX (kA,kB=numbers in corresp subsets)
C	QM=DOUBLE PREC Q
C	km=DIM OF QM IN MAIN PROG (SQUARE)(same for QAAI,QBBI- see GMAT2)
C	ko=DIM OF Gosc IN MAIN PROG (SQUARE)
C	k1=DIM OF GAB,GBA,QAAI,QBBI in main prog (square, all same)
C
	allocatable::Q1
	real*8 Q1(:,:)
	real*8 QM(km,km),Gosc(ko,ko)
	real*8 GAB(k1,k1),GBA(k1,k1),QAAI(km,km),QBBI(km,km)
	real*8 one
	INTEGER AB
C
	kd=km		!dimension of allocatable arrays
	ALLOCATE(Q1(km,km))
	one=1.0d0
	call GMAT2(AB,QM,GAB,Km,k1,QAAI)	!GAB in GAB
C Separate the digits of 'AB' as in SUBMAT so AB='ij'
	i=AB/10	!LEFT HAND DIGIT
	j=mod(AB,10)	!RIGHT HAND DIGIT
	ji=j*10+i         !integer that represents 'BA'
	call GMAT2(ji,QM,GBA,Km,k1,QBBI)	!GBA in GBA
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
	DEALLOCATE(Q1)
	RETURN
	END

