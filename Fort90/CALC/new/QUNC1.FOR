	SUBROUTINE QUNC1(Q,k,ibad,kQ)
c
c To calculate the Q matrix, QUNC, for the totally uncoupled mechanism that
c is similar to the mechanism described by Q  (Kienker 1989, page 292)
c
c Need SAA=matrix with cols=eigenvectors of QAA
c Need SFF=matrix with cols=eigenvectors of QFF
c
c Calls QMAT1 to return M, N rather than AMAT
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 Q(kq,kq)
	integer AA,AF,FA,FF
	allocatable:: QD,QUNC,Q1,Q2
	real*8 QD(:,:),QUNC(:,:),Q1(:,:),Q2(:,:)
	allocatable::EMAA,ENAA,eigAA
	allocatable::EMFF,ENFF,eigFF
	real*8 eMAA(:,:),eMFF(:,:),eNAA(:,:),eNFF(:,:)
	real*8 eigAA(:),eigFF(:)
c	allocatable:: unit,mwork,iwork
c	real*8 unit(:,:)
c	integer mwork(:),iwork(:)
c
c	logical calcdet
c	real*8 det
c
	logical discprt
	common/dp/discprt
	common/materr/ifail,ibad1
	common/KBLK/KA,KB,KC,KD
c
c
	AA=11
	AF=15	!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	kF=kB+kC
	kD=0
c
	ALLOCATE(QD(k,k),QUNC(k,k),Q1(k,k),Q2(k,k),eigAA(k),eigFF(k))
	ALLOCATE(emAA(k,k),emFF(k,k))
	ALLOCATE(enAA(k,k),enFF(k,k))
c	ALLOCATE(unit(k,k),mwork(k),iwork(k))
	IBAD=0
	IFAIL=0
c
C MAKE COPY OF INPUT
	do i=1,k
	   do j=1,k
		QD(i,j)=Q(i,j)
	   enddo
	enddo
c
c Calc MAA and MFF (=SOO and SCC in Kienker)
c
	call SUBMAT(QD,AA,Q1,k,k,k,k)      	!QAA IN Q1
c======call QMAT1(Q1,EMAA,ENAA,kA,eigAA,ibad,kQ,kN,kE)
	call QMAT1(Q1,EMAA,ENAA,kA,eigAA,ibad,k,k,k)
	call MATMUL(ENAA,Q1,Q2,kA,kA,kA,1.d0,
     & k,k,k,k,k,k)	!inv(MAA)*QAA in Q2
	call MATMUL(Q2,eMAA,Q1,kA,kA,kA,1.d0,
     & k,k,k,k,k,k)	!inv(MAA)*QAA*MAA in Q1
c
c Check that inv(MAA)*Q1*MAA=diag(eigAA)
	print 12
	if(discprt) write(8,12)
12	format(/,' inv(MAA)*QAA*MAA = diag(eigAA)')
	call ATYPD(Q1,'diag(AA)' ,kA,kA,k,k)
	print 13,(eigAA(i),i=1,kA)
	if(discprt) write(8,13) (eigAA(i),i=1,kA)
13	format(' Eigenvalues of QAA = ',10g13.6)

c copy to AA section of QUNC
	do i=1,kA
	   do j=1,kA
		QUNC(i,j)=Q1(i,j)
	   enddo
	enddo

	call SUBMAT(QD,FF,Q1,k,k,k,k)      	!QFF IN Q1
	call QMAT1(Q1,EMFF,ENFF,kF,eigFF,ibad,k,k,k)
	call MATMUL(ENFF,Q1,Q2,kF,kF,kF,1.d0,
     & k,k,k,k,k,k)	!inv(MFF)*QFF in Q2
	call MATMUL(Q2,eMFF,Q1,kF,kF,kF,1.d0,
     & k,k,k,k,k,k)	!inv(MFF)*QFF*MFF in Q1
c Check that inv(MAA)*Q1*MAA=diag(eigAA)
	print 14
	if(discprt) write(8,14)
14	format(/,' inv(MFF)*QFF*MFF = diag(eigFF)')
	call ATYPD(Q1,'diag(FF)' ,kF,kF,k,k)
	print 15,(eigFF(i),i=1,kF)
	if(discprt) write(8,15) (eigFF(i),i=1,kF)
15	format(' Eigenvalues of QFF = ',20g13.6)
c
c copy to FF section of QUNC
	do i=1,kF
	   i1=i+kA
	   do j=1,kF
		j1=j+kA
		QUNC(i1,j1)=Q1(i,j)
	   enddo
	enddo
c
c Now the off-diagonal parts of QUNC
	call SUBMAT(QD,AF,Q1,k,k,k,k)      	!QAF IN Q1
	call MATMUL(ENAA,Q1,Q2,kA,kA,kF,1.d0,
     & k,k,k,k,k,k)	!inv(MAA)*QAF in Q2
	call MATMUL(Q2,eMFF,Q1,kA,kF,kF,1.d0,
     & k,k,k,k,k,k)	!inv(MAA)*QAF*MFF in Q1
c
c copy to AF section of QUNC
	do i=1,kA
	   do j=1,kF
		j1=j+kA
		QUNC(i,j1)=Q1(i,j)
	   enddo
	enddo
c
	call SUBMAT(QD,FA,Q1,k,k,k,k)      	!QAF IN Q1
	call MATMUL(ENFF,Q1,Q2,kF,kF,kA,1.d0,
     & k,k,k,k,k,k)	!inv(MFF)*QFA in Q2
	call MATMUL(Q2,eMAA,Q1,kF,kA,kA,1.d0,
     & k,k,k,k,k,k)	!inv(MFF)*QFA*MAA in Q1
c
c copy to FA section of QUNC
	do i=1,kF
	   i1=i+kA
	   do j=1,kA
		QUNC(i1,1)=Q1(i,j)
	   enddo
	enddo
c
c Now checks, and printout
	print 10
	if(discprt) write(8,10)
10	format(/,' M(AA) matrix')
	call ATYPD(EMAA,' MAA    ' ,kA,kA,k,k)
	print 11
	if(discprt) write(8,11)
11	format(/,' M(FF) matrix')
	call ATYPD(EMFF,' MFF    ' ,kF,kF,k,k)
c
	print 2
	if(discprt) write(8,2)
2	format(/,' Q matrix for the similar uncoupled model ')
	call ATYPD(QUNC,' Q(unc) ' ,k,k,k,k)
c
c Row sums of QUNC should be 1
c
	do i=1,k
	   s=0.0d0
	   do j=1,k
		s=s+QUNC(i,j)
	   enddo
	   print 1,i,s
	   if(discprt) write(8,1) i,s
1	   format(' Sum of row ',i2,' of Q(Uncoupled) = ',g13.6)
	enddo
c Do spectral expansion on QUNC
	call QMAT1(QUNC,EMFF,ENFF,k,eigFF,ibad,k,k,k)	!overwrites emff,enff,eigFF
c Eigenvalues of QUNC
	do i=1,k
	   print 3,i,eigFF(i)
	   if(discprt) write(8,3) i,eigFF(i)
3	   format(' Eigenvalue ',i2,' of Q(unc) = ',g13.6)
	enddo
c
	DEALLOCATE(QD,QUNC,Q1,Q2,eigAA,eigFF)
	DEALLOCATE(eMAA,eMFF)
	DEALLOCATE(eNAA,eNFF)
c	DEALLOCATE(unit,mwork,iwork)
	RETURN
	END


