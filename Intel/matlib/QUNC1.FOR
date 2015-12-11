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
	allocatable:: QD,QUNC,Q1,Q2,SAA,SFF
	real*8 QD(:,:),QUNC(:,:),Q1(:,:),Q2(:,:),SAA(:,:),SFF(:,:)
	allocatable::EMAA,ENAA,eigAA,peq
	allocatable::EMFF,ENFF,eigFF
	real*8 eMAA(:,:),eMFF(:,:),eNAA(:,:),eNFF(:,:)
	real*8 eigAA(:),eigFF(:),peq(:)
c	allocatable:: unit,mwork,iwork
c	real*8 unit(:,:)
c	integer mwork(:),iwork(:)
c
	logical calcdet
	real*8 det
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
	ALLOCATE(SAA(k,k),SFF(k,k))
	ALLOCATE(emAA(k,k),emFF(k,k),peq(k))
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
c Calc MAA and MFF
c
	call SUBMAT(QD,AA,Q1,k,k,k,k)      	!QAA IN Q1
c======call QMAT1(Q1,EMAA,ENAA,kA,eigAA,ibad,kQ,kN,kE)
	call QMAT1(Q1,EMAA,ENAA,kA,eigAA,ibad,k,k,k)
c Normalise M using row sums of N, as in Qmat-uncoupled.mcd
	do i=1,kA
	   do j=1,kA
		Q2(i,j)=0.d0
	   enddo
	enddo
	do i=1,k
	   s=0.0d0
	   do j=1,k
		s=s+ENAA(i,j)
	   enddo
	   Q2(i,i)=s
	enddo
c Q2 now contains diagonal matrix with row sums of NAA on diagonal
c multiply MAA by this to get SAA
	call MATMUL(EMAA,Q2,SAA,kA,kA,kA,1.d0,
     & k,k,k,k,k,k)
c Invert SAA (overwrite NAA from Qmat1 which is not normalised correctly)
	call MATINV2(SAA,kA,k,ENAA,k,calcdet,det,ndscale)
c so inv(SAA) now in eNAA
	call MATMUL(ENAA,Q1,Q2,kA,kA,kA,1.d0,
     & k,k,k,k,k,k)	!inv(SAA)*QAA in Q2
	call MATMUL(Q2,SAA,Q1,kA,kA,kA,1.d0,
     & k,k,k,k,k,k)	!inv(SAA)*QAA*SAA in Q1
c
c Check that inv(SAA)*Q1*SAA=diag(eigAA)
c	print 12
c	if(discprt) write(8,12)
c12	format(/,' inv(SAA)*QAA*SAA = diag(eigAA)')
c	call ATYPD(Q1,'diag(AA)' ,kA,kA,k,k)
c	print 13,(eigAA(i),i=1,kA)
c	if(discprt) write(8,13) (eigAA(i),i=1,kA)
c13	format(' Eigenvalues of QAA = ',10g13.6)

c copy to AA section of QUNC
	do i=1,kA
	   do j=1,kA
		QUNC(i,j)=Q1(i,j)
	   enddo
	enddo
c
c Same for QFF
	call SUBMAT(QD,FF,Q1,k,k,k,k)      	!QFF IN Q1
	call QMAT1(Q1,EMFF,ENFF,kF,eigFF,ibad,k,k,k)
c Normalise M using row sums of N, as in Qmat-uncoupled.mcd
	do i=1,kF
	   do j=1,kF
		Q2(i,j)=0.d0
	   enddo
	enddo
	do i=1,k
	   s=0.0d0
	   do j=1,k
		s=s+ENFF(i,j)
	   enddo
	   Q2(i,i)=s
	enddo
c Q2 now contains diagonal matrix with row sums of NFF on diagonal
c multiply MFF by this to get SFF
	call MATMUL(EMFF,Q2,SFF,kF,kF,kF,1.d0,
     & k,k,k,k,k,k)
c Invert SFF (overwrite NFF from Qmat1 which is not normalised correctly)
	call MATINV2(SFF,kF,k,ENFF,k,calcdet,det,ndscale)
c so inv(SFF) now in eNFF
	call MATMUL(ENFF,Q1,Q2,kF,kF,kF,1.d0,
     & k,k,k,k,k,k)	!inv(SFF)*QFF in Q2
	call MATMUL(Q2,SFF,Q1,kF,kF,kF,1.d0,
     & k,k,k,k,k,k)	!inv(SFF)*QFF*SFF in Q1
c Check that inv(SFF)*Q1*SFF=diag(eigFF)
c	print 14
c	if(discprt) write(8,14)
c14	format(/,' inv(SFF)*QFF*SFF = diag(eigFF)')
c	call ATYPD(Q1,'diag(FF)' ,kF,kF,k,k)
c	print 15,(eigFF(i),i=1,kF)
c	if(discprt) write(8,15) (eigFF(i),i=1,kF)
c15	format(' Eigenvalues of QFF = ',20g13.6)
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
     & k,k,k,k,k,k)	!inv(SAA)*QAF in Q2
	call MATMUL(Q2,SFF,Q1,kA,kF,kF,1.d0,
     & k,k,k,k,k,k)	!inv(SAA)*QAF*SFF in Q1
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
     & k,k,k,k,k,k)	!inv(SFF)*QFA in Q2
	call MATMUL(Q2,SAA,Q1,kF,kA,kA,1.d0,
     & k,k,k,k,k,k)	!inv(SFF)*QFA*SAA in Q1
c
c copy to FA section of QUNC
	do i=1,kF
	   i1=i+kA
	   do j=1,kA
		QUNC(i1,j)=Q1(i,j)
	   enddo
	enddo
c
c Now checks, and printout
c	print 10
c	if(discprt) write(8,10)
c10	format(/,' S(AA) matrix')
c	call ATYPD(SAA,' SAA    ' ,kA,kA,k,k)
c	print 11
c	if(discprt) write(8,11)
c11	format(/,' S(FF) matrix')
c	call ATYPD(SFF,' SFF    ' ,kF,kF,k,k)
cc
c	print 2
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
	   if(dabs(s).gt.1.e-10) then
c		print 1,i,s
		if(discprt) write(8,1) i,s
1		format(
     &' Sum of row ',i2,' of Q(Uncoupled) = ',g13.6,' (should be zero)')
	   endif
	enddo
c Do spectral expansion on QUNC
	call QMAT1(QUNC,EMFF,ENFF,k,eigFF,ibad,k,k,k)	!overwrites emff,enff,eigFF
c Eigenvalues of QUNC
	do i=1,k
c	   print 3,i,eigFF(i)
	   if(discprt) write(8,3) i,eigFF(i)
3	   format(' Eigenvalue ',i2,' of Q(unc) = ',g13.6)
	enddo
c Print lifetimes etc for uncoupled
c Note -occupancies NOT the same
c	print 16
c	if(discprt) write(8,16)
c16	format(/,' Occupancies etc from uncoupled mechanism')
c	call EQOCCd(Qunc,k,k-1,k,Peq)		!CALC EQUILIB OCCS
c	call SLIFED2(Qunc,Peq,.false.,discprt,k,k) !print it
c	print 17
	if(discprt) write(8,17)
17	format(/,' End of analysis of uncoupled mechanism',/,
     &  ' -------------------------------------------------')
c

	DEALLOCATE(QD,QUNC,Q1,Q2,eigAA,eigFF)
	DEALLOCATE(eMAA,eMFF)
	DEALLOCATE(eNAA,eNFF,peq)
c	DEALLOCATE(unit,mwork,iwork)
	RETURN
	END


