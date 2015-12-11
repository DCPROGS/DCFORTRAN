	subroutine MSWAP1(QD,IQ,k,inew,ndim)
c Version of MSWAP that also swaps IQ (for HJCFIT) (like MPERMs,MSWAPs)
C SWAPS ROWS AND COLS OF QM ACCORDING TO INEW
	allocatable::Q1,IQ1
	REAL*8 QD(ndim,ndim),Q1(:,:)
	INTEGER INEW(ndim),IQ(ndim,ndim),IQ1(:,:)
C
	allocate(Q1(k,k),IQ1(k,k))
	do 1 i=1,k
	do 1 j=1,k
	i1=inew(i)
	j1=inew(j)
	Q1(i,j)=QD(i1,j1)
	IQ1(i,j)=IQ(i1,j1)
1	continue
C COPY RESULT BACK TO QD
	do 2 i=1,k
	do 2 j=1,k
	QD(i,j)=Q1(i,j)
	IQ(i,j)=IQ1(i,j)
2	continue
	deallocate(Q1,IQ1)
	RETURN
	END


