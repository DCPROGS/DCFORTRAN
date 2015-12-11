	subroutine RNORM(row,phi,k,kdim)
c To normalise row vector, ROW, with result in PHI (can be same array)
	real*8 ROW(1,kdim),PHI(1,kdim),sum
c
	sum=0.0d0
	do 1 j=1,k
1	sum=sum+row(1,j)
	do 2 j=1,k
2	phi(1,j)=row(1,j)/sum
c
	RETURN
	end


