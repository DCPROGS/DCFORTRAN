	subroutine VECMUL(row,col,scalar,k,kd1,kd2)
c To multiply a row vector by a col vector with scalar result
c in SCALAR (double precision)
	real*8 row(1,kd1),col(kd2,1),scalar
c
	scalar=0.0d0
	do 1 i=1,k
1	scalar=scalar + row(1,i)*col(i,1)
	RETURN
	end


