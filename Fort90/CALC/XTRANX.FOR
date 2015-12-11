	subroutine XTRANX(x,z,n,np)
c To calculate the product z=X'*X where X'=transpose(X) without making
c an array for the transpose. X is n*np (so X' is np*n and Z is np*nP.
c Modif 10/06/93 10:14am so array dimensions not specified separately
c from n,np
	real*8 x(n,np),z(np,np)
c
	do 1 i=1,np
	do 1 j=1,np
	z(i,j)=0.0d0
	do 3 k=1,n
3     z(i,j)=z(i,j) + x(k,i)*x(k,j)
1	continue
	RETURN
	end
