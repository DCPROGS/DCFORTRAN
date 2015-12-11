	subroutine IVECREV(ivec,n,ndim)
	integer ivec(ndim)
c To reverse the order of the 1st n elements of vector IVEC
c Swap elements 1 and n; then elements 2 and n-1...
	nswap=n/2		!if n is odd central element does not move
	do 1 i=1,nswap
	j=n-i+1		!=n,n-1,...
	itemp=ivec(i)
	ivec(i)=ivec(j)
	ivec(j)=itemp
1	continue
	RETURN
	end


