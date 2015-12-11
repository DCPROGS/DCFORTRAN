	subroutine IVECROT(ivec,n,nr,ndim)
c To rotate a vector elements ivec(1)-ivec(n) nr places to right, with wrap
c Done the lazy way with subsidiary array (up to 50 elements)
	integer ivec(ndim),ivec1(50)
c
	if(nr.eq.0) RETURN
	do 1 i=1,n
	j=i-nr
	if(j.lt.1) j=j+n
	if(j.gt.n) j=j-n
	ivec1(i)=ivec(j)
1     continue
c Put output back in ivec
	do 2 i=1,n
2	ivec(i)=ivec1(i)
	RETURN
	end

