	subroutine SORTI(IA,n,up,ndim)
c Sorts integer array IA(1)-IA(n) into ascending order if up=true, and
c otherwise into descending order.
c Shellsort method
	dimension IA(ndim)
	logical up
c
	i1=1
	do 1 i=1,n,i1
	i1=i
1	m=2*i-1
C
2	m=m/2
	if(m.eq.0) goto 3
	k=n-m
	do 4 j=1,k
	do 5 i=j,1,-m
	if(IA(i+m).ge.IA(i)) goto 4
	iw=IA(i)
	IA(i)=IA(i+m)
	IA(i+m)=iw
5	continue	!i LOOP
4	continue	!j LOOP
	goto 2	!m LOOP
3	continue
	if(up) RETURN
c otherwise reverse order to get descending order (do as in IVECREV)
	nswap=n/2		!if n is odd central element does not move
	do 6 i=1,nswap
	j=n-i+1		!=n,n-1,...
	itemp=ia(i)
	ia(i)=ia(j)
	ia(j)=itemp
6	continue
	RETURN
	END
