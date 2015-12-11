	subroutine SORTINDI(IA,index,n,ndim)
c Sorts integer array IA(1)-IA(n) into ascending order, and also outputs
c an array, index, that gives rank order of sort (initially index set
c to 1,2,...,n internally)
c Shellsort method
	dimension IA(ndim),index(ndim)
c
	do 6 i=1,n
6	index(i)=i		!initially
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
	in=index(i)
	index(I)=index(I+M)
	index(I+M)=in
5	continue	!i LOOP
4	continue	!j LOOP
	goto 2	!m LOOP
3	continue
	RETURN
	END
