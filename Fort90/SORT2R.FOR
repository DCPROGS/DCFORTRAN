	subroutine SORT2r(A,B,n,ndim,up)
c Sorts real*4 array A(1)-A(n) into ascending order if up=true, and also
c outputs a 2nd real array, B, into the corresponding order (ie A dicatates
c order, B follows it)
c Shellsort method
	dimension A(ndim),B(ndim)
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
	if(A(i+m).ge.A(i)) goto 4
	a1=A(i)
	A(i)=A(i+m)
	A(i+m)=a1
	b1=B(i)
	B(i)=B(i+m)
	B(i+m)=b1
5	continue	!i LOOP
4	continue	!j LOOP
	goto 2	!m LOOP
3	continue
c If descending order required now reverse order
	if(UP) RETURN
	do 200 i=1,n/2
	i1=n-i+1	!=n,n-1,...,1
	a1=a(i)
	a(i)=a(i1)
	a(i1)=a1
	b1=B(i)
	B(i)=B(i1)
	B(i1)=b1
200	continue
	RETURN
	END

