	subroutine SORTr3(X,Y,W,j,n,up,nd1,nd2,nw1,nw2)
c Sorts real*4 array X(1,j)-X(n,j) into ascending order if up=true, and also
c outputs Y(i,j), W(i,j) in the corresponding order (ie X dictates
c order, Y, W follow it).  For sorting xval, yval, weight in CVFIT
c Shellsort method
	real*4 X(nd1,nd2),Y(nd1,nd2),W(nw1,nw2)
	logical up
c
	i1=1
	do i=1,n,i1
	   i1=i
	   m=2*i-1
	enddo
C
2	m=m/2
	if(m.eq.0) goto 3
	k=n-m
	do 4 j1=1,k
	do 5 i=j1,1,-m
	if(X(i+m,j).ge.X(i,j)) goto 4
	a1=X(i,j)
	X(i,j)=X(i+m,j)
	X(i+m,j)=a1
	b1=Y(i,j)
	Y(i,j)=Y(i+m,j)
	Y(i+m,j)=b1
	c1=W(i,j)
	W(i,j)=W(i+m,j)
	W(i+m,j)=c1
5	continue	!i LOOP
4	continue	!j1 LOOP
	goto 2	!m LOOP
3	continue
c If descending order required now reverse order
	if(UP) RETURN
	do 200 i=1,n/2
	i1=n-i+1	!=n,n-1,...,1
	a1=X(i,j)
	X(i,j)=X(i1,j)
	X(i1,j)=a1
	b1=Y(i,j)
	Y(i,j)=Y(i1,j)
	Y(i1,j)=b1
	c1=W(i,j)
	W(i,j)=W(i1,j)
	W(i1,j)=c1
200	continue
	RETURN
	END

