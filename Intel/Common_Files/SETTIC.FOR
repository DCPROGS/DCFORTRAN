	subroutine SETTIC(tic)
c To round tic length/bin width etc to acceptable value given a
c a nominal input value
	dimension dx(9)
c
c Set DX(i)=1,1.5,2,2.5,3,4,5,7,10 = acceptable values
	do 1 i=1,7
	if(i.le.5) dx(i)=1. + float(i-1)/2.	!1.,1.5,2.,2.5,3.
1	if(i.gt.5) dx(i)=float(i-2)		!4.,5.
	dx(8)=7.
	dx(9)=10.
c
c	do 4 i=1,9
c4	print 5,i,dx(i)
c5	format(i5,g13.6)
c
	x=alog10(tic)
	n=ifix(x)
	if(x.lt.0.) n=n-1
	x=tic*10.**(-n)		!get in range 1-10
	do 2 i=1,9
	j=i
	if(dx(i).gt.x) goto 3
c	if(dx(i).lt.x) goto 2
c	goto 3
2	continue
3	tic=dx(j)*(10.**n)
	return
	end

