	program tcomf
c	integer*2 iy(10)
	integer*2 n
c	common/foo/iy(10)
c	common iy
c
c	do 1 i=1,10
c1     iy(i)=10-i
	n=2
	call TCOM(n)
	print *,n
	end


