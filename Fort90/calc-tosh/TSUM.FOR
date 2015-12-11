	program tsum
c To test sumsc()
c (original tsum was lost -new version to test summing of jumps (length
c fixed, number of openings variable) as well as steady state records
c (number of intervals fixed, when simulated, but length variable)
c
c	subroutine SUMSC(tint1,iamp1,n1,tint2,iamp2,n2,nomit,ndim)
	real*4 tint(10),tint1(10)
	integer*2 iampl(10),iampl1(10)
c
	tint(1)=1.0
	iampl(1)=0
	tint(2)=5.0
	iampl(2)=50
	tint(3)=4.0
	iampl(3)=0
c
	tint1(1)=2.0
	iampl1(1)=0
	tint1(2)=8.0
	iampl1(2)=50
	tint1(3)=4.0
	iampl1(3)=0
c
	n1=3
c	n2=2
	n2=3
c
	n3=max(n1,n2)
	n4=min(n1,n2)
	do i=1,n3
	   if(i.le.n4) then
		print 2,i,tint(i),iampl(i),tint1(i),iampl1(i)
2		format(i6,g13.6,i6,4x,g13.6,i6)
	   else if(n4.eq.n2) then
		print 1,i,tint(i),iampl(i)
	   else
		print 3,i,tint1(i),iampl1(i)
3		format(i6,23x,g13.6,i6)
	   endif
	enddo
c
	nomit=0
	ndim=10
	call SUMSC(tint,iampl,n1,tint1,iampl1,n2,nomit,ndim,ndim)

	do i=1,n1
	   print 1,i,tint(i),iampl(i)
1	   format(i6,g13.6,i6)
	enddo
	print 4,nomit
4	format(' nomit = ',i4)
c
	end

