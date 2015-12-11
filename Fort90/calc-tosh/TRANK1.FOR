	program TRANK
c
c To test use of NAGLIB routines to find rank of matrix
	real*8 Q(10,10)
c For NAG
c	real*8 aijmax(5)		!output
c	real*8 d(5),u(10,5),du(5)	!workspace
	real*8 cx,tol
c	integer inc(5)
c For SVD
	real*8 v(10,10),w(10)
	real*8 prec
c
	q(1,1)=7.d0
	q(1,2)=-2.d0
	q(1,3)=4.d0
	q(1,4)=9.d0
	q(1,5)=1.8d0

	q(2,1)=3.d0
	q(2,2)=8.d0
	q(2,3)=-4.d0
	q(2,4)=6.d0
	q(2,5)=1.3d0

	q(3,1)=9.d0
	q(3,2)=6.d0
	q(3,3)=1.d0
	q(3,4)=5.d0
	q(3,5)=2.1d0

	q(4,1)=-8.d0
	q(4,2)=7.d0
	q(4,3)=5.d0
	q(4,4)=2.d0
	q(4,5)=0.6d0

	q(5,1)=4.d0
	q(5,2)=-1.d0
	q(5,3)=2.d0
	q(5,4)=8.d0
	q(5,5)=1.3d0

	q(6,1)=1.d0
	q(6,2)=6.d0
	q(6,3)=3.d0
	q(6,4)=-5.d0
	q(6,5)=0.5d0

c
	call RANK(Q,5,5,10,10,irank)
	print 1,irank
1	format(' From DC routine, rank = ',i3)
c
c Need F90 verion of NAGLIB
cc	m=6
c	m=5
c	n=5
c	ia=10
c	iu=10
cc	tol=1.d-10
cc set tolerance to 10*norm of Q
c	cx=1.d0
c	do i=1,m
c	   do j=1,n
c		cx=cx+q(i,j)**2
c	   enddo
c	enddo
c	tol= dsqrt(cx)*X02AJF()
cc	ifail=0
c	ifail=1
c
c	call F01BLF(m,n,tol,Q,ia,aijmax,irank,inc,d,u,iu,du,ifail)
c	print 2,irank
c2	format(' From NAG routine, rank = ',i3)
cc
c Test SVD from Press et al
c	m=6
	m=5
	n=5
      call dsvdcmp(Q,m,n,10,10,w,v)		!destroys Q

c set tolerance to 10*norm of Q
	cx=1.d0
	do i=1,m
	   do j=1,n
		cx=cx+q(i,j)**2
	   enddo
	enddo
	prec=2.d0**(-31)		!machine precision
	tol=dsqrt(cx)*prec

	irank=0
	do i=1,5
	   if(dabs(w(i)).gt.tol) irank=irank+1
	enddo
	print 3,irank
3	format(' From SVD routine (Press et al), rank = ',i3)
c
c
	q(1,1)=7.d0
	q(1,2)=-2.d0
	q(1,3)=4.d0
	q(1,4)=9.d0
	q(1,5)=1.8d0

	q(2,1)=3.d0
	q(2,2)=8.d0
	q(2,3)=-4.d0
	q(2,4)=6.d0
	q(2,5)=1.3d0

	q(3,1)=9.d0
	q(3,2)=6.d0
	q(3,3)=1.d0
	q(3,4)=5.d0
	q(3,5)=2.1d0

	q(4,1)=-8.d0
	q(4,2)=7.d0
	q(4,3)=5.d0
	q(4,4)=2.d0
	q(4,5)=0.6d0

	q(5,1)=4.d0
	q(5,2)=-1.d0
	q(5,3)=2.d0
	q(5,4)=8.d0
	q(5,5)=1.3d0

	q(6,1)=1.d0
	q(6,2)=6.d0
	q(6,3)=3.d0
	q(6,4)=-5.d0
	q(6,5)=0.5d0
c
	call RANK_SVD(Q,m,n,10,10,irank)
	print 4,irank
4	format(' From RANK_SVD routine, rank = ',i3)
c
	end

