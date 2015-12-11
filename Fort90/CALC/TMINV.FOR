c To test MATINV2
	real*8 A(100,100),B(100,100),C(100,100)
	real*8 dif,difmax,det,fac
c	integer lwork(100),mwork(100)
	logical calcdet
	common/rand/ix,iy,iz
	COMMON/determ/det
c
	ix=895
	iy=1710
	iz=971
c
	k=90
	fac=10.d0
	print 2
2	format(' fac = ')
	call INPUTr(x)
	fac=dble(x)

	do i=1,k
	   do j=1,k
		A(i,j)=dble(random()+10.*random())
		A(i,j)=fac*A(i,j)
	   enddo
	enddo
c
	kd1=100
	kd2=100
c
	calcdet=.true.
	call timer(n1)
	call MATINV2(A,k,kd1,B,kd2,calcdet,det,ndscale)
	call timer(n2)
	time=10.*float(n2-n1)
	print 10,time,det,ndscale
c10	format(' time = ',g11.4,' det = ',g13.6,' ndscale = ',i4)
	call MATINV2(B,k,kd1,C,kd2,calcdet,det,ndscale)
	call timer(n3)
	time=10.*float(n3-n3)
	print 10,time,det,ndscale
c
	difmax=0.d0
	do i=1,k
	   do j=1,k
		dif=C(i,j)-A(i,j)
		if(dabs(dif).gt.difmax) difmax=dif
	   enddo
	enddo
	print 1,difmax,n2-n1,n3-n2
c1	format(' max difference = ',g16.8,/,
c     &  ' time 1,2 = ',2i8)
	pause
c
	call timer(n1)
	call MATINV(A,k,kd1,B,kd2)
	call timer(n2)
	time=10.*float(n2-n1)
	print 10,time,det,ndscale
10	format(' time = ',g11.4,' det = ',g13.6,' ndscale = ',i4)
	call MATINV(B,k,kd1,C,kd2)
	call timer(n3)
	time=10.*float(n3-n3)
	print 10,time,det,ndscale
c
	difmax=0.d0
	do i=1,k
	   do j=1,k
		dif=C(i,j)-A(i,j)
		if(dabs(dif).gt.difmax) difmax=dif
	   enddo
	enddo
	print 1,difmax,n2-n1,n3-n2
1	format(' max difference = ',g16.8,/,
     &  ' time 1,2 = ',2i8)

	end


