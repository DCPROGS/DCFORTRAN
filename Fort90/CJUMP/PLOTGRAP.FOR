c=============================================================================
	program plotgraph
c=============================================================================

	real*8  	countmin ,countmax,tau
	real*8      yf(:)
	real*4      x(100),y(100),z(100)

	allocatable::yf
	OPEN(unit=11,file='exp1.dat',status='unknown',
     &access='SEQUENTIAL',form='formatted',recl=75)
	read(unit=11,fmt=100) tau,countmin,countmax,n,nlag,nbin
100   format(3d15.8,i15,i8,i7)
	print 100,tau,countmin,countmax,n,nlag,nbin
	allocate(yf(int(nlag*nbin+1)))
	rows=float(nlag*nbin)/5.
	k=1
	do i=1,int(rows)
	read(unit=11,fmt=110) (yf(j),j=k,k+4)
110   format(5d15.8)
	k=k+5
	enddo
	print*,k,yf(int(nlag*nbin))

	print 120, (yf(i),i=1,6)
120     format(6d15.8)
	ans=getch(b)
	do i=1,nbin
		x(i)=float(i*0.5)
		z(i)=float(2)
		y(i)=yf(nbin+1-i)*100000
	enddo
	call iniplt(0,.false.,1.0)
	call viewport(0,10000,0,7500)
	call graphboundary(2000,8000,1000,6000)
	call scale(0.,30.,0.,20.)
	call setorigin(15.,10.,2.)
	call setviewpt(30.,70.,50.)
	call setaperture(0.,0.)
	call polyline3d(x,y,z,60,14,1)
	ans=getch(b)

	end
