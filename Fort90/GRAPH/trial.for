	PROGRAM TRIAL
c To experiment with Gino 3D graphics
c First try two independent Guassian or exponential variables.
c To simulate exponentials wit correlations need to use model as in
c SCSIM
c Is there a way to extend RANORM to simulate general (correlated) Gaussian
c variables?
      allocatable::x,y
	real*4 xaxis(101),yaxis(101),z(20,20)
	real*4 xmid(20),ymid(20),x(:),y(:)
	logical repeat,mono
      INTEGER IKEY, INDEX(14)		!for contour
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	integer icol(100)		!for DCMENU
	character*75 title1
	common/hlp/help		!for QDIALOG
	common/rand/ix,iy,iz
	COMMON/cols/icol,mono
c
	call RANDSK(ix,iy,iz,0,repeat)		!read IX,IY,IZ
c
1	iopt=1
c
	n=10000
	print 11
11	format(' Number of x, y values to simulate [10000] = ')
	call INPUTi(n)
	ALLOCATE(x(n),y(n))
	if(iopt.eq.1) then
	   ax=10.
	   ay=10.
	   print 2
2	   format(' Means for x, y [10,10] = ')
	   call INPUT2r(ax,ay)
	   sx=1.
	   sy=1.
	   print 4
4	   format(' SD for x, y [1,1] = ')
	   call INPUT2r(sx,sy)
	   do i=1,n
		x(i)=ax+sx*ranorm()
	   enddo
	   do j=1,n
		y(j)=ay+sy*ranorm()
	   enddo
c Set the bin boundaries
	   nbinx=20
	   xaxis(1)=ax-5.*sx
	   dx=10.*sx/float(nbinx)
	   do i=1,nbinx
		xaxis(i+1)=xaxis(1)+float(i)*dx
		xmid(i)=0.5*(xaxis(i)+xaxis(i+1))   !midpoint of bin
	   enddo
	   nbiny=20
	   yaxis(1)=ay-5.*sy
	   dy=10.*sy/float(nbiny)
	   do j=1,nbiny
		yaxis(j+1)=yaxis(1)+float(j)*dy
		ymid(j)=0.5*(yaxis(j)+yaxis(j+1))   !midpoint of bin
	   enddo
	endif		!end of Guassian
c
c Now sort values into bins
c In this case we have n pairs of x,y values -x=length of open period,
c and y=length of following shut period (not nx, ny independent
c observation).  Need to locate which of the nbinx*nbiny bins each
c pair falls into
c Initialise frequencies
	do i=1,nbinx
	   do j=1,nbiny
		z(i,j)=0.0
	   enddo
	enddo
	nxlo=0
	nxhi=0
	nylo=0
	nyhi=0
c Sort into bins
	do k=1,n
	   xv=x(k)
	   yv=y(k)
c Check for values outside range of bins
	   if(xv.lt.xaxis(1)) nxlo=nxlo+1
	   if(xv.ge.xaxis(nbinx+1)) nxhi=nxhi+1
	   if(yv.lt.yaxis(1)) nylo=nylo+1
	   if(yv.ge.yaxis(nbiny+1)) nyhi=nyhi+1
c
	   do i=1,nbinx
		do j=1,nbiny
		   if(xv.ge.xaxis(i).and.xv.lt.xaxis(i+1).and.
     &		yv.ge.yaxis(j).and.yv.lt.yaxis(j+1)) then
			z(i,j)=z(i,j)+1.0
		   endif
		enddo
	   enddo
	enddo
	call GINO
	idrawt=1
	idrawx=1
	idrawy=1
	idrawz=1
	icol(23)=0
	icol(25)=9
	icol(71)=7
	xtitle='this is x'
	ytitle='this is y'
	ztitle='this is z'
	title1='3d projection'
	call gplot3d(xmid,ymid,z,nbinx,nbiny,xtitle,ytitle,ztitle,
     &		title1,idrawt,idrawx,idrawy,idrawz)
100   CONTINUE
	call RANDSK(ix,iy,iz,1,repeat)		!write IX,IY,IZ
      END

