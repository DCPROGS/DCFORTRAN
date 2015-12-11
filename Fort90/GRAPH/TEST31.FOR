	program TEST31
c To experiment with Gino 3D graphics
c First try two independent Guassian or exponential variables.
c To simulate exponentials wit correlations need to use model as in
c SCSIM
c Is there a way to extend RANORM to simulate general (correlated) Gaussian
c variables?
c
	real*4 x(10000),y(10000)
	real*4 xaxis(101),yaxis(101),z(20,20),AZ(80,80)
	real*4 xmid(20),ymid(20),p(400),aax(400),aay(400)
	real w(:)
	character ans,getch
	allocatable::w
      INTEGER ISTYLE(6),ISUP(6)
	logical repeat
      DATA ISTYLE/3,3,3,3,3,3/
      DATA ISUP/0,0,0,0,0,0/
	common/rand/ix,iy,iz
c
	call RANDSK(ix,iy,iz,0,repeat)		!read IX,IY,IZ
c
1	iopt=1
c
	if(iopt.eq.1) then
	   ax=10.
	   ay=20.
	   print 2
2	   format(' Means for x, y [10,20] = ')
	   call INPUT2r(ax,ay)
	   sx=1.
	   sy=2.
	   print 4
4	   format(' SD for x, y [1,2] = ')
	   call INPUT2r(sx,sy)
	   n=10000
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
	   l=1
	   do i=1,nbinx
		do j=1,nbiny
		   if(xv.ge.xaxis(i).and.xv.lt.xaxis(i+1).and.
     &		yv.ge.yaxis(j).and.yv.lt.yaxis(j+1)) then
			z(i,j)=z(i,j)+1.0
		   endif
		enddo
	   enddo
	enddo

	   do i=1,nbinx
		do j=1,nbiny
			p(l)=z(i,j)
			aax(l)=xmid(i)
			aay(l)=ymid(j)
			l=l+1
	   enddo
	   enddo
c
c Now have data for 3D plot.
c Next smooth the data by Magleby's 3x3 method.The value of z(i,j) is
c plotted against xmid(i), ymid(j), i=1,...,nbinx, j=1,..,nbiny
c===============
c Before plotting, smooth the data by Magleby's 3x3 method.
c==============
c Next show dependency plot


C
C
C  INITIALISE VARIABLES
C
      XLOW=XMID(1)
      XHIGH=XMID(NBINX)
      YLOW=YMID(1)
      YHIGH=YMID(NBINY)
	NP=450
C
C
C  MINIMUM WORKSPACE SIZES ARE :
C  2 * NUMX * NUMY + 4 * NP FOR RANGRD
C  NUMX*NUMY + 20*MAX(NUMX,NUMY) FOR PERFIL
C
      NW = MAX(2*100*100 + 4*NP,100*100 + 20*MAX(100,100))
	ALLOCATE(W(NW+10000))
C
C
      CALL GINO
      CALL XXXXX
      CALL PAPENQ(XPAP,YPAP,IPAPTY)
      CALL SETVP2(0.0,1000.0,0.0,750.0,0.0,XPAP,0.0,YPAP)
      CALL PICCLE
      CALL CHASIZ(7.5,7.5)
C
C  DRAW PERSPECTIVE SURFACE

      CALL SURF
C
      RADIUS=900.0
      THETA=35.0
      PHI=98.0
      CALL WINDO2(15.,985.,50.,670.)
      CALL PRJFRA(2)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL ANNSTR(-2,' ')
      CALL HEIRAT(0.35)
	call LINCOL(3)
c      CALL PERPRJ(NBINX,XLOW,XHIGH,NBINY,YLOW,YHIGH,Z,
c     +              RADIUS,THETA,PHI,NW,W)
      CALL RANGRD(400,AAX,AAY,P,80,XLOW,XHIGH,80,YLOW,YHIGH,AZ,
     +            NW,W)
      CALL PERPRJ(80,XLOW,XHIGH,80,YLOW,YHIGH,AZ,
     +              RADIUS,THETA,PHI,NW,W)

C
C  ADD 3D AXES

c      CALL WINDOW(2)
c      CALL PRJLIN(1,1,1)
c      CALL PRJSCA(1.0,1.0,0.1)
c      CALL PERAXE(NW,W)
      CALL WINDOW(2)
      CALL PRJLIN(1,1,1)
      CALL PRJSCA(1.0,1.0,5.0)
      CALL PERAXE(NW,W)
	 DEALLOCATE(W)
C
C  CLOSE DOWN GINOSURF, THE DEVICE AND GINO
C
      CALL SUREND
      CALL DEVSUS
      CALL GINEND
C
C  CLOSE PROGRAM
C
      STOP
      END

