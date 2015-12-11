	program TEST3D
c To experiment with Gino 3D graphics
c First try two independent Guassian or exponential variables.
c To simulate exponentials wit correlations need to use model as in
c SCSIM
c Is there a way to extend RANORM to simulate general (correlated) Gaussian
c variables?
c
	real*4 xaxis(101),yaxis(101),z(20,20),gridz(80,80)
	real*4 xmid(20),ymid(20),p(400),aax(400),aay(400)
	logical bad(20,20)
	allocatable::w,ifill,x,y
	real w(:),x(:),y(:)
	integer ifill(:,:)
	character*1 ans
      INTEGER ISTYLE(6),ISUP(6)
	logical repeat
      DATA ISTYLE/3,3,3,3,3,3/
      DATA ISUP/0,0,0,0,0,0/
      INTEGER IKEY, INDEX(14)		!for contour
	common/rand/ix,iy,iz
c for gplot3d
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	integer icol(100)		!for DCMENU
	character*75 title1
	logical mono
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

	zmin=1.e36
	zmax=0.
	k=1
	do i=1,nbinx
	   do j=1,nbiny
		p(k)=z(i,j)
		aax(k)=xmid(i)
		aay(k)=ymid(j)
		k=k+1
		if(z(i,j).gt.zmax) zmax=z(i,j)
		if(z(i,j).lt.zmin) zmin=z(i,j)
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
	ans='N'
	call DCASK('Test GINOSURF examples',ans,ans)
	if(ans.eq.'N') goto 70

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
      CALL GINO		!Gino banner
      CALL XXXXX		!screen blanked
      CALL PAPENQ(XPAP,YPAP,IPAPTY)
      CALL SETVP2(0.0,1000.0,0.0,750.0,0.0,XPAP,0.0,YPAP)	!screen->grey
      CALL PICCLE
      CALL CHASIZ(7.5,7.5)
C
c
C  DRAW PERSPECTIVE SURFACE

      CALL SURF		!Surf banner
C
      RADIUS=900.0
      THETA=35.0
      PHI=98.0
      CALL WINDO2(15.,985.,50.,670.)
      CALL PRJFRA(2)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL ANNSTR(-2,' ')
      CALL HEIRAT(0.35)
c	do icol=0,15
c	   print 10,icol
c10	   format('icol = ',i3)
c	   call LINCOL(icol)
c         CALL PERPRJ(NBINX,XLOW,XHIGH,NBINY,YLOW,YHIGH,Z,
c     &              RADIUS,THETA,PHI,NW,W)
c         CALL PICCLE
c	   pause
c	enddo
c
	ALLOCATE(ifill(nbinx-1,nbiny-1))
c	ioff=1
c	do i=1,nbinx-1
c	   do j=1,nbiny-1
c		av=(z(j,i)+z(j,i+1)+z(j+1,i)+z(j+1,i+1))/4.
c		ifill(j,i)=ioff+int(av/100.)
c	   enddo
c	enddo
c Divide into 16 colour bands (avoid black=1)
	nc=9
	bw=(zmax-zmin)/float(nc)
	do i=1,nbinx-1
	   do j=1,nbiny-1
		do k=1,nc
		   blo=zmin+float(k-1)*bw
		   bhi=zmin+float(k)*bw
		   zv=z(i,j)
		   if(zv.ge.blo.and.zv.le.bhi) ifill(i,j)=k+1	!avoid 1
		enddo
	   enddo
	enddo

      CALL PERFIL(NBINX,XLOW,XHIGH,NBINY,YLOW,YHIGH,Z,
     &              RADIUS,THETA,PHI,ifill,NW,W)
      CALL PICCLE
	DEALLOCATE(ifill)
	pause
c
c Now monochrome with an interpolated grid but no fill
	call LINCOL(5)
	numx=80
	numy=80
      CALL RANGRD(400,AAX,AAY,P,numx,XLOW,XHIGH,numy,YLOW,YHIGH,gridz,
     +            NW,W)
      CALL PERPRJ(80,XLOW,XHIGH,80,YLOW,YHIGH,gridz,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed

C  ADD 3D AXES
      CALL WINDOW(2)
      CALL PRJLIN(1,1,1)
      CALL PRJSCA(1.0,1.0,5.0)
      CALL PERAXE(NW,W)		!axes displayed
c
	pause
      CALL PICCLE
c
c Next do interpolated mesh + colour fill
	n1=80
	ALLOCATE(ifill(n1-1,n1-1))
c	ioff=2 !avoid col=1 which is black at present
c	do i=1,n1-1
c	   do j=1,n1-1
c		av=(gridz(j,i)+gridz(j,i+1)+gridz(j+1,i)+gridz(j+1,i+1))/4.
c		ifill(j,i)=ioff+int(av/100.)
c		ifill(j,i)=mod(ifill(j,i),16)
c	   enddo
c	enddo
c Find min and max of gridz(i,j)
	gzmin=1.e36
	gzmax=0.
	k=1
	do i=1,n1
	   do j=1,n1
		if(gridz(i,j).gt.gzmax) gzmax=gridz(i,j)
		if(gridz(i,j).lt.gzmin) gzmin=gridz(i,j)
	   enddo
	enddo
c Divide into nc colour bands (avoid black=1)
c	nc=15
	nc=9
	bw=(gzmax-gzmin)/float(nc)
	do i=1,n1-1
	   do j=1,n1-1
		do k=1,nc
		   blo=gzmin+float(k-1)*bw
		   bhi=gzmin+float(k)*bw
		   zv=gridz(i,j)
		   if(zv.ge.blo.and.zv.le.bhi) ifill(i,j)=k+1	!avoid 1
		enddo
	   enddo
	enddo
	numx=80
	numy=80
      RADIUS=900.0
      THETA=35.0
      PHI=98.0
      CALL WINDO2(15.,985.,50.,670.)
      CALL PRJFRA(2)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL ANNSTR(-2,' ')
      CALL HEIRAT(0.35)
      CALL RANGRD(400,AAX,AAY,P,numx,XLOW,XHIGH,numy,YLOW,YHIGH,gridz,
     +            NW,W)
      call PERFIL(numx,XLOW,XHIGH,numy,YLOW,YHIGH,gridz,
     +              RADIUS,THETA,PHI,ifill,NW,W)	!grid displayed
	DEALLOCATE(ifill)
C
C  ADD 3D AXES

c      CALL WINDOW(2)
c      CALL PRJLIN(1,1,1)
c      CALL PRJSCA(1.0,1.0,0.1)
c      CALL PERAXE(NW,W)
c Fix tics and zmin, zmax
	call FIXAX(gzmin,gzmax,gzmin1,gzmax1,gztic,0)
c	call PRJLEV(gzmin1,gzmax1)	!this call overrides auto choice
c
	call WINDOW(2)
c      call PRJLIN(1,1,1)
c      call PRJSCA(1.0,1.0,gztic)
c      call PRJSCA(1.0,1.0,0.0)
      call PERAXE(nw,w)		!axes displayed
c
	pause
      CALL PICCLE
c
c next Contours
	ncont=12
	ism=0
      CALL PAPENQ(XPAP,YPAP,IP)
      CALL SETVP2(0.0,1000.0,0.0,750.0,0.0,XPAP,0.0,YPAP)
      CALL PICCLE
      CALL WINDOW(2)
      CALL COLINF(NDC,NDT)
      CALL COLENQ(ICOL1)
      IF(ICOL1.EQ.10) ICOL1=13
      CALL CHASIZ(7.5,7.5)
C
C  SET UP COLOUR TABLE AND FILL INDEX
C
      CALL RGBDEF(2,0.5,0.8,1.0)
      CALL RGBDEF(3,0.0,0.7,0.15)
      CALL RGBDEF(4,0.0,0.9,0.2)
      CALL RGBDEF(5,0.25,0.95,0.4)
      CALL RGBDEF(6,0.5,1.0,0.6)
      CALL RGBDEF(7,0.75,0.95,0.35)
      CALL RGBDEF(8,1.0,0.9,0.1)
      CALL RGBDEF(9,0.85,0.75,0.15)
      CALL RGBDEF(10,0.7,0.6,0.2)
      CALL RGBDEF(11,0.6,0.5,0.2)
      CALL RGBDEF(12,0.5,0.4,0.2)
      CALL RGBDEF(13,1.0,1.0,1.0)
      CALL RGBDEF(14,0.0,0.0,0.9)
      CALL RGBDEF(15,0.0,1.0,1.0)
C
C  ALLOCATE FILLING STYLES
C
      IKEY = 1
      DO 40 I=1,NCONT+1
         INDEX(I)=I+1
   40 CONTINUE
C
      CALL WINDO2(40.0,960.0,250.0,730.0)
C  DRAW CONTOUR MAP WITH 12 CONTOURS RANGING FROM -50 TO 550
C  ADJUST ANNOTATION VALUES AND FORCE TO BE INTEGERS
C
      CALL ANNDIG(1,3,0,0)
      CALL ANNDIG(2,3,0,0)
      CALL CONANN(-1,0.0,2.0,0,0,0,0,0)
      CALL ANNSTR(6,'Projection against X axis')
      CALL CONFRA(0)
      CALL CONLIN(ICOL1,12,12,0)
c      CALL CONSCA (STEP,STEP,50.0,1)
c      CALL LEVELS(-50.,550.0)
	step=gzmax1/12.
      CALL CONSCA (STEP,STEP,step,1)
      CALL LEVELS(0.0,qzmax1)
      IF (NDC.LE.1) THEN
        CALL DRACON (NUMX, XLOW, XHIGH, NUMY, YLOW, YHIGH, GRIDZ,
     +                           NCONT, ISM, NW, W)
      ELSE
        CALL FILCON (NUMX, XLOW, XHIGH, NUMY, YLOW, YHIGH, GRIDZ,
     +                           NCONT, ISM, INDEX, IKEY, NW, W)
      END IF
C  DRAW LINE OF CROSS SECTION ON THE CONTOUR MAP
C
	x1=xmid(1)
	x2=xmid(nbinx)
	y1=ay
	y2=ay
      CALL CONSPA(XLOW, YLOW, X1, Y1)
      CALL CONSPA(XHIGH, YHIGH, X2, Y2)
      CALL LINCOL(ICOL1)
      CALL MOVTO2(X1, Y1)
      CALL LINTO2(X2, Y2)
C
C  ADD GRID OVER CONTOUR MAP
C
	step=gzmax1/12.
      CALL CONGRD(XLOW,XHIGH,2.0*STEP,YLOW,YHIGH,2.0*STEP)
C
C  ANNOTATE CONTOUR MAP
C
      IF (NDC.GT.1)THEN
        CALL MOVTO2(X2+20.0,Y1-15.0)
        CALL CHASTR('Metres above sea level')
      END IF
C
C  ADD WINDOW TO REDUCE CROSS SECTION HEIGHT
C
      CALL WINDOW(2)
      CALL CRSBND(X1,X2,120.0,270.0)
C
C  DRAW CROSS SECTION OF SURFACE
C
      CALL CRSSEC (XLOW, YLOW, XHIGH, YHIGH, NW, W)
C

c Now monochrome again but with shadow from 'light source'
c
	pause
      CALL PICCLE
C  SET UP GINO COLOUR TABLE
C
      CALL COLENQ(ICOL1)
      CALL COLINF(NDC,NDT)
      CALL RGBENQ(ICOL1,RED,GREEN,BLUE)
      CALL RGBDEF(1,RED,GREEN,BLUE)
      CALL RGBDEF(2,0.0,1.0,1.0)
      NCOL = NDC
      ISET=4
      NDC=MIN(NDC-ISET,64)
      IF(NDC.GT.0) CALL SCOL(NDC,ISET-1)
	n1=80
	numx=n1
	numy=n1
C  CALCULATE IFILL ARRAY FROM POINT LIGHT SOURCE
	ALLOCATE(ifill(numx-1,numy-1))
C
      XSOURC=-100000.0
      YSOURC=100000.0
      ZSOURC=150000.0
      DO 300 I=1,NUMX-1
        DO 310 J=1,NUMY-1
          XNORM=GRIDZ(I,J)-GRIDZ(I+1,J)
          YNORM=GRIDZ(I+1,J)-GRIDZ(I+1,J+1)
          ZNORM=1.0
          MAGNIT=SQRT(XNORM**2+YNORM**2+ZNORM**2)
          XNORM=XNORM/MAGNIT
          YNORM=YNORM/MAGNIT
          ZNORM=ZNORM/MAGNIT
          XLIGHT=XSOURC-float(I)
          YLIGHT=YSOURC-float(J)
          ZLIGHT=ZSOURC-GRIDZ(I,J)
          MAGNIT=SQRT(XLIGHT**2+YLIGHT**2+ZLIGHT**2)
          XLIGHT=XLIGHT/MAGNIT
          YLIGHT=YLIGHT/MAGNIT
          ZLIGHT=ZLIGHT/MAGNIT
          SFILL=(XNORM*XLIGHT+YNORM*YLIGHT+ZNORM*ZLIGHT)
          IF(SFILL.LT.0.0)SFILL=0.0
          IF(SFILL.GT.0.9)SFILL=0.9
          IFILL(I,J)=NINT(SFILL*float(NDC-1))+ISET
310     CONTINUE
300   CONTINUE
	call PRJGRD(numx-1,numy-1)		!no grid lines
      call PERFIL(80,XLOW,XHIGH,80,YLOW,YHIGH,gridz,
     +              RADIUS,THETA,PHI,ifill,NW,W)	!grid displayed
	DEALLOCATE(ifill)
C
C  ADD 3D AXES

c Fix tics and zmin, zmax
	call FIXAX(gzmin,gzmax,gzmin1,gzmax1,gztic,ilog)
	call PRJLEV(gzmin1,gzmax1)	!this call overrides auto choice
c
	call WINDOW(2)
      call PRJLIN(1,1,1)
      call PRJSCA(1.0,1.0,gztic)
      call PERAXE(nw,w)		!axes displayed
	pause
      CALL PICCLE
c
C  CLOSE DOWN GINOSURF, THE DEVICE AND GINO
C
	DEALLOCATE(w)
C
      call SUREND
      call DEVSUS
      call GINEND
C
C  CLOSE PROGRAM
C
	pause
	call VIDEOMOD(3)

c Now test gplot3d
70	continue
	ans='Y'
	call DCASK('Test GPLOT3D',ans,ans)
	if(ans.eq.'N') goto 71
	call GINO
	idrawt=1
	idrawx=1
	idrawy=1
	idrawz=1
	icol(23)=0		      !in common for gplot3d
	icol(25)=9
	icol(71)=7
	xtitle='X axis'
	ytitle='Y axis'
	ztitle='Z axis'
	title1='Test GPLOT3D'
	do i=1,nbinx
	   do j=1,nbiny
		bad(i,j)=.false.
	   enddo
	enddo
c====
c Test plane
	do i=1,nbinx
	   do j=1,nbiny
		z(i,j)=0.0
	   enddo
	enddo
	z(1,1)=1.
	z(1,2)=-1.
c===
c test exact grid pos
	nbinx=10
	nbiny=10
	do i=1,nbinx
c	   xmid(i)=float(i)**2	!NB x, y must be equally spaced
c	   xmid(i)=float(i)*2.
	   xmid(i)=float(i)
	   do j=1,nbiny
		ymid(j)=float(j)
		bad(i,j)=.false.
		z(i,j)=0.0
	   enddo
	enddo
	z(3,3)=1.
	bad(3,3)=.true.
c===
	CALL gplot3d(xmid,ymid,z,bad,nbinx,nbiny,20,20,
     &   xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz)

71	continue
	call VIDEOMOD(3)
      STOP
      END

c NB  SUBROUTINE SCOL(NDC,ISET) is now in gplot3d
c      DO 100 I=ISET,ISET+NDC
c        CALL HLSDEF(I,80.0,
c     +         float(I-ISET)/float(NDC*3)+0.2,0.8)
c100   CONTINUE
c      RETURN
c      END
c
