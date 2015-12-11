	subroutine GETRANGE(tgap,topen,ylo,yhi,nval,sy,syy,ny,
     & sy2,syy2,ny2,sx,sxx,nx,sx2,sxx2,nx2,deb,nr)
c To accumulate, for calc of mean and sd, the open times (topen) according
c to the range in which the specified shut time, tgap, falls
c The values are kept in sy,syy (=sy0 or sy1 in call according to whether
c it is a preceding or following gap), and they are alse kept in sy2 etc
c whether gap precedes OR follows).  The mean and SD of the shut times is
c accumulated in sx,sxx,nx
	real*4 sy(20),syy(20),sy2(20),syy2(20)
	real*4 sx(20),sxx(20),sx2(20),sxx2(20)
	real*4 ylo(20),yhi(20)
	integer ny(20),ny2(20)
	integer nx(20),nx2(20)
	logical deb
c
	nr=0					!if tgap not in ANY range
	do 1 m=1,nval
	if(tgap.ge.ylo(m).and.tgap.lt.yhi(m)) then		!OK- in range
	  nr=m				!range number for this call (for debug)
	  sy(m)=sy(m)+topen		!add open times for mean
	  sy2(m)=sy2(m)+topen		!add open times for mean
	  syy(m)=syy(m)+topen*topen	! for SD
	  syy2(m)=syy2(m)+topen*topen	! for SD
	  ny(m)=ny(m)+1			!increment number
	  ny2(m)=ny2(m)+1			!increment number
	  sx(m)=sx(m)+tgap 		!add shut times for mean
	  sx2(m)=sx2(m)+tgap		!add shut times for mean
	  sxx(m)=sxx(m)+tgap*tgap	! for SD
	  sxx2(m)=sxx2(m)+tgap*tgap	! for SD
	  nx(m)=nx(m)+1			!increment number
	  nx2(m)=nx2(m)+1			!increment number
	endif
1	continue
	RETURN
	end


	subroutine PRANGE(title,ylo,yhi,nval,sy,syy,ny,sx,sxx,nx,
     & yval1,xval1,ndv1,ndimd,j,weight,kwi,kwj,icurvw)
c To print obs conditional means, and also define xval1,yval1 for plotting
c them
	real*4 sy(20),syy(20)
	real*4 sx(20),sxx(20)
	real*4 ylo(20),yhi(20)
	real XVAL1(ndv1,ndimd),YVAL1(ndv1,ndimd)
c	dimension XVAL1(2048,3),YVAL1(2048,3)
	integer ny(20),nx(20)
	character*74 title
	logical discprt
	real*4 weight(kwi,kwj)
	integer*4 icurvw(ndimd)
	common/dp/discprt
c=	COMMON/SD/weight(100,10)
c NB don't alter input j value
	do i=1,ndimd
	   icurvw(i)=-1	!no SD unless reset below
	enddo
c

	if(discprt) write(7,1) title
1	format(1x,a74,/,7x,
     & ' Gap length: range',7x,'mean gap  # of ops   mean open time',
     & '   Std dev')
c
	noptot=0
	do 428 i=1,nval
c	 xval1(i,j)=(ylo(i)+yhi(i))/2.0 !midpoint of shut time range -NOW use mean!
	 n=ny(i)
	 noptot=noptot+n
	 if(ny(i).ne.nx(i)) then
	  
ccc	   print 2,i,nx(i),ny(i)
2	   format(' ERROR in PRANGE: range #',i2,' nx,ny = ',2i8)
	   
	 endif
	 if(n.gt.1) then
	  ym=sy(i)/float(n)
	  xm=sx(i)/float(n)
	  sd=sqrt((syy(i)-(sy(i)*sy(i)/float(n)))/float(n-1))
c	  sdx=sqrt((sxx(i)-(sx(i)*sx(i)/float(n)))/float(n-1))
	  xval1(i,j)=xm
	  yval1(i,j)=ym
	  weight(i,j)=float(n)/(sd*sd)	!display standard errors
	  icurvw(j)=1
	  if(yhi(i).lt.1.e6) then
	  
	   if(discprt) write(7,426) i,ylo(i),yhi(i),xm,n,ym,sd
426	   format(i3,1x,f10.3,' to ',f10.3,2x,f11.4,3x,i5,2(3x,g12.5))
	  else if(yhi(i).ge.1.e6.and.yhi(i).lt.3.e10) then
	   
	   if(discprt) write(7,40) i,ylo(i),yhi(i),xm,n,ym,sd
40	   format(i3,f13.1,' to ',f13.1,1x,f11.4,3x,i5,2(3x,g12.5))
	  else	!assume yhi(i)=3.1536e10msec=1 year as set above
	   
	   if(discprt) write(7,42) i,ylo(i),xm,n,ym,sd
42	   format(i3,1x,f10.3,' to   1 year  ',2x,f11.4,3x,i5,2(3x,g12.5))
	  endif
	 else
	  weight(i,j)=0.
	  if(yhi(i).lt.3.e10) then
        
         if(discprt) write(7,427) i,ylo(i),yhi(i),n
427	   format(i3,1x,f10.3,' to ',f10.3,16x,i5)
	  else
        
         if(discprt) write(7,43) i,ylo(i),n
43	   format(i3,1x,f10.3,' to   1 year  ',16x,i5)
	  endif
	 endif
428	continue
      
      if(discprt) write(7,3) noptot
3	format(32x,' Total   ',i8)
	RETURN
	end


	subroutine PCRANGE(title,ylo,yhi,nval,ny,den1,ycal,xcal,
     & ndc1,ndimc,j)
c To print calc conditional means
	character*74 title
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
c	real*4 xcal(2048,3),ycal(2048,3)
	real*4 ylo(20),yhi(20)
	real*4 den1(20)
	integer ny(20)
	logical discprt
	common/dp/discprt
c
	ntot=0
	do 27 i=1,nval
27	 ntot=ntot+ny(i)
	
	if(discprt) write(7,1) title
1	format(1x,a74,/,7x,' Gap length: range',7x,
     & 'Mean gap    P[tlo<t<thi]  N*P   Mean open time')
c
	do 428 i=1,nval
	  enp=(float(ntot))*den1(i)
	  if(yhi(i).lt.3.e10) then
	   if(discprt) write(7,426) i,ylo(i),yhi(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
426	   format(i4,1x,f10.3,' to ',f10.3,2x,f11.4,1x,f8.5,2x,
     &	f10.1,3x,g13.6)
	  else	!assume yhi(i)=3.1536e10 msec=1 year as set above
	   if(discprt) write(7,42) i,ylo(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
42	   format(i3,1x,f10.3,' to   1 year  ',2x,f11.4,1x,f8.5,2x,
     &	f10.1,3x,g13.6)
	endif
428	continue
	RETURN
	end

	