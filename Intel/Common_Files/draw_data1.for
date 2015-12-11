 	subroutine draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,
     &	logx,logy,sqrty,y0,yinf,x0,ilog,idev,ndat,
     &    ijoin,icol,isym,syms,thick,barleng,interp,npint,Xint,Yint,
     &    nint,yp1,ypn,Y2int,ndv1,ndimd,weight,kwi,kwj,dxs,dys,icurvw,
     &	jmiss,n1,idraw,ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
	 
	use gino_f90

	real*4 XVAL(n1:ndv1,ndimd),YVAL(n1:ndv1,ndimd)
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	integer*4 ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	integer*4 icurvw(ndimd),jmiss(ndimd)
	real*4 syms(ndimd)		!symbol size
	real*4 weight(kwi,kwj),THICK(250)
c	real*4 weight(niobs,njset)
	integer ICOL(250),ITYPE(250),IDRAW(250)
	logical ivplot,onjump,tload,logity,logx,logy,sqrty,allpnt
	logical mono,landscape,interp
	character cnum1*22
	character*33 adcfil
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,
     &	xmin,xmax,ymin,ymax
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),
     &	xoff1,y1v,y2v,y1c,y2c,ncjump,nvjump,ivplot
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
c	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2 
      idrawc=idraw(241)
	idrawv=idraw(242)
	barleng=0.01*(xmax-xmin)
	nbig=10000
	do j1=1,ncurvd		!plot ncurvd data sets
	   jflag=0
	   j=icurvd(j1)		!data set to be plotted (col # where data starts)

	   sx=syms(j)*0.7*dxs
	   sy=syms(j)*0.7*dys

	   call SETBIGJ(j,j0)
         call glincols(icol(j0),idev)	 !col for jth data set: symbols+line+SD
	   if(jmiss(j).eq.1) call glincols(0,idev)
	   linetype=ijoin(j)		!line to join data points
	   iud=1				!draw line between points
	   if(linetype.eq.-1) iud=0	!don't
	   npint=0			!points for interpolation
	   if(ndat(j).le.nbig.or.allpnt) then
	      ndelt=1
		rj0=thick(j0)	!line thickness for current data curve
	   else
	      ndelt=1
	      call COUNTDAT(j,ndat,ndelt,xval,yval,logx,logy,logity,sqrty,
     &      y0,yinf,ymin,ymax,x0,xmin,xmax,ndimd,NDISP,ndv1)
	      if(ndisp.gt.nbig) then	!calc ndelt>1
		   ndelt=1 + (ndisp-1)/nbig
		   rj0=0		!thin line for speed
	      endif
	   endif
	   if(ndelt.gt.1) then
	      call INTCONV(ndelt,cnum1)
	      if(ndelt.eq.2) then
		   cnum1(1:3)='2nd'
	      else if(ndelt.eq.3) then
		   cnum1(1:3)='3rd'
	      else
		    n=nblank1(cnum1)
		   cnum1=cnum1(1:n)//'th'
	      endif
c	      call WDIALOG(2,'Every '//(cnum1)//' point',12)
c		call WDIALOG(1,
c     &  'Points omitted, thin line, for speed (ALL POINTS reverses)',12)
	   endif
	   tload=.true.					!thickness to be loaded in loop
	   if((iud.eq.1).and.(isym(j).eq.0)) then
	      call LINWID(thick(j0))
	      tload=.false.					!thickness already loaded
	   else if(isym(j).ne.0.and.ijoin(j).eq.-1) then
	      call LINWID(thick(27))
	      tload=.false.					!thickness already loaded
	   endif
	   if(itrace.eq.2) then
	      reclen=xval(ndat(1),1)-xval(1,1)	!length of trace
	      mulpage=.true.
c	      call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
c     &      ndat(1),ijoin(1),icol(1),thick(1),ndv1,ndimd,ntrace,
c     & 	ytsep,reclen,idraw(1),icol(71),
c     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,3)
c	      itrace=1		!so this bit not done again
c 	      call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	      RETURN
	   endif
	   if(j.eq.1.and.ntrace.gt.1) then
c	      call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
c     &      ndat(1),ijoin(1),icol(1),thick(1),ndv1,ndimd,ntrace,
c     & 	ytsep,reclen,idraw(1),icol(71),
c     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,2)
	   else
	   rj0=wid*rj0
		  icw=icurvw(j)	!SD control for curve 1,2,....
		  icolj=icol(100+j)
		  if(jmiss(j).ne.1) then
			if(icolj.ge.0) then
c	        if(idev.eq.0) then
			call DRAWDAT5(j,xval,yval,ndelt,
     &       logity,logx,logy,sqrty,y0,yinf,x0,mono,ilog,
     &       xmin,xmax,ymin,ymax,iud,tload,idev,
     &       ndat(j),ijoin(j),icolj,isym(j),syms(j),thick(j+100),
     &       thick(241),thick(242),barleng,interp,npint,
     &       Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd,
     &	   weight,kwi,kwj,icw,sx,sy,n1)
		
			endif
	   endif
	   endif
	enddo		!end of sets loop

c     28=jump bar (logo) for C-jumps
c     29=jump bar (logo) for V-jumps
	thick(241)=thick(244)
	thick(242)=thick(244)

	call broken(0)
	if(.not.IVplot) then
	   if(ncjump.gt.0.and.idrawc.eq.1) then
            call glincols(icol(241),idev)		!colour for c-logo
		call JLOGO(xmin,xmax,ncjump,t1c,t2c,
     &           xoff1,idev,thick(244),1,y1c,y2c)
	   endif
	   if(nvjump.gt.0.and.idrawv.eq.1) then
            call glincols(icol(242),idev)		!colour for v-logo
		call JLOGO(xmin,xmax,nvjump,t1v,t2v,
     &         xoff1,idev,thick(244),1,y1v,y2v)
	   endif
	endif
	call gFlushGraphics()
	end