	subroutine GETPOP(xval1,yval1,iread,ymin2,ymax2,xmin2,xmax2,yopen,
     &   yshut,offset,tseg,npseg,dtx,npline,
     &  imode,nppage,iform,graph1_1,graphics1_1,GraphMainPanel1_1,
     &  pixbufs,pixbufo,readrec,
     &  npopoints,indx1,indx2,indy1,indy2,xi,xf,pixbuf1,pixbuf2,
     &  ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil,srate,nsam,cjdat)

	use menu_f90
c  imode=1 -does large display of whole page on one line with cursor for open and
c		shut levels
c  imode=0	-uses previous open/shut levels
c
	character*33 adcfil2
	character*30 adcfil
	integer :: Graph1_1(100)
	integer :: GraphMainPanel1_1(100)
	integer :: Graphics1_1(100)
	character :: cnumb*11, title_graph*60, gfile*60
	logical xdone,ydone,meta,wmeta,wbmp,nojump,redo,cjdat
	character*40 titlex,titley,titxsav,titysav
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	character*75 oldtext
	character*64 TITLE1
	character*10   cnumx(25),cnumy(25),cnumz(25)
      character*10   cexpz(25),cexpx(25),cexpy(25)
	integer pixbufo(800,10),pixbufs(800,10)
	integer pixbuf1(10,800),pixbuf2(10,800)
	character*200 parval,text1,str(100)
	logical onjump,tload,calbar,blank
	logical vert,monsav,redrawn,pbmp,pwmf,readrec
	logical plot,plotcols,rescale,hdisp
	real theta(100)
c	real*4 xval(nppage,1),yval(nppage,1),
	real*4 xval1(npopoints,1),yval1(npopoints,1)
	
c for VPLOT
		character*80   newtext(20),nwtext		!extra text
	
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer jmiss(1)
	integer*4 icurvw(:)
	ALLOCATABLE XCAL,YCAL
	real XCAL(:,:),YCAL(:,:)		!for VPLOT
	integer ndat(1),icurvd(1),isym(1),ijoin(1)	!for VPLOT data
	real syms(1)
	integer ncal(1),icurvc(1),iline(1)		!for calc curve
	LOGICAL fitted,doframe,draft,autplt,landscap,ivplot
	logical logx,logy,sqrty,calbarx,calbary,landscape,logity
	real xint(2048),yint(2048),Y2int(2048)
	logical mono,offset,interp
	INTEGER LEFT, CENTER, RIGHT
      DATA LEFT, CENTER, RIGHT /-1, 0, 1 /     !new definition
	logical bold,italik,underline
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
	
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
      
	COMMON/TPOS/IDRAW(250),ICOL(250),THICK(250),C_THICK(250),
     & ITYPE(250),
     & IFNT(100),ANGLE(100),IJUS(100),SIZEtext(100),RXBOX(4,100),
     & RYBOX(4,100),
     & RX(100),RY(100),NARROW,NLINE,NHLINE,NVLINE, XBEG(50),YBEG(50),
     & XEND(50),YEND(50),
     & NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,
     & NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c,ncjump,nvjump,ivplot
c	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,
     &	xmin,xmax,ymin,ymax

	common/calib/calbarx,calbary,xbeg4,ybeg4,xend4,yend4,
     &     xbeg5,ybeg5,xend5,yend5

	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
	common/plotopen/
     & icombo1,poplot,itext1,itext2,
     & itext_1,itext_2,itext_3,itext_4,ivalp1,ivalp2,ivalp3,ivalp4,
     & iradiop1,iradiop2,iradiop3,iradiop4
      
c For setline
	logical delete
c
	character defolt*30,cans*30		!to hold text & result of QDIALOG
c	character*11 cnum
c	character*1 ans
c
c If imode=1 then draw a whole page on one line in vplot5, and then use
c cursors to define open and shut levels
	adcfil2=adcfil
	ycross=yopen
	ntrace=1
	ndelt=1
	if(imode.ge.1) then
	   ymin=ymin2	!so values in call not changed
	   ymax=ymax2	!so values in call not changed
	   xmin=xmin2	!so values in call not changed
	   xmax=xmax2	!so values in call not changed
	   ncurvd=1
	   icurvd(1)=1
c	   ndat(1)=nppage
c	   ndat(1)=npline
	   
	 
	   ncurvc=0
	   ndv1=nppage
	!	ndv1=npline
	   ndimd=1
	   ndc1=1
	   ndimc=1
	   n1=1
	  if(npline.gt.nppage) ndat(1)=nppage
	 ndat(1)=npopoints
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight,xcal,ycal)
	   endif
	   kwi=NDAT(1)
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj),xcal(1,1),ycal(1,1))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
c
	   do i=1,100
		icol(i)=-1
	   enddo
	   cbig=2.5
	   ifont=3
	   ifitype=0		!no display of fitted parameters
	   ntx=5		!if not logt set initial input values
	   nty=5
	   itx=1
	   ity=1
	   ilabel=1
	   titlex='milliseconds'
	   titley='current (pA)'
	   title1=' '
	   ilog=0
	   iscal=1		!scale internally
	   xlo=-1		!whole screen
	   syms(1)=-1.0 	!default symbol sizes
	   isym(1)=0		!points
	   doframe=.true.
	   landscap=.true.
	   autplt=.false.		!to ensure all defaults used
	   fitted=.false.
	   inumx=-1		!don't allow scaling
	   inumy=-1
	do i=1,250
		idraw(i)=-2
		icol(i)=1
				
	enddo
	do i=1,100
		ifnt(i)=4
		sizetext(i)=4
		
	enddo
	
      ishp=0
      mono=.false.
      adcfil2=adcfil
      hdisp=.false.
      ntrace=-1
      interp=.false.
	call calc_default(XVAL1,YVAL1,NDAT,icurvd,ncurvd,ijoin,syms,
     & ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,
     & titlex,titley,ilabel,doframe,autplt,itit,title1,
     & ISHP,ifont,landscap,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver,
     & redrawn,plot,redo,pbmp,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,
     & izoom,xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,
     & ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil2,srate,nsam,cjdat)
	iplot=99
	jplot=99
	ix=200
	iy=200
	logx=.false.
	logy=.false.
	sqrty=.false.
	logity=.false.
	ilog=0
	
	ipos=0
      mono=.false.
      do i=1,kwi
      do j=1,kwj
        weight(i,j)=1
      enddo
      enddo
	call graph1(iplot,jplot,iform,ix,iy,graph1_1,GraphMainPanel1_1,
     & graphics1_1,wxmin,wxmax,wymin,wymax,ipos,gfile,plot,-2,0,npagtot)
	call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,
     &	xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,
     &	logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,
     &  calbarx,calbary,xbeg4,ybeg4,xend4,yend4,
     &     xbeg5,ybeg5,xend5,yend5)
          
      		call draw_data(xval1,yval1,icurvd,ncurvd,ndelt,logity,
     &	logx,logy,sqrty,y0,yinf,x0,ilog,idev,ndat,
     &    ijoin,icol,isym,syms,thick,barleng,interp,npint,Xint,Yint,
     &    nint,yp1,ypn,Y2int,ndv1,ndimd,weight,kwi,kwj,dxs,dys,icurvw,
     &	jmiss,n1,idraw,ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil2)
	
	readrec=.true.
	call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,
     &cnumy,cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,
     &inumx,inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,
     &icol,ifnt,str,dxs,dys)	
c	   call WDIALOG(1,
c     & 'Adjust shut level with arrow keys/IMS; then <enter>',11)

	 !  eps=0.02		!0.02 pA (prev line is problem when yshut=0.!)
	   ic=10		!green
	   dim=0.0
	nx=800
	ny=10
	   if(yopen.le.ymin) then
			yopen=ymin+0.1*(ymax-ymin)
		endif
		if(yshut.ge.ymax) then
			yshut=ymax-0.1*(ymax-ymin)
		endif
	  delete=.false.
		
		call getpixelarray(xmin,yshut,xmax,yshut,pixbufs,nx,ny)
		call drawline(xmin,yshut,xmax,yshut,ic,idev)

	  ! eps=0.02		!0.02 pA
	   ic=12		!red
	  
		call getpixelarray(xmin,yopen,xmax,yopen,pixbufo,nx,ny)
		call drawline(xmin,yopen,xmax,yopen,ic,idev)
	   dim=0.0
	   delete=.false.
	   xi=xmin+2000.*dtx
	   xf=xmax-2000.*dtx
	   xi=xmin
	   xf=xval1(npopoints,1)
	   
		if(xf.gt.xmax) xf=xmax
	  ic=13
	  call getpixelarray(xi,ymin,xi,ymax,pixbuf1,ny,nx)
		call drawline(xi,ymin,xi,ymax,ic,idev)
		ic=13
		
		call getpixelarray(xf,ymin,xf,ymax,pixbuf2,ny,nx)
		call drawline(xf,ymin,xf,ymax,ic,idev)
	  
	  call gmsetvaluesetting(ivalp1,yopen)
		call gmsetvaluesetting(ivalp2,yshut)
		call gmsetvaluesetting(ivalp3,xi)
		call gmsetvaluesetting(ivalp4,xf)
c
		call gmActivateGraphicsFrame(graphics1_1(99))
	endif		!end of imode=1
c
c
c Now calculate values of P(open) for current page and keep in popen(i),
c and xpval(i)
	
	if(allocated(icurvw)) then
	   DEALLOCATE(icurvw,weight,xcal,ycal)
	endif
c	deallocate(xval1,yval1)
	RETURN
	END
