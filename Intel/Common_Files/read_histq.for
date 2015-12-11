	subroutine Read_histq(istrec,xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,
     & ndat,ijoin,ncurvc,ncal,iline,xmin,xmax,ymin,ymax,xcross,ycross,
     & xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,itit,title1,
     & ilog,iscal,doframe,titlex,titley,ilabel,inumx,inumy,
     & theta,ifitype,ncomp,sval,isval,xwbase,lt2,screen,colplotter,
     & iptype,ndv1,ndc1,kmax,iver,idev,dxs,dys)
c     & iver,idev)
c
c Subroutine for AUTPLT to read and decode plot queue files written by
c VHSQ1 in VHIST.
c Modif 11/14/97 10:59am for VHIST5/VHSQ5 -signalled by iptype=21
c Modif 03/24/94 10:23am Xcal, Ycal increased to 2048 as VHIST3
c Modif 02/15/93 01:06pm ICOL(), mono added as arguments
c	real XVAL(0:511,10),YVAL(0:511,10)
c	real XCAL(2048,10),YCAL(2048,10)
c	real theta(20)
	real XVAL(0:ndv1,ndimd),YVAL(0:ndv1,ndimd)
	real XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	real theta(kmax),theta1(100)
      integer*2 jstrec1(200),lstrec1(200)		!for
	dimension ndat(ndimd),ncal(ndimc),ijoin(ndimd),iline(ndimc)
	dimension ndat1(20),ncal1(20),ijoin1(20),iline1(20)  !for queue
c	character*1 ans,UC
	character*40 titlex,titley
	character*64 title1	!plot title (local)
	LOGICAL doframe,mono,mono1,slock
	logical discprt
	
	logical screen,colplotter
c extra args to hold all details of posh plots
	integer*2 lth(100),ilt 		!for line thickness
	real RX(100),RY(100),rlth(100),angle(100)
	real rRX(100),rRY(100),rangle(100)
	real rxbox(4,100),rybox(4,100)
	real rrxbox(4,100),rrybox(4,100)
	integer IXJUS(100),IYJUS(100),IJUS(100)
	integer iangle(100),idraw(100),icol(100)
	integer jdraw(250),jcols(250),itype(250)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25),cnumz(25)
	character*10 cexpz(25)
	integer ifnt(30),jfont(100)
	real csfac(30),sizet(30),sizetext(100)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c integer*2 copies to save disc space
	integer*2 IXJUS1(100),IYJUS1(100),IJUS1(100),ival
	integer*2 iangle1(100),idraw1(100),icol1(100)
c
	real*4 DATCOP(:)		!put in common to save space
	integer*2 INT2COP(:)
	character*10 CHARCOP(:)
	integer*4 INT4COP(:)
      ALLOCATABLE :: DATCOP,INT2COP,CHARCOP,int4cop
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10),xbeg(50),ybeg(50),xend(50),yend(50)		!record x value
	real*4 yvlb(10),yvle(10),thick(250),c_thick(250)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	common/ptext/ameant(100),areat(100)
	
	COMMON/TPOS/jDRAW,jCOLs,thick,c_thick,ITYPE,jFoNT,rANGLE,IJUS,
     &	SIZEtext,rRXBOX,rRYBOX,rRX,rRY,NARROW,NLINE,NHLINE,NVLINE,
     &	XBeg,YBeg,XEnd,YEnd,NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,
     &	CEXPX,CEXPY,CEXPZ,NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
	common/dp/discprt
c
	if(iptype.eq.16) then
	ALLOCATE(datcop(ndim),int2cop(1000),charcop(100))
	
	else
      ALLOCATE(datcop(500000),int2cop(1000),charcop(100))
      endif
      ALLOCATE(int4cop(ndimd+ndimc))
c
c
c Note: PLOTQ/POSHPLOT now hold values for things are already defined at
c time plot is queued/stored. Thus default angle,line thickness etc not
c defined for new things that are added. This is done here: valid values
c from queue overwrite these defaults when read in below
	ncomp=0

	ndv2=ndv1
	ndc2=ndc1
	do  i=1,40
	titlex(i:i)=char(32)
	titley(i:i)=char(32)
	enddo
	do  i=1,64
	title1(i:i)=char(32)
	enddo
	do j=1,20
	do i=1,80
	  newtext(j)(i:i)=char(32)
	enddo
	enddo
	do 8 i=1,30
	ifnt(i)=ifont
	csfac(i)=1.0
8	continue
	
	do 81 i=1,100
	  angle(i)=0.
	  idraw(i)=-2		!until defined
	jdraw(i)=-2
	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
	ameant(i)=0
	areat(i)=0
81	continue
	do 83 i=1,10
83	iltype(i)=0		!continuous line for extra lines
	ilt=20		!default thickness, unless reset
c	if(autplt.and.draft) ilt=0	!'draft' set below
	do i=1,100

	lth(i)=ilt
      rlth(i)=0.01*lth(i)
      enddo
c	lth(21)=25	!axes (and axis labels at present!)
 	lth(22)=25	!frame
	lth(26)=10	!param value text
	lth(27)=15	!symbols
	lth(28)=12	!SD bars
      rlth(22)=0.25	!frame
	rlth(26)=0.10	!param value text
	rlth(27)=0.15	!symbols
	rlth(28)=0.12	!SD bars

c End of initialisation
c
c
c Read back first data record from unit=11, opened in main prog
	krn=istrec
	if(idev.eq.0) then
		xp=213.000
		yp=159.667
	else if(idev.eq.5) then
		xp=280.500
		yp=196.700
	else if(idev.eq.6) then
		XP=203.2
		YP=266.7
	else if(idev.eq.3) then
		XP=210.
		YP=297.
	else if(idev.eq.4) then
		XP=297.
		YP=210.
	endif
      read(11,rec=1) nplot,jstrec1,lSTREC1,iver
      if (iver.eq.1100) then
	  if(iptype.eq.2) then
	    read(11,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
     &    xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     &    iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,
     &    ndat,ncurvc,ncal,ijoin,
     &    iline,inumx,inumy,sval,isval,
     &    theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     &    narrow,nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     &    jcol,mono1,ioffset
	  else if(iptype.eq.21) then
	    read(11,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     &     itit,title1,xmin,xmax,ymin,ymax,
     &     xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     &     iscal,doframe,titlex,titley,ilabel,ncurvd,ndat,
     &     ncurvc,ncal,ijoin,
     &     iline,inumx,inumy,sval,isval,
     &     theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     &     nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     &     jcol,mono1,ioffset
	
	  else if(iptype.eq.22) then
	!	if(ncomp.eq.0) ncomp=1
	    read(11,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     &     itit,title1,xmin,xmax,ymin,ymax,
     &     xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     &     iscal,doframe,titlex,titley,ilabel,ncurvd,ndat,
     &     ncurvc,ncal,ijoin,
     &	 iline,inumx,inumy,sval,isval,
     &     theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     &     nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     &     jcol,mono1,ioffset,
     &     (ameant(i),i=1,ncomp),(areat(i),i=1,ncomp)
        else if(iptype.eq.23) then
	    read(11,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     &     itit,title1,xmin,xmax,ymin,ymax,
     &     xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     &     iscal,doframe,titlex,titley,ilabel,ncurvd,ndat,
     &     ncurvc,ncal,ijoin,
     &	 iline,inumx,inumy,sval,isval,
     &     theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     &     nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     &     jcol,mono1,ioffset,
     &     (ameant(i),i=1,ncomp),(areat(i),i=1,ncomp)
	  
		
		
	 endif  
	else if(iver.eq.1200) then
		if(iptype.eq.23) then
	    read(11,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     &     itit,title1,xmin,xmax,ymin,ymax,
     &     xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     &     iscal,doframe,titlex,titley,ilabel,ncurvd,ndat,
     &     ncurvc,ncal,ijoin,
     &	 iline,inumx,inumy,sval,isval,
     &     theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     &     nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     &     jcol,mono1,ioffset,
     &     (ameant(i),i=1,ncomp),(areat(i),i=1,ncomp)
	  
		
		endif
      else
     	 read(11,rec=krn) iptype,itit,title1(1:44),xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat,
     & ncurvc,ncal,ijoin,iline,inumx,inumy,sval,isval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     & jcol,mono1
	 a=float(ixlo)/10000.
	 b=float(iylo)/7500.
	 c=float(ixhi)/10000.
	 d=float(iyhi)/7500.
       xlo=xp*a
       ylo=yp*b
       xhi=xp*c*0.88
       yhi=yp*d*0.88
	endif
	do i=1,ndimd
!		ndat(i)=ndat1(i)
!		ijoin(i)=ijoin1(i)
	enddo
	do i=1,ndimc
!		ncal(i)=ncal1(i)
!		iline(i)=iline1(i)
	enddo
	if (csize.lt.2.) csize=3.0
	if(iver.eq.1100.or.iver.eq.1200) then
	   istr1=istrec+ioffset
	   if(ntext.eq.0) then
		read(11,rec=istr1) (INT2COP(i),i=1,k4),
     &	  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3)
	   else
		read(11,rec=istr1) (INT2COP(i),i=1,k4),
     &     (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3),(NEWTEXT(i),i=1,ntext)
	   endif
	   goto 1010		!decode datcop etc
	endif
c
c Check value of mono in case data came from an old queue in which
c these were not present -now use value of MONO defined in INAUT to set
c default colours for old queues if wanted (read as mono1 to avoid overwriting
c value already set if this file did not have colour)
	if(jcol.eq.1002) then		!colours were queued
	   if(screen.or.(.not.screen.and.colplotter)) then
		mono=mono1			!use queued value of mono
	   endif
	else if(jcol.ne.1002) then	!colours NOT queued -mono as set in INAUT
	   call SET_CoLourS(mono,jcols,.false.,.true.,0)	!set plotter colours
	endif
c
c Read back compressed data array from unit=11
	nbyte1=4*k1
	nbyte2=80*k2
	nbyte3=10*k3
	nbyte4=2*k4		!no of bytes in INT2COP
	nrec1=1 + (nbyte1-1)/1024	!for DATCOP
c First read DATCOP
	n=1
	do 380 j=1,nrec1
	  m=n+255
	  krn=krn+1024
	  read(11,rec=krn) (DATCOP(i),i=n,m)
	  n=n+256
380	continue
c
	if(ntext.eq.0) then
	   if(nbyte3+nbyte4.le.1024) then
		krn=krn+1024
		read(11,rec=krn) (CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else
		krn=krn+1024
		read(11,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1024
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   endif
	else if(ntext.gt.0) then
	   if(nbyte2+nbyte3+nbyte4.le.1024) then
		krn=krn+1024
		read(11,rec=krn) (NEWTEXT(i),i=1,ntext),
     &		(CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else if(nbyte2+nbyte3.le.1024) then
		krn=krn+1024
		read(11,rec=krn)(NEWTEXT(i),i=1,ntext),(CHARCOP(i),i=1,k3)
		krn=krn+1024
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   else if(nbyte2.le.1024.and.(nbyte3+nbyte4.le.1024))then
		krn=krn+1024
		read(11,rec=krn)(NEWTEXT(i),i=1,ntext)
		krn=krn+1024
		read(11,rec=krn) (CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else if(nbyte2.le.1024)then
		krn=krn+1024
		read(11,rec=krn) (NEWTEXT(i),i=1,ntext)
		krn=krn+1024
		read(11,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1024
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   else
		krn=krn+1024
		read(11,rec=krn) (NEWTEXT(i),i=1,12)
		krn=krn+1024
		read(11,rec=krn) (NEWTEXT(i),i=13,20)
		krn=krn+1024
		read(11,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1024
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   endif	!end of ntext>0
	endif
c
c	if(debug()) pause '1'

c DECODE SECTION
1010	continue
c All compressed data arrays read in. Now decode them.
	k=0		!index for datcop
	if(ncurvd.le.0) goto 365
	do 37 j=1,ncurvd
	n=ndat(j)+2
	   do 371 i=0,n
	   k=k+1
371	   Xval(i,j)=datcop(k)
	   do 372 i=0,n
	   k=k+1
372	   Yval(i,j)=datcop(k)
37	continue
365	continue
	if(ncurvc.le.0) goto 366
	do 39 j=1,ncurvc
	n=ncal(j)
	   do 391 i=1,n
	   k=k+1
391	   xcal(i,j)=datcop(k)
	   do 392 i=1,n
	   k=k+1
392	   ycal(i,j)=datcop(k)
39	continue
366	continue
c
c Now add onto DATCOP the real*4 data for posh plots- keep incrementing k
c
	do 682 i=1,10
	   k=k+1
         if(iver.eq.1100.or.iver.eq.1200) then
         	sizet(i)=DATCOP(k)
          	 k=k+1
        	angle(i)=DATCOP(k)
         else
		csfac(i)=DATCOP(k)
		sizet(i)=csfac(i)*csize
         endif
	   k=k+1
	   rx(i)=DATCOP(k)
	   k=k+1
	   ry(i)=DATCOP(k)
	   do 680 j=1,4
	     k=k+1
	     rxbox(j,i)=DATCOP(k)
	     k=k+1
	     rybox(j,i)=DATCOP(k)
680	   continue
682	continue		!keep 1-10 always
c
	if(ntext.gt.0) then
	do 68 i1=1,ntext
	   i=i1+10
	   k=k+1
          if(iver.eq.1100.or.iver.eq.1200) then
         	sizet(i)=DATCOP(k)
          	 k=k+1
        	angle(i)=DATCOP(k)
         else
	   csfac(i)=DATCOP(k)
	   sizet(i)=csfac(i)*csize
         endif
	   k=k+1
	   rx(i)=DATCOP(k)
	   k=k+1
	   ry(i)=DATCOP(k)
	   do 681 j=1,4
	     k=k+1
	     rxbox(j,i)=DATCOP(k)
	     k=k+1
	     rybox(j,i)=DATCOP(k)
681	   continue
68	continue
	
	do ki=1,ntext
		if(ilog.eq.5.or.ilog.eq.6) then
			ry(ki+10)=ry(ki+10)**2
		else if(ilog.eq.2.or.ilog.eq.3) then
			ry(ki+10)=10**ry(ki+10)
		endif
		if(ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6) then
			rx(ki+10)=10**rx(ki+10)
		endif
	enddo
	
	endif
	do 69 i1=1,numbx
	   i=i1+30
	   k=k+1
         if(iver.eq.1100.or.iver.eq.1200) then
            angle(i)=DATCOP(k)
		K=K+1
         endif
	   rx(i)=DATCOP(k)
	   k=k+1
	   ry(i)=DATCOP(k)
	   do 691 j=1,4
	     k=k+1
	     rxbox(j,i)=DATCOP(k)
	     k=k+1
	     rybox(j,i)=DATCOP(k)
691	   continue
69	continue
	do 70 i1=1,numby
	   i=i1+55
	   k=k+1
         if(iver.eq.1100.or.iver.eq.1200) then
            angle(i)=DATCOP(k)
		k=k+1
         endif
	   rx(i)=DATCOP(k)
	   k=k+1
	   ry(i)=DATCOP(k)
	   do 701 j=1,4
	     k=k+1
	     rxbox(j,i)=DATCOP(k)
	     k=k+1
	     rybox(j,i)=DATCOP(k)
701	   continue
70	continue
	if(narrow.gt.0) then
	   do 71 i=1,narrow
	   k=k+1
	   xb(i)=DATCOP(k)
	   k=k+1
	   xe(i)=DATCOP(k)
	   k=k+1
	   yb(i)=DATCOP(k)
	   k=k+1
	   ye(i)=DATCOP(k)
71	   continue
	endif
	if(nline.gt.0) then
	   do 72 i=1,nline
	   k=k+1
	   xlb(i)=DATCOP(k)
	   k=k+1
	   xle(i)=DATCOP(k)
	   k=k+1
	   ylb(i)=DATCOP(k)
	   k=k+1
	   yle(i)=DATCOP(k)
72	   continue
	endif
      if(iver.eq.1100.or.iver.eq.1200) then
        do 601 i=1,30	!keep 1st 30 RLTH always
	   k=k+1
	    RLTH(i)=DATCOP(k)
601	   continue
	  if(ntext.gt.0) then
	   do 611 i=1,ntext
	   k=k+1
	    RLTH(i+30)=DATCOP(k)
611	   continue
	endif
	if(narrow.gt.0) then
	   do 621 i=1,narrow
	   k=k+1
	   rLTH(i+50)= DATCOP(k)
621	   continue
	endif
	if(nline.gt.0) then
	   do 631 i=1,nline
	   k=k+1
	   rLTH(i+60)=DATCOP(k)
631	   continue
	endif
      else
      do m=1,100
         rlth(i)=0.01*lth(i)
      end do
 	RLTH(23)=0.5		!labels
 	RLTH(24)=0.5		!numbers
 	RLTH(25)=0.5		!title
 	RLTH(26)=0.25	!param value text
	do i=31,50
		rlth(i)=0.5
	enddo
      endif
c end of DATCOP
c
	k=0
	do 73 i=1,numbx
	k=k+1
	cnumx(i)=CHARCOP(k)
	k=k+1
	cexpx(i)=CHARCOP(k)
73	continue
c
	do 74 i=1,numby
	k=k+1
	cnumy(i)=CHARCOP(k)
	k=k+1
	cexpy(i)=CHARCOP(k)
74	continue
c=====================================================

	k=0		!index for int2cop
      if(iver.ne.1100.and.iver.ne.1200) then
	do 60 i=1,30	!keep 1st 30 lth always
	   k=k+1
	   lth(i)=INT2COP(k)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   lth(i+30)=INT2COP(k)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   lth(i+50)=INT2COP(k)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   lth(i+60)=INT2COP(k)
63	   continue
	endif
      endif
	do 64 i=1,10
         if(iver.ne.1100.and.iver.ne.1200) then
            k=k+1
	      ixjus1(i)=INT2COP(k)
		k=k+1
	      iyjus1(i)=INT2COP(k)
	      k=k+1
	      iangle1(i)=INT2COP(k)
         else
            k=k+1
            ijus1(i)=INT2COP(k)
         endif
	   k=k+1
	   idraw1(i)=INT2COP(k)
64	continue
	if(ntext.gt.0) then
	   do 65 i=1,ntext
         if(iver.ne.1100.and.iver.ne.1200) then
	   	k=k+1
	   	ixjus1(i+10)=INT2COP(k)
	   	k=k+1
	   	iyjus1(i+10)=INT2COP(k)
	   	k=k+1
	   	iangle1(i+10)=INT2COP(k)
         else
         	k=k+1
         	ijus1(i+10)=INT2COP(k)
         endif
	   k=k+1
	   idraw1(i+10)=INT2COP(k)
65	   continue
	endif
	do 66 i=1,numbx
       if(iver.ne.1100.and.iver.ne.1200) then
	   k=k+1
	   ixjus1(i+30)=INT2COP(k)
	   k=k+1
	   iyjus1(i+30)=INT2COP(k)
	   k=k+1
	   iangle1(i+30)=INT2COP(k)
         else
         k=k+1
         ijus1(i+30)=INT2COP(k)
       endif
	   k=k+1
	   idraw1(i+30)=INT2COP(k)
66	continue
	do 67 i=1,numby
       if(iver.ne.1100.and.iver.ne.1200) then
	   k=k+1
	   ixjus1(i+55)=INT2COP(k)
	   k=k+1
	   iyjus1(i+55)=INT2COP(k)
	   k=k+1
	   iangle1(i+55)=INT2COP(k)
       else
         k=k+1
         ijus1(i+55)=INT2COP(k)
       endif
	   k=k+1
	   idraw1(i+55)=INT2COP(k)
67	continue
c
c Next read colours (if present)
c	if(.not.mono) then
	if(jcol.eq.1002) then
	  do i=1,30	!keep 1st 30 ICOL always
	   k=k+1
	   icol1(i)=INT2COP(k)
	  enddo
	  if(ntext.gt.0) then
	    do i=1,ntext
	      k=k+1
	      icol1(i+30)=INT2COP(k)
	    enddo
	  endif
	  if(narrow.gt.0) then
	    do i=1,narrow
	      k=k+1
	      icol1(i+50)=INT2COP(k)
	    enddo
	  endif
	  if(nline.gt.0) then
	    do i=1,nline
	      k=k+1
	      icol1(i+60)=INT2COP(k)
	    enddo
	  endif
c and background colour
	  k=k+1
	  icol1(71)=INT2COP(k)
c convert to int*4 (only if icol1 read here!)
	  do j=1,100
	    icol(j)=int4(icol1(j))
	  enddo
	endif
c End of int2cop
c
c Convert back from int*2

c
c For old queues must convert justification to new convention (see CONSTS.FOR)
	if(jcol.ne.1002) then
	   do i=1,100
		ival=ixjus1(i)
		if(ival.eq.2) ixjus1(i)=1
		if(ival.eq.1) ixjus1(i)=2
		ival=iyjus1(i)
		if(ival.eq.2) iyjus1(i)=1
	   enddo
	endif
	do 50 j=1,100
       if(iver.eq.1100.or.iver.eq.1200) then
          ijus(j)=int4(ijus1(j))
       else
          IJUS0=int4(IXJUS1(j))
	    if(ijus0.eq.0) then
		  ijus(j)=-1
	    else if(ijus0.eq.1) then
		  ijus(j)=1
	    else if(ijus0.eq.2) then
		  ijus(j)=0
	    else if(ijus0.eq.3) then
		  ijus(j)=-1
	    endif
	    angle(j)=float(iangle1(j))
       endif
	 idraw(j)=int4(idraw1(j))
50	continue
c
c Modif 08/21/90 12:46pm for case where ifitype seems to be not 0, though
c ncomp=0 (error in VHIST1?)
	if(ncomp.eq.0) then
	   idraw(6)=0
	   ifitype=0
	endif

	do i=1,10
		xbeg(i)=xb(i)
		ybeg(i)=yb(i)
		xend(i)=xe(i)
		yend(i)=ye(i)
		xbeg(i+10)=xlb(i)
		ybeg(i+10)=ylb(i)
		xend(i+10)=xle(i)
		yend(i+10)=yle(i)
	    itype(i+200)=iltype(i)
		xbeg(i+20)=xhlb(i)
		ybeg(i+20)=yhline(i)
		xend(i+20)=xhle(i)
		yend(i+20)=yhline(i)
		itype(i+210)=ilhtype(i)
		xbeg(i+30)=xvline(i)
		ybeg(i+30)=yvlb(i)
		xend(i+30)=xvline(i)
		yend(i+30)=yvle(i)
		itype(i+220)=ilvtype(i)
		
		jcols(i+200)=icol(i+50)
		jcols(i+210)=icol(i+60)
		jcols(i+220)=icol(i+71)
		jcols(i+230)=icol(i+81)
		jdraw(i+80)=idraw(i+10)
		jfont(i+80)=ifnt(i+10)
		jcols(i+80)=icol(i+30)
		sizetext(i+80)=sizet(i+10)
		jcols(i+100)=icol(i)
		jcols(i+150)=icol(i+10)
		jdraw(i+100)=idraw(i)
		jdraw(i+150)=idraw(i+10)
		
	enddo

	do i=6,55
		if(i.lt.30) ijus(i)=0
		jfont(i)=ifnt(7)
		sizetext(i)=sizet(7)
		jdraw(i)=idraw(i+25)
		jcols(i)=icol(24)
		rangle(i)=angle(i+25)
		thick(i)=rlth(24)
		rrx(i)=rx(i+25)
		rry(i)=ry(i+25)
		if(i.ge.31) then
	!		rrx(i)=rx(i+25)-2.5*dxs
	!		rry(i)=ry(i+25)+dys
		endif
		do j=1,4
			rrxbox(j,i)=rxbox(j,i+25)
			rrybox(j,i)=rybox(j,i+25)
		enddo
	enddo
	
	jcols(1)=icol(25)
	jdraw(1)=idraw(10)
	jfont(1)=ifnt(10)
	rangle(1)=angle(10)
	sizetext(1)=sizet(10)
	thick(1)=rlth(25)
	rrx(1)=rx(10)
	rry(1)=ry(10)
	do j=1,4
		rrxbox(j,1)=rxbox(j,10)
		rrybox(j,1)=rybox(j,10)
	enddo

	jcols(2)=icol(26)
	jdraw(2)=idraw(6)
	jfont(2)=ifnt(6)
	rangle(2)=angle(6)
	sizetext(2)=sizet(6)
	thick(2)=rlth(26)
	rrx(2)=rx(6)
	rry(2)=ry(6)
	do j=1,4
		rrxbox(j,2)=rxbox(j,6)
		rrybox(j,2)=rybox(j,6)
	enddo

	jcols(3)=icol(23)
	jdraw(3)=idraw(8)
	jfont(3)=ifnt(8)
	rangle(3)=angle(8)
	sizetext(3)=sizet(8)
	thick(3)=rlth(23)
	rrx(3)=rx(8)
	rry(3)=ry(8)
	do j=1,4
		rrxbox(j,3)=rxbox(j,8)
		rrybox(j,3)=rybox(j,8)
	enddo

	jcols(4)=icol(23)
	jdraw(4)=idraw(9)
	jfont(4)=ifnt(9)
	rangle(4)=angle(9)
	sizetext(4)=sizet(9)
	thick(4)=rlth(23)
	rrx(4)=rx(9)
	rry(4)=ry(9)
	do j=1,4
		rrxbox(j,4)=rxbox(j,9)
		rrybox(j,4)=rybox(j,9)
	enddo
	
	do j=1,20
		rrx(j+80)=rx(10+j)
	    rry(j+80)=ry(10+j)
	enddo

	jcols(244)=icol(22)
	jcols(245)=icol(21)
	thick(244)=rlth(22)
	thick(245)=rlth(21)
	if(iptype.eq.2) then
		
		do j=1,30
		if(j.gt.2) ijus(j)=0
		if(j.ge.6) rry(j)=(rry(j)+rry(3))/2.
		enddo
	    
	endif

c
      DEALLOCATE(DATCOP,INT2COP,CHARCOP)


	RETURN
	end

