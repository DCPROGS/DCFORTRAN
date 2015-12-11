	subroutine RDVPLQ(istrec,xval,yval,xcal,ycal,ndimd,ndimc,ncurvd,
     & ndat,isym,ijoin,ncurvc,ncal,iline,syms,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,
     & itit,title1,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & interp,screen,colplotter,
     & itrace,ntrace,ytsep,ndv1,ndc1,
     & kwi,kwj,icurvw,kmax,iver)
c
c Modif 04/04/95 11:18am for records written by VLPQ5 (in VPLOT5)
c  Since all plots now done with vplot5 etc, ifstcol, ndatsav removed.
c
c 05/22/95 07:23am DATCOP dimension increased to 100000 in RDVPLQ and VPLQ5
c Modif 09/11/94 11:46am for records written by VLPQ4 (in VPLOT4)
c Modif 02/15/93 01:06pm ICOL(), mono, interp added as arguments
c Subroutine for AUTPLOT to read and decode plot queue files written by
c VPLQ1 in VPLOT, or by VPLQR1 in VPLOTR
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	real weight(kwi,kwj)
	integer icurvw(ndimd)
	real theta(kmax),syms(ndimd)
      integer*2 jstrec(200),lstrec(200),ival		!for
	dimension ndat(ndimd),isym(ndimd),ijoin(ndimd)
	integer ncal(ndimc),iline(ndimc)
c arrays needed when more than 2048 points/plot from VPLOTR
c=	integer ifstcol(10),ndatsav(10),ijoinsav(10),isymsav(10)
c	character*1 ans,UC
	character*40 titlex,titley
	character*64 title1	!plot title (local)
	LOGICAL doframe,mono,mono1,interp,slock,pon
	logical discprt
	logical caplock,debug
	logical screen,colplotter
c extra args to hold all details of posh plots
	integer*2 lth(100),ilt 		!for line thickness
	real RX(100),RY(100),rlth(100)
	real rxbox(4,100),rybox(4,100)
     	integer IXJUS(100),IYJUS(100),IJUS(100)
	integer idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)
	real csfac(30),size(30),angle(100)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c for VPLOTR:
c	dimension XVAL2(2048,10),YVAL2(2048,10)	!for VPLOTR
c	dimension XCAL2(2048,10),YCAL2(2048,10)
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	logical ivplot
c	logical bigplot
c integer*2 copies to save disc space
	integer*2 IXJUS1(100),IYJUS1(100),ijus1(100)
	integer*2 iangle1(100),idraw1(100),icol1(100)
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
c
	allocatable:: syms1,ndat1,isym1,ncal1,ijoin1,iline1
	integer ndat1(:),isym1(:),ncal1(:),ijoin1(:),iline1(:)
	real syms1(:)
c
	allocatable::datcop,int2cop,int4cop,charcop
	real*4 DATCOP(:)
	integer*2 INT2COP(:)
	integer*4 INT4COP(:)
	character*10 CHARCOP(:)
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	COMMON/cols/icol,mono
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
	allocate(int2cop(1000),charcop(100))
c
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

	if(iptype.lt.15) then
	   ALLOCATE(syms1(10),ndat1(10),isym1(10),ncal1(10),
     &	ijoin1(10),iline1(10))
	endif
	iver=-1
c Note: PLOTQ/POSHPLOT now hold values for things are already defined at
c time plot is queued/stored. Thus default angle,line thickness etc not
c defined for new things that are added. This is done here: valid values
c from queue overwrite these defaults when read in below
c NB iangle,idraw,ixjus,iyjus are read as integer*2 (iangle1 etc), and all
c these converted to integer*4 at end so it is int*2 versions, iangle1,idraw1,
c that must be initialised here (as long as idraw=-2 then ixjus,iyjus will
c be defined in VPLOT, eg after rescale that makes more numbers on X axis
c than the numbx values in queue)
	do 8 i=1,30
	ifnt(i)=ifont
	csfac(i)=1.0
8	continue
	do 81 i=1,100
	  iangle1(i)=0
	  idraw1(i)=-2		!until defined
	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
81	continue
	idraw1(28)=1	!default for c-jump logo
	idraw1(29)=1	!default for v-jump logo
	do 83 i=1,10
83	iltype(i)=0		!continuous line for extra lines
	ilt=20		!default thickness, unless reset
	do i=1,100
	 lth(i)=ilt
       rlth(i)=0.01*lth(i)
      enddo
c	lth(21)=25	!axes (and axis labels at present!)
 	lth(22)=25	!frame
	lth(26)=10	!param value text
	lth(27)=15	!symbols
	lth(28)=12	!SD bars
      rlth(22)=0.3	!frame
 	RLTH(23)=0.5		!labels
 	RLTH(24)=0.5		!numbers
 	RLTH(25)=0.5		!title
 	RLTH(26)=0.25	!param value text
	do i=31,50
		rlth(i)=0.5
	enddo
	rlth(27)=0.15	!symbols
	rlth(28)=0.12	!SD bars
c End of initialisation
c
c Read back first data record from unit=11
c iptype=12 for data queued by VPLOT2 (iptype=1 for VPLOT1, iptype=11
c for VPLOTr, both now obsolete)
	krn=istrec
	if(iptype.eq.1) then
	   read(11,rec=krn) iptype,itit,title1(1:44),xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     &iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,isym1,
     & ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,
     & sval,theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4
c
	else if(iptype.eq.11.or.iptype.eq.12) then
	   read(11,rec=krn) iptype,itit,title1(1:44),xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,
     & isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & jcol,mono1,interp
c
	else if(iptype.eq.14) then
	  read(11,rec=krn) iptype,ndv1,ndimd,ndc1,ndimc,itit,title1(1:44),
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
     & csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     & ndat1,isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & jcol,mono1,interp,nhline,nvline,ntrace,ytsep
c
	else if(iptype.eq.15) then
         read(11,rec=1) nplot,jstrec,lstrec,iver
         if(iver.eq.1100) then
          read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     &  itit,title1,
     &  xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,
     &  ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     &  ncurvc,inumx,inumy,sval,
     &  ifitype,ncomp,numbx,numby,ifnt,ntext,k1,k2,k3,k4,k5,
     &  narrow,nline,iltype,y0,yinf,ntx,nty,itx,ity,
     &  ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     &  mono1,interp,nhline,nvline,ntrace,ytsep,ioffset
         else
	    read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     &  itit,title1(1:44),
     &  xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
     &  csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     &  ncurvc,inumx,inumy,sval,
     &  ifitype,ncomp,numbx,numby,ifnt,ntext,k1,k2,k3,k4,k5,
     &  narrow,nline,iltype,y0,yinf,ntx,nty,itx,ity,
     &  ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     &  mono1,interp,nhline,nvline,ntrace,ytsep,ioffset
         endif
	endif
c For old queues, convert position of jump logos to Y units (see not in AUTPLOT)
	if(iver.lt.1100) then
	  y1c=ymin + (ymax-ymin)*float(iy1c-iylo)/float(iyhi-iylo)
	  y2c=ymin + (ymax-ymin)*float(iy2c-iylo)/float(iyhi-iylo)
	  y1v=ymin + (ymax-ymin)*float(iy1v-iylo)/float(iyhi-iylo)
	  y2v=ymin + (ymax-ymin)*float(iy2v-iylo)/float(iyhi-iylo)
	endif
	if(screen) then
		xp=213.000
		yp=159.667
	else
		xp=280.500
		yp=196.700
	endif
	if(iver.ne.1100) then
	 a=float(ixlo)/10000.
	 b=float(iylo)/7500.
	 c=float(ixhi)/10000.
	 d=float(iyhi)/7500.
	 if(iptype.lt.15) then
		c=c*0.88
		d=d*0.88
	 endif
       xlo=xp*a
       ylo=yp*b
       xhi=xp*c
       yhi=yp*d
c	 a=float(iy1v)/7500.
c	 b=float(iy2v)/7500.
c	 c=float(iy1c)/7500.
c	 d=float(iy2c)/7500.
c	 y1v=yp*a
c	 y2v=yp*b
c	 y1c=yp*c
c	 y2c=yp*d
	endif
	if (csize.lt.2.) csize=3.0
c
	ALLOCATE(datcop(k1+1000))
	if(iptype.ge.15.and.iptype.le.25) then
	   istr1=istrec+ioffset
	   ALLOCATE(int4cop(ndimd+ndimc))
	   if(ntext.eq.0) then
		read(11,rec=istr1) (INT2COP(i),i=1,k4),(INT4COP(i),i=1,k5),
     &	  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3)
	   else
		read(11,rec=istr1) (INT2COP(i),i=1,k4),(INT4COP(i),i=1,k5),
     &     (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3),(NEWTEXT(i),i=1,ntext)
	   endif
	   goto 10		!decode datcop etc
	endif
c
	if(iptype.lt.15) then
	   n=ndimd
	   if(n.gt.10) n=10
	   do i=1,n
		ndat(i)=ndat1(i)
		ijoin(i)=ijoin1(i)
		isym(i)=isym1(i)
		syms(i)=syms1(i)
	   enddo
	   n=ndimc
	   if(n.gt.10) n=10
	   do i=1,n
		ncal(i)=ncal1(i)
		iline(i)=iline1(i)
	   enddo
	   DEALLOCATE(syms1,ndat1,isym1,ncal1,ijoin1,iline1)
	endif
c Check value of mono, interp in case data came from an old queue in which
c these were not present -now use value of MONO defined in INAUT to set
c default colours for old queues if wanted (read as mono1 to avoid overwriting
c value already set if this file did not have colour)
	if(jcol.eq.1002) then		!colours were queued
	   if(screen.or.(.not.screen.and.colplotter)) then
		mono=mono1			!use queued value of mono
	   endif
	else if(jcol.ne.1002) then	!colours NOT queued -mono as set in INAUT
	   interp=.false.
	   call SETCLS(mono,icol,.false.,.true.,0)	!set plotter colours
	endif
c	if(debug()) print 800,y0,yinf,k1,k2,k3,k4,titlex
c800	format(' y0,yinf,k1,k2,k3,k4,titlex',2g13.6,4i8,/,1x,a40)
c
c Read back compressed data array from unit=11
	nbyte1=4*k1
	nbyte2=80*k2
	nbyte3=10*k3
	nbyte4=2*k4		!no of bytes in INT2COP
	if(iptype.eq.15) then
	   nbyte4=nbyte4 + 4*(ncurvd+ncurvc)
	endif
	nrec1=1 + (nbyte1-1)/1024	!for DATCOP
c First read DATCOP
	n=1
	do 380 j=1,nrec1
	  m=n+255
	  krn=krn+1
	  read(11,rec=krn) (DATCOP(i),i=n,m)
	  n=n+256
380	continue
c
	if(ntext.eq.0) then
	   if(nbyte3+nbyte4.le.1024) then
		krn=krn+1
		 read(11,rec=krn) (CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else
		krn=krn+1
		read(11,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   endif
	else if(ntext.gt.0) then
	   if(nbyte2+nbyte3+nbyte4.le.1024) then
		krn=krn+1
		read(11,rec=krn) (NEWTEXT(i),i=1,ntext),
     &		(CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else if(nbyte2+nbyte3.le.1024) then
		krn=krn+1
		read(11,rec=krn)(NEWTEXT(i),i=1,ntext),(CHARCOP(i),i=1,k3)
		krn=krn+1
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   else if(nbyte2.le.1024.and.(nbyte3+nbyte4.le.1024))then
		krn=krn+1
		read(11,rec=krn)(NEWTEXT(i),i=1,ntext)
		krn=krn+1
		read(11,rec=krn) (CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else if(nbyte2.le.1024)then
		krn=krn+1
		read(11,rec=krn) (NEWTEXT(i),i=1,ntext)
		krn=krn+1
		read(11,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   else
		krn=krn+1
		read(11,rec=krn) (NEWTEXT(i),i=1,12)
		krn=krn+1
		read(11,rec=krn) (NEWTEXT(i),i=13,20)
		krn=krn+1
		read(11,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1
		read(11,rec=krn) (INT2COP(i),i=1,k4)
	   endif	!end of ntext>0
	endif
c
c	if(debug()) pause '1'
c
c DECODE SECTION
10	continue

c All compressed data arrays read in. Now decode them.
c Decode INT2COP first, because for VPLOT5 onwards ndat() etc is here
c NB ndat() could be bigger than 32k, so kept as integer*4
	k=0
      if(iver.eq.1100) goto 101		!index for int2cop
	do 602 i=1,30	!keep 1st 30 lth always
	   k=k+1
	   lth(i)=INT2COP(k)
602	continue
	if(ntext.gt.0) then
	   do 612 i=1,ntext
	   k=k+1
	   lth(i+30)=INT2COP(k)
612	   continue
	endif
	if(narrow.gt.0) then
	   do 622 i=1,narrow
	   k=k+1
	   lth(i+50)=INT2COP(k)
622	   continue
	endif
	if(nline.gt.0) then
	   do 632 i=1,nline
	   k=k+1
	   lth(i+60)=INT2COP(k)
632	   continue
	endif
101	if(nhline.gt.0) then
	   do i=1,nhline
		k=k+1
		ilhtype(i)=INT2COP(k)
		k=k+1
		ihlinrel(i)=INT2COP(k)	!ihlinrel actually int*1 !
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
		k=k+1
		ilvtype(i)=INT2COP(k)
		k=k+1
		ivlinrel(i)=INT2COP(k)	!ivlinrel actually int*1 !
	   enddo
	endif
c
	do 64 i=1,10
         if(iver.ne.1100) then
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
         if(iver.ne.1100) then
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
       if(iver.ne.1100) then
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
       if(iver.ne.1100) then
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
	if(jcol.eq.1002.or.iptype.ge.15) then
	  do i=1,30	!keep 1st 30 ICOL always
	   k=k+1
	   icol1(i)=INT2COP(k)
	  enddo
	  do i=31,100
	   icol1(i)=-1	!so default colours set below, if not defined in queue
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
	  if(nhline.gt.0) then
	    do i=1,nhline
	      k=k+1
	      icol1(i+71)=INT2COP(k)
	    enddo
	  endif
	  if(nvline.gt.0) then
	    do i=1,nvline
	      k=k+1
	      icol1(i+81)=INT2COP(k)
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
c And add icurvw() for VPLOT5
	if(iptype.ge.15) then
	   do i=1,ncurvd
		k=k+1
		isym(i)=int4(INT2COP(k))
	   enddo
	   do i=1,ncurvd
		k=k+1
		ijoin(i)=int4(INT2COP(k))
	   enddo
	   do i=1,ncurvd
		k=k+1
	      icurvw(i)=int4(INT2COP(k))
	   enddo
	   do i=1,ncurvc
		k=k+1
		iline(i)=int4(INT2COP(k))
	   enddo
c Now int4cop() when iptype.ge.15
	   k=0		!index for int4cop
	   do j=1,ncurvd
		k=k+1
		ndat(j)=INT4COP(k)
	   enddo
	   do j=1,ncurvc
	      k=k+1
	      ncal(j)=INT4COP(k)
	   enddo
	endif
c End of int2cop and int4cop
c If any colours not sey above by queue, then set to defaults (screen)
c now, so that if text, lines etc added in VPLOT they will have a default colour
	call SETCLS(mono,icol,.false.,.false.,1)
c
c Convert back from int*2
c
c now datcop()
	k=0		!index for datcop
	if(ncurvd.le.0) goto 365
	if(iptype.eq.1.or.iptype.ge.14) then
	 do j=1,ncurvd
	   n=ndat(j)
c	   ifstcol(j)=j
	   do i=1,n
	     k=k+1
	     Xval(i,j)=datcop(k)
	   enddo
	   do i=1,n
	     k=k+1
	     Yval(i,j)=datcop(k)
	   enddo
	   if(iptype.ge.15) then
	      k=k+1
	      syms(j)=datcop(k)
		if(icurvw(j).ge.0) then
		   do i=1,n
			k=k+1
			weight(i,j)=datcop(k)
		   enddo
		endif
	   else
		if(isdev.ge.0) then
		   do i=1,n
			k=k+1
			weight(i,j)=datcop(k)
		   enddo
		endif
	   endif
	 enddo	!end of j=1,ncurvd
	else if(iptype.eq.11.or.iptype.eq.12) then
c	 bigplot=.false.
	 do 76 j=1,ncurvd
c	 ndatsav(j)=ndat(j)	!copy so ndat etc can be rearranged below
c	 ijoinsav(j)=ijoin(j)
c	 isymsav(j)=isym(j)
c	 if(ndat(j).gt.2048) bigplot=.true.
76	 continue
c	 jc=1
	 do 137 j=1,ncurvd
	 if(debug()) then
		print 805,ncurvd,j,ndat(j)
805	 	format(' ncurvd,j,ndat(j) = ',3i8)
		pause
	 endif
	 n=ndat(j)
	   do 1371 i=1,n
	   k=k+1
1371	   Xval(i,j)=datcop(k)
	   do 1372 i=1,n
	   k=k+1
1372	   Yval(i,j)=datcop(k)
c	 endif
	 if(iptype.eq.12.and.isdev.ge.0) then	!VPLOT may have SD
	   do 20 i=1,n
		k=k+1
20	   weight(i,j)=datcop(k)
	 endif
137	 continue
	endif
365	continue
	if(ncurvc.le.0) goto 366
	if(iptype.eq.1.or.iptype.ge.14) then
	 do 39 j=1,ncurvc
	 n=ncal(j)
	   do 391 i=1,n
	   k=k+1
391	   xcal(i,j)=datcop(k)
	   do 392 i=1,n
	   k=k+1
392	   ycal(i,j)=datcop(k)
39	 continue
	 if(iptype.ge.15) then
	   do i=1,kmax
		k=k+1
 		theta(i)=datcop(k)
	   enddo
	 endif
	else if(iptype.eq.11.or.iptype.eq.12) then
	 do 139 j=1,ncurvc
	 n=ncal(j)
	   do 1391 i=1,n
	   k=k+1
1391	   xcal(i,j)=datcop(k)
	   do 1392 i=1,n
	   k=k+1
1392	   ycal(i,j)=datcop(k)
139	 continue
	endif
366	continue
c
c Now add onto DATCOP the real*4 data for posh plots- keep incrementing k
c
	do 682 i=1,10
	   k=k+1
	   if (iver.eq.1100) then
            size(i)=DATCOP(k)
            k=k+1
            angle(i)=DATCOP(k)
         else
	      csfac(i)=DATCOP(k)
            size(i)=csfac(i)*csize
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
         if (iver.eq.1100) then
            size(i)=DATCOP(k)
            k=k+1
            angle(i)=DATCOP(k)
         else
	      csfac(i)=DATCOP(k)
            size(i)=csfac(i)*csize
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
	endif
	do 69 i1=1,numbx
	   i=i1+30
	   k=k+1
         if (iver.eq.1100) then
		 angle(i)=DATCOP(k)
             k=k+1
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
         if (iver.eq.1100) then
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
c now add new lines
	if(nhline.gt.0) then
	   do i=1,nhline
		k=k+1
		xhlb(i)=DATCOP(k)
		k=k+1
		xhle(i)=DATCOP(k)
		k=k+1
		yhline(i)=DATCOP(k)
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
		k=k+1
		xvline(i)=DATCOP(k)
		k=k+1
		yvlb(i)=DATCOP(k)
		k=k+1
		yvle(i)=DATCOP(k)
	   enddo
	endif
      if (iver.eq.1100) then
      do 60 i=1,30	!keep 1st 30 lth always
	   k=k+1
	   rlth(i)=DATCOP(k)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   rlth(i+30)=DATCOP(k)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   rlth(i+50)=DATCOP(k)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   rlth(i+60)=DATCOP(k)
63	   continue
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
c
c========int2cop was here
c For old queues must convert justification to new convention (see CONSTS.FOR)
	if(iptype.le.14.and.jcol.ne.1002) then
	   do i=1,100
		ival=ixjus1(i)
		if(ival.eq.2) ixjus1(i)=1
		if(ival.eq.1) ixjus1(i)=2
		ival=iyjus1(i)
		if(ival.eq.2) iyjus1(i)=1
	   enddo
	endif
	do 50 j=1,100
       if(iver.eq.1100) then
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
c Finally, for versions earlier than iptype=14 which queued only isdev,
c must now define icurvw() as needed by VPLOT5
	if(iptype.lt.15) then
	   do j=1,ncurvd
		icurvw(j)=isdev
	   enddo
	endif
c
	DEALLOCATE(datcop,int2cop,charcop)
	if(allocated(int4cop)) DEALLOCATE(int4cop)
	RETURN
	end

