	subroutine READQ(irec,xval,yval,ndimd,ndimc,ncurvd,
     & ndat,isym,ijoin,syms,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,ixlo,ixhi,iylo,iyhi,
     & itit,title,csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & interp,itrace,ntrace,ytsep,ndv1,ndc1,
     & kwi,kwj,icurvw,kmax)
c
c This is to read data back from plot queue (plotq.dat) for (re-)fitting
c in CJFIT (also version for CVFIT?).  Based on RDVPLQ (used in autplot)
c Purpose is to read back data for (re-)fitting so xcal, ycal etc not
c returned -just allocated locally here for reading the data
c
c
c Notes for RDVPLQ
c Modif 04/04/95 11:18am for records written by VLPQ5 (in VPLOT5)
c  Since all plots now done with vplot5 etc, ifstcol, ndatsav removed.
c 10/26/95 10:57am DATCOP dimension increased to 200000 in RDVPLQ and VPLQ5
c 05/22/95 07:23am DATCOP dimension increased to 100000 in RDVPLQ and VPLQ5
c Modif 09/11/94 11:46am for records written by VLPQ4 (in VPLOT4)
c Modif 02/15/93 01:06pm ICOL(), mono, interp added as arguments
c Subroutine for AUTPLOT to read and decode plot queue files written by
c VPLQ1 in VPLOT, or by VPLQR1 in VPLOTR
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
c	dimension XVAL(2048,10),YVAL(2048,10)
c	dimension XCAL(2048,10),YCAL(2048,10)
c=	real weight(100,10)
	real weight(kwi,kwj)
	integer icurvw(ndimd)
	real theta(kmax),syms(ndimd)
	integer ndat(ndimd),isym(ndimd),ijoin(ndimd)
	ALLOCATABLE::xcal,ycal,ncal,iline
	real*4 XCAL(:,:),YCAL(:,:)
	integer ncal(:),iline(:)
c arrays needed when more than 2048 points/plot from VPLOTR
c=	integer ifstcol(10),ndatsav(10),ijoinsav(10),isymsav(10)
c	character*1 ans,UC
	character*40 titlex,titley
	character*44 title	!plot title (local)
	LOGICAL doframe,mono,mono1,interp,slock,pon
	logical discprt
	logical caplock,debug
c extra args to hold all details of posh plots
	integer*2 ilth(100),ilt 		!for line thickness
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IXJUS(100),IYJUS(100)
	integer iangle(100),idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)
	real csfac(30)
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
	integer*2 IXJUS1(100),IYJUS1(100)
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
	COMMON/TPOS/rx,ry,rxbox,rybox,ixjus,iyjus,iangle,idraw,
     & ifnt,csfac,ilth,thbig,narrow,xb,yb,xe,ye,
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
c Now allocate datcop() below, after k1 read
	ALLOCATE(int2cop(1000),charcop(100))
c
	if(iptype.lt.15) then
	   ALLOCATE(syms1(10),ndat1(10),isym1(10),ncal1(10),
     &	ijoin1(10),iline1(10))
	endif
c Local allocation for reading calc curves (not returned)
	ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	ALLOCATE(ncal(ndimc),iline(ndimc))
c
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
	do 6 i=1,100
6	ilth(i)=ilt
c	ilth(21)=25	!axes (and axis labels at present!)
 	ilth(22)=25	!frame
	ilth(26)=10	!param value text
	ilth(27)=15	!symbols
	ilth(28)=12	!SD bars
c End of initialisation
c
c Read back first data record from unit=11
c iptype=12 for data queued by VPLOT2 (iptype=1 for VPLOT1, iptype=11
c for VPLOTr, both now obsolete)
	krn=irec
	if(iptype.eq.1) then
	   read(11,rec=krn) iptype,itit,title,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     &iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,isym1,
     & ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,
     & sval,theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4
c
	else if(iptype.eq.11.or.iptype.eq.12) then
	   read(11,rec=krn) iptype,itit,title,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,
     & isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & jcol,mono1,interp
c
	else if(iptype.eq.14) then
	  read(11,rec=krn) iptype,ndv1,ndimd,ndc1,ndimc,itit,title,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
     & csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     & ndat1,isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & jcol,mono1,interp,nhline,nvline,ntrace,ytsep
c
	else if(iptype.eq.15) then
	   read(11,rec=irec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     &  itit,title,
     &  xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
     &  csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     &  ncurvc,inumx,inumy,sval,
     &  ifitype,ncomp,numbx,numby,ifnt,ntext,k1,k2,k3,k4,k5,
     &  narrow,nline,iltype,y0,yinf,ntx,nty,itx,ity,
     &  ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     &  mono1,interp,nhline,nvline,ntrace,ytsep,ioffset
c
	endif
c
c Can now allocate datcop()
	ALLOCATE(datcop(k1))
c
	if(iptype.ge.15.and.iptype.le.25) then
	   istr1=irec+ioffset
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
	   mono=mono1			!use queued value of mono
	else if(jcol.ne.1002) then	!colours NOT queued -mono as set in INAUT
	   interp=.false.
	   call SETCOLS(mono,icol,.false.,.true.)	!set plotter colours
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
	k=0		!index for int2cop
	do 60 i=1,30	!keep 1st 30 ILTH always
	   k=k+1
	   ilth(i)=INT2COP(k)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   ilth(i+30)=INT2COP(k)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   ilth(i+50)=INT2COP(k)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   ilth(i+60)=INT2COP(k)
63	   continue
	endif
	if(nhline.gt.0) then
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
	   k=k+1
	   ixjus1(i)=INT2COP(k)
	   k=k+1
	   iyjus1(i)=INT2COP(k)
	   k=k+1
	   iangle1(i)=INT2COP(k)
	   k=k+1
	   idraw1(i)=INT2COP(k)
64	continue
	if(ntext.gt.0) then
	   do 65 i=1,ntext
	   k=k+1
	   ixjus1(i+10)=INT2COP(k)
	   k=k+1
	   iyjus1(i+10)=INT2COP(k)
	   k=k+1
	   iangle1(i+10)=INT2COP(k)
	   k=k+1
	   idraw1(i+10)=INT2COP(k)
65	   continue
	endif
	do 66 i=1,numbx
	   k=k+1
	   ixjus1(i+30)=INT2COP(k)
	   k=k+1
	   iyjus1(i+30)=INT2COP(k)
	   k=k+1
	   iangle1(i+30)=INT2COP(k)
	   k=k+1
	   idraw1(i+30)=INT2COP(k)
66	continue
	do 67 i=1,numby
	   k=k+1
	   ixjus1(i+55)=INT2COP(k)
	   k=k+1
	   iyjus1(i+55)=INT2COP(k)
	   k=k+1
	   iangle1(i+55)=INT2COP(k)
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
c If any colours not set above by queue, then set to defaults (screen)
c now, so that if text, lines etc added in VPLOT they will have a default colour
	call SETCOL(mono,icol,.false.,.false.,1)
c
c Convert back from int*2
	do 50 j=1,100
	 IXJUS(j)=int4(IXJUS1(j))
	 IYJUS(j)=int4(IYJUS1(j))
	 iangle(j)=int4(iangle1(j))
	 idraw(j)=int4(idraw1(j))
50	continue
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
c	 if(bigplot) then
c	   ifstcol(j)=jc
cc	   NB VPLOTR and VPLOTQ get ndat etc from ndat(jc) not ndat(j)!
c	   ndat(jc)=ndatsav(j)
c	   ijoin(jc)=ijoinsav(j)
c	   isym(jc)=isymsav(j)
c	   do 1373 i=1,n
c	   k=k+1
c1373	   Xval(i,jc)=datcop(k)
c	   do 1374 i=1,n
c	   k=k+1
c1374	   Yval(i,jc)=datcop(k)
c	   ngr=1+(n-1)/2048	!# of cols for current data
c	   jc=jc+ngr		!col of Yval(i,j) where next data put
c	 else
c	   ifstcol(j)=j
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
	   csfac(i)=DATCOP(k)
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
	   csfac(i)=DATCOP(k)
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
		ival=ixjus(i)
		if(ival.eq.2) ixjus(i)=1
		if(ival.eq.1) ixjus(i)=2
		ival=iyjus(i)
		if(ival.eq.2) iyjus(i)=1
	   enddo
	endif
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
	DEALLOCATE(xcal,ycal,ncal,iline)
	RETURN
	end

