	subroutine RDCJDAT(istrec,xval2,yval2,xcal2,ycal2,ndimd,ndimc,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,landscap,
     & ncurvd,ndat,isym,ijoin,ncurvc,ncal,iline,syms,ntx,nty,itx,ity,
     & ixlo,iask,itit,title1,csize,ifont,ilog,iscal,
     & doframe,titlex,titley,ilabel,ifitype,ncjump,t1c,t2c,nvjump,
     & t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,nplot,screen,
     & ndv1,ndc1,iscalfix,ioffset,newform)
c
c Subroutine for AUTPLOT to read CJUMP.DAT records (inc CALCNS2 at end)
c Modified (02/10/95 03:13pm) to cope with CJUMP4 also: e.g. now read header
c   with READ14 (as in CJFIT) now, to cope with different CJUMP
c   versions -this always returns ikeep1() as integer*4 (as in CJUMP4)
c   and make jmask() allocatable (as in READJUMP for SCAN).  Similarly
c  CALCNS2 replaced with version from current CJFSUBS.
c
c Jumps will be plotted by VPLOTQ so must set all common/TPOS so drawing
c is at default positions, since no positions specified in CJUMP.DAT
c (note that ixlo etc are defined by PLOTAUT)
c ***colour not yet fixed (apart from adding ICOL, mono to common/tpos/)
c
c For disc read
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 iver1
	ALLOCATABLE::jmask
	integer*1 jmask(:)
	integer*2 kstep(5)
	integer*4 ikeep1(4,2)
	integer*2 ivhclamp,ipatch
	logical keepall
      character cdate*11,ctime*11,cplot*11
	logical sampv,control,vjump
c=	integer*2 iADC(2048)
	ALLOCATABLE::iADC
	integer*2 iADC(:)
c for VPLOT
	real*4 XVAL2(ndv1,ndimd),YVAL2(ndv1,ndimd)	!for VPLOT
	real*4 XCAL2(ndc1,ndimc),YCAL2(ndc1,ndimc)
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	logical ivplot,landscap
	real syms(10)
	dimension ndat(10),isym(10),ncal(10),ijoin(10),iline(10)
c	character*1 ans,UC
	character*40 titlex,titley
	character*64 title1	!plot title
	character*79 title	!sweep title
c Misc
	LOGICAL doframe,slock,pon
	logical discprt
	logical caplock,debug,screen,mono,scalfix,newform
c Arrays to hold all details of posh plots
	real*4 rlth(100) 		!for line thickness
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IJUS(100)
	integer idraw(100),icol(100)
	real*4 angle(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*80 newtext(20)		!extra text
	integer ifnt(30)
	real csfac(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
      integer left, center, right
      data left, center, right /-1, 0, 1 /     !new definition
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,csfac,rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	COMMON/cols/icol,mono
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
	ALLOCATE(iADC(ndv1))
c Can plot only the raw current records from CJUMP (no subtracted records
c or IV plots, which must be calc in CJFIT)
	ivplot=.false.
	landscap=.true.
	ifitype=0
c Initialise all positions etc, as in VPLOTR
	linetype=0		!continuous line
	narrow=0	!number of arrows added
	nline=0	!number of lines added
	ntext=0	!number of extra text strings
	cbig=2.5
	thbig=1.0		!line thickness factor for whole page
	csize=cbig
	thick=thbig
	thfacsml=0.6		!line thickness factor for small plots
	s=0.8*csize		!default symbol size
	if(syms(1).le.0.) then
	   do 9 i=1,ndimd
9	   syms(i)=s
	endif
c
	do 8 i=1,30
	ifnt(i)=ifont
	csfac(i)=1.0
8	continue
	do 81 i=1,100
	  angle(i)=0.
	  idraw(i)=-2		!until defined
	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
81	continue
	idraw(28)=1		!c-jump logo
	idraw(29)=1		!v-jump logo
	do 83 i=1,10
83	iltype(i)=0		!continuous line for extra lines
c
	csfac(6)=0.7		!csize for param values=0.7*csize
	csfac(10)=1.3		!for title
	ijus(8)=center		!for x-axis label
	angle(9)=90.		!for y-axis label
	ijus(9)=center
	if(ifitype.eq.0) idraw(6)=0	!no param values
c
c Default line thickness
c Line thickness for whole page plots in rlth(i) as follows ( actual
c thickness =THICK*rlth(i) were THICK=THBIG=1.0 for whole page)
c 1-10= data sets (#1-10) (symbol and joining-line if any);
c 11-20=calc curves (#1-10);
c 21=axes; 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols;
c 28=jump bar (logo) for C-jumps
c 29=jump bar (logo) for V-jumps
c 30 spare
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c 71-100 =spare for future
C [OLD- 31-35=extra text (#1-5); 36-40=arrows (#1-5)]
	rlt=0.4		!default thickness, unless reset
c	if(autplt.and.draft) ilt=0	!'draft' set below
	do i=1,100
	   rlth(i)=rlt
	enddo
 	rlth(22)=0.3	!frame
	rlth(26)=0.2	!param value text
	rlth(27)=0.2	!symbols
	rlth(28)=0.2	!C-jump logo
	rlth(29)=0.2	!V-jump logo
c
c Read data from CJUMP.DAT
	irec=istrec
c Read the specified record  'header' -now fixed for CJUMP3
c Do this with READ14 (as in CJFIT) now, to cope with different CJUMP
c versions -this always returns ikeep1() as integer*4 (as in CJUMP4)
c and make jmask() allocatable (as in READJUMP for SCAN).
c  (READ_14 is AUTPLOT version that does not call GETPATCH)
	imode=0		!so READ14 reads header (and corrects it if nec)
	call READ_14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,imode)
	   ipatch=int4(jpatch)	!integer*4 for SCAN
	   ffilt=ffilt/1000.		!convert to kHz
c Now have value for nmax, so arrays can be allocated (iver1=-1004)
c This 'list details' loop needs only jmask (for calcns2)
	   nsamp0=nsamp		!# of points if all points kept
	   if(iver1.le.-1003) then
		if(allocated(jmask)) DEALLOCATE(jmask)
		ALLOCATE(jmask(nsamp0))
		call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &		 keepall,nsamp0)
	      nsamp0=nsamp		!orig # of points samples
	      nsamp=nsamp1		!# of points kept on disc
	   else
		keepall=.true.
	   endif
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
c
c Read the current data (not voltage, if it was sampled) into Yval2(i,1)
c
	acal=6553.6		!adc units/volt
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
	if(newform) then
	   jc=1
	   irec=irec+ioffset		!points to start of data
	   if(iav.eq.0) then
		read(14,rec=irec) (iADC(i),i=1,nsamp)
      	irec=nsamp*2+irec
c	Float the iADC values here
		do i=1,nsamp
c		   avcur(i)=float(iADC(i))
		   Yval2(i,jc)=calfac*float(int4(iADC(i)))
	      enddo
	   else
		read(14,rec=irec) (Yval2(i,jc),i=1,nsamp)
      	irec=irec+4*nsamp
		do i=1,nsamp
	         Yval2(i,jc)=calfac*Yval2(i,jc)
	      enddo
	   endif
	else
	   if(iav.eq.0) then
		nrec=1 + (nsamp-1)/256		!for iADC(i)  =int*2 so 1 rec=256 values
		lastn=nsamp - (nrec-1)*256	!number to read from last record
	   else
		nrec=1 + (nsamp-1)/128		!for real*4 1 rec=128 values
		lastn=nsamp - (nrec-1)*128	!number to read from last record
	   endif
	   jc=1
	   if(iav.eq.0) then
		n=1
		do j=1,nrec
		   m=n+255
		   if(j.eq.nrec) m=n+lastn-1
		   irec=irec+1
		   read(14,rec=irec) (iADC(i),i=n,m)
		   n=n+256
		enddo
c	Float the iADC values here
		do i=1,nsamp
c		   avcur(i)=float(iADC(i))
		   Yval2(i,jc)=calfac*float(iADC(i))
		enddo
	   else
		n=1
		do j=1,nrec
		   m=n+127
		   if(j.eq.nrec) m=n+lastn-1
		   irec=irec+1
		   read(14,rec=irec) (Yval2(i,jc),i=n,m)
		   n=n+128
		enddo
		do i=1,nsamp
	         Yval2(i,jc)=calfac*Yval2(i,jc)
		enddo
	   endif
	endif
c
c Define xval2
c	xmin1=0.0					!now modif for CJUMP3 data
c	xmax1=float(itsamp)/1000.		!msec
c	xoff1=0.
c	dx=(xmax1-xmin1)/float(nsamp)
c	do 130 i=1,nsamp
c130	xval2(i,1)=xmin1+float(i-1)*dx
	  xmin1=0.0
	  xmax1=float(itsamp)/1000.		!msec
	  xoff1=0.
	  dx=1.e3/float(irate)
	  if(keepall) then
	     do 130 i=1,nsamp
130	     xval2(i,1)=xmin1+float(i-1)*dx		!1st point at t=0
	  else
c	     dx=(xmax-xmin)/float(nsamp0)	!already def via irate (=max rate)
	     i1=0
	     do 152 i=1,nsamp0
	      if(jmask(i).eq.0) goto 152	!skipped point
	      t=float(i-1)*dx			!=msec from 0 to itsamp-1
	      i1=i1+1
	      xval2(i1,1)=t
152	     continue
	   endif
	if(allocated(jmask)) DEALLOCATE(jmask)
c Define xmin ...
	ymin1=1.e37
	ymax1=-1.e37
	do i=1,nsamp
	   if(yval2(i,1).lt.ymin1) then
		ymin1=yval2(i,1)
		imin=i
	   endif
	   if(yval2(i,1).gt.ymax1) then
		ymax1=yval2(i,1)
		imax=i
	   endif
	enddo
c
	if(iscalfix.eq.1) then !do not alter xmin,.. defined in PLOTOPT (but define tic)
	   call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
	   ymin=ymin1
	   ymax=ymax1
	   call FIXAX(ymin,ymax,x1,x2,ytic,0)
	else if(iscalfix.eq.2) then
	   call FIXAX(ymin,ymax,x1,x2,ytic,0)
	   xmin=xmin1
	   xmax=xmax1
	   call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
	else if(iscalfix.eq.3) then
	   call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
	   call FIXAX(ymin,ymax,x1,x2,ytic,0)
	else if(iscalfix.eq.0) then
	   call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)
	   call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
	   xmax=xmax1		!reset in case FIXAX increases it
	endif
	xcross=xmin
	ycross=ymin
c Define logos
	dA=1.e6/float(irate)	!# of microsec between ADC samples
	if(nvjump.gt.0) then
	  t1v(1)=(float(nAv-1)*dA + dnAv)/1000. !msec from trigger to start of 1st pulse
	  do 211 i=1,nvjump
	    t2v(i)=t1v(i)+float(ilenv(i))*1.e-3	!msec from trigger to end of pulse
c	    if(debug()) print 213,i,t1v(i),t2v(i)
c213		format(' i,t1v(i),t2v(i) = ',i4,2g11.4)
	    if(i.eq.nvjump) goto 211
	    t1v(i+1)=t2v(i) + float(igapv(i))*1.e-3 !time for start of next pulse
211	  continue
	endif
	if(ncjump.gt.0) then
	  t1c(1)=(float(nAc-1)*dA + dnAc)/1000. !msec from trigger to start of 1st pulse
	  do 212 i=1,ncjump
	    t2c(i)=t1c(i)+float(ilenc(i))*1.e-3	!msec from trigger to end of pulse
c	    if(debug()) print 214,i,t1c(i),t2c(i)
c214		format(' i,t1c(i),t2c(i) = ',i4,2g11.4)
	    if(i.eq.ncjump) goto 212
	    t1c(i+1)=t2c(i) + float(igapc(i))*1.e-3 !time for start of next pulse
212	  continue
	endif
	iy1c=6600   !initial positions for jump logos -must give when iscal=0
	iy2c=6800
	iy1v=6550
	iy2v=6350
c Define titles
	titlex='time (ms)'
	titley='current (pA)'
	call INTCONV(nplot,cplot)
	n=NBLANK(cplot)
	title1=cdate//' Sweep '//cplot(1:n)//': '//ctime(1:8)
	n=NBLANK1(title1)		!terminate with ASCII 0
	itit=1
c Define ncurvd etc
	ncurvd=1
c	icurvd(1)=1
	ndat(1)=nsamp
	isym(1)=0	!points
	ijoin(1)=-1	!not joined
	ncurvc=0
	xoff1=0.0
	ilog=0
	ifont=4
	csize=2.5
	iscal=1	!scale internally
c	doframe=.true.
	doframe=.false.
	ilabel=1    !so axis labels drawn
	ntx=5
	nty=5
	itx=1
	ity=1
	xlo=-1.
	iask=3
c	isdev=0
c Print something about current plot
	if(.not.screen) print 102,nplot,title
c      if(pon()) write(7,102) nplot,title
      if(discprt) write(8,102) nplot,title
102	format(' Sweep number ',i4,/,1x,a79,/)
	DEALLOCATE(iADC)
	RETURN
	end

	subroutine ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,iconv)
c  If iconv=0 then converts ilenc etc to ilen2c etc
c  If iconv=1 then converts ilen2c etc to ilenc etc
c Scaling of ilenc etc for int*2 versions on disc
c  (1) If iDd=1000 ( 1kHz  DAC rate) then keep in msec (up to 32.7 sec)
c  (2) If iDd=10000 ( 100 Hz  DAC rate) then keep in 10ms units (up to 327 sec)
c  (3) If iDd=100 ( 10 kHz  DAC rate) then keep in 0.1ms units (up to 3.27 sec)
c i.e. keep ilenc,igapc etc in number of DAC tics
c length in microsec=iDd*ilenc(i)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	logical discprt,pon,slock,prt
	common/dp/discprt
c
	pon()=slock()
c
	prt=pon()
	if(iDd.ne.100.and.iDd.ne.1000.and.iDd.ne.10000) then
	   iDd=1000
	   print 1
	   if(prt) write(7,1)
	   if(discprt) write(8,1)
1	   format(' DAC rate assumed to be 1 kHz')	!for early files
	endif
c
	if(iconv.eq.0) then
	   do 10 i=1,10
	   ilen2c(i)=int2(ilenc(i)/iDd)     !integer*2 DAC ticks
	   igap2c(i)=int2(igapc(i)/iDd)
	   ilen2v(i)=int2(ilenv(i)/iDd)
	   igap2v(i)=int2(igapv(i)/iDd)
10	   continue
	else if(iconv.eq.1) then
	   do 20 i=1,10
	   ilenc(i)=iDd*int4(ilen2c(i))     !integer*4 microsec
	   igapc(i)=iDd*int4(igap2c(i))
	   ilenv(i)=iDd*int4(ilen2v(i))
	   igapv(i)=iDd*int4(igap2v(i))
20	   continue
	endif
	RETURN
	end

	subroutine CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &	keepall,ndim)
c=	integer*2 ikeep1(4,2),kstep(5)
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	integer*1 jmask(ndim)
	logical keepall
c CALCNS2 is same as CALCNS1 but input is ikeep1 (ie values for a single
c sweep, so parameter m not needed)
c
c Much simplified version of MODKEEP that returns only (1) keepall, (2) nsamp1
c =number of points kept on disc and (2) JMASK() -array to speed location of
c points to be kept: jmask=1 to keep, =0 to omit (for multiple sweeps jmask
c may need to be reset each cycle (e.g. for double pulse expts) -can do by
c calling this subroutine
	keepall=nkeep.eq.1.and.ikeep1(1,1).eq.1.and.
     &  ikeep1(1,2).eq.nsamp
	if(keepall) then
	   do 1 i=1,nsamp
1	   jmask(i)=1
	   nsamp1=nsamp
	   RETURN
	endif
	nsamp1=0	!# of points actually kept
	do 2 i=1,nsamp
2	jmask(i)=0
c
	do 28 i=1,nkeep
c	  nsamp1=nsamp1+ikeep1(i,2)-ikeep1(i,1)+1
	   n1=ikeep1(i,1)		      !1st point of kept bit #i
	   n2=ikeep1(i,2)		      !last point of kept bit #i
	   do 11 j=n1,n2
		nsamp1=nsamp1+1
		jmask(j)=1			!keep all points in 'kept section'
11	   continue
c Now the bits outside the 'kept sections'
	  if(i.eq.1.and.ikeep1(1,1).gt.1) then
c	add these extra points to nsamp1
	    n2=ikeep1(1,1)-1
	    n=int4(kstep(1))
	    do 5 k=1,n2,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
5	    continue
	  endif
	  if(i.gt.1) then
	   	n1=ikeep1(i-1,2)+kstep(i)
	   	n2=ikeep1(i,1)-1    !1st point of next kept bit -1
	      n=int4(kstep(i))
	      do 6 k=n1,n2,n
	      nsamp1=nsamp1+1
	      jmask(k)=1
6		continue
	  endif
28	continue	!end of i=1,nkeep loop
c
c points kept after the last bit
	  if(ikeep1(nkeep,2).lt.nsamp) then
c	add these extra points to nsamp1
	    n1=ikeep1(nkeep,2)+int4(kstep(nkeep+1))
	    n=int4(kstep(nkeep+1))
	    do 7 k=n1,nsamp,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
7	    continue
	  endif
c
	RETURN
	end

