	subroutine HJCDISP(iplot,nint,burst,chsvec,rootA,rootF,theta,
     & tres,tcrit,QT,nlig,ylod,yhid,nranged,readini,kfit,k,
     & tint,ampl,iprops,nd1,nd2,nintt,iscan,idsav,idiskq,
     & qfile,tresd,nofit,curvonly,ligname,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,badend(10),
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,kAx,km)
c=======================================================================
c To define arrays for display of final fit in HJCFIT
c (at present, exclusion of 'dubious amplitudes', and specification
c of amplitudes for open periods, are not included. Also no Yval defined for
c iplot=4 since correlation calc not done here)
c
c Modified 08/03/01 04:16pm by adding nofit,nodata,curvonly to parameters to allow
c display of calc curves only: 01/14/02 05:01pm nodata removed from args
c and put in common (for hjclik)
c
c 08/21/95 06:39am Change to VPLOT5 and add new graphs
c   - Open time pdf conditional on length of adjacent gap (continuous version
c	calculated in MOPADJc() which is based on old HJCCOND with imode=3)
c	-could also add continuous version of pdf of open times (based on old
c	 HJCCOND with imode=4) -NB both of these seemed fo have an error!
c
c 11/16/94 04:33pm Modified to use ampl(), iprops etc, and VPLOT4 (with
c allocatable arrays). NB MOPADJ has xcal1,ycal1 in call.
c  Also updated to VHIST3 (with change to POPADJ) -need allocatable array
c version (VHIST4) soon.
c
c Converted for Lahey V5.n 07/09/93 02:57pm (version known as HJCDISP1 for
c old compiler).  Also VHIST2 now used.
c
c iplot=
c (1) Plot histogram of apparent OPEN periods, with fit
c (2) Plot histogram of apparent SHUT times, with fit
c (3) Open time pdf conditional on adjacent gap, with fit
c (4) Mean open vs adjacent gap plot, with fit
c 08/18/95 12:25pm add 2 more options (as in old HJCDIST) -ideal
c relationships (not directly comparable with histogram)
c (5) Open time pdf conditional on length of adjacent gap
c (6) Mean open time conditional on length of adjacent gap
c These are not now options for separate plots (so iplot=1,..4 still),
c but are options for 'ideal' curves to be superimposed on (3), (4)
c respectively (also need option to plot alone?)
c
c   Modified 10/10/92 09:00pm for multiple data sets.  To do plots for a
c specified data set (#iset, say), call HJCLIK to do the calculations
c for this set only by setting ONESET=true (previously relied on last call
c to HJCLIK to provide calcs). Note that some of the calculations needed
c for plots are done in HJCLIK only when idebug=8, as used for print of
c the final pdfs.  Modified now so that these calcs are done also if
c ONESET=true (but pdfs not printed again)
c   Modified 02/27/92 03:58pm so that 'mean open for openings adjacent to
c specified gaps' are now determined in same run through data as for the
c open and shut times.  The method differs from before in that (1) open
c periods rather than indiv openings are used (as assumed in HJC),
c (2) when burst=true any burst with bad opening or shutting is rejected
c entirely so this does not contribute to 'mean open adj to gap' either
c (3) now keep 3 different versions (a) openings vs preceding gap (sy0,ny0)
c (b) openings vs following gap (sy1,ny1) and (c) openings that are adj
c to a gap in spec range whether it is before OR after the opening (ie
c opening counted twice, once according to the range in which prev
c gap falls, and once according to range in which following gap falls)
c
c Declare allocated arrays
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	integer nint(10)
	ALLOCATABLE:: tval,nopen	!now alloc/dealloc as needed
	integer*2 nopen(:)
	real*4 tval(:)
	logical nofit,nodata
	logical curvonly
	common/ndata/nodata
c
	real*8 QT(km,km),QD(100,100)
c next 5 lines are now parameters
	character*20 ligname(10)
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
c
	real*4 tcrit(10)
	logical burst(10),chsvec(10),badend(10)
	real*4 ylo(20),yhi(20)		!up to 20 gap ranges
	real*4 ylod(20),yhid(20)	!for defaults
	real*4 den1(20)
	real*4 sy0(20),syy0(20),sy1(20),syy1(20),sy2(20),syy2(20)
	real*4 sx0(20),sxx0(20),sx1(20),sxx1(20),sx2(20),sxx2(20)
	integer ny0(20),ny1(20),ny2(20),nx0(20),nx1(20),nx2(20)
	real*4 FREQ(510),XAXIS(510),xsav(5,4)
	integer IX(100),JX(100),IL(100)		!for common/cpar/
	character*1 ans,ans1,UC
	character filnam*33
	logical debug,caplock,logt,deb,readini,oneset
c	logical exass
	logical fitted,open,good,bad,bad1,nores,scale,setmax,btest
c	logical first
	logical sbin,shist,sres,sexp
	logical corprev,cornext		!true only for iplot=3
	logical allocated,replot
	character title*74
	character*75 xtitle,ytitle,ztitle,title2	!output from LAXES
c for gplot3d
	character qfile*40
c for PHIo1,pdfopen etc (zero resolution calcs)
	real*8 Peq1(100),phio(1,100)
	real*4 tau1(100),area(100),am,sd
c==from scvd1
	character*40 titlex,titley
	real XVAL(0:511,1),YVAL(0:511,1)
	real*4 XCAL(2048,10),YCAL(2048,10)
c for histo data
	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
	integer ncal(10),icurvc(10),iline(10)
c
	real*8 theta(200)
	allocatable:: thetas
	real*4 thetas(:)		!for call to vplot5 (not used at present)
c	integer*2 videotyp
	character*64 title1
	logical landscap,autplt,draft,plotonly,ivplot,doframe,interp,mono
C Arrays with fixed dimension (see hjclik)
c next 2 lines in common/fitblk
	real*8 rootA(100),rootF(100),ampA(100),ampF(100)
c next 2 lines in common/fitblk
	real*8 eigen(100)
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
c
	real*8 g00(100),g10(100),g11(100)
	real*8 F0HJC,F1HJC		!functions
	real*8 time,tres,FTCT,tcrit2,ptc,ucol(100,1)
	real*8 tresd(10)
	real*4 amp(100),tau(100)
c for display when iplot=4 use VPLOT4
	ALLOCATABLE:: XVAL1,YVAL1,XCAL1,YCAL1,weight,icurvw
	real*4 XVAL1(:,:),YVAL1(:,:)
	real*4 XCAL1(:,:),YCAL1(:,:)
	real*4 weight(:,:)
	integer*4 icurvw(:)
c	dimension xcal1(2048,3),ycal1(2048,3)
c	dimension XVAL1(2048,3),YVAL1(2048,3)
c for data
	integer ndat1(5),icurvd1(5),ijoin1(5),isym(5)
	real*4 syms(5)
c for calc curves
	integer ncal1(2),icurvc1(2),iline1(2)
c For 3D display of dependency plot (gplot3D)
	allocatable::fdep,ZNEW,shutt,opent
	real*4 fdep(:,:),ZNEW(:,:),shutt(:),opent(:)
	allocatable::badval
	logical badval(:,:),posneg
c
	common/KBLK/kA,kB,kC,kD
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
	real*4 conc(10,10)
	common/CBLK/nset,conc,jsetlast	!for HJCLIK, checkqd,qset_hjc,hjcdisp
	common/cpar/ncdep,IX,JX,x
	common/LIG/nligsav,IL
	logical discprt
	common/dp/discprt
	COMMON/RBLCK/treso,tresg,acrit,avamp	!for resint
	common/sblk/sbin,shist,sres,sexp
	common/deb/idebug
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F		!from HJCLIK
	common/fitblk1/ampA,ampF		!from HJCASYMP
c
	common/maxblk/xmin,xmax,ymin,ymax	!from cdist3d
c
	COMMON/cols/icol(100),mono		!for VPLOT
c
c to store alpha, beta to allow check on correlation
c=	real*8 alpha2(10000),beta2(10000),aloglik(10000)
c=	common/absave/nab,alpha2,jalpha,beta2,jbeta,aloglik
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
	common/absave/nab,jalpha,jbeta		!in hjclik, hjcdisp
c
	debug()=caplock()
c
c Initialise
	kth=1			!must allocate for vplot call, but not used
	allocate(thetas(kth))
c Default colours unless otherwise specified
	isetcol=0
	do i=1,100
	   icol(i)=-1	!default colour
	enddo
	kF=kB+kC+kD
	if(k.ne.kA+kF) then
	   call BELL(2)
	   print 43,k,kA,kF
43	   format(' ERROR: k,kA,kF = ',3i4,' in HJCDISP')
	   STOP
	endif

	do i=1,km
	   ucol(i,1)=1.d0
	enddo
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	ans1='Y'
	interp=.false.
	replot=.false.
c
	ONESET=.true.	!so HJCLIK calcs pdf for set #iset ONLY
	iset=1
	tres=tresd(iset)
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	treso=tres1
	tresg=tres1
226	continue		!return for next plot
c Default colours unless otherwise specified
	isetcol=0
	do i=1,100
	   icol(i)=-1	!default colour
	enddo
	corprev=.false.	!correl'n dep on prev gap (used for iplot=3)
	cornext=.false.	!correl'n dep on next gap (used for iplot=3)
	if(.not.nodata) then
	   print 227,iset,iplot
227	   format(/,' SET # ',i3,/,
     & ' (1) Plot histogram of apparent OPEN periods, with fit',/,
     & ' (2) Plot histogram of apparent SHUT times, with fit',/,
     & ' (3) Open time pdf conditional on adjacent gap',/,
     & ' (4) Mean open vs adjacent gap plot, with fit',/,
     & ' (5) Dependency plot',/,
     & ' (6) No plots -exit now',/,
     & ' Option number [',i2,'] = ')
	else if (nodata) then
	   print 2271,iplot
2271	   format(/,
     & ' (1) Plot HJC distribution of apparent OPEN periods',/,
     & ' (2) Plot HJC distribution of apparent SHUT  times',/,
     & ' (3) Open time pdf conditional on adjacent gap',/,
     & ' (4) Mean open vs adjacent gap plot',/,
     & ' (5) Dependency plot',/,
     & ' (6) No plots -exit now',/,
     & ' Option number [',i2,'] = ')
	endif
	i=iplot
	call INPUTi(i)
	if(i.ge.1.and.i.le.6) iplot=i
	if(iplot.eq.6)  goto 999
c Plots to be done, so specify which data set (if nset>1) and recalc values
	if(nset.gt.1) then
	   print 63
63	   format(' set    ligand    concentration')
	   do j=1,nset
		do i=1,nlig
		   print 65,j,i,ligname(i),1.e6*conc(i,j)
65	         format(' Set ',i2,': conc (muM) of ligand ',i2,' (',
     &	      a10,') = ',g11.4)
		enddo
	   enddo
	   print 62,nset,iset
62	   format(' Data set to be plotted (1 - ',i3,') [',i3,'] = ')
	   j=iset
	   call INPUTi(j)
	   if(j.ge.1.and.j.le.nset) iset=j
	   tres=tresd(iset)
	   tres1=sngl(1.d3*tres)	!tres in msec
	   if(tres.eq.0.d0) tres1=0.0001
	   treso=tres1
	   tresg=tres1
	endif
	print 66, iset
66    format(/,
     & ' Plots for set number ',i3,/,
     & ' ------------------------------------------------------------')
	do i=1,nlig
         print 67,ligname(i),conc(i,iset)*1.e6
         if(discprt) write(8,67) ligname(i),conc(i,iset)*1.e6
67       format(/,
     & '   concentration of ',a10,' = ',g13.6,/)
	enddo
	print 671,iset
671	format(' Calculating values for data set #',i3,' . . . . ')
c Make QD for the specified concentration(s)
	call QNEW_HJC(QT,iset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
c Call HJCLIK again (with oneset=true) to calc, for current set (#iset) only,
c the values of eigen,g00A,g10A,g11A,g00F,g10F,g11F, and (in HJCASYMP) values
c of ampA and ampF.
c (In case where NODATA in commom is true, returns after calculating
c distributions-no likelihood calculated)
	sm=HJCLIK(kfit,THETA,
     &    tint,ampl,iprops,nd1,nd2,
     &    Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &    XAF,XFA,QexpQA,QexpQF,
     &    alpha2,beta2,aloglik,kab,
     &    kAm,kFm,km)
c
c Separate section for iplot=5 here (uses PLOT3D)
c Shut=x, open=y
c===========================================================
	if(iplot.eq.5) then
c Observed dependency plot
	   print 68
68	   format(' Show observed dependency plot')
	   call CDIST3D(tint,ampl,iprops,nint,nintt,ndimy,
     &	 iscan,treso,tresg,tres,idsav,idiskq)
c=	   goto 993
c========================================================

c Theoretical (fitted) dependency plot
c This can be calculated over any range of open/shut, but for comparison
c with observations best calculate over smae rnage as used for observed
c plot as transmitted from cdist3d via common/maxblk/xmin,xmax,ymin,ymax

	   xmin=tres1
c	   xmax=10000.*xmin
	   ymin=tres1
c	   ymax=1000.*ymin
	   nvdec=10
c	   xv=-12.		!viewpoint
c	   yv=-12.
c	   zv=10.
c	   iax=1
c	   izv=1
811	   continue		!return here to redraw
	   print 75,xmin,xmax
75	   format(' FITTED DEPENDENCY PLOT',/,
     &    ' Min and max shut time (ms) [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(xmin,xmax)
	   print 76,ymin,ymax
76	   format('&Min and max open time (ms) [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(ymin,ymax)
c	   xmin1=xmin		!default for axes
c	   xmax1=xmax		!default for axes
c	   ymin1=ymin		!default for axes
c	   ymax1=ymax		!default for axes
	   xmin=alog10(xmin)
	   xmax=alog10(xmax)
	   ymin=alog10(ymin)
	   ymax=alog10(ymax)
	   print 77,nvdec
77	   format(' Number of values per decade[',i4,'] = ')
	   call INPUTi(nvdec)
c 	   calculate the number of x, y values and allocate arrays
	   ns=1 + nvdec*ifixr(xmax-xmin)
	   no=1 + nvdec*ifixr(ymax-ymin)
	   nx=ns
	   ny=no
c=	   ALLOCATE(fdep(nx,ny),znew(nx,ny),xx(nx),yy(ny),
	   ALLOCATE(fdep(nx,ny),znew(nx,ny),
     &	shutt(nx),opent(ny))
	   if(allocated(badval)) DEALLOCATE(badval)
	   ALLOCATE(badval(nx,ny))
c No bad values for calculated dependency plot (applies only to observed)
	   do i=1,nx
		do j=1,ny
		   badval(i,j)=.false.
		enddo
	   enddo
c 	   calculate the x, y values, equally spaced on log scales
	   delt=(xmax-xmin)/float(ns-1)
	   do i=1,ns
		shutt(i)=xmin + (float(i-1))*delt		!log values
	   enddo
	   delt=(ymax-ymin)/float(no-1)
	   do i=1,no
		opent(i)=ymin + (float(i-1))*delt		!log values
	   enddo
c 	 Calculate DEP(i,j) -DEPEND fixed to expect times in log(msec)
c=	   call DEPEND(tres,k,kA,kF,km,ns,no,shutt,opent,fDEP,
c=     &	zmin,zmax)
	   call DEPEND(tres,k,kA,kF,ns,no,shutt,opent,fDEP,
     &	 zmin,zmax,
     &	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &	 XAF,XFA,QexpQA,QexpQF,
     &	 kAm,kFm,km)
c  	   round zmin, zmax to integers?
	   z=ifixr(zmin)
	   if(z.gt.zmin) then
		zmin=z - 1.0
	   else
		zmin=z
	   endif
	   z=ifixr(zmax)
	   if(z.lt.zmax) then
		zmax=z + 1.0
	   else
		zmax=z
	   endif
c Plot the 3D graph
	   idev=0		!screen
	   ans='Y'
	   call DCASK('Use 3D smoothing on plot',ans,ans)
	   if(ans.eq.'Y') then
		power=1.0
     		call SMOOTH3D(fdep,znew,nx,ny,power,nx,ny)
	   else
		do i=1,nx
		   do j=1,ny
			znew(i,j)=fdep(i,j)
		   enddo
		enddo
	   endif

	   idrawt=1
	   idrawx=1
	   idrawy=1
	   idrawz=1
	   icol(23)=0		      !in common for gplot3d
	   icol(25)=9
	   icol(71)=7
	   xtitle='shut time'
	   ytitle='yopen time'
	   ztitle='dependency'
	   title2='Fitted dependency plot'
	   ndx=nx
	   ndy=ny

c===from cdist3d
c	   call GPLOT3D(xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
c     &    xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz)
c===gplot3d
c	   call GPLOT3D(xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
c     &    xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz)
c===from fortran77 version
c	   call GPLOT3D(xx,yy,znew,badval,nx,ny,ndx,ndy,
c	   call GPLOT3D(shutt,opent,znew,badval,nx,ny,ndx,ndy,
c     &	xtitle,ytitle,ztitle,title2,idrawt,idrawx,idrawy,idrawz)
	   kcol=2
	   posneg=.true.
	   isetcol=2
	   call GPLOT3D(shutt,opent,znew,badval,nx,ny,ndx,ndy,
     &    xtitle,ytitle,ztitle,title2,idrawt,idrawx,idrawy,idrawz,
     &       kcol,posneg,isetcol,qfile)

c
	   DEALLOCATE(fdep,znew,shutt,opent,badval)
c
	   goto 226	!another plot?
	endif 		!End of dependency plot (iplot=5)
c
	if(iplot.eq.3) then
	   nrange=1
	   itype=3		!default
	   print 36,itype
36	   format(
     &   ' (1) Show open times conditional on PRECEDING shut time',/,
     &   ' (2) Show open times conditional on FOLLOWING shut time',/,
     &   ' (3) Show open times conditional on EITHER shut time',/,
     &   ' Option number [',i2,'] = ')
	   call INPUTi(itype)
	   if(itype.eq.1.or.itype.eq.3) corprev=.true. !correl'n dep on prev gap
	   if(itype.eq.2.or.itype.eq.3) cornext=.true. !correl'n dep on next gap
	   if(itype.eq.1) then
	     print 331
	     if(discprt) write(8,331)
331	     format(/,
     & ' Display pdf of durations of apparent openings that are',/,
     & '  PRECEDED by gaps with durations in following range (ms)',/)
	   else if(itype.eq.2) then
	     print 332
	     if(discprt) write(8,332)
332	     format(/,
     & ' Display pdf of durations of apparent openings that are',/,
     & '  FOLLOWED by gaps with durations in following range (ms)',/)
	   else if(itype.eq.3) then
	     print 333
	     if(discprt) write(8,333)
333	     format(/,
     & ' Display pdf of durations of apparent openings that are',/,
     & '  ADJACENT to gaps with durations in following range (ms)',/)
	   endif
	endif

c========================================================================

	if(iplot.eq.4) then
	   if(readini) then
		nrange=nranged
		print 53,nrange
53		format(' Default = ',i3,' gap ranges:')
		do i=1,nrange
		   ylo(i)=ylod(i)
		   yhi(i)=yhid(i)
		   print 55,i,ylo(i),yhi(i)
		enddo
55	      format(i4,1x,f10.3,' to ',f10.3)
555	      print 56
56	      format(
     &' Number of gap length ranges to be used (<= 20) [default] = ')
		nrange1=0		!signals use of default ranges
	      call INPUTi(nrange1)
		if(nrange1.gt.20) goto 555
		if(nrange1.ge.1) nrange=nrange1
	   else
		nrange1=1		!so doesn't think default wanted
57	      print 420
420	      format(
     &' Number of gap length ranges to be used (<= 20) = ')
	      call INPUTi(nrange)
		if(nrange.lt.1) goto 57
	   endif
	   print 2571
	   if(discprt) write(8,2571)
2571	   FORMAT(/,
     & ' Mean durations of individual apparent openings that are',/,
     & '  adjacent to gaps with durations in following ranges (ms)',/)
	endif
c
	ncalc=512	!for all calc curves (could be up to 2048 now)
c
c Now allocate data arrays (NB yval1 needed in PRANGE for iplot=3)
	if(iplot.eq.3.or.iplot.eq.4) then
c	   ndv1,ndc1=allocated dimensions of Xval etc.
	   ndv1=nrange
	   ndimd1=5
	   ndc1=ncalc	!need ncalc if theoretical curve plotted for iplot=4
	   ndimc1=2
	   ALLOCATE(XVAL1(ndv1,ndimd1),YVAL1(ndv1,ndimd1),
     &	XCAL1(ndc1,ndimc1),YCAL1(ndc1,ndimc1))
	   ALLOCATE(weight(100,10),icurvw(ndimd1))	!for VPLOT5
	   do i=1,ndimd1
		icurvw(i)=-1	!no SD unless reset below
	   enddo
	   kwi=100							!ditto
	   kwj=10							!ditto
	   kmax=100							!dimension for theta
	endif
c
351	if(iplot.eq.3.or.iplot.eq.4) then
	   do i=1,nrange
		sy0(i)=0.		!initialise
		syy0(i)=0.
		ny0(i)=0
		sy1(i)=0.
		syy1(i)=0.
		ny1(i)=0
		sy2(i)=0.
		syy2(i)=0.
		ny2(i)=0
		sx0(i)=0.		!initialise
		sxx0(i)=0.
		nx0(i)=0
		sx1(i)=0.
		sxx1(i)=0.
		nx1(i)=0
		sx2(i)=0.
		sxx2(i)=0.
		nx2(i)=0
		if(iplot.eq.4.and.nrange1.eq.0) goto 741	!use default
c
c Now removed upper limit of tcrit in the burst case -just print warning
35	      print 421,i,i,ylo(i),yhi(i)
421	      format('&(',i2,
     &   ') Gap times (ms) for range',i2,': low t, high t [',
     &	 g9.3,',',g9.3,'] = ')
	  	call INPUT2r(ylo(i),yhi(i))
		if(ylo(i).gt.yhi(i)) then
		   a1=ylo(i)
	         ylo(i)=yhi(i)
		   yhi(i)=a1
		endif
		if(ylo(i).lt.tres1-0.00001) then
		   call BELL(1)
		   print 34,tres1
34		   format(
     & '&Lower limit must not be less than resolution = ',g13.6,' ms',/)
		   goto 35
		endif
		if((iplot.eq.3.or.iplot.eq.4).and.burst(iset).and.
     &		  ylo(i).gt.tcrit(iset))then
		   call BELL(1)
		   print 341,tcrit(iset)
341		   format(
     & ' WARNING: shut times longer than tcrit = ',f10.2,' ms',/,
     & '   are not predicted by fit when fitting bursts',/,
     & ' Use then anyway [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   if(UC(ans).eq.'N') goto 35
		   print 342,ylo(i),tcrit(iset)
		   if(discprt) write(8,342) ylo(i),tcrit(iset)
342		   format(/,
     & ' WARNING: the lower limit, ',f10.2,' ms is above tcrit = ',
     &  g13.6,' ms',/,
     & ' and so uses shut times that are not predicted by fit when',
     & ' fitting bursts',/)
		endif
c older code:
c		if(iplot.eq.4.and.burst(iset).and.i.eq.nrange) then
c		   print 419,i,tcrit(iset)
c		   if(discprt) write(8,419) i,tcrit(iset)
c419		   format(
c     &   '&For range #',i3,' gaps of tcrit =',f10.2,' and greater used')
c		   ylo(i)=tcrit(iset)
c		   yhi(i)=3.1536e10		!msec=1 year! -see PRANGE
c		else
c35	         print 421,i,i
c421	         format('&(',i2,
c     &   ') Gap times (ms) for range',i2,': low time, high time = ')
c	  	   call INPUT2r(ylo(i),yhi(i))
c		   if(ylo(i).gt.yhi(i)) then
c		      a1=ylo(i)
c	      	ylo(i)=yhi(i)
c		      yhi(i)=a1
c		   endif
c		   if(ylo(i).lt.tres1-0.00001) then
c			call BELL(1)
c			print 34,tres1
c34			format(
c     & '&Lower limit must not be less than resolution = ',g13.6,' ms',/)
c			goto 35
c		   endif
c		   if(iplot.eq.3.and.burst(iset).and.ylo(1).gt.
c     &		  tcrit(iset))then
c			call BELL(1)
c			print 341,tcrit(iset)
c341			format(
c     & '&Longest allowable range is tcrit = ',f10.2,
c     & ' and greater: use this [Y] ? ')
c			ans='Y'
c			call INPUTa(ans)
c			if(UC(ans).eq.'N') then
c			   goto 35
c			else
c			   ylo(1)=tcrit(iset)
c			   yhi(1)=3.1536e10		!msec=1 year! -see PRANGE
c			endif
c		   endif
c=		endif
741	      continue
	   enddo	!end of nrange loop
	  deb=debug()
	endif		!gap range(s) set for iplot=3,4
c Keep default ranges for iplot=4
	if(iplot.eq.4) then
	   do i=1,20
		ylod(i)=ylo(i)
		yhid(i)=yhi(i)
		nranged=nrange
	   enddo
	endif
c
c 	For open and shut times, go through the observations exactly as in
c 	HJCLIK (when 'burst' used, some openings, and all shuttings>tcrit, will
c 	be excluded
c
	if(nodata) goto 4062	!no data or bins
c
c	if(.not.allocated(nopen)) ALLOCATE(nopen(nint(1)))
c	if(.not.allocated(tval)) ALLOCATE(tval(nint(1)))
	if(allocated(nopen)) DEALLOCATE(nopen)
	ALLOCATE(nopen(nint(iset)))
	if(allocated(tval)) DEALLOCATE(tval)
	ALLOCATE(tval(nint(iset)))
	in=1		!counter for intervals
	ng=0			!to count number of groups
	nopen(1)=0		!number of openings in group #ng
	j=0			!index in tval()
	islast=0		!index in tint() of last shut period
c Start at first (good) opening
	call FINDOPEN(in,iset,iop,ibad,ampl,iprops,nd1,nd2)
	if(ibad.eq.1) goto 991		!no good openings!
	in=iop		!make the opening the current obs
	nr0=0			!count number not on any range for iplot=4
c
90	continue		!return here for next opening
c 	GET LENGTH OF OPEN PERIOD-look forward to see if there are more openings
c 	IAVAL is in main prog file (NB returns int*2 value, unlike version in EKDIST)
	top=0.	!to accum length of open period
	i1=in
	iofst=-1	!records index in tint() of 1st opening in the open period
	do 51 jn=i1,nint(iset)
	   open=ampl(jn,iset).ne.0.
	   good=.not.BTEST(iprops(jn,iset),3)	!tint(i) was unusable (bit 3='8' set)
	   if(open) then
		if(good) then
		   top=top+tint(jn,iset)
		   if(iofst.eq.-1) iofst=jn !index in tint of 1st opening in open period
		   iolast=jn		!index in tint of last opening in open period
		else		!bad opening, so end group
		   in=jn
		   if(burst(iset)) then
			goto 93	!abandon whole burst & look for next
		   else
			goto 91	!end group at end of prev shutting
		   endif
		endif
	   else
		inext=jn	!index of shutting that ends open period
		goto 52	!shut, so jump out
	   endif
51	continue
c 	Get here if last opening reached (and it is good), but in this case
c 	a gap is not found and inext not updated. Therefore set inext=nint+1 to
c 	signal (below) that end reached
	inext=nint(iset)+1
c
52	continue	!shutting found (index=inext), so jumped out
	topen=top
c Now have an open period, length=topen
c At this point the length of the last good shutting will still be in tshut
c (except for 1st open period, for which islast=0 still) so can be used for
c iplot=4 as long as it was adjacent to the opening just found (eg haven't
c skipped a bad burst in between), which will be so if iofst=islast+1
cc===DEBUG CHECK
c	if(.not.good) then
c         print 83,in,jn,tshut,topen
c         if(discprt) write(8,83) in,jn,tshut,topen
c83	   format(' Bad interval at line 710',/,
c     &	' in,jn,tshut,topen = ',2i8,3x,2g13.6)
c	   call DEBPRT(jn,nint,iset,tint,ampl,iprops,nd1,nd2)
c	endif
cc===
	if((corprev.or.iplot.eq.4).and.
     &    islast.gt.0.and.iofst.eq.islast+1) then
	   call GETRANGE(tshut,topen,ylo,yhi,nrange,sy0,syy0,ny0,
     &    sy2,syy2,ny2,sx0,sxx0,nx0,sx2,sxx2,nx2,deb,nr)
	   if(nr.eq.0) nr0=nr0+1	!tgap not in any range
	   if(idebug.eq.20) then	!debug
		if(nr.eq.nrspec) then
		   print 80,nrspec,in,jn,tshut,topen
		   if(discprt) write(8,80) nrspec,in,jn,tshut,topen
80		   format(' tshut in range ',i5,/,
     &	' in,jn,tshut,topen = ',2i8,3x,2g13.6)
		   call DEBPRT(jn,nint,iset,tint,ampl,iprops,nd1,nd2)
		endif
	   endif
	   if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		j=j+1
		tval(j)=topen		!for iplot=3 (prev gap, or both)
	   endif
	endif
c Record values for pdf of all openings (iplot=1)
	if(iplot.eq.1) then
	   j=j+1
	   tval(j)=topen		!in msec
	endif
c
c   Count number of openings in current group (ng not updated until group ends)
	nopen(ng+1)=nopen(ng+1) + 1
c
c NOW GET THE NEXT SHUT TIME (or end the group)
c At present in=index of 1st opening in open period
c If last interval reached, and it is open, then no more gaps, and inext was
c set to nint+1, above, so end the last group at this opening
	if(inext.gt.nint(iset)) goto 92
	in=inext		!should be shut -check, for debug anyway!
	open=ampl(in,iset).ne.0.
	if(open) then
	   call BELL(2)
	   print 61,in
61	   format(' INTERVAL # ',i5,' should be shut')
	endif
c Check for 2 adjacent gaps, or bad gap
c=	bad=tint(in,iset).lt.0.0		!gap marked bad
	bad=BTEST(iprops(in,iset),3)	!tint(i) was unusable (bit 3='8' set)
	if(in.lt.nint(iset)) then
	   bad1=ampl(in+1,iset).eq.0.    !also bad if next interval is shut too
	endif
	if(bad.or.bad1) then
	   if(burst(iset)) then
		goto 93	!abandon whole burst & look for next
	   else
		goto 92		!end present group with prev opening
	   endif
	endif
	tshut=tint(in,iset)
	if(burst(iset).and.tshut.gt.tcrit(iset)) then
c      End the group; first collect openings followed by gaps>tcrit
c	 for correlation,in burst case
	   if((cornext.or.iplot.eq.4).and.burst(iset)
     &   .and.in.eq.iolast+1) then
		call GETRANGE(tshut,topen,ylo,yhi,nrange,sy1,syy1,ny1,
     &       sy2,syy2,ny2,sx1,sxx1,nx1,sx2,sxx2,nx2,deb,nr)
		if(nr.eq.0) nr0=nr0+1	!tgap not in any range
		if(idebug.eq.20) then	!debug
		   if(nr.eq.nrspec) then
			print 80,nrspec,in,jn,tshut,topen
			if(discprt) write(8,80) nrspec,in,jn,tshut,topen
c80			format(' tshut in range ',i5,/,
c     &	'    in,jn,tshut,topen = ',2i8,3x,2g13.6)
			call DEBPRT(jn,nint,iset,tint,ampl,iprops,nd1,nd2)
		   endif
		endif
	      if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		   j=j+1
		   tval(j)=topen		!for iplot=3 (following gap, or both)
	      endif
	   endif
	   islast=in	!index in tint() of last shut period
	   goto 92	!end present group with prev opening
	endif
c
c Now have a good shut time, in tshut say
c At this point the length of the last good open period will still be in topen
c so it can be used for iplot=3,4 as long as the last opening in this open
c period was adjacent to the gap just found (eg haven't skipped a bad burst in
c between), which will be so if islast=iolast+1. In this case can accum sy1 etc
c which have data for 'following gap'
	tshut=tint(in,iset)
	islast=in	!index in tint() of last shut period
	if((cornext.or.iplot.eq.4).and.
     &	islast.eq.iolast+1) then
	   call GETRANGE(tshut,topen,ylo,yhi,nrange,sy1,syy1,ny1,
     &    sy2,syy2,ny2,sx1,sxx1,nx1,sx2,sxx2,nx2,deb,nr)
	   if(nr.eq.0) nr0=nr0+1	!tgap not in any range
	   if(idebug.eq.20) then	!debug
		if(nr.eq.nrspec) then
		   print 80,nrspec,in,jn,tshut,topen
		   if(discprt) write(8,80) nrspec,in,jn,tshut,topen
c80		   format(' tshut in range ',i5,/,
c     &	' in,jn,tshut,topen = ',2i8,3x,2g13.6)
		   call DEBPRT(jn,nint,iset,tint,ampl,iprops,nd1,nd2)
		endif
	   endif
	   if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		j=j+1
		tval(j)=topen		!for iplot=3 (following gap, or both)
	   endif
	endif
	if(iplot.eq.2) then
	   j=j+1
	   tval(j)=tshut		!in msec
	endif
c End of shutting.  Next interval should be an opening
	in=in+1
	if(in.gt.nint(iset)) goto 91		!end of data -group ends with shutting
	open=ampl(in,iset).ne.0.
	if(.not.open) then
	   call BELL(2)
	   print 611,in
611	   format(' INTERVAL # ',i5,' should be open')
	endif
	goto 90		!get next open period -continue with group
c
c END OF GROUP (#ng) when group ends with a shutting (either because a bad
c opening is found, or because last interval in the data is a (good) shutting
c (neither of these should happen with real data!)
91	continue
	if(burst(iset)) then
	   call BELL(3)
         print 911,ng+1,nopen(ng+1)		!ng not updated yet
         if(discprt) write(8,911) ng+1,nopen(ng+1)
911	   format(
     & ' ERROR: burst cannot end with shut time: Group ',i5,
     &	' nopen = ',i6)
	endif
	if(nopen(ng+1).eq.0) goto 73	!skip this group -look for next
	ng=ng+1		!update number of groups now #ng has ended
c	if(deb) then
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
	   print 799,ng,nopen(ng),inext,ampl(inext,iset),tint(inext,iset)
	   if(discprt) write(8,799) ng,nopen(ng),
     &	inext,ampl(inext,iset),tint(inext,iset)
799	   format(/,
     & ' GROUP # ',i6,': ',i3,' openings',/,
     & '  index, amp, length = ',i8,2g13.6,' Last interval=shut',/)
	endif
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(iset)) goto 991
73	nopen(ng+1)=0	!initialise number of openings in next group
c must first find a gap that precedes next good opening
	call FINDGAP(in,iset,is,ibad,ampl,iprops,nd1,nd2)
	if(ibad.ne.0) goto 991
	in=is		!index of the gap
	call FINDOPEN(in,iset,iop,ibad,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.ne.0) goto 991
	in=iop			!index of the opening
	goto 90			!start new group with the next good opening
c
c
c END OF GROUP (#ng) when group ends with an OPENING (normal way)
92	continue
	if(nopen(ng+1).eq.0) goto 74	!skip this group -look for next
	ng=ng+1		!update number of groups now #ng has ended
c	if(deb) then
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
	   print 800,ng,nopen(ng),inext,ampl(inext,iset),tint(inext,iset)
	   if(discprt) write(8,800) ng,nopen(ng),
     &	inext,ampl(inext,iset),tint(inext,iset)
800	   format(/,
     & ' GROUP # ',i6,': ',i3,' openings',/,
     & '  i, amp(i), length of gap that ends group= ',i8,2g13.6,/)
	endif
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(iset)) goto 991
74	nopen(ng+1)=0	!initialise number of openings in next group
c Interval #in should be shut at this point, so now find next good opening
c to start a new group
	call FINDOPEN(in,iset,iop,ibad,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.ne.0) goto 991
	in=iop			!index of the opening
522	continue
	goto 90			!start new group with the next good opening
c
c ABANDON WHOLE BURST and look for next (without incrementing ng) if bad
c interval is found within a burst
93	continue
	if(.not.burst(iset)) then		!should be!!
	   call BELL(3)
	   print 932
932	   format(' ERROR: should get here only for ''bursts''')
	endif
c Look for next burst: find a good gap>tcrit
933	call FINDGAP(in,iset,igap,ibad,ampl,iprops,nd1,nd2)
	if(ibad.ne.0) goto 991
	if(tint(igap,iset).lt.tcrit(iset)) then   !look again
	   in=igap+1
	   goto 933
	endif
c Gap #igap is > tcrit- get next good opening after it
	in=igap		!index of the gap
	call FINDOPEN(in,iset,iop,ibad,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.ne.0) goto 991
	in=iop			!index of the opening
c reset initial vector
	nopen(ng+1)=0	!reinitialise number of openings in next group
	goto 90			!start new group with the next good opening
c
991	continue		!end if definition of open and shut times
	nyval=j
	if(corprev.or.iplot.eq.4) then
c	  Print means and define xval1,yval1 for plotting (weight is in common)
	   title='Mean open time conditional on PRECEDING shut time'
	   call PRANGE(title,ylo,yhi,nrange,sy0,syy0,ny0,sx0,sxx0,nx0,
     &	yval1,xval1,ndv1,ndimd1,1,weight,kwi,kwj,icurvw)
	endif
	if(cornext.or.iplot.eq.4) then
	   title='Mean open time conditional on FOLLOWING shut time'
	   call PRANGE(title,ylo,yhi,nrange,sy1,syy1,ny1,sx1,sxx1,nx1,
     &	yval1,xval1,ndv1,ndimd1,2,weight,kwi,kwj,icurvw)
	endif
	if((corprev.and.cornext).or.iplot.eq.4) then
	   title='Mean open time conditional on EITHER shut time'
	   call PRANGE(title,ylo,yhi,nrange,sy2,syy2,ny2,sx2,sxx2,nx2,
     &	yval1,xval1,ndv1,ndimd1,3,weight,kwi,kwj,icurvw)
	endif
	if(iplot.eq.4) then
	   print 79,nr0
	   if(discprt) write(8,79) nr0
79	   format(' Number of shut times not in any range = ',i7,/)
	   goto 4062	!no bins etc -just plot with vplot
	endif
c
c
c END OF DATA REACHED -tval defined
	if(allocated(nopen)) DEALLOCATE(nopen)
c
99	continue
	ibad=0
	if(ng.eq.0) then
	   call BELL(5)
	   print 992
992	   format(' NO GROUPS FOUND')
	endif
c
c
c Now set bin widths (based on SETBIN.FOR)
	if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
	if(nyval.eq.0) then
	  print 4132
4132	  format(' NO VALUES FOR DISTRIBUTION')
	  call BELL(4)
	  STOP
	endif
c
c For pdfs, variable is in X axis so calc as xmin,xmax
	call VARV1(tval,Nyval,xbar,varx,xmin,xmax)
	print 413,NYVAL,xbar,sqrt(varx),xmin,xmax
	if(discprt) write(8,413) NYVAL,xbar,sqrt(varx),xmin,xmax
413	FORMAT(/,' Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
	xmin=0.0		!reset for histogram
	if(debug()) then
	  print 700,1,xmin,xmax,ncalc
700	  format(' Pos#',i2,': xmin,xmax,ncalc=',2g13.6,i8)
	  pause
	endif
c
c NOW GET X AXIS
	setmax=.false.	!xmax set automatically, not manually
	if(.not.sbin) logt=.false.
	if(sbin) goto 303	!same bins so XAXIS,XWBASE as before
c	iopt=2
c	if(iplot.eq.2) iopt=1	!default for shut times
	iopt=1
	print 2092,iopt
2092	format(
     & ' (1) Distribution of log durations',/,
     & ' (2) Distribution of durations- set bins manually',/,
     & ' (3) Distribution of durations- 20 bins',/,
     & ' (4) Distribution of durations- 40 bins',/,
     & ' (5) Distribution of durations- 60 bins',/,
     & ' (6) Skip histogram',/,
     & ' Option number [',i2,'] = ')
c     & '',/,
	i=iopt
	call INPUTi(i)
	if(i.ge.1.and.i.le.6) iopt=i
	if(iopt.eq.6) then
	   nbin=0
	   if(allocated(thetas)) deallocate(thetas)
	   RETURN		!skip histo
	endif
	logt=iopt.eq.1
501	nbw=1
	if(logt.or.iopt.ge.3) goto 2093
503	print 208
208	format(
     & ' Number of different bin widths (-1 to skip histo) [1]= ')
	call INPUTi(nbw)
	if(nbw.eq.0) nbw=1
	if(nbw.le.-1) then
	   nbin=0
	   if(allocated(thetas)) deallocate(thetas)
	   RETURN		!skip histo
	endif
2093	continue
	tres=tresd(iset)
	xaxis(1)=sngl(tres*1.d3)	!in msec
c=2093	continue
	if(iopt.ge.3) goto 291	!auto bins
	print 2091,xaxis(1)
2091	format('&Histogram to start at ',f8.5,' ms.  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') goto 291
28	print 209
209	format('&Start histogram at x (ms, pA etc) [0.0] = ')
	call INPUTr(xaxis(1))
c	read 103,xaxis(1)
c103	format(g13.6)
c
291	continue
c
C   GET BIN WIDTHS
c Calculate bin boundaries in XAXIS
	m=1		!counts X axis entries
303	continue
c Sep bit for dist of log(t)- have NBW bins starting at xaxis(1), up to xend
c	if(.not.logt) goto 308
	if(logt) then
	   if(sbin) then
	     dx=exp(alog(10.)/float(nbdec))
	     goto 731
	   endif
c	   if(xaxis(1).le.0.) goto 2093
	   if(xaxis(1).le.0.) then
		call BELL(3)
		print 3031,xaxis(1)
3031		format(' Can not start log scale at t = ',g11.4,/,
     &      ' New starting value [0.01 ms] = ')
		Xaxis(1)=0.01
		goto 2093
	   endif
	   if(nyval.le.300) nbdec=5				!default
	   if(nyval.gt.300.and.nyval.le.1000) nbdec=8	!default
	   if(nyval.gt.1000.and.nyval.le.3000) nbdec=10	!default
	   if(nyval.gt.3000) nbdec=12				!default
1048	   continue
	   print 1041,nbdec
1041	   format(' Number of bins/decade [',i3,'] = ')
	   i=nbdec
	   call INPUTi(i)
	   if(i.gt.0) nbdec=i
	   dx=exp(alog(10.)/float(nbdec))
	   xend=1. + xmax - amod(xmax,1.)
	   print 1046,xend
1046	   format('&Last x value (ms)= ',g13.6,'  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
	      print 72
72	      format('&Last x value (ms)= ')
	      call INPUTr(xend)
		setmax=.true.	!xmax set manually
	   endif
	   nbin=1+ifix(alog(xend/xaxis(1))/alog(dx))
	   xend=(dx**nbin)*xaxis(1)
	   if(setmax) xmax=xend
	   print 1047,nbin,xend
1047	   format(
     &   '&',1x,i5,' bins; last x reset to ',g13.6,': O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(setmax) xmax=xend
	   if(UC(ans).eq.'N') goto 1048
	   do 1043 i=1,nbin
1043	    xaxis(i+1)=xaxis(1)*(dx**i)
	    mlast=nbin+1
731	    xwbase=alog10(dx)	  !in log10 units to scale fit
	    print 1045,nbin,dx
	    if(discprt) write(8,1045) nbin,dx
1045	    format(
     & ' Distribution of log(t) displayed- ',i6,' bins, factor= ',g11.4)
	else if(.not.logt) then
c Section done when not logt
	   do 210 i=1,nbw
	   if(sbin) goto 306
	   if(iopt.ge.3) then
c auto-binwidth to give 20,40,60 bins (nbw=1, not logt)
		nbin=20
		if(iopt.eq.4) nbin=40
		if(iopt.eq.5) nbin=60
		dx=(xmax-xmin)/float(nbin)
		call SETTIC(dx)
		mlast=1
	   else
		setmax=.true.		!xmax set manually
	   	mLAST=m
212	   	print 104,I
104	   	format(' (',I2,') Bin width, Last x value (ms, pA etc)= ')
	   	call INPUT2r(dx,xend)
32	   	xn=(xend-xaxis(mlast))/dx
C ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
c- also check that not more than 101 bins
		itemp=IFIX(0.5+xn)
	   	if(abs(xn-float(itemp)).lt.dx*.01) goto 30	!OK
		itemp=IFIXr(xn)
	   	xend=xaxis(mlast) + (float(itemp))*dx
	   	call BELL(1)
	   	print 31,xend
31	   	format('&Last x reset to ',g13.6,' O.K. [Y] ? ')
		if(setmax) xmax=xend
	      ans='Y'
	      call INPUTa(ans)
	   	if(UC(ans).eq.'N') goto 212
		goto 32
30		nbin=IFIX(0.5+xn)
	   endif
311	   if(nbin.eq.0) goto 303
	   do 211 j=1,nbin
	   m=m+1
	   if(m.gt.501) then
	     call bell(2)
	     print *, ' TOO MANY BINS'
	     goto 501
	   endif
	   XAXIS(m)=XAXIS(mLAST)+(float(J))*DX
211	   continue
c   save values for printing if repeated with same bins
1044	   xsav(i,1)=float(nbin)
	   xsav(i,2)=dx
	   xsav(i,3)=xaxis(mlast)
	   xsav(i,4)=xaxis(m)
306	   continue
	   if(discprt) write(8,2111) (xsav(i,j),j=1,4)
	   if(sbin.or.iopt.ge.3) print 2111, (xsav(i,j),j=1,4)
2111	   format(1x,f5.0,
     & ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
210	   continue		!end if i loop
C
	   if(sbin) goto 307
	   nbin=m-1		!NOW NBIN=TOTAL NO OF BINS
	   xwbase=dx		!IF ONLY ONE BIN WIDTH
	   if(nbw.eq.1) goto 307
	   print 107
107	   FORMAT( ' Base width for frequency density= ')
	   call INPUTr(xwbase)
307	   if(discprt) write(8,1071) xwbase
	   if(sbin) print 1071,xwbase
1071	   FORMAT(' Base width for frequency density= ',g13.6)
	endif
c
C NOW SORT Y VALUES AND CALC FREQ (DENSITY) FOR EACH FILE
c No need to repeat this if SHIST (same histo, same bins)
	if(shist.and.sbin.and.sexp) goto 4071
	print 82
82	format(' Sorting data into bins...')
	flo=0.
	fhi=0.
	do i=1,510
	   freq(i)=0.0
	enddo
c
	do 223 i=1,nyval
	yv=tval(i)
	if(yv.lt.xaxis(1)) then
	   flo=flo+1.0
	   goto 223
	endif
	if(yv.gt.xaxis(nbin+1)) then
	   fhi=fhi+1.0
	   goto 223
	endif
	do 221 j=1,nbin
	if(yv.ge.xaxis(j).and.yv.lt.xaxis(j+1)) freq(j)=freq(j)+1.0
221	continue	!j loop (bins)
c For top bin include those EQUAL to boundary (eg so Popen=1.0 not excluded
c from top bin)
	if(yv.eq.xaxis(nbin+1)) freq(nbin)=freq(nbin)+1.0
223	continue	!i loop (obs)
	goto 85
c Sorting done
85	continue
	if(allocated(tval)) DEALLOCATE(tval)
c
	if(flo.lt.0.1) flo=-2.	!do not plot if flo=0
	if(fhi.lt.0.1) fhi=-2.	!ditto
	fmax=0.
C CONVERT FREQ TO FREQ DENSITY NEXT
c N.B. FLO and FHI are abs numbers, not densities- cannot express FHI
c as a density if upper lim is infinite! FLO can be expressed as density
c once bin width has been fixed (see VHIST)
	do 218 j=1,nbin
	if(.not.logt) freq(j)=freq(j)*xwbase/(xaxis(j+1)-xaxis(j))
	if(freq(j).gt.fmax) fmax=freq(j)
218	continue
	if(flo.gt.fmax) fmax=flo
	if(fhi.gt.fmax) fmax=fhi
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	j=ifix(0.1+fhi)
	if(fhi.lt.0.) j=0
	print 222,i,j
	if(discprt) write(8,222) i,j
222	FORMAT(' No of values below Xlow= ',i8,
     & ' No of values above Xhigh= ',i8)
4071	print 407
407	FORMAT(' Type frequency densities [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).NE.'Y') GOTO 406
	if(.not.logt) then
		print 1061
		if(discprt) write(8,1061)
1061 		format(
     & ' Bin no.         X values                Frequency')
		DO 220 I=1,NBIN
		print 106,I,xaxis(i),xaxis(i+1),FREQ(I)
220		if(discprt) write(8,106) I,xaxis(i),xaxis(i+1),FREQ(I)
106		FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
	endif
c
	if(logt) then
		print 1062
		if(discprt) write(8,1062)
1062 		format(
     & ' Bin no.         X values                Frequency',/,
     & '              log(X) values              sqrt(freq)')
		DO 1063 I=1,NBIN
		print 1064,I,xaxis(i),xaxis(i+1),FREQ(I)
		if(discprt) write(8,1064) I,xaxis(i),xaxis(i+1),FREQ(I)
		if((xaxis(i).le.0.).or.(freq(i).lt.0.)) goto 1063
		print 1065,alog10(xaxis(i)),
     &	alog10(xaxis(i+1)),sqrt(freq(i))
		if(discprt) write(8,1065) alog10(xaxis(i)),
     &	alog10(xaxis(i+1)),sqrt(freq(i))
1063		continue
1064		FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
1065		FORMAT(8x,   g13.6,' - ',g13.6,3x,G13.6)
	endif
c
	print 108
	if(discprt) write(8,108)
	pause ' Display follows'
406	CONTINUE
C
4061	CONTINUE
	call flush(7)
	if(flo.lt.0.) flo=0.
	if(fhi.lt.0.) fhi=0.
c end code from SETBIN
c
4062	continue
	ilabel=1
	titley='Frequency '
	xw=xwbase  !so Y axis labelled 'per 10 ms' (reset below as nec)
	if(logt) xw=0.	!not so labelled for log-bins
	if(nodata) then
	   titley='Probability density'
	   xw=0.
	endif
c
	if(iplot.eq.1) then
	   titlex='Apparent open time (ms) '
	else if(iplot.eq.2) then
	   if(.not.burst(iset)) then
	      titlex='Apparent shut time (ms) '
	   else if(burst(iset)) then
	      titlex='App. shut time | t < tcrit (ms)'
	   endif
c=	else if(iplot.eq.3.or.iplot.eq.5) then
	else if(iplot.eq.3) then
	   if(itype.eq.1) titlex='App open time (preceded by spec gap)'
	   if(itype.eq.2) titlex='App open time (followed by spec gap)'
	   if(itype.eq.3) titlex='App open time (adj to spec gap)'
	else if(iplot.eq.4) then
	   titlex='Adjacent shut time (mean)'
	   titley='Mean open time (adj to spec gap range) '
	   xw=0.
	endif
	autplt=.false.
	draft=.false.
	plotonly=.false.
	doframe=.true.
	landscap=.true.
	fitted=.true.
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
	iask=-2	!do not ask before leaving display; delete graph
	xlo=-1
c
c Separate section for iplot=4 here (uses VPLOT)
86	continue
	if(iplot.eq.4) then	!use VPLOT for mean open vs adj gap
	   if(replot) goto 861
	   replot=.false.
	   tres1=sngl(1.d3*tres)    !reset here (may have beeen set to log value)
c	   isetcol=0	!all default colours
	   isetcol=1	!use icol() for colour if not -1
	   do i=1,100
		icol(i)=-1	!default colour
	   enddo
	   isym(1)=-7	!circle for data set 1 -preceding gap range
	   isym(2)=-3	!square for data set 2 -following gap range
	   isym(3)=-4	!diamond for data set 3 -both
	   ijoin1(1)=0	!cont line joins data pnts
	   ijoin1(2)=0	!cont line joins data pnts
	   ijoin1(3)=0	!cont line joins data pnts
	   do j=1,5
	 	ndat1(j)=nrange
c	      syms(j)=0.	!so symbol size set internally
	      syms(j)=3.0		!internal default=2.5
	   enddo
c
c Put calculated values for mean open vs spec gap range into data rather
c than calc curve, so symbol can be used
c	   ijoin1(4)=2   !short-dash line joins calc pnts
c	   ijoin1(5)=2   !short-dash line joins calc pnts
	   ijoin1(4)=-1   !not joimed
	   ijoin1(5)=-1   !not joimed
c Hollow symbols for calc points
	   isym(4)=7	!circle for calc set 1 -preceding gap range
	   isym(5)=-3	!solid square for calc set 2 -following gap range
c
	   print 108		!blank line
	   if(nodata) then
		ncurvd=0
	   else
	      call DCASK(
     &  ' Show observations only for adjacent (before OR after) gap',
     &	'Y',ans)
	      if(ans.eq.'Y') then
		   ncurvd=3
		   do j=1,ncurvd
			icurvd1(j)=j+2		!=3,4,5
		   enddo
		   icol(1)=10	!green for data set 3  -both
c		   icol(2)=14	!yellow for calc set 1 -preceding gap range
c		   icol(3)=12	!red for calc set 2    -following gap range
		   icol(2)=11	!light blue for calc set 1 -preceding gap range
		   icol(3)=9	!dark blue for calc set 2    -following gap range
		else
		   ncurvd=5
		   do j=1,ncurvd
	 		icurvd1(j)=j
		   enddo
		   icol(1)=14	!yellow for data set 1 -preceding gap range
		   icol(2)=12	!red for data set 2    -following gap range
		   icol(3)=10	!green for data set 3  -both
c		   icol(4)=14	!yellow for calc set 1 -preceding gap range
c		   icol(5)=12	!red for calc set 2    -following gap range
		   icol(4)=11	!light blue for calc set 1 -preceding gap range
		   icol(5)=9	!dark blue for calc set 2    -following gap range
	      endif
	   endif
c Define Ycal1(i,1), xcal1(i,1), i=1,nrange -NOW in Xval1,Yval1
c (ie calc Mean OPen ADJacent to spec gap range)
c=	   call MOPADJ(tres,ylo,yhi,nrange,QD,k,kA,kF,km,ycal1,xcal1,
c=     &	ndc1,ndimc1,den1)
c Add j1,j2 in call to MOPADJ, and put results in xval, yval (not xcal,ycal)
c -also put xval, yval into call of PCRANGE
c Check that current set has not got ylo(1)<tres -this can happen if
c default ranges used and later set has larger (worse) resolution
	   tres1=sngl(1.d3*tres)	!tres in msec
	   do i=1,nrange	!restore original ranges in case reset for another set
		ylo(i)=ylod(i)
		yhi(i)=yhid(i)
	   enddo
	   if(ylo(1).lt.tres1-0.00001) then
		call BELL(2)
		print 78,ylo(1),tres1,tres1,yhi(1)
78		format(
     & ' For current set, lower limit = ',g8.3,' is below resolution '
     & ' = ',f8.3,/,
     & ' First range reset to ',f8.3,' to ',f8.3,' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   ylo(1)=tres1
		else
		   goto 351
		endif
	   endif
c
	   j1=4
	   j2=5
	   call MOPADJ(tres,ylo,yhi,nrange,QD,k,kA,kF,
     &	yval1,xval1,ndv1,ndimd1,den1,j1,j2,
     &	QEXPQA,QEXPQF,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   title=
     &	' CALCULATED values for mean open given preceding gap range'
	   call PCRANGE(title,ylo,yhi,nrange,ny0,den1,yval1,xval1,
     & 	ndv1,ndimd1,j1)
	   title=
     &	' CALCULATED values for mean open given following gap range'
	   call PCRANGE(title,ylo,yhi,nrange,ny1,den1,yval1,xval1,
     & 	ndv1,ndimd1,j2)
c Calc theoretical relationship in xcal1(i,j), ycal1(i,j) if req.
c (j=j1 for preceding gap; j=j2 for following gap)
c
c Calc curves
c  Ask if continuous (theoretical) version to be plotted also
	   print 108		!blank line
	   call DCASK(
     &  ' Show also the continuous (theoretical) relationship','Y',ans)
	   print 108		!blank line
	   ncurvc=0
	   if(ans.eq.'Y') then
		ncurvc=2
		icurvc1(1)=1
		icurvc1(2)=2
		ncal1(1)=ncalc
		ncal1(2)=ncalc
		iline1(1)=0		!contin
		iline1(2)=3		!dashed
		icol(11)=14	!yellow for calc set 3 -preceding gap (continuous)
		icol(12)=12	!red for calc set 4    -following gap (continuous)
		j1=1
		j2=2
c Define xcal1(i,j1) in ms for calc curve (equally spaced on log scale)
c Make the upper limit 3*slowest shut time constant (can get overflow in
c mopadjc if time too big)
c=		tmax=4.0*sngl(-(1.d3/rootF(kF)))	!in msec
		tmax=yhi(nrange)
		dx=(alog10(tmax) - alog10(tres1))/float(ncalc)
		do i=1,ncalc
		   tr=tres1
		   if(tres1.eq.0.0) tr=0.0001
		   x=alog10(tr) + float(i-1)*dx
		   xcal1(i,j1)=10.0**x
		enddo
		call MOPADJc(tres,QD,k,kA,kF,
     &	ycal1,xcal1,ncalc,ndc1,ndimc1,j1,j2,
     & QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   endif
c
	   if(ncurvd.eq.5) then
          print 8
          if(discprt) write(8,8)
8	    format(/
     & ' Observations -preceding gap yellow, filled circles, cont. line'
     &   ,/,
     & ' Observations -following gap red, filled squares, cont. line'
     &   ,/,
     & ' Observations -adjacent gap green, filled diamonds, cont. line'
     &   ,/,
     & ' Calculated -preceding gap light blue, open circles'
     &   ,/,
     & ' Calculated -preceding gap dark blue, solid squares'
     &   ,/,
     & ' Theoretical continuous relationship:',/,
     & '   preceding gap yellow curve, following gap red curve.'
     &     ,/)
c8	    format(/
c     & ' Observations -preceding gap yellow, filled circles, cont. line'
c     &   ,/,
c     & ' Observations -following gap red, filled squares, cont. line'
c     &   ,/,
c     & ' Observations -adjacent gap green, filled diamonds, cont. line'
c     &   ,/,
c     & ' Calculated -preceding gap yellow, open circles, dash line'
c     &   ,/,
c     & ' Calculated -following gap red, open squares, dash line'
c     &   ,/,
c     & ' Theoretical continuous relationship:',/,
c     & '   preceding gap yellow curve, following gap red curve.'
c     &     ,/)
	   else if(ncurvd.eq.3) then
          print 81
          if(discprt) write(8,81)
81	    format(/
     & ' Observations -adjacent gap green, filled diamonds, cont. line'
     &   ,/,
     & ' Calculated -preceding gap light blue, open circles'
     &   ,/,
     & ' Calculated -preceding gap dark blue, solid squares'
     &   ,/,
     & ' Theoretical continuous relationship:',/,
     & '   preceding gap yellow curve, following gap red curve.'
     &     ,/)
c81	    format(/
c     & ' Observations -adjacent gap green, filled diamonds, cont. line'
c     &   ,/,
c     & ' Calculated -preceding gap yellow, open circles, dash line'
c     &   ,/,
c     & ' Calculated -following gap red, open squares, dash line'
c     &   ,/,
c     & ' Theoretical continuous relationship:',/,
c     & '   preceding gap yellow curve, following gap red curve.'
c     &     ,/)
	   endif
c
861	   continue
	   ilog=1		!y vs log(x)
	   iscal=1		!scale internally
	   itrace=0		!not multiple traces
c	ndv1,ndc1=allocated dimensions of Xval1 etc.
c Call with thetas() and kmax=kth (not used because ifitype=0)
	   pause ' Display follows'
	  call VPLOT5(XVAL1,YVAL1,ndat1,icurvd1,ncurvd,ijoin1,syms,ndimd1,
     &   XCAL1,YCAL1,ncal1,icurvc1,ncurvc,iline1,ndimc1,isym,ilog,iscal,
     &   XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &   XLO,XHI,fYLO,fYHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     &  titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     &   cbig,ifont,landscap,fitted,iask,thetas,ifitype,ncomp,interp,
     &   isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kth,iver)
	   goto 994			!where next?
	endif		!end of diversion for iplot=4
c
c Back to iplot=1,2,3 (pdfs -shown in VHIST)
	ndimd=1
	ndimc=10
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos
c===c IFITYPE=-1 for HJC time histos (display tres and asymptotic fit)
	ifitype=0
c===	if(iplot.eq.1.or.iplot.eq.2) ifitype=-1
c===first not needed??
c	first=.false.
c
101	format(a1)
108	FORMAT(/)
c HISTOS
c BIN DEFINITION for VHIST5
c	ndat(j)=nbin.
c	LO bin is from Xval(0,j) to Xval(1,j); freq in Yval(0,j)
c	First bin is from Xval(1,j) to Xval(2,j); freq in Yval(1,j)
c	ith bin is from Xval(i,j) to Xval(i+1,j); freq in Yval(i,j)
c	last bin is from Xval(nbin,j) to Xval(nbin+1,j); freq in Yval(nbin,j)
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
	ncurvd=1
	if(nodata) then
	   ncurvd=0
	else
	   icurvd(1)=1
	   ndat(1)=nbin
	   ijoin(1)=0
	   do i=1,nbin+1
		xval(i,1)=xaxis(i)
	   enddo
	   dx=xaxis(nbin+1) - xaxis(nbin)
	   xval(nbin+2,1)=xval(nbin+1,1) + dx		!extra bin in case fhi>0
	   if(setmax) xmax=xval(nbin+2,1)
	   do i=1,nbin
		yval(i,1)=freq(i)
	   enddo
	endif
c Now fix lo,hi bins etc and define xmin,fmin etc (all here rather than
c in SCVD1 now). Want to display with fit straight away -no prelim display
c without calc curve for scaling so better scale externally -do as in HJCVDU
c CODE FROM HJCVDU:
c
c NB logt already set!
21	continue		!return here to change from open to shut
	icomp=1
	if(iplot.eq.1.or.iplot.eq.2) then
	   print 13,icomp
13	   format(
     & ' (1) Show ''exact'' pdf only (2*exact + asymptotic)',/,
     & ' (2) Show asymptotic distribution also ',/,
     & ' (3) Show asymptotic distribution and its components also ',/,
     & ' Option number [',i2,'] = ')
	   i=icomp
	   call INPUTi(i)
	   if(i.ge.1.and.i.le.3) icomp=i
	   if(icomp.eq.1) then
	    call DCASK('Show also the pdf without missed events',ans1,ans)
	    ans1=ans		!default next time
	    nores=ans.eq.'Y'
	    if(nores) then
	       call DCASK(
     &	'Rescale this pdf to unit area above t=tres','y',ans)
	       scale=ans.eq.'Y'
	    endif
	   endif
	endif
c
	if(debug()) then
	  print 700,2,xmin,xmax,ncalc
	  pause
	endif
c Now use xmax as for histo (based on data), calc by VARV1 (or get xmax
c as 5*slowest time constant if no data
	xmin=0.
	if(nodata) then	!get xmax, for calculation of xcal()
	   ans='Y'
	   call DCASK('Plot p.d.f. of log(t)',ans,ans)
	   logt=ans.eq.'Y'
	   if(iplot.eq.1.or.iplot.eq.3) then		!open
		xmax=5.0*sngl(-1.d3/rootA(kA))		!slowest=last
	   else if(iplot.eq.2) then		!shut
		xmax=5.0*sngl(-1.d3/rootF(kF))		!slowest=last
	   endif
	endif
	if(logt) then
	   xmin=0.010001	!10 microseconds should be below any tres!
	   xmax=2.*xmax	!make bigger for log display
	   xmin=alog10(xmin)
	   xmax=alog10(xmax)
	endif
	if(debug()) then
	  print 700,3,xmin,xmax,ncalc
	  pause
	endif
c
c CALCs for iplot=1,2
	if(iplot.eq.1.or.iplot.eq.3) then	!open
	   ncomp=kA
	   do m=1,kA
		tau(m)=sngl(-1.d3/rootA(m))	!define tau for asymptotic pdf
		amp(m)=sngl(ampA(m))		!define amplitude for ditto
	   enddo
	   do m=1,k
		g00(m)=g00A(m)
		g10(m)=g10A(m)
		g11(m)=g11A(m)
	   enddo
	else if(iplot.eq.2) then
	   ncomp=kF
	   do m=1,kF
		tau(m)=sngl(-1.d3/rootF(m))	!define tau for asymptotic pdf
		amp(m)=sngl(ampF(m))		!define amplitude for ditto
	   enddo
	   do m=1,k
		g00(m)=g00F(m)
		g10(m)=g10F(m)
		g11(m)=g11F(m)
	   enddo
	endif
c
19	continue	!return here to recalculate points
	if(debug()) then
	  print 700,4,xmin,xmax,ncalc
	  pause
	endif
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	tres2=2.*tres1
	tres3=3.*tres1
c  Calc dx (=dx1 say) for xcal(i,1) (points from tres1 to xmax)
	if(logt) then		!xmax already in log units
	   dx1=(xmax-alog10(tres1))/float(ncalc)
	   tres1=alog10(tres1)
	   tres2=alog10(tres2)
	   tres3=alog10(tres3)
	else
	   dx1=(xmax-tres1)/float(ncalc)
	endif
	ires2=1+ifix(1.0+(tres2-tres1)/dx1)	!index of point just below tres2
	ires3=1+ifix(1.0+(tres3-tres1)/dx1)	!index of point just below tres3
c
c Define xcal separately, so can arrange to have 2 points at t=tres, one with
c y=0.0, one with y=F0HJC(tres), so vertical line drawn nicely -these might
c as well be the first two points! So set 1st point=tres1,y=0.0, 2nd point
c =tres1 also
	xcal(1,1)=tres1
	do i=2,ncalc
	   xcal(i,1)=tres1+(float(i-2))*dx1      !so xcal(2,1)=tres1 also
	enddo
c
c
c Xcal(i,1) now defined. But VPLOT, and F0HJC etc expect non-log values so
c must convert back
	if(logt) then
	   tres1=10.**tres1
	   tres2=10.**tres2
	   tres3=10.**tres3
	   do i=1,ncalc
	      xcal(i,1)=10.**xcal(i,1)
	   enddo
	endif
c	if(debug()) print 1531,ires2,ires3,tres1,tres2,tres3
c1531	format(' ires2,ires3,tres1,tres2,tres3=',/,2i5,3g13.6)
c
c DEFINE Ycal() (including multiply by antrue*xwbase, to get superimposition
c on the histogram.  In this case the true number is just the observed number
c so antrue replaced by float(nyval).  Note that xwbase is in msec, but pdf
c is in 1/sec, so to convert latter to 1/msec must divide by 1000
c For bursts must divide by P[t<tcrit] in case of the shut time pdf to allow
c for fact that shuttings longer than tcrit are excluded (not, of course, for
c open times in iplot=1,3)
	fac=(float(nyval))*xwbase/1000.
	if(nodata) fac=1.0	!to get p.d.f.
c
	if(burst(iset).and.iplot.eq.2) then
	   tcrit2=dble(tcrit(iset))*1.d-3	!in sec
	   Ptc=FTCt(tcrit2,tres,k,kA,kF,ucol,
     &     Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   fac=fac/sngl(ptc)
	   print 603,tcrit(iset),ptc
	   if(discprt) write(8,603) tcrit(iset),ptc
603	   format(' Prob[observed shut time < ',f8.3,' ms] = ',g13.6)
	   if(ptc.gt.1.d0.or.ptc.lt.0.d0) then
		call BELL(2)
		print 6031
		if(discprt) write(8,6031)
6031		format(' ILLEGAL PROBABILITY!')
		pause
	   endif
	endif
c
c  For iplot=3 calc the conditional pdfs in POPADJ, in ycal(i,2) and ycal(i,3),
c and then also do next bit to put unconditional open pdf in ycal(i,1) (the
c appropriate g10 etc for open times were defined above in iplot=3 case)
c Modified 03/10/00 08:59am so that POPADJ results go into ycal(i,j1),
c ycal(i,j2) rather than always taking j1=2, j2=3, as before.
c No -simpler to take j1=2, j2=3, j0=1 so as before
c Modif
	j0=1		!for unconditional pdf
	if(iplot.eq.3) then
	   j1=2
	   j2=3
	   call POPADJ(tres,ylo,yhi,ncalc,k,kA,kF,ycal,xcal,j1,j2,den1,
     &    QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &    XAF,XFA,kAm,kFm,kAx,km)
	   j0=1	!for unconditional pdf
	endif
	ycal(1,j0)=0.0	!to draw vertical line
	deb=debug()
	do i=2,ncalc
	   t=xcal(i,j0)
	   time=dble(t)
c	   if(deb) then
c		print 701,i,t
c701		format(' xcal(i,j0) = ',i7,g13.6)
c	   endif
	   if(i.le.ires2) then
		ycal(i,j0)=sngl(F0HJC(time,g00,eigen,tres,k,km))
	   else if(i.gt.ires2.and.i.le.ires3) then
		ycal(i,j0)=sngl(F1HJC(time,g00,g10,g11,eigen,tres,k,km))
	   else         		!asymptotic pdf
		f=0.0
		do m=1,ncomp
		   fc=amp(m)*exp(-(t-tres1)/tau(m))	!NB needs excess time here
c==		   ycal(i,m+2)=fc	!components
		   f=f + fc
		enddo
		ycal(i,j0)=f
	   endif
	enddo		!do i=2,ncalc
c
	jval=1
	if(iplot.eq.3) jval=3   !uncond + conditional (pre=gap + post-gap)
	do j=1,jval
	 if(logt) then
	   do i=1,ncalc
		ycal(i,j)=fac*ycal(i,j)*xcal(i,j)*2.30259	!f(log10(t))=2.3*t*f(t)
	   enddo
	 else
	   do i=1,ncalc
		ycal(i,j)=fac*ycal(i,j)
	   enddo
	 endif
	enddo
c
c In order to see the asymptotic distribution, and its components,  right back
c to t=0, calculate it, and its components, separately, with their own xcal().
c NB components skipped for iplot=3 (for which icomp=1 always)
c  BUT the asymptotic distn is defined only for 'excess time', so probably
c should not use below tres?
c   Keep asymptotic dist in ycal(i,2), and its components in ycal(i,3) to
c ycal(ncomp+2).
c  If NORES then use ycal(i,2),xcal(i,2) for pdf with zero res
	if(iplot.le.2.and.icomp.eq.1.and.NORES) then	!calc pdfs by CH82 methods
	   icurvc(2)=2
c	   call EQOCCd(QD,k,k-1,km,Peq1)	!calc equilib occs
	   call EQOC_HJC(QD,peq1,k,km,km)
	   call PHIo1(QD,Peq1,phio,km)		!calc init vector
	   if(iplot.eq.1) then
	 	call PDFopen(QD,phio,area,tau1,kA,km)
	 	print 108
		 if(discprt) write(8,108)
		 call PDFOUTs('Open time pdf with zero resolution',
     &	-1,-1,area,tau1,kA,am,sd,km,.false.,.false.,discprt)
	 	kX=kA
	   else if(iplot.eq.2) then
		call PDFshut(QD,phio,area,tau1,kF,km)
		print 108
		if(discprt) write(8,108)
		call PDFOUTs('Shut time pdf with zero resolution',
     &	-1,-1,area,tau1,kF,am,sd,km,.false.,.false.,discprt)
		kX=kF
	   endif
	   fac1=fac
	   if(scale) then
		f=0.0
	      do m=1,kX
		   f=f + area(m)*exp(-tres1/tau1(m))	!area above tres
		enddo
		fac1=fac1/f					!new scale factor
	   endif
	   xmin1=0.001001	  !calc from 1 microsec for zero res
	   if(logt) xmin1=alog10(xmin1)	!xmax already log in this case
	   dx2=(xmax-xmin1)/float(ncalc)
	   do i=1,ncalc
		t=xmin1+(float(i-1))*dx2       !start at 1 microsec
		if(logt) t=10.**t
		xcal(i,2)=t
		f=0.0
		do m=1,kX
		   f=f + (area(m)/tau1(m))*exp(-t/tau1(m))
		enddo
		if(logt) f=f*t*2.30259		!f(log10(t))=2.3*t*f(t)
		ycal(i,2)=f*fac1*1000.
	   enddo
	endif
	if(icomp.eq.2.or.icomp.eq.3) then !define separate xcal for components
	 dx2=(xmax-xmin)/float(ncalc)
	 if(debug()) then
	  print 700,5,xmin,xmax,ncalc
	  pause
	 endif
	 deb=debug()
	 do 152 i=1,ncalc
	   t=xmin+(float(i-1))*dx2       !start at xmin
	   if(logt) t=10.**t
	   xcal(i,2)=t
c	 if(deb) then
c	   print 702,i,t
c702	   format(' xcal(i,2) = ',i7,g13.6)
c	 endif
	 do 152 m=3,ncomp+2
	   xcal(i,m)=xcal(i,2)
152	 continue
c and calculate the asymptotic pdf from t=0 to tmax
	 do 23 i=1,ncalc
	   f=0.0
	   t=xcal(i,2)
	   t1=t-tres1		!needs EXCESS time here
	   do 161 m=1,ncomp
		fc=amp(m)*exp(-t1/tau(m))	!NB needs excess time here
		f=f + fc
		if(logt) fc=fc*t*2.30259		!f(log10(t))=2.3*t*f(t)
		ycal(i,m+2)=fc*fac	!components in ycal(i,3),ycal(i,4),...
161	   continue
	   if(logt) f=f*t*2.30259		!f(log10(t))=2.3*t*f(t)
	   ycal(i,2)=f*fac                !total asymptotic in ycal(i,2)
23	 continue
	endif
c
c Inputs for VHIST:
	if(debug()) then
	  if(nores) jval=2
1125	  print 1121,jval
1121	  format(
     &' Print Xcal(i,j), Ycal(i,j) for j=1,',i2,': i = i1,i2 [skip] = ')
	   read 4,i1,i2
4	   format(2i8)
	   if(i1.eq.0) goto 1122
	   if(i2.lt.i1) i2=i1
	   do 1123 i=i1,i2
	   do 1123 j=1,jval
		print 1124,i,j,xcal(i,j),ycal(i,j)
		if(discprt) write(8,1124) i,j,xcal(i,j),ycal(i,j)
1124		format('i,j = ',2i8,'  x,y = ',2g13.6)
1123	   continue
	   goto 1125
1122	   continue
	endif
	if(iplot.eq.1.or.iplot.eq.2) then
	   isetcol=0
	   icurvc(1)=1		!total
	   iline(1)=0		!total curve continuous
	   ncal(1)=ncalc
	   do 22 j=2,ncomp+2		!ncomp+2 to allow for total asymptotic
	    ncal(j)=ncalc
	    icurvc(j)=j
	    iline(j)=2		!short dash for components
22	   continue
	   iline(2)=3	!long dash for total asymptotic
c==	   ncal(2)=ires31
	   ncurvc=1
	   if(icomp.eq.1) then
	     ncurvc=1
	     if(nores) then
		 ncurvc=2		!show also pdf with zero res
		 icurvc(2)=2
	     endif
	   else if(icomp.eq.2) then
	     ncurvc=2
	   else if(icomp.eq.3) then
	     ncurvc=ncomp+2
	   endif
	else if(iplot.eq.3) then
c	   (1)=uncond; (2)=prec gap; (3)=following gap (see POPADJ)
	   isetcol=1
	   icol(1)=9	!blue for histogram
	   icol(11)=12	!red for unconditional
	   icol(12)=9	!blue for fitted curve
	   icol(13)=9	!blue for fitted curve
	   do i=1,3
		ncal(i)=ncalc
	   enddo
	   iline(1)=2           !short dash for unconditional
c	default display
	   ncurvc=2
	   if(itype.eq.1.or.itype.eq.3) then
	     icurvc(1)=1				!unconditional
	     icurvc(2)=2				!prec gap
	     iline(1)=2
	     iline(2)=0           !continuous for preceding gap
	     print 40
40	     format(' Calculated curves shown:',/,
     &' (1) Unconditional pdf of apparent openings (short dash)',/,
     &' (2) pdf for openings preceded by spec gaps (contin line)',/,
     & ' O.K. [Y] ? ')
	   else if(itype.eq.2) then
	   	icurvc(1)=1		!unconditional
		icurvc(2)=3		!following gap
	      iline(1)=2
		iline(3)=0           !contin for following gap
	     print 41
41	     format(' Calculated curves shown:',/,
     &' (1) Unconditional pdf of apparent openings (short dash)',/,
     &' (2) pdf for openings followed by spec gaps (contin line)',/,
     & ' O.K. [Y] ? ')
	   endif
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).ne.'N') goto 38
c
	   iline(1)=2           !short dash for unconditional
	   iline(2)=0           !continuous for preceding gap
	   iline(3)=3           !long dash for following gap
	   print 37
37	   format(
     &  ' Options for calculated curves:',/,
     &' (1) Unconditional pdf of apparent openings (short dashes)',/,
     &' (2) pdf for openings preceded by spec gaps (contin line)',/,
     &' (3) pdf for openings followed by spec gaps (long dashes)',/,
     &' Number of calculated curves to be displayed = ')
	   call INPUTi(ncurvc)
	   do 42 i=1,ncurvc
		print 46,i
46		format('&(',i2,') Curve number = ')
	      call INPUTi(icurvc(i))
42	   continue
c
38	   continue
	endif
c
	fmin=0.
	fmax=fmax*1.2		     !value from SETBIN
	itemp=1+ifix(fmax)
	fmax=float(itemp)	!ROUND UP
	ftic=0.5*10.0**ifix(alog10((fmax-fmin)*0.5))
2102	if((fmax-fmin)/ftic.gt.10.) goto 2101
	ftic=ftic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
	xtic=xtic*5.
	if(.not.logt) ftic=ftic*5.	!smaller tics for sqrt scale for now
c
c	LO bin is from Xval(0,j) to Xval(1,j); freq in Yval(0,j)
c	First bin is from Xval(1,j) to Xval(2,j); freq in Yval(1,j)
c	ith bin is from Xval(i,j) to Xval(i+1,j); freq in Yval(i,j)
c	last bin is from Xval(nbin,j) to Xval(nbin+1,j); freq in Yval(nbin,j)
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
	yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
c	xval(0,1)=xval(1,1)	!if no flo- it is checked in VHIST (in MINMAX)
c	xval(nbin+2,1)=xval(nbin+1,1)		!ditto if no fhi bin
	yval(nbin+1,1)=fhi	!=0. if no hi bin
c	if(flo.gt.0.0001) then
c	   xval(0,1)=xval(1,1)-xwbase
c	   if(logt) xval(0,1)=xval(1,1)*(xval(1,1)/xval(2,1))
c	endif
c	yval(nbin+2,1)=fhi	!NB Yval(nbin+1,1) not used- set (as flo, above)
c	if(fhi.gt.0.0001) then
c	   xval(nbin+2,1)=xval(nbin+1,1)+xwbase
c	   if(logt) xval(nbin+2,1)=xval(nbin+1,1)*
c     &	(xval(nbin+1,1)/xval(nbin,1))
cc	  keep ratio for hi bin as for other bins, so same width on log scale
c	   xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
c	 endif
c
c Curves: plot calc distribution (and also asymptotic only as dashed line)
c  Calculation based on HJCVDU.FOR (in \CALC), but need to consider
c also where xmin,xmax,antrue defined (the pdfs are defined only above t=tres
c so can prob simply use float(nfit)=anfit say, rather then antrue)
	lt2=2       !short dash for lo,hi bins
	ilog=0
c	if(logt) ilog=1		!plot vs log(t)
	if(logt) ilog=6		!plot sqrt(y) vs log(t)
c	iscal=0			!use input xmin,xmax etc
	iscal=1		!scale internally
	ndv1=511	!dimensions as for earlier versions
	ndimd=1
	ndc1=2048
	ndimc=10
	kmax=20
c	isetcol=0
c Call with thetas() and kmax=kth (not used because ifitype=0)
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,fYLO,fYHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & thetas,ifitype,ncomp,sval,isval,iask,
     & isetcol,ndv1,ndc1,kth,iver)
c
c where next?
994	continue
	if(iplot.ne.4) then
	   iopt=3
	   print 18,iopt
18	   format(
     & ' (1) Display again, but recalc points for new Xmin,Xmax',/,
     & ' (2) Display again (original display)',/,
     & ' (3) Do a different plot',/,
     & ' Option number [',i2,'] = ')
	   i=iopt
	   call INPUTi(i)
	   if(i.ge.1.and.i.le.3) iopt=i
	else if(iplot.eq.4) then
	   iopt=4
	   if(ncurvd.eq.3) then
		print 181,iopt
181		format(
     & ' (1) Display again, but recalc points for new Xmin,Xmax',/,
     & ' (2) Display again (original display)',/,
     & ' (3) Show also the observations for preceding and following gap'
     &	,/,
     & ' (4) Do a different plot',/,
     & ' Option number [',i2,'] = ')
	   else if(ncurvd.eq.5) then
		print 182,iopt
182		format(
     & ' (1) Display again, but recalc points for new Xmin,Xmax',/,
     & ' (2) Display again (original display)',/,
     & ' (3) Show the observations only for adjacent gap (green)'
     &	,/,
     & ' (4) Do a different plot',/,
     & ' Option number [',i2,'] = ')
	   endif
	   call INPUTi(iopt)
	   if(iopt.eq.3.and.ncurvd.eq.5) then
		ncurvd=3
		do j=1,ncurvd
		   icurvd1(j)=j+2		!=3,4,5
		enddo
		replot=.true.
		goto 86		!back to vplot
	   else if(iopt.eq.3.and.ncurvd.eq.3) then
		ncurvd=5
		do j=1,ncurvd
	 	   icurvd1(j)=j
		enddo
		replot=.true.
		goto 86		!back to vplot
	   endif
c=	   if(iopt.eq.4) iplot=3 	!====reason?? ===surely iplot=4 ?
	endif
c
	if(ALLOCATED(xval1)) then
	   DEALLOCATE(XVAL1,YVAL1,XCAL1,YCAL1,weight,icurvw)
	endif
	if(iopt.eq.3) then
	   goto 993
	endif
	if(iopt.eq.1) then
	   iscal=0		!use current xmin,xmax etc
	   goto 19		!recalc 512 points between xmin,xmax
	else if(iopt.eq.2) then
	   iscal=1
	   goto 19           !original display
	endif
c
993	continue
	if(iset.lt.nset) then
	   iset=iset+1
	else
	   iplot=iplot+1
	   iset=1
	endif
	if(iplot.gt.6) iplot=6
	goto 226
c
999	continue
	if(allocated(thetas)) deallocate(thetas)
	RETURN
	end

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
	character title*74
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
	print 1,title
	if(discprt) write(8,1) title
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
	   call BELL(2)
	   print 2,i,nx(i),ny(i)
2	   format(' ERROR in PRANGE: range #',i2,' nx,ny = ',2i8)
	   pause
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
	   print 426,i,ylo(i),yhi(i),xm,n,ym,sd
	   if(discprt) write(8,426) i,ylo(i),yhi(i),xm,n,ym,sd
426	   format(i3,1x,f10.3,' to ',f10.3,2x,f11.4,3x,i5,2(3x,g12.5))
	  else if(yhi(i).ge.1.e6.and.yhi(i).lt.3.e10) then
	   print 40,i,ylo(i),yhi(i),xm,n,ym,sd
	   if(discprt) write(8,40) i,ylo(i),yhi(i),xm,n,ym,sd
40	   format(i3,f13.1,' to ',f13.1,1x,f11.4,3x,i5,2(3x,g12.5))
	  else	!assume yhi(i)=3.1536e10msec=1 year as set above
	   print 42,i,ylo(i),xm,n,ym,sd
	   if(discprt) write(8,42) i,ylo(i),xm,n,ym,sd
42	   format(i3,1x,f10.3,' to   1 year  ',2x,f11.4,3x,i5,2(3x,g12.5))
	  endif
	 else
	  weight(i,j)=0.
	  if(yhi(i).lt.3.e10) then
         print 427,i,ylo(i),yhi(i),n
         if(discprt) write(8,427) i,ylo(i),yhi(i),n
427	   format(i3,1x,f10.3,' to ',f10.3,16x,i5)
	  else
         print 43,i,ylo(i),n
         if(discprt) write(8,43) i,ylo(i),n
43	   format(i3,1x,f10.3,' to   1 year  ',16x,i5)
	  endif
	 endif
428	continue
      print 3,noptot
      if(discprt) write(8,3) noptot
3	format(32x,' Total   ',i8)
	RETURN
	end


	subroutine PCRANGE(title,ylo,yhi,nval,ny,den1,ycal,xcal,
     & ndc1,ndimc,j)
c To print calc conditional means
	character title*74
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
	print 1,title
	if(discprt) write(8,1) title
1	format(1x,a74,/,7x,' Gap length: range',7x,
     & 'Mean gap    P[tlo<t<thi]  N*P   Mean open time')
c
	do 428 i=1,nval
	  enp=(float(ntot))*den1(i)
	  if(yhi(i).lt.3.e10) then
c	   print 426,i,ylo(i),yhi(i),xm,n,ym,sd
	   print 426,i,ylo(i),yhi(i),xcal(i,j),den1(i),enp,ycal(i,j)
	   if(discprt) write(8,426) i,ylo(i),yhi(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
426	   format(i4,1x,f10.3,' to ',f10.3,2x,f11.4,1x,f8.5,2x,
     &	f10.1,3x,g13.6)
	  else	!assume yhi(i)=3.1536e10 msec=1 year as set above
	   print 42,i,ylo(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
	   if(discprt) write(8,42) i,ylo(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
42	   format(i3,1x,f10.3,' to   1 year  ',2x,f11.4,1x,f8.5,2x,
     &	f10.1,3x,g13.6)
	endif
428	continue
	RETURN
	end

	subroutine DEBPRT(jn,nint,iset,tint,ampl,iprops,nd1,nd2)
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	integer nint(10)
	logical discprt
	common/dp/discprt
c
	i1=jn-8
	if(i1.lt.1) i1=1
	i2=jn+5
	if(i2.gt.nint(iset)) i2=nint(iset)
	do i=i1,i2
         print 80,i,tint(i,iset),ampl(i,iset),iprops(i,iset)
         if(discprt) write(8,80) i,tint(i,iset),ampl(i,iset),
     &	 iprops(i,iset)
80	   format(4x,i8,2g13.6,3x,i4)
	enddo
	RETURN
	end
