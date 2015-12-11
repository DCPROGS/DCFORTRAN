	subroutine CORCALS(yval,ygap,nyval,ndimy,nintt,yhio,yhig,
     & ymaxo,ymaxg,idiskq,imode)
c==USED ONLY FOR OPEN-SHUT NOW -EXCLUSION OF OUTLIERS ADDED
C==AS IN NEW CORCAL; NEEDS ALSO 'WITHIN BURST' CALC?
c Special version of CORCAL to calculate correlation of open period
c length with preceding shut time, using Ygap() array defined in CDIST1
c (sep version, CORCALJ.for, used for jumps in CDISTJ).
c ygap(i) contains the shut time that follows the open period length in
c yval(i), i=1,2,...,nyval (or nyval-1?)
c
c  IMODE=1 for open-shut correlation
c  IMODE=2 for open-shut correlation within bursts?? NOT YET DONE
c
c 11/18/95 10:44am Added upper limits for durations to parameters (yhio, yhig
c for open, shut resp) (so can be used for conditional means too.
C 11/10/95 12:05pm vplot ADDED
c Normally ygap(nyval)=-1000 because last gap in steady state record
c is set to -1000.  So  last open-gap pair that are usable are
c yval(nyval-1) and gap after this=ygap(nyval-1)
c
c Subroutines VAR and GTRANS are in file CORCAL.FOR
c
	real*4 yval(ndimy),ygap(nintt)
	allocatable::yopen,yshut
	real*4 yopen(:),yshut(:)
	ALLOCATABLE::ndf,z,rbar1,ndf1
	real*4 z(:),rbar1(:)
	integer ndf(:),ndf1(:)
	character*1 ans,UC
	logical trans,good
	logical discprt,slock,caplock,debug,pon,debon
	common/dp/discprt
c For VPLOT
	ALLOCATABLE XVAL2,YVAL2,XCAL,YCAL
	real XVAL2(:,:),YVAL2(:,:)		!for VPLOT
	real XCAL(:,:),YCAL(:,:)		!for VPLOT/VHIST
	integer ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
	real syms(1)				!for data
	integer ncal(1),icurvc(1),iline(1)	!for calc curve
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
	logical autplt,fitted,plotonly,interp,ivplot,doframe,landscap
	character*40 titlex,titley
	character*64 title1
	character*20 colhead(30)
	character*70 text		!for ASCWRITE
	character*40 qfile
	common/queue/qfile	!for vplot and vhist
c
	pon()=slock()
	debug()=caplock()
c
	debon=debug()
	ny1=nyval-1
	trans=.false.
c Allocate temporary arrays so Gaussian transformation does not alter
c orig data
	ALLOCATE(yopen(ny1),yshut(ny1))
	ymaxo=-1.e37
	ymaxg=-1.e37
	do i=1,ny1
	   yopen(i)=yval(i)
	   yshut(i)=ygap(i)
	   if(yval(i).gt.ymaxo) ymaxo=yval(i)
	   if(ygap(i).gt.ymaxg) ymaxg=ygap(i)
	enddo
c
	if(imode.eq.2) goto 100
c
	print 7
	if(pon()) print 7
	if(discprt) write(8,7)
7	format(/,/,
     &' CORRELATION OF SHUT TIME WITH FOLLOWING OPEN PERIOD LENGTH',/)
c
c
c Check whether to cut out outliers
	if(yhio.gt.1.e36) then
	   print 65,nyval,ymaxo
65	   format(' Open periods: ',i6,' values, maximum = ',g13.6,/,
     &    ' Exclude any outlying values (check distribution!) [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	else		!value set from .ini
	   ans='Y'
	endif
	if(UC(ans).eq.'Y') then
	   print 371,yhio
371	   format(
     & ' Exclude all open periods longer than Ymax (ms) [',f10.1,'] = ')
	   call INPUTr(yhio)
         if(discprt) write(8,500)nyval,yhio
500	   format(/,
     &   ' Autocorrelation calculated on ',i6,' open periods',/,
     &   '  (values above ',g13.6,' terminate calculation and new',/,
     &   '   correlation coeff started after bad value)')
	else
	   yhio=1.e37
         if(discprt) write(8,501)nyval,ymaxo
501	   format(/,
     &   ' Autocorrelation calculated on ',i6,' open periods',/,
     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
c
	if(yhig.gt.1.e36) then
	   print 66,nyval,ymaxg
66	   format(' Shut times: ',i6,' values, maximum = ',g13.6,/,
     & 	' Exclude any outlying values (check distribution!) [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	else		!value set from .ini
	   ans='Y'
	endif
	if(UC(ans).eq.'Y') then
	   print 67,yhig
67	   format(
     & ' Exclude all shut times longer than Ymax (ms) [',f10.1,'] = ')
	   call INPUTr(yhig)
c	   print 68, nyval,yhig
         if(discprt) write(8,68)nyval,yhig
68	   format(/,
     &   ' Autocorrelation calculated on ',i6,' shut times',/,
     &   '  (values above ',g13.6,' terminate calculation and new',/,
     &   '   correlation coeff started after bad value)')
	else
	   yhig=1.e37
c	   print 502, nyval,ymaxg
	   if(pon()) write(7,502)nyval,ymaxg
         if(discprt) write(8,502)nyval,ymaxg
502	   format(/,
     &   ' Autocorrelation calculated on ',i6,' shut times',/,
     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
c
	print 1
1	format(/,
     &' Calculate shut-open correlation for lags up to n [50]. n = ')
	nlag=50
	call INPUTi(nlag)
	ALLOCATE(ndf(ny1),z(ny1),rbar1(nlag),ndf1(nlag))

3	continue	!return here to repeat after transformation
	call VAR(yopen,ny1,ybaro,varo,1,1,no)  !mean & var for open periods
	call VAR(yshut,ny1,ybarg,varg,1,1,ng)  !mean & var for shut times
c
c Correlate gap with next (lag=1), next but one (lag=2) open period length
c (for lag=1 want ygap(i),yval(i))
	print 4, ybaro,sqrt(varo),no
	if(pon()) write(7,4) ybaro,sqrt(varo),no
      if(discprt) write(8,4)  ybaro,sqrt(varo),no
4	format(
     & ' Mean & SD for open period lengths = ',2g13.6,'  n = ',i6)
	print 15, ybarg,sqrt(varg),ng
	if(pon()) write(7,15)  ybarg,sqrt(varg),ng
      if(discprt) write(8,15) ybarg,sqrt(varg),ng
15	format(' Mean & SD for shut times= ',2g13.6,'  n = ',i6)
	print 5
	if(pon()) write(7,5)
      if(discprt) write(8,5)
5	format(
     & '   Lag   Correlation coeff  Normal dev.  # pooled',
     & '   total d.f.')
	do lag=1,nlag
	   if(debon) then
	      print 20,lag
      	if(discprt) write(8,20) lag
20		format(' Lag = ',i3)
	   endif
	   i=1		!index for yval(=yopen)
	   nr=0
10	   continue
	   if(mod(i-1,50).eq.0) debon=debug()
	   sp=0.0
	   sg=0.0
	   sgg=0.0
	   so=0.0
	   soo=0.0
	   n=0
11	   continue
	   yv1=yshut(i)
	   yv2=yopen(i+lag)
c To be OK, all gaps between ygap(i) and ygap(i+lag-1) must be below yhi limit!
c and all opens between yval(i+1) and ygap(i+lag) must be below yhi limit!
	   good=.true.
	   do m=i,i+lag-1
		if(yshut(m).gt.yhig) good=.false.
		if(yopen(m+1).gt.yhio) good=.false.
	   enddo
c
	   if(good) then
		n=n+1			!number of pairs in current correl coeff
		if(debon) then
	         print 21, i,yv1,yv2
	         if(discprt) write(8,21) i,yv1,yv2
21		   format(' ',i4,' shut = ',g13.6,' open = ',g13.6)
		endif
		sp=sp + yv1*yv2	  !open period and next gap
		sg=sg + yv1
		sgg=sgg + yv1*yv1
		so=so + yv2
		soo=soo + yv2*yv2
	      i=i+1
	      if(i+lag.le.ny1) goto 11
	   else
c complete calc of prev coefficient and initialise for the next
		if(n.gt.3) then
		   en=float(n)
		   nr=nr+1		!number of coeff
		   spp=sp - (sg*so)/en
		   sso=(soo - (so*so)/en)
		   ssg=(sgg - (sg*sg)/en)
		   r=spp/sqrt(sso*ssg)
		   ndf(nr)=n - 3
		   z(nr)=0.5*alog((1.0+r)/(1.0-r))
		   if(debon) then
		      print 22,nr,r,z(nr),n
		      if(discprt) write(8,22) nr,r,z(nr),n
22   			format(' ',i4,' r = ',f8.3,' z = ',f8.3,' n = ',i4)
		   endif
		endif
	      i=i+1
		n=0		!in case this is last obs
	      if(i+lag.le.ny1) goto 10
	   endif
c Evaluate the last correlation coeff after all obs tested (if last cycle
c was good then last one will not have been done)
	   if(n.gt.3) then
		en=float(n)
		nr=nr+1		!number of coeff
		spp=sp - (sg*so)/en
		ssg=(sgg - (sg*sg)/en)
		sso=(soo - (so*so)/en)
		r=spp/sqrt(ssg*sso)
		ndf(nr)=n - 3
		z(nr)=0.5*alog((1.0+r)/(1.0-r))
		if(debon) then
		   print 22,nr,r,z(nr),n
		   if(discprt) write(8,22) nr,r,z(nr),n
		endif
	   endif
c Pool the r values
	   s=0.0
	   nd=0.0
	   do i=1,nr
		s=s + ndf(i)*z(i)
		nd=nd + ndf(i)
	   enddo
	   d=float(nd)
	   zbar=s/d
	   varz=1.0/d
	   dev=zbar/sqrt(varz)
	   s=exp(2.0*zbar)
	   rbar=(s-1.0)/(s+1.0)
	   rbar1(lag)=rbar		!keep for plot
	   ndf1(lag)=nd
	   print 6, lag,rbar,dev,nr,nd
	   if(pon()) write(7,6) lag,rbar,dev,nr,nd
         if(discprt) write(8,6) lag,rbar,dev,nr,nd
6	   format(i5,5x,g13.6,3x,g13.6,2x,i5,5x,i5)
	enddo
c
	ans='Y'
	call DCASK('Plot this correlation',ans,ans)
	if(ans.eq.'Y') then
c Allocations
	   ndv1=nlag
	   ndimd=2
	   ncalc=100
	   ndc1=ncalc
	   ndimc=1
	   ALLOCATE(xval2(ndv1,ndimd),yval2(ndv1,ndimd))
	   ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	   kwi=1
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
c Data
	   ndat(1)=nlag
	   ncurvd=1
	   icurvd(1)=1
	   isym(1)=-7		!circles
	   syms(1)=0.5
	   ijoin(1)=0		!points joined
	   do i=1,nlag
		xval2(i,1)=float(i)
		yval2(i,1)=rbar1(i)
		yval2(i,2)=ndf1(i)      !not plotted -for ASCIO only
	   enddo
c Cal curves
	   ncurvc=1
	   icurvc(1)=1
	   ncal(1)=ncalc
	   dx=float(nlag-1)/float(ncalc-1)
	   do i=1,ncalc
		xcal(i,1)=1.0+float(i-1)*dx
		ycal(i,1)=0.0           !zero line
	   enddo
	   iline(1)=2		!short dash
c
c Misc inputs
	   iscal=1		!scale internally
c	   iscal=4		!so xmin,...,ymax (only) set here
c	   iscal=0		!so xmin,...,ycross (only) set here
	   cbig=2.5
	   ifont=4
	   ifitype=0		!no display of fitted parameters
	   kmax=1			!dimension of theta (not declared!)
	   ilog=0 		!arithmetic
	   ilabel=1		!default pos for axis labels
	   titlex='lag'
	   titley='pooled r'
	   iask=-2
	   ncjump=0
	   nvjump=0
	   autplt=.false.
	   fitted=.false.
	   plotonly=.false.
	   interp=.false.
	   ivplot=.false.
	   doframe=.true.
	   landscap=.true.
	   xlo1=-1		!whole screen
	   ntx=5
	   nty=5
	   itx=1		!ticks orientated normally
	   ity=1
	   isetcol=0		!for VPLOT
	   itrace=0
	   call VPLOT5(XVAL2,YVAL2,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c     & isetcol,itrace,ndv1,ndc1)
	   ans='N'
	   call DCASK('Save correlations as ASCII file',ans,ans)
	   if(ans.eq.'Y') then
		ndata=nlag	!number of lines of data
		ncol=2	!two entries in each line
		icolwid=13		!the minimum col width
		colhead(1)='pooled r'
		colhead(2)='d.f.'
		ndim1=ndv1
		ndim2=ndimd
		if(imode.eq.1) then
		   text='Open period-shut autocorrelation'
		endif
		lentext=nblank(text)
		call ASCWRITE(yval2,ndata,ncol,text,ndim1,ndim2,lentext,
     &		icolwid,colhead,1)
c		call ASCIO(yval2,ndata,jcol,ndim1,ndim2,.true.,
c     &		icolwid,colhead)
	   endif
	   DEALLOCATE(xval2,yval2,xcal,ycal)
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	endif
c
	if(trans) goto 9
c
	print 2
	if(pon()) print 2
	if(discprt) write(8,2)
2	format(/,' After transformation to Gaussian: ')
	call GTRANS(yopen,ny1,ybaro,1,1,ny1)
	call GTRANS(yshut,ny1,ybarg,1,1,ny1)
	trans=.true.
	goto 3
c
c
c  IMODE=2 for open-open correlation using tcrit + ygap() to divide open
c	times in yval() into bursts, and then pooling correl coeff within
c	bursts
100	continue
c ===to be done
c
9	continue
	DEALLOCATE(yopen,yshut)
	DEALLOCATE(ndf,z,rbar1,ndf1)
	RETURN
	end

