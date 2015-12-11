	subroutine CORCAL(YVAL,NYVAL,OHIST,GHIST,BHIST,OBHIST,
     & IDTYPE,idiskq,IDEST,ndimy)
C Version of CORCAL for EKDIST
c 11/07/95 09:21am VPLOT added
c 11/04/95 04:34pm -
c	(1) tidied up
c	(2) normal deviate now calculated via Fisher transform
c	(3) yval1 now allocated
C	(4) Major revision in the way outliers are dealt with (important
c	since one outlier can destroy a clear correlation, or generate
c	an artefactual correlation).  When a value greater than yhi is
c	encountered, calc of the current correllation coef is terminated
c	and value recorded -new one started after ths bad value is passed
c	and at the end all the correlation coeffs pooled (via Fisher Z transform)
c	to give pooled value.
c	(5) Remove option to display transformed values
c
C SUBROUTINE FOR OVERLAY IN EKDIST TO CALC TESTS FOR CORRELATION
C (A)RUNS TEST
C (B)AUTOCORRELATION
C
	dimension YVAL(ndimy)
	ALLOCATABLE yval1
	real*4 yval1(:)
	ALLOCATABLE::ndf,z,rbar1,ndf1
	real*4 z(:),rbar1(:)
	integer ndf(:),ndf1(:)
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
      character*70 text	!for ASCWRITE
c	dimension YVAL1(20480)	!in case some values exc for cor calcs
	character*1 ans,UC
	LOGICAL OHIST,GHIST,BHIST,OBHIST,AUTO,TRANS,pon
	logical discprt,slock,caplock,debug,debon,good
	character*40 qfile
	common/queue/qfile	!for vplot and vhist
	common/dp/discprt
C
	pon()=slock()
	debug()=caplock()
c
c	print 31
c	if(pon()) write(7,31)
c	if(discprt) write(8,31)
c31	FORMAT(/'AUTOCORRELATION CALCULATIONS',/)
c
	ALLOCATE(yval1(nyval))
	ndimy1=nyval	!allocated dimension of yval1
	Nyval1=NYVAL	!COPY IN CASE ALTERED BELOW
300	trans=.false.	!RETURN HERE FOR ANOTHER TEST
	NYVAL=Nyval1	!RESTORE
c Copy data to YVAL1 in case orig data altered by omission of some
c values, or by transformation (need orig data if another test done)
c
	ymax=-1.e37
	ymin=1.e37
	do i=1,nyval
	   yv=yval(i)
	   yval1(i)=yv
	   if(yv.gt.ymax) ymax=yv
	   if(yv.lt.ymin) ymin=yv
	enddo
c
	IF(BHIST.AND.IDTYPE.EQ.11) GOTO 60
c
	print 1
1	FORMAT(' Runs test [Y] ? ')
	read 101,ans
101	format(a1)
	if(UC(ans).EQ.'N') GOTO 13
c===========PROBLEM -RESULT OF RUNS TEST DEPENDS ON WHETHER FIT DONE FIRST
c============NOT THE SAME 2ND TIME!
C RUNS TEST NOW
9	continue
	auto=.false.
	if(.not.(ohist.or.ghist.or.bhist)) goto 91
c If X axis is time ask whether default Xcrit to be used
	print 92
92	format('&Use default X(crit) (0.1, 0.3, 1.0,...ms) [Y] ? ')
	read 101,ans
	auto=UC(ans).ne.'N'
	if(auto) goto 7
91	print 55
55	FORMAT('&Xcrit (type-1.for HELP) = ')
	call INPUTr(xcrit)
	IF(XCRIT.GT.0.) GOTO 7
	print 8
	if(discprt) write(8,8)
8	FORMAT(
     & ' Observations are assigned a value 0 if less than Xcrit',/,
     & '  or 1 if equal or greater than Xcrit. The runs tests counts',/,
     & '  runs of 0 values and of 1 values. If the number of runs',/,
     & '  is smaller than expected for a random process then',/,
     & '  small/large values occur in runs i.e. values are ',/,
     & '  positively correlated',/)
	GOTO 9
7	IF(OBHIST) XCRIT=XCRIT-0.01	!TO PREVENT ROUNDING ERRORS
C
	k=0			!number of xcrit values done if auto
c
94	continue		!return here for next Xcrit if auto
	N0=0		!intialise for current Xcrit
	N1=0
	NRUN=1
	k=k+1
	j=mod(k,2)	!=1 for odd k
	x=10.0**((k-1)/2 - 1)	!=0.1,0.1,1,1,10,10,...
	if(auto) Xcrit=float(j)*x + float(1-j)*3.0*x	!=0.1,.3,1.,3.,...
C
	if(debug()) print 15
15	FORMAT('   i      Yval(i)      m        Nrun',/)
	DO I=1,NYVAL1
	   M=0
	   IF(YVAL1(I).GT.XCRIT) M=1
	   IF(M.EQ.0) N0=N0+1
	   IF(M.EQ.1) N1=N1+1
	   if(m.ne.mlast.and.i.ne.1) then
		nrun=nrun+1
	   endif
	   MLAST=M
	   if(debug()) print 14,I,YVAL1(I),M,NRUN
14	   FORMAT(I8,2X,G13.6,2I8)
	enddo		!end of Yval loop
C
	N=N0+N1
	IF(N.NE.NYVAL1) print 12,n0,n1,n,nyval
12	FORMAT(' ERROR: N NOT EQUAL TO NYVAL n0,n1,n,nyval= ',4i8,/)
	if(.not.auto) goto 93
	if(n0.lt.4) goto 94	!next Xcrit if auto
	if(n1.lt.4) goto 95	!no more Xcrit if auto
93	continue
	A=FLOAT(N)
	A0=FLOAT(N0)
	A1=FLOAT(N1)
	AMEAN=1.0+(2.0*A0*A1/A)
	AVAR=2.0*A0*A1*(2.0*A0*A1-A)/(A**3-A**2)
	Z1=(FLOAT(NRUN)-AMEAN)/SQRT(AVAR)
	print 20,xcrit,N0,N1,NRUN,AMEAN,SQRT(AVAR),Z1
	if(pon()) write(7,20)xcrit,N0,N1,NRUN,AMEAN,SQRT(AVAR),Z1
      if(discprt) write(8,20)xcrit,N0,N1,NRUN,AMEAN,SQRT(AVAR),Z1
20	FORMAT(' Runs test: critical value= ',g13.6,/,
     & ' No of 0, 1 values= ',2i8,/,
     & ' Observed no of runs= ',I8,/,
     & ' Expected no of runs= ',g13.6,': SD = ',G13.6,/,
     & ' Normal deviate= ',G13.6,/)
	if(auto) goto 94	!next Xcrit if auto
c
95	print 69
69	format(' Try another Xcrit value [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') GOTO 9
C
C
13	CONTINUE
C AUTOCORRELATION COEFFICIENT. FIRST CALC FOR LAG=1,2,...,5, BOTH
C WITH ORIG DATA, AND WITH TRANSFORMED DATA (+NORMAL DEVIATE FOR LATTER)
C THEN ASK IF MORE CALCS REQUIRED (E.G. OTHER LAGS, OR LIMITED RANGE
C OF VALUES- N.B. LATTER WILL REMOVE SOME VALUES THUS AFFECTING
C THE EFFECTIVE LAG).
	print 4
4	FORMAT(' Calculate autocorrelation coefficients [Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') goto 999	!RETURN
c
	print 35,nyval,ymax
35	format(1x,i6,' values: maximum value = ',g13.6,/,
     & ' Exclude any outlying values (check distribution!) [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
	   print 37
37	   format(' Exclude all values above Ymax = ')
	   call INPUTr(yhi)
	   print 50, nyval,yhi
	   if(pon()) write(7,50)nyval,yhi
         if(discprt) write(8,50)nyval,yhi
50	   format(/,' Autocorrelation calculated on ',i6,' values',/,
     &   '  (values above ',g13.6,' terminate calculation and new',/,
     &   '   correlation coeff started after bad value)')
	else
	   yhi=1.e37
	   print 501, nyval,ymax
	   if(pon()) write(7,501)nyval,ymax
         if(discprt) write(8,501)nyval,ymax
501	   format(/,' Autocorrelation calculated on ',i6,' values',/,
     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
c
	nlag=50
	print 44,nlag
44	FORMAT('   Calculate for lags 1-n [',i3,']. n = ')
	call INPUTi(nlag)
	call VAR(YVAL1,NYVAL1,YBAR,VARY,1,1,N)  !MEAN AND VAR FOR ORIG DATA
	print 27,YBAR
27	FORMAT('&Mean (for Gaussian transformation)= ',
     & G13.6,'  O.K.[Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   print 28
28	   FORMAT(' mean= ')
	   call INPUTr(ybar)
	endif
c
	ny1=nyval-1
	ALLOCATE(ndf(ny1),z(ny1),rbar1(nlag),ndf1(nlag))
c
3	continue
c***denominator is same for all in CS85 version, namely
c SUM(1,n)[(y-ybar)^2]. i.e.
c	ssy=vary*float(nyval-1)
c===no -won't do -because it will include values > yhi (if any)
	sy=0.0
	ssy=0.0
	n=0
	do i=1,nyval1
	   yv=yval1(i)
	   if(yv.le.yhi) then
		n=n+1
		sy=sy+yv
		ssy=ssy+yv*yv
	   endif
	enddo
	ssy=ssy - sy*sy/float(n)
	ybar1=sy/float(n)
	n1=n		!for print
c***
	print 5
	if(pon()) write(7,5)
      if(discprt) write(8,5)
5	format(
     & '   Lag   Correlation coeff  Normal dev.  # pooled',
     & '   total d.f.')
	do lag=1,nlag
	   if(debon) then
	      print 30,lag
      	if(discprt) write(8,30) lag
30		format(' Lag = ',i3)
	   endif
	   i=1		!index for yval(=yopen)
	   nr=0
10	   continue
	   if(mod(i-1,50).eq.0) debon=debug()
	   sp=0.0
	   s1=0.0
	   s11=0.0
	   s2=0.0
	   s22=0.0
c===alternative direct calc of numerator in sxy
	   sxy=0.0
	   n=0
11	   continue
c to be OK, all values between yv1 and yv2 must be below yhi limit!
	   good=.true.
	   do m=i,i+lag
		if(yval(m).gt.yhi) good=.false.
	   enddo
	   if(good) then
		n=n+1			!number of pairs in current correl coeff
		yv1=yval1(i)
		yv2=yval1(i+lag)
c		if(debon) then
c	         print 21, i,yv1,yv2
c	         if(discprt) write(8,21) i,yv1,yv2
c21		   format(' ',i4,' open1 = ',g13.6,' open2 = ',g13.6)
c		endif
		sp=sp + yv1*yv2	  !open period and next gap
c===alternative direct calc of numerator in sxy
		sxy=sxy + (yv1-ybar1)*(yv2-ybar1)
		s1=s1 + yv1
		s11=s11 + yv1*yv1
		s2=s2 + yv2
		s22=s22 + yv2*yv2
	      i=i+1
c===	      if(i+lag.le.ny1) goto 11
	      if(i+lag.le.nyval1) goto 11
	   else
c complete calc of prev coefficient and initialise for the next
		if(n.gt.3) then
		   en=float(n)
		   nr=nr+1		!number of coeff
		   spp=sp - (s1*s2)/en
		   ss1=(s11 - (s1*s1)/en)
		   ss2=(s22 - (s2*s2)/en)
		   den=sqrt(ss1*ss2)
		   r0=spp/den	!original version
		   r1=spp/ssy
c===alternative direct calc of numerator in sxy: use this now
		   r=sxy/ssy
c
		   ndf(nr)=n - 3
		   z(nr)=0.5*alog((1.0+r)/(1.0-r))
		   if(debon) then
		      print 22,nr,r,z(nr),n
		      if(discprt) write(8,22) nr,r,z(nr),n
22   			format(' ',i4,' r = ',f8.3,' z = ',f8.3,' n = ',i6)
		      print 221,nr,spp,sxy,den,ssy,r0,r1,r
		      if(discprt) write(8,221) nr,spp,sxy,den,ssy,r0,r1,r
221  			format(' spp, sxy, den, ssy = ',4g13.6,/,
     &		' r0=spp/den = ',f8.4,' r1=spp/ssy = ',f8.4,
     &		' r=sxy/ssy = ',f8.4)
		   endif
		endif
	      i=i+1
		n=0		!in case this is last obs
c===	      if(i+lag.le.ny1) goto 10
	      if(i+lag.le.nyval1) goto 10
	   endif
c Evaluate the last correlation coeff after all obs tested (if last cycle
c had good=true then last one will not have been done
	   if(good.and.n.gt.3) then
		en=float(n)
		nr=nr+1		!number of coeff
		spp=sp - (s1*s2)/en
		ss1=(s11 - (s1*s1)/en)
		ss2=(s22 - (s2*s2)/en)
		r=spp/sqrt(ss1*ss2)
c===
		den=sqrt(ss1*ss2)
		r0=spp/den
		r1=spp/ssy
c===alternative direct calc of numerator in sxy
		r=sxy/ssy
c===
		ndf(nr)=n - 3
		z(nr)=0.5*alog((1.0+r)/(1.0-r))
		if(debon) then
		   print 22,nr,r,z(nr),n
		   if(discprt) write(8,22) nr,r,z(nr),n
		   print 221,spp,sxy,den,ssy,r0,r1,r
		   if(discprt) write(8,221) spp,sxy,den,ssy,r0,r1,r
C221  		   format(' spp, sxy, den, ssy = ',4g13.6,/,
C     &	   ' r0=spp/den = ',f8.4,' r1=spp/ssy = ',f8.4,
C     &	   ' r=sxy/ssy = ',f8.4)
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
	enddo		!end of lag=1,...,nlag
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
	   iline(1)=0		!continuous
c
c Misc inputs for vplot
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
		text=
     &	'Autocorrelation'
		lentext=nblank(text)
		ndim1=ndv1
		ndim2=ndimd
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
	if(trans) goto 99
c
C NOW TRANSFORM AND REPEAT CALC FOR NEW DATA- RESULT IN ACG(K)
	CALL GTRANS(YVAL1,NYVAL1,YBAR,1,1,ndimy1)
	TRANS=.TRUE.
	call VAR(YVAL1,NYVAL1,YBAR,VARY,1,1,N) !RECALC FOR TRANSFORMED YVAL
	print 33,YBAR,VARY
	if(pon()) write(7,33)YBAR,VARY
      if(discprt) write(8,33)YBAR,VARY
33	FORMAT(
     & ' Mean and var for transformed values (should be 0,1)= ',
     & 2G13.6,/)
	print 2
	if(pon()) print 2
	if(discprt) write(8,2)
2	format(/,' After transformation to Gaussian: ')
	goto 3
C
99	continue
	DEALLOCATE(ndf,z,rbar1,ndf1)
47	print 301
301	FORMAT( ' Another test [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') GOTO 300
	IF(.NOT.TRANS) goto 999	!RETURN
	GOTO 61
C
C SECTION TO CALC CORRELATION BETWEEN 1ST AND JTH OPENING LENGTH IN
C A BURST. IN THIS CASE (IDTYPE=11) YVAL CONTAINS ALTERNATELY THE
C LENGTHS OF THE 1ST AND JTH OPENINGS (SO NYVAL SHOULD BE EVEN!)
C FIRST CALC FOR UNTRANSFORMED VALUES.
60	IDEST=21
c	do 3621 i=1,nyval	!already copied
c3621	yval1(i)=yval(i)
701	CALL VAR(YVAL1,NYVAL1,YBAR,VARY,1,2,N)  !MEAN & VAR FOR 1ST OPENING
c (N is output from VAR)
	if(pon()) write(7,62)YBAR,SQRT(VARY),N
      if(discprt) write(8,62)YBAR,SQRT(VARY),N
	print 62,YBAR,SQRT(VARY),N
62	FORMAT(' Mean & SD for Ith opening= ',2G13.6,'  N= ',I6,/)
	CALL VAR(YVAL1,NYVAL1,YBAR1,VARY1,2,2,N)  !DITTO FOR JTH OPENING
	if(pon()) write(7,63)YBAR1,SQRT(VARY1),N
      if(discprt) write(8,63)YBAR1,SQRT(VARY1),N
	print 63,YBAR1,SQRT(VARY1),N
63	FORMAT(' Mean & SD for Jth opening= ',2G13.6,'  N= ',I6,/)
	sp=0.0
	do i=1,nyval1-1,2
	   sp=sp + (yval1(I)-ybar)*(yval1(i+1)-ybar1)
	enddo
	r=SP/(float(n-1)*sqrt(vary)*sqrt(vary1))
	z1=0.5*alog((1.0+r)/(1.0-r))	!Fisher transform
	varz=1.0/float(n-3)
	dev=z1/sqrt(varz)
	print 65,r,dev
	if(pon()) write(7,65) r,dev
      if(discprt) write(8,65) r,dev
65	FORMAT( ' Correlation coeff= ',G13.6,/,
     & ' normal deviate= ',G13.6,/)
	IF(TRANS) GOTO 61
	if(pon()) print 66
	if(discprt) write(8,66)
	print 66
66	FORMAT(/,' After transformation to Gaussian: '/)
	CALL GTRANS(YVAL1,NYVAL1-1,YBAR,1,2,ndimy1)	!TRANSFORM Ith OPENINGS
	CALL GTRANS(YVAL1,NYVAL1,YBAR1,2,2,ndimy1)	!TRANSFORM Jth OPENINGS
	TRANS=.TRUE.
	GOTO 701		!REPEAT CALCS FOR TRANSFORMED VALUES
c
61	continue
C idest=21- correlation only
c idest=22- correlation + fit. But if display of transformed variables
c is req here then cannot do fit as histo is lost
c	IF(IDEST.EQ.21) IDEST=31
c	idest=31	!display transformed variables
999	continue
	DEALLOCATE(yval1)
	RETURN
	END


	SUBROUTINE GTRANS(Y,N,YBAR,N1,NS,ndimy)
	dimension Y(ndimy)
C
C TO TRANSFORM EXPONENTIAL VARIATE IN YVAL TO GAUSSIAN
C STARTS AT Y(N1) AND INCREMENTS INDEX BY NS UNTIL Y(N)
	PI=3.1415927
	DO 1 I=N1,N,NS
	Q=EXP(-Y(I)/YBAR)	!NEAR ZERO FOR LONG INTERVALS!
	X=4.0*Q*(1.0-Q)
	IF(X.LT.1.E-32) X=1.E-32	!TO PREVENT LOG(0)
	X=SQRT(-0.5*PI*ALOG(X))
	IF(Q.GT.0.5) X=-X
	Z=X+7.8365E-3*(X**3)-2.881E-4*(X**5)+4.3728E-6*(X**7)
	Y(I)=Z
1	CONTINUE
	RETURN
	END


	SUBROUTINE VAR(Y,N,YBAR,VARY,N1,NS,NVAL)
C	CALCULATES MEAN,YBAR,AND VARIANCE,VARY OF FIRST N
C	ELEMENTS OF Y. SINGLE PASS YOUNGS AND CRAMER METHOD.
C STARTS AT Y(N1) AND INCREMENTS INDEX BY NS UNTIL Y(N)
C
	dimension Y(n)
C
C	SUMY=Y(1)
	SUMY=Y(N1)	!** MODIF FOR N1,N,NS
	SUMYY=0.
	NVAL=1
C
C	DO 1 I=2,N
	DO 1 I=N1+NS,N,NS	!**
	NVAL=NVAL+1		!**
	YI=Y(I)
	FI=FLOAT(I)
	SUMY=SUMY+YI
	SUMYY=SUMYY+(FI*YI-SUMY)**2/(FI*(FI-1))
1	CONTINUE
C
	YBAR=SUMY/FLOAT(NVAL)
	VARY=SUMYY/FLOAT(NVAL-1)
	RETURN
	END


