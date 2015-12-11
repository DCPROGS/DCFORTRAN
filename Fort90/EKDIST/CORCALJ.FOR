	subroutine CORCALJ(yval,ygap,index,indexg,nsweep,nyval,ndimy,nint,
     & idiskq,imode)
c
c Version for jump data (called in CDISTj).
c
c 11/11/95 04:49pm vplot ADDED
c
c Calculate correlation coeff separately for each jump (if more three
c values available).  The get Fisher transform of each
c	z=0.5*ln[(1+r)/(1-r)]
c which is approx normal with variance = 1/(n-3)
c  The calculate weighted mean
c	zbar=SUM[(n-3)z]/SUM[n-3]
c and transform back for pooled value
c	rbar=(exp(2z)-1)/exp(2z)+1)
c variance of which is 1/SUM[n-3]
c
c Special version of CORCAL to calculate correlation of open period
c length with preceding shut time, using Ygap() array defined in CDISTJ
c
c  IMODE=1 for open-shut correlation
c  IMODE=2 for open-open correlation
c  IMODE=3 for shut-shut correlation
c
c ygap(i) contains the shut time that follows the open period length in
c  yval(i), i=1,2,...,nyval (or nyval-1?)
c index(:)	defined so index(i)=jump # in which yval(i) occurs
c indexg(:)	defined so index(i)=jump # in which ygap(i) occurs
c
c Normally ygap(nyval)=-1000 because last gap in steady state record
c is set to -1000.  So  last open-gap pair that are usable are
c yval(nyval-1) and gap after this=ygap(nyval-1)
c
c Subroutines VAR and GTRANS are in file CORCAL.FOR
c
	real*4 yval(ndimy),ygap(nint)
	integer index(nint),indexg(nint)
	allocatable::yopen,yshut
	real*4 yopen(:),yshut(:)
	logical trans,good
	logical discprt,slock,caplock,debug,pon,debon
	character*1 ans,UC
	common/dp/discprt
	ALLOCATABLE::ndf,z,rbar1,ndf1
	real*4 z(:),rbar1(:)
	integer ndf(:),ndf1(:)
c
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
c
	pon()=slock()
	debug()=caplock()
c
	ny1=nyval-1
	debon=debug()
	trans=.false.
c Allocate temporary arrays so Gaussian transformation does not alter
c orig data
	ALLOCATE(yopen(ny1),yshut(ny1))
	ALLOCATE(ndf(nsweep),z(nsweep))
	ymaxo=-1.e37
	ymaxg=-1.e37
	do i=1,ny1
	   yopen(i)=yval(i)
	   yshut(i)=ygap(i)
	   if(yval(i).gt.ymaxo) ymaxo=yval(i)
	   if(ygap(i).gt.ymaxg) ymaxg=ygap(i)
	enddo
c
	print 231
	if(discprt) write(8,231)
231	format(
     & ' WARNING: check stability plots! Instability in data can',/,
     & '  cause false positive correlation (independent of lag)',/)
	if(imode.eq.2) goto 50
	if(imode.eq.3) goto 51
c
	print 7
	if(pon()) print 7
	if(discprt) write(8,7)
7	format(/,/,
     &' CORRELATION OF SHUT TIME WITH FOLLOWING OPEN PERIOD LENGTH',/,
     &' (pooled value of correlation coefficients within each jump',/,
     &' is found via weighted mean of Fisher z-transforms)')
c
c Check whether to cut out outliers
	print 65,nyval,ymaxo
65	format(1x,i6,' open times: maximum value = ',g13.6,/,
     & ' Exclude any outlying values (check distribution!) [N] ? ')
	read 101,ans
101	format(a1)
	if(UC(ans).eq.'Y') then
	   print 37
37	   format(' Exclude all values above Ymax = ')
	   call INPUTr(yhio)
	   print 500, nyval,yhio
	   if(pon()) write(7,500)nyval,yhio
         if(discprt) write(8,500)nyval,yhio
500	   format(/,
     &   ' Autocorrelation calculated on ',i6,' open periods',/,
     &   '  (values above ',g13.6,' terminate calculation and new',/,
     &   '   correlation coeff started after bad value)')
	else
	   yhio=1.e37
	   print 501, nyval,ymaxo
	   if(pon()) write(7,501)nyval,ymaxo
         if(discprt) write(8,501)nyval,ymaxo
501	   format(/,
     &   ' Autocorrelation calculated on ',i6,' open periods',/,
     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
	print 66,nyval,ymaxg
66	format(1x,i6,' shut times: maximum value = ',g13.6,/,
     & ' Exclude any outlying values (check distribution!) [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
	   print 67
67	   format(' Exclude all values above Ymax = ')
	   call INPUTr(yhig)
	   print 68, nyval,yhig
	   if(pon()) write(7,68)nyval,yhig
         if(discprt) write(8,68)nyval,yhig
68	   format(/,
     &   ' Autocorrelation calculated on ',i6,' shut times',/,
     &   '  (values above ',g13.6,' terminate calculation and new',/,
     &   '   correlation coeff started after bad value)')
	else
	   yhig=1.e37
	   print 502, nyval,ymaxg
	   if(pon()) write(7,502)nyval,ymaxg
         if(discprt) write(8,502)nyval,ymaxg
502	   format(/,
     &   ' Autocorrelation calculated on ',i6,' shut times',/,
     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
c
	print 1
1	format(
     &' Calculate shut-open correlation for lags up to N [10]. N = ')
	nlag=50
	call INPUTi(nlag)
	ALLOCATE(rbar1(nlag),ndf1(nlag))

3	continue	!return here to repeat after transformation
c	call VAR(yopen,ny1,ybaro,varo,1,1,no)  !mean & var for open periods
c	call VAR(yshut,ny1,ybarg,varg,1,1,ng)  !mean & var for shut times
c
c Correlate gap with next (lag=1), next but one (lag=2) open period length
c (for lag=1 want ygap(i),yval(i))
c	print 4, ybaro,sqrt(varo),no
c	if(pon()) write(7,4) ybaro,sqrt(varo),no
c      if(discprt) write(8,4)  ybaro,sqrt(varo),no
c4	format(
c     & ' Mean & SD for open period lengths = ',2g13.6,'  N = ',i6)
	print 5
	if(pon()) write(7,5)
      if(discprt) write(8,5)
5	format(
     & '   Lag   Correlation coeff  Normal dev.  # pooled',
     & '   total d.f.')
c First defined gap is ygap(1), and open period after it (lag=1) is yval(2);
c the open period after (lag=2) that is yval(3) . . .
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
	   jnum1=indexg(i)   		!i=1,2
	   jnum2=index(i+lag)		!=2,3 for lag=1; =3,4 for lag=2
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
	   if(jnum1.eq.jnum2.and.good) then
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
	   titley='r (shut-open)'
	   ireturn=72		!return to 72 after plot done
	   goto 100			!do plot
	endif
72	continue			!return here after plot
c
	if(trans) goto 9		!Finished open-shut correlation
c
	call VAR(yopen,ny1,ybaro,varo,1,1,no)  !mean & var for open periods
	call VAR(yshut,ny1,ybarg,varg,1,1,ng)  !mean & var for shut times
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
c
c Now section for open=open correlation
50	continue
	print 71
	if(pon()) print 71
	if(discprt) write(8,71)
71	format(/,/,
     &' Correlation of open period with following open period length',/,
     &' (pooled value of correlation coefficients within each jump',/,
     &' is found via weighted mean of Fisher z-transforms)')
	print 70,nyval,ymaxo
70	format(1x,i6,' open periods: maximum value = ',g13.6,/,
     & ' Exclude any outlying values (check distribution!) [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
	   print 37
c37	   format(' Exclude all values above Ymax = ')
	   call INPUTr(yhio)
	   print 500, nyval,yhio
	   if(pon()) write(7,500)nyval,yhio
         if(discprt) write(8,500)nyval,yhio
c500	   format(/,
c     &   ' Autocorrelation calculated on ',i6,' open periods',/,
c     &   '  (values above ',g13.6,' terminate calculation and new',/,
c     &   '   correlation coeff started after bad value)')
	else
	   yhio=1.e37
	   print 501, nyval,ymaxo
	   if(pon()) write(7,501)nyval,ymaxo
         if(discprt) write(8,501)nyval,ymaxo
c501	   format(/,
c     &   ' Autocorrelation calculated on ',i6,' open periods',/,
c     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
c
	nlag=50
	print 31,nlag
31	format(
     &' Calculate correlation for lags up to lag=n [',
     &	i3,']. n = ')
	call INPUTi(nlag)
	ALLOCATE(rbar1(nlag),ndf1(nlag))

33	continue	!return here to repeat after transformation
c	call VAR(yopen,ny1,ybaro,varo,1,1,no)  !mean & var for open periods
c	call VAR(yshut,ny1,ybarg,varg,1,1,ng)  !mean & var for shut times
cc
c Correlate open period with next (lag=1), next but one (lag=2) open period
c length (for lag=1 want ygap(i),yval(i))
c	print 4, ybaro,sqrt(varo),no
c	if(pon()) write(7,4) ybaro,sqrt(varo),no
c      if(discprt) write(8,4)  ybaro,sqrt(varo),no
c4	format(
c     & ' Mean & SD for open period lengths = ',2g13.6,'  N = ',i6)
c	print 5, ybarg,sqrt(varg),ng
c	if(pon()) write(7,5)  ybarg,sqrt(varg),ng
c      if(discprt) write(8,5) ybarg,sqrt(varg),ng
c5	format(' Mean & SD for shut times= ',2g13.6,'  N = ',i6,/,
c     & '   Lag   Correlation coeff  Normal dev.  # pooled',
c     & '   total d.f.')
	print 35
	if(pon()) write(7,35)
      if(discprt) write(8,35)
35	format(
     & '   Lag   Correlation coeff  Normal dev.  # pooled',
     & '   total d.f.')
c First defined open period is yval(1), and open period after it (lag=1)
c is yval(2); the open period after that (lag=2) is yval(3) . . .
	do lag=1,nlag
	   if(debon) then
	      print 20,lag
      	if(discprt) write(8,20) lag
c20		format(' Lag = ',i3)
	   endif
	   i=1		!index for yval(=yopen)
	   nr=0
310	   continue
	   if(mod(i-1,50).eq.0) debon=debug()
	   sp=0.0
	   s1=0.0
	   s11=0.0
	   s2=0.0
	   s22=0.0
	   n=0
311	   continue
	   yv1=yopen(i)
	   yv2=yopen(i+lag)
	   jnum1=index(i)   		!i=1,2
	   jnum2=index(i+lag)		!=2,3 for lag=1; =3,4 for lag=2
c To be OK, all values between yv1 and yv2 must be below yhi limit!
	   good=.true.
	   do m=i,i+lag
		if(yopen(m).gt.yhio) good=.false.
	   enddo
	   if(jnum1.eq.jnum2.and.good) then
		n=n+1			!number of pairs in current correl coeff
		if(debon) then
	         print 321, i,yv1,yv2
	         if(discprt) write(8,321) i,yv1,yv2
321		   format(' ',i4,' open1 = ',g13.6,' open2 = ',g13.6)
		endif
		sp=sp + yv1*yv2	  !open period and next gap
		s1=s1 + yv1
		s11=s11 + yv1*yv1
		s2=s2 + yv2
		s22=s22 + yv2*yv2
	      i=i+1
	      if(i+lag.le.ny1) goto 311
	   else
c complete calc of prev coefficient and initialise for the next
		if(n.gt.3) then
		   en=float(n)
		   nr=nr+1		!number of coeff
		   spp=sp - (s1*s2)/en
		   ss1=(s11 - (s1*s1)/en)
		   ss2=(s22 - (s2*s2)/en)
		   r=spp/sqrt(ss1*ss2)
		   ndf(nr)=n - 3
		   z(nr)=0.5*alog((1.0+r)/(1.0-r))
		   if(debon) then
		      print 22,nr,r,z(nr),n
		      if(discprt) write(8,22) nr,r,z(nr),n
c22   			format(' ',i4,' r = ',f8.3,' z = ',f8.3,' n = ',i4)
		   endif
		endif
	      i=i+1
		n=0		!in case this is last obs
	      if(i+lag.le.ny1) goto 310
	   endif
c Evaluate the last correlation coeff after all obs tested (if last cycle
c was good then last one will not have been done)
	   if(n.gt.3) then
		en=float(n)
		nr=nr+1		!number of coeff
		spp=sp - (s1*s2)/en
		ss1=(s11 - (s1*s1)/en)
		ss2=(s22 - (s2*s2)/en)
		r=spp/sqrt(ss1*ss2)
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
c6	   format(i5,5x,g13.6,3x,g13.6,2x,i5,5x,i5)
	enddo
c
	ans='Y'
	call DCASK('Plot this correlation',ans,ans)
	if(ans.eq.'Y') then
	   titley='r (open-open)'
	   ireturn=73		!return to 73 after plot done
	   goto 100			!do plot
	endif
73	continue			!return here after plot
c
	if(trans) goto 9		!Finished open-shut correlation
c
	call VAR(yopen,ny1,ybaro,varo,1,1,no)  !mean & var for open periods
	print 2
	if(pon()) print 2
	if(discprt) write(8,2)
c2	format(/,' After transformation to Gaussian: ')
	call GTRANS(yopen,ny1,ybaro,1,1,ny1)
	trans=.true.
	goto 33
c
c
c Now section for shut-shut correlation (imode=3)
51	continue
	debon=debug()
	print 52
	if(pon()) print 52
	if(discprt) write(8,52)
52	format(/,/,
     &' Correlation of shut time with following shut time length',/,
     &' (pooled value of correlation coefficients within each jump',/,
     &' is found via weighted mean of Fisher z-transforms)')
	print 66,nyval,ymaxg
c66	format(1x,i6,' shut times: maximum value = ',g13.6,/,
c     & ' Exclude any outlying values (check distribution!) [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
	   print 67
c67	   format(' Exclude all values above Ymax = ')
	   call INPUTr(yhig)
	   print 68, nyval,yhig
	   if(pon()) write(7,68)nyval,yhig
         if(discprt) write(8,68)nyval,yhig
c68	   format(/,
c     &   ' Autocorrelation calculated on ',i6,' shut times',/,
c     &   '  (values above ',g13.6,' terminate calculation and new',/,
c     &   '   correlation coeff started after bad value)')
	else
	   yhig=1.e37
	   print 502, nyval,ymaxg
	   if(pon()) write(7,502)nyval,ymaxg
         if(discprt) write(8,502)nyval,ymaxg
c502	   format(/,
c     &   ' Autocorrelation calculated on ',i6,' shut times',/,
c     &   '  (no values excluded: maximum = ',g13.6,')')
	endif
c
	nlag=50
	print 31,nlag
c31	format(
c     &' Calculate correlation for lags up to lag=n [',
c     &	i3,']. n = ')
	call INPUTi(nlag)
	ALLOCATE(rbar1(nlag),ndf1(nlag))
c
53	continue	!return here to repeat after transformation
	print 35
	if(pon()) write(7,35)
      if(discprt) write(8,35)
c35	format(
c     & '   Lag   Correlation coeff  Normal dev.  # pooled',
c     & '   total d.f.')
c First defined shut time is ygap(1), and shut time after it (lag=1)
c is ygap(2)
	do lag=1,nlag
	   if(debon) then
	      print 20,lag
      	if(discprt) write(8,20) lag
c20		format(' Lag = ',i3)
	   endif
	   i=1		!index for yval(=yopen)
	   nr=0
54	   continue
	   if(mod(i-1,50).eq.0) debon=debug()
	   sp=0.0
	   s1=0.0
	   s11=0.0
	   s2=0.0
	   s22=0.0
	   n=0
55	   continue
	   yv1=yshut(i)
	   yv2=yshut(i+lag)
	   jnum1=indexg(i)   		!i=1,2
	   jnum2=indexg(i+lag)		!=2,3 for lag=1; =3,4 for lag=2
c To be OK, all values between yv1 and yv2 must be below yhi limit!
	   good=.true.
	   do m=i,i+lag
		if(yshut(m).gt.yhig) good=.false.
	   enddo
	   if(jnum1.eq.jnum2.and.good) then
		n=n+1			!number of pairs in current correl coeff
		if(debon) then
	         print 56, i,yv1,yv2
	         if(discprt) write(8,56) i,yv1,yv2
56		   format(' ',i4,' shut1 = ',g13.6,' shut2 = ',g13.6)
		endif
		sp=sp + yv1*yv2
		s1=s1 + yv1
		s11=s11 + yv1*yv1
		s2=s2 + yv2
		s22=s22 + yv2*yv2
	      i=i+1
	      if(i+lag.le.ny1) goto 55
	   else
c complete calc of prev coefficient and initialise for the next
		if(n.gt.3) then
		   en=float(n)
		   nr=nr+1		!number of coeff
		   spp=sp - (s1*s2)/en
		   ss1=(s11 - (s1*s1)/en)
		   ss2=(s22 - (s2*s2)/en)
		   r=spp/sqrt(ss1*ss2)
		   ndf(nr)=n - 3
		   z(nr)=0.5*alog((1.0+r)/(1.0-r))
		   if(debon) then
		      print 22,nr,r,z(nr),n
		      if(discprt) write(8,22) nr,r,z(nr),n
c22   			format(' ',i4,' r = ',f8.3,' z = ',f8.3,' n = ',i4)
		   endif
		endif
	      i=i+1
		n=0		!in case this is last obs
	      if(i+lag.le.ny1) goto 54
	   endif
c Evaluate the last correlation coeff after all obs tested (if last cycle
c was good then last one will not have been done)
	   if(n.gt.3) then
		en=float(n)
		nr=nr+1		!number of coeff
		spp=sp - (s1*s2)/en
		ss1=(s11 - (s1*s1)/en)
		ss2=(s22 - (s2*s2)/en)
		r=spp/sqrt(ss1*ss2)
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
c6	   format(i5,5x,g13.6,3x,g13.6,2x,i5,5x,i5)
	enddo
c
	ans='Y'
	call DCASK('Plot this correlation',ans,ans)
	if(ans.eq.'Y') then
	   titley='r (shut-shut)'
	   ireturn=74		!return to 74 after plot done
	   goto 100			!do plot
	endif
74	continue			!return here after plot
c
	if(trans) goto 9		!Finished shut-shut correlation
c
	call VAR(yshut,ny1,ybarg,varg,1,1,ng)  !mean & var for shut times
	print 2
	if(pon()) print 2
	if(discprt) write(8,2)
c2	format(/,' After transformation to Gaussian: ')
	call GTRANS(yshut,ny1,ybarg,1,1,ny1)
	trans=.true.
	goto 53
c
9	continue
	DEALLOCATE(yopen,yshut)
	DEALLOCATE(ndf,z,rbar1,ndf1)
	RETURN
c
c Separate section to do plots (after which return to 72,73 or 74
100	continue
c
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
	   idiskq=-1
	   ilog=0 		!arithmetic
	   ilabel=1		!default pos for axis labels
	   titlex='lag'
c	   titley='pooled r'  !(now defined above)
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
c  IMODE=1 for open-shut correlation
c  IMODE=2 for open-open correlation
c  IMODE=3 for shut-shut correlation
		if(imode.eq.1) then
		   text='Jump: open period-shut autocorrelation'
		else if(imode.eq.2) then
		   text='Jump: open period-open period autocorrelation'
		else if(imode.eq.3) then
		   text='Jump: shut-shut autocorrelation'
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
c
	if(ireturn.eq.72) then
	   goto 72
	else if(ireturn.eq.73) then
	   goto 73
	else if(ireturn.eq.74) then
	   goto 74
	endif
c
	END


