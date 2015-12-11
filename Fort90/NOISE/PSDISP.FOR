	subroutine PSDISP(PS,npspec,nspec,nsize,rate,res,
     & varv,nchan,cmean,varsav,ybsav,timsav,ymsav1,cmean1,
     & ymsav2,cmean2,ibcor,idiskq,idest)
c
c To finish calcs and display/plot spectrum in PSREAL
c Also sep section to display mean vs time and var vs mean (idest=1); this
c section now includes the calcs that were in old VMCALC subroutine
c
c Now use VPLOTR rather than VPLOT1 to display spectra because it will
c take up to 2048 points (facilities in VPLOTR for relaxations not used!)
c
	real*4 PS(2048),freq(2048)
	real*4 varsav(1024),ybsav(1024),timsav(1024)
	real*4 ymsav1(1024),ymsav2(1024)
	real*4 varplt(1024),ymplot(1024)
c For disc read:
	integer*2 NSPECT,nxtrec,jdisc,jfirst
	character*12 name
	integer*2 istrc1,nrec1,npspec1,nspec1
	character cdate1*12
	character ndev*2,dirfil*15,nfil*12
	character*56 title1
c
c for VPLOTR
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real*4 Xval(2048,4),Yval(2048,4)
	real*4 xcal(2048,4),ycal(2048,4)
	dimension ndat(4),icurvd(4),isym(4),ijoin(4)	!for data
	dimension ncal(4),icurvc(4),iline(4)		!for calc curve
	real symsiz(4)				!for data
	logical interp,ivplot
c	integer*2 videotyp
	character*40 titlex,titley
	character*44 title
	logical fitted,doframe,draft,autplt,landscap
	real theta(20) !not used but needed for VplotR call (fix ifitype for PS!)
c
	logical pon,debug,slock,caplock
	character*1 ans,UC
	logical discprt
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	ndim=1024		!dimension of timsav,varsav,ybsav etc
	ndimd=4		!dimension of Xval,Yval
	ndimc=4		!dimension of xcal,ycal
	cbig=2.5
	ifont=4
	ifitype=0	!no display of fitted parameters (fixed for spectra in PSFIT)
	itit=0		!no title yet
	iask=-2		!leave with graph erased, no query
	itx=1			!normal tic orientation
	ity=1			!normal tic orientation
	ilabel=1
	ixlo=-1		!whole screen
	doframe=.true.
	landscap=.true.
	autplt=.false.
c Switch off options for relaxations and I/V curves:
	ncjump=0
	nvjump=0
c	iy1c=-1		!no jump logos
c
c	 if(idest.eq.1) goto 103		!var vs mean
c
	if(nspec.le.0) then
	   print 31
31	   format(' No data for spectra')
	   call bell(3)
	   stop
	endif
c
	SCAL=(2.0*float(nsize))/rate
C SCALE FAC FOR FFT OUTPUT. NSIZE**2 TIMES FACTOR FOR INTEGER ROUTINE
C BECAUSE MONRO'S COMPONENTS DIVIDED BY NSIZE
	SCAL=SCAL/(float(nspec))	!to form mean spectrum
c====	SCAL=SCAL*calfac2		!converts PS (now pA**2?) to amp**2 (see PSIN)
	SCAL=SCAL*1.e-24		!converts PS (now pA**2?) to amp**2
c===	scal=scal*corfac		!correct for masking -must be done separately
c					!for each PS as each may have diff # of bad points
c
	do 151 i=1,Npspec
	freq(i)=float(i)*res
	PS(i)=PS(i)*scal	!mean scaled spectrum in amp**2
151	continue
c	if(debug()) then
c  print 152,scal,nspec,nsize,npspec
c	  if(discprt) write(8,152) scal,nspec,nsize,npspec
c152	  format(' scal,nspec,nsize,npspec = ',g13.6,3i8,/,
c     & ' Type FREQ,PS for i=i1,i2= ')
c	  read 153,i1,i2
c153	  format(2i8)
c	  do 154 i=i1,i2
c	  print 155,i,freq(i),ps(i)
c155	  format(i8,2g13.6)
c154	  continue
c	endif
c
	varv=varv/float(nspec)
	cmean=cmean/float(nspec)
	cmean1=cmean1/float(nspec)
	cmean2=cmean2/float(nspec)
	print 43,varv*1.e-24,sqrt(varv)
	if(pon()) write(7,43) varv*1.e-24,sqrt(varv)
      if(discprt) write(8,43) varv*1.e-24,sqrt(varv)
43	FORMAT( ' Overall variance (amp**2)= ',G13.6,/,
     & ' Overall standard deviation (pA)= ',G13.6)
c
	if(ibcor.eq.1) then
	   print 45,cmean
	   if(pon()) write(7,45)cmean
         if(discprt) write(8,45)cmean
45	   format(' Overall mean current (pA)= ',g13.6)
	else if(ibcor.eq.2.or.ibcor.eq.3) then
	   imeanc=2		!default
	   print 44,cmean,cmean1,cmean2
	   if(pon()) write(7,44)cmean,cmean1,cmean2
         if(discprt) write(8,44)cmean,cmean1,cmean2
44	   format(
     &  ' (1) Overall mean current from observed points (pA)= ',g13.6,/,
     &  ' (2) Same but with fitted asymptote subtracted (pA)= ',g13.6,/,
     &  ' (3) Overall mean current from fitted points (pA)  = ',g13.6)
	   print 441,imeanc
441	   format(
     &  ' Which of these to be used for plots [',i2,'] = ')
c	   read 15,i
	   call INPUTi(imeanc)
c	   if(i.ge.1.and.i.le.3) imeanc=i
         if(pon()) write(7,442) imeanc
         if(discprt) write(8,442) imeanc
442	   format('&Option used for plots = ',i4)
	endif
c
c Following same for all calc curves
	ncalc=301
	do 51 i=1,4
	iline(i)=0		!continuous
	ncal(i)=ncalc
51	continue	!end of j loop
c
	print 1,nspec
1	format(' List',i4,' means, times and variances [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 2
	if(ibcor.eq.1) then
         print 3
         if(pon()) write(7,3)
         if(discprt) write(8,3)
3	   format(
     & ' Spec no     time (s)   mean current (pA)   var (pA**2)')
	   do 4 i=1,nspec
	   print 5,i,timsav(i),ybsav(i),varsav(i)
	   if(pon()) write(7,5)i,timsav(i),ybsav(i),varsav(i)
         if(discprt) write(8,5)i,timsav(i),ybsav(i),varsav(i)
5	   format(i8,2x,g13.6,3x,g13.6,5x,g13.6)
4	   continue
	else if(ibcor.eq.2.or.ibcor.eq.3) then
         print 53
         if(pon()) write(7,53)
         if(discprt) write(8,53)
53	   format(
     & ' Spec no  time (s)          mean current (pA)',
     & '            var (pA**2)',
     &/,'                      mean(1)    mean(2)    mean(3)')
	   do 41 i=1,nspec
	   print 52,i,timsav(i),ybsav(i),ymsav1(i),ymsav2(i),varsav(i)
	   if(pon()) write(7,52)i,timsav(i),ybsav(i),ymsav1(i),
     &	ymsav2(i),varsav(i)
         if(discprt) write(8,52)i,timsav(i),ybsav(i),ymsav1(i),
     &	ymsav2(i),varsav(i)
52	   format(i6,1x,g12.5,1x,4g12.5)
41	   continue
	endif
2	continue
c
261	continue
	varvc=0.	!control values
	cmeanc=0.
c NB if imeanc=2,3 control current already allowed for
	if(ibcor.eq.1.or.(ibcor.ge.2.and.imeanc.eq.1)) then
	   print 26
26	   format(
     & ' Subtract control values from variance & mean current [Y] ? ')
	else if(ibcor.ge.2.and.imeanc.gt.1) then
	   print 262
262	   format(
     & ' Subtract control value from variance [Y] ? ')
	endif
	read 101,ans
	if(UC(ans).eq.'N') goto 13
	i1=1
	print 34,i1
34	format(
     & ' (1) Control values to be typed in next',/,
     & ' (2) Control values to be read from disc (for specified PS)',/,
     & '  Option number [',i2,'] = ')
c	read 15,i
	call INPUTi(i1)
c	if(i.eq.1.or.i.eq.2) i1=i
15	format(i8)
	if(i1.eq.1) goto 16
c Section to read control values from noise data disc
	ndev='A:'		!default = floppy
	call DISCNUM(id,ndev,-1,1)	!which disc?
	if(id.eq.-1) goto 13
	NFIL='PSDIR.DAT'
	DIRFIL=ndev//'\'//nfil
      OPEN(unit=14,file=DIRFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128,err=200)
	read(14,rec=1)Nspect,nxtrec,Jdisc,jfirst,name

	print 42,name,jdisc,jfirst,nspect
42	format(1x,a12,' Noise data disc number ',i2,/,
     & '  -contains experiments ',i4,' to ',i4,/,' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   CLOSE(unit=14)
	   goto 261
	endif
	print 341
341	format('&Spectrum number (for unsubtracted spectrum) = ')
c	read 15,ispec
	call INPUTi(ispec)
	ndr=ispec-int4(jfirst)+2
	READ(14,rec=ndr)cdate1,TITLE1,istrc1,nrec1,npspec1,nspec1,
     & res1,filt1,calfac1,varv1,cmean10,filt12,calfac12,cmean11,cmean21
	CLOSE(unit=14)
	if(ibcor.eq.1.or.(ibcor.ge.2.and.imeanc.eq.1)) then
	   print 421,varv1,cmean10
421	   format(
     & ' Control var, mean current (pA) = ',2g13.6,' O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 261
	   cmeanc=cmean10		!if OK
	   varvc=varv1
	else if(ibcor.ge.2.and.imeanc.gt.1) then
	   print 422,varv1
422	   format(
     & ' Control variance (pA**2) = ',g13.6,' O.K. [Y] ? ')
	   if(UC(ans).eq.'N') goto 261
	   varvc=varv1
	endif
	goto 13		!end of read of controls from disc
c Read controls in here:
16	continue
	if(ibcor.eq.1.or.(ibcor.ge.2.and.imeanc.eq.1)) then
	   print 20
20	   format('&Control variance (pA**2), mean (pA) = ')
c	   read 21,varvc,cmeanc
	   call INPUT2r(varvc,cmeanc)
c21	   format(2g13.6)
	else if(ibcor.ge.2.and.imeanc.gt.1) then
	   print 19
19	   format('&Control variance (pA**2) = ')
c	   read 22,varvc
	   call INPUTr(varvc)
c22	   format(g13.6)
	endif
      print 123,varvc,cmeanc
      if(pon()) write(7,123) varvc,cmeanc
      if(discprt) write(8,123) varvc,cmeanc
123	format(
     & ' Control values subtracted:  Variance (pA**2), mean (pA)= ',
     & 2g13.6)
13	continue
c
c Subtract varvc,cmeanc
	print 110,nspec
110	format(
     & ' List ',i4,' means, times and variances minus control [Y] ? ')
	read 101,ans
	ans=UC(ans)
	if(ans.ne.'N') then
         print 3
         if(pon()) write(7,3)
         if(discprt) write(8,3)
	endif
	do 17 i=1,nspec
17	varplt(i)=varsav(i)-varvc
	if(ibcor.eq.1.or.(ibcor.ge.2.and.imeanc.eq.1)) then
	   do 171 i=1,nspec
171	   ymplot(i)=ybsav(i)-cmeanc
	else if(ibcor.ge.2.and.imeanc.eq.2) then
	   do 172 i=1,nspec
172	   ymplot(i)=ymsav1(i)
	else if(ibcor.ge.2.and.imeanc.eq.3) then
	   do 173 i=1,nspec
173	   ymplot(i)=ymsav2(i)
	endif
	if(ans.ne.'N') then
	   do 174 i=1,nspec
	   print 5,i,timsav(i),ymplot(i),varplt(i)
	   if(pon()) write(7,5)i,timsav(i),ymplot(i),varplt(i)
         if(discprt) write(8,5)i,timsav(i),ymplot(i),varplt(i)
174	   continue
	endif
c
c
c Define all data curves:
c j=1 for spectral density
c j=2 mean vs time
c j=3 for variance vs time
c j=4 for variance vs mean
c First j=1 for spectral density
	ndat(1)=npspec
	icurvd(1)=1		!- the first
	isym(1)=-3		!-7=filled circle, -3=filled square
	ijoin(1)=-1		!do not join data points with lines
	symsiz(1)=0.7	!
	do 10 i=1,npspec
	xval(i,1)=freq(i)
	yval(i,1)=ps(i)
10	continue
c
c j=2 mean vs time
	ndat(2)=nspec
	isym(2)=-7		!-7=filled circle, -3=filled square
	ijoin(2)=-1		!do not join data points with lines
	symsiz(2)=0.8
	do 33 i=1,ndat(2)
	xval(i,2)=timsav(i)
	yval(i,2)=ymplot(i)
33	continue
c
c j=3 for variance vs time
	ndat(3)=nspec
	isym(3)=-7		!-7=filled circle, -3=filled square
	ijoin(3)=-1		!do not join data points with lines
	symsiz(3)=0.8
	do 18 i=1,ndat(3)
	xval(i,3)=timsav(i)
	yval(i,3)=varplt(i)
18	continue
c
c j=4 for variance vs mean
	ndat(4)=nspec
	isym(4)=-7		!-7=filled circle, -3=filled square
	ijoin(4)=-1		!do not join data points with lines
	symsiz(4)=0.8
	do 14 i=1,ndat(4)
	xval(i,4)=ymplot(i)
	yval(i,4)=varplt(i)
14	continue
c
c which to plot?
	iplot=1		!initial default
	iscal=1		!scale internally
c
121	continue
	print 120,iplot
120	format(' Display graph of',/,
     & ' (1) Average spectral density function',/,
     & ' (2) Mean current vs time',/,
     & ' (3) Variance vs time',/,
     & ' (4) Variance vs mean current',/,
     & ' (5) No more graphs -finish',/,
     & ' Option number [',i2,'] = ')
	read 15,i
	call INPUTi(iplot)
c	if(i.ge.1.and.i.le.5) iplot=i
	goto (301,302,303,304,99) iplot
c
c Mean spectrum
301	continue
	ncurvd=1
	icurvd(1)=1
	titlex='Frequency (Hz) '
c	sub=char(4)
c	sup=char(21)
	titley='Spectral density (A'//char(21)//'2'//char(4)//'s)'
	ncurvc=0		!no calc curve
c Misc inputs
	ilog=3		!log/log
	inumx=-1		!X axis in fixed (Fn.d) format
	inumy=1		!X axis in exponent format (if log)
	goto 400		!plot it
c
c Mean current vs time
302	continue
	ncurvd=1
	icurvd(1)=2       !set #2
	ilog=0
	titlex='Time (s) '
	if(ibcor.eq.2.or.ibcor.eq.3) titlex='Time (ms) '
	titley='Mean current (pA) '
c Calc slope of mean vs time (control only?)
	if(ibcor.eq.2.or.ibcor.eq.3) then
	   ncurvc=0			!no straight line fit
	else
	   ncurvc=1
c	   call REG2(X,Y,ndim,N1,N2,NA,NB,A,B,varb)
	   call REG2(timsav,ymplot,ndim,1,nspec,0,0,A1,B1,varb)
	   print 7,b1
	   if(pon()) write(7,7)b1
         if(discprt) write(8,7)b1
7	   format(' Mean current changing at ',g13.6,' pA/s')
	   call MAXV(timsav,xmin,xmax,nspec,ndim)
	   dxcalc=(xmax-xmin)/float(ncalc-1)
	   do 11 i=1,ncal(2)
	   xcal(i,2)=xmin+float(i-1)*dxcalc
	   ycal(i,2)=a1 + b1*xcal(i,2)
11	   continue
	   icurvc(1)=2
	endif
	goto 400		!plot it
c
c Variance vs time
303	continue
	ncurvd=1
	icurvd(1)=3       !set #3
	ilog=0
	titlex='Time (s) '
	if(ibcor.eq.2.or.ibcor.eq.3) titlex='Time (ms) '
	titley='Variance (pA'//char(21)//'2'//char(4)//')'
c Calc slope of variance vs time (control only?)
	if(ibcor.eq.2.or.ibcor.eq.3) then
	   ncurvc=0			!no straight line fit
	else
	   ncurvc=1
c	   call REG2(X,Y,ndim,N1,N2,NA,NB,A,B,varb)
	   call REG2(timsav,varplt,ndim,1,nspec,0,0,A4,B4,varb)
	   print 71,b4
	   if(pon()) write(7,71)b4
         if(discprt) write(8,71)b4
71	   format(' Variance changing at ',g13.6,' pA*pA/s')
	   call MAXV(timsav,xmin,xmax,nspec,ndim)
	   dxcalc=(xmax-xmin)/float(ncalc-1)
	   do 111 i=1,ncal(3)
	   xcal(i,3)=xmin+float(i-1)*dxcalc
	   ycal(i,3)=a4 + b4*xcal(i,3)
111	   continue
	   icurvc(1)=3
	endif
	goto 400		!plot it
c
c Variance vs mean
304	continue
	ncurvd=1
	icurvd(1)=4       !set #4
	ilog=0
	ncurvc=2		!2 calc curves
	icurvc(1)=1
	icurvc(2)=4
	titley='Variance (pA'//char(21)//'2'//char(4)//')'
	titlex='Mean current (pA) '
c Calc slope of variance vs mean = curve 4 (unconstrained) and
c curve 1 (constrained through origin)
c Calc slope of variance vs mean
	call REG2(ymplot,varplt,ndim,1,nspec,0,0,A2,B2,varb)
	if(varb.gt.0.) sb=sqrt(varb)
	print 8,b2,sb,a2
	if(pon()) write(7,8)b2,sb,a2
      if(discprt) write(8,8)b2,sb,a2
8	format(
     & ' Unit current from slope ',g13.6,' pA  (s(b)= ',g13.6,' pA)',
     & /,'  intercept (pA**2)= ',g13.6)
c Calc slope of variance vs mean through origin
	call REG0(ymplot,varplt,ndim,nspec,B3,varb)
c	SUBROUTINE REG0v(X,Y,ndim,N,B,Varb)
	if(varb.gt.0.) sb=sqrt(varb)
	print 9,b3,sb
	if(pon()) write(7,9)b3,sb
      if(discprt) write(8,9)b3,sb
9	format(' Unit current from slope through origin',g13.6,' pA',/,
     & '( s(b)= ',g13.6,' pA)')
c
	call MAXV(ymplot,xmin,xmax,nspec,ndim)
	dxcalc=(xmax-xmin)/float(ncalc-1)
	do 12 i=1,ncalc
	xcal(i,1)=xmin+float(i-1)*dxcalc
	xcal(i,4)=xcal(i,1)
	ycal(i,4)=a2 + b2*xcal(i,4)		!line
	ycal(i,1)= b3*xcal(i,1)		!line through origin
12	continue
	goto 400		!plot it
c
c
400	continue
	ivplot=.false.
	interp=.false.
	isdev=-1	!no SD
	call VPLOT2(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,isdev,interp)
c	call VPLOTR(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
c     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
c     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
c     & iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,
c    & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,vramp,
c     & titlex,titley,ilabel,doframe,idiskq,
c     & autplt,draft,itit,title,cbig,ifont,landscap,fitted,iask,
c     & theta,ifitype,ncomp)
c
c	print 97
c97	format(' Display again [Y] ? ')
c	read 101,ans
c	if(UC(ans).eq.'N') goto 99
c	iscal=0		!retain existing scaling
	iscal=1		!xmin etc wrong if diff graph type req
c
	iplot=iplot+1	!increment default plot type
	if(iplot.gt.5) iplot=1
	goto 121		!another plot?
c
99	RETURN
c
c File open error
200	continue
	call BELL(2)
	print 54
54	format(' CANNOT OPEN NOISE DATA FILE')
	goto 261
	END


	subroutine MAXV(xv,xmin,xmax,n,ndim)
	dimension xv(ndim)
c
	xmin=1.e32
	xmax=-1.e32
	do 1 i=1,n
	if(xv(i).lt.xmin) xmin=xv(i)
1	if(xv(i).gt.xmax) xmax=xv(i)
	RETURN
	end


