	subroutine BETZDISP(pvalue,aicdif,apair,hill2,nrun,ieqn)
c To display distributions of Pvalue estimates in betzfit
c (modified from hypdisp)
c
	real*4 pvalue(nrun),aicdif(nrun),apair(nrun),hill2(nrun)
	allocatable yval1
	real*4 yval1(:)
	real theta(20)
	dimension FREQ(510),XAXIS(510),xsav(5,4)
	character*1 ans,UC
c	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical fitted
	logical pon,slock,debug,caplock
	logical landscap,autplt,draft,doframe
	logical discprt
c	character cnum*11
c For VHIST
	real XVAL(0:511,1),YVAL(0:511,1)
	real XCAL(1,1),YCAL(1,1)
c for histo data
	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
	integer ncal(1),icurvc(1),iline(1)
	character*40 titlex,titley
	character*64 title1
c
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
c
	im=1
	xaxis(1)=0.
c
12	continue	!next distribution
c	ipmax=2
c	print 10,im
c10	format(' Plot distribution of estimates by:',/,
c     & ' (1) Lineweaver-Burk',/,
c     & ' (2) Scatchard',/,
c     & ' (3) Eadie-Hofstee',/,
c     & ' (4) x/y versus x',/,
c     & ' (5) Least squares estimates for hyperbola',/,
c     & ' (6) Least squares estimates for Hill equation',/,
c     & ' (7) Distributions of LS estimates of hyperbola from expts',/,
c     & '     where limits exclude the true value of K',/,
c     & ' (8) Distributions of LS estimates of hyperbola from expts',/,
c     & '     where limits exclude the true value of Ymax',/,
c     & ' (9) Plot LS estimates against each other',/,
c     & ' (0) Finish',/,
c     & ' Option number [',i2,'] = ')
c	call INPUTi(im)
c
	if(ieqn.eq.1) then
	  print 10,im
10	  format(/,
     & ' (1) Plot distribution of P values from lik-ratio test',/,
     & ' (2) Plot distribution AIC(3)-AIC(4)',/,
     & ' (0) Finish',/,
     & ' Option number [',i2,'] = ')
	else if(ieqn.eq.2) then
	  print 110,im
110	  format(/,
     & ' (1) Plot distribution of P values from lik-ratio test',/,
     & ' (2) Plot distribution AIC(2)-AIC(1)',/,
     & ' (3) Plot estimate of smaller amplitude',/,
     & ' (4) Plot estimate of Hill coeff (triplet)',/,
     & ' (0) Finish',/,
     & ' Option number [',i2,'] = ')
	endif
	call INPUTi(im)
	if(im.eq.0) goto 999
c
c
c Define labels
	itit=1
	ilabel=1
	if(im.eq.1) then
	   titlex='P value'
	   title1='Result of likelihood ratio test             '
	else if(im.eq.2) then
	   if(ieqn.eq.1) then
		titlex='AIC(3)-AIC(4)'
		title1='Akaike IC difference (positive means 4 comp better)'
	   else if(ieqn.eq.2) then
		titlex='AIC(2)-AIC(1)'
		title1='Akaike IC difference (positive means 2 comp better)'
	   endif
	else if(im.eq.3) then
	   titlex='amplitude (1)'
	   title1='Estimate of amplitude of minor component    '
	else if(im.eq.4) then
	   titlex='Hill coeff(1)'
	   title1='Estimate of Hill coeff    '
	endif
c Copy data to yval1
	ALLOCATE(yval1(nrun))
	nyval=0
	if(im.eq.1) then
	   do i=1,nrun
		nyval=nyval+1
		yval1(nyval)=pvalue(nyval)
	   enddo
	else if(im.eq.2) then
	   do i=1,nrun
		nyval=nyval+1
		yval1(nyval)=aicdif(nyval)
	   enddo
	else if(im.eq.3) then
	   do i=1,nrun
		nyval=nyval+1
		yval1(nyval)=apair(nyval)
	   enddo
	else if(im.eq.4) then
	   do i=1,nrun
		nyval=nyval+1
		yval1(nyval)=hill2(nyval)
	   enddo
	endif
c
	if(nyval.eq.0) then
	   print 4132
4132	   format(' NO VALUES FOR DISTRIBUTION')
	   call BELL(4)
	   DEALLOCATE(yval1)
	   goto 12
	endif
c
	call VARV1(yval1,Nyval,YBAR,VARY,ymin,ymax)
	print 413,NYVAL,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) NYVAL,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) NYVAL,YBAR,sqrt(vary),ymin,ymax
413	FORMAT(' Mean and SD of ',I5,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
c
501	print 208
208	format(
     & ' Number of different bin widths (-1 to skip histo) [1] = ')
	nbw=1
	call INPUTi(nbw)
	if(nbw.le.-1) then
	   nbin=0		!SKIP
	   RETURN
	endif
	print 33,xaxis(1)
33	format('&Start histogram at x [',f8.3,'] = ')
	call INPUTr(xaxis(1))
c   GET BIN WIDTHS
c Calculate bin boundaries in XAXIS
	K=1		!COUNTS X AXIS ENTRIES
	do 210 i=1,nbw
	klast=k
212	continue
	print 36,i,dx,xend
36	format(' (',i2,') Bin width, Last x value [',f8.3,',',f8.3,'] = ')
	call INPUT2r(dx,xend)
	if(dx.le.0.00001.or.xend.le.xaxis(1)+dx) goto 212
32	xn=(xend-xaxis(klast))/dx
C ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
c- also check that not more than 510 bins
	if(abs(xn-float(ifix(0.5+xn))).lt.dx*.01) goto 30	!OK
	xend=xaxis(klast) + float(ifixr(xn))*dx
	call bell(1)
	print 31,xend
31	format('&Last x reset to ',g13.6,' O.K. [Y] ? ')
	call INPUTa(ans)
	if(UC(ans).eq.'N') goto 212
	goto 32
c
30	nbin=ifix(0.5+xn)
311	if(nbin.eq.0) goto 212
	do j=1,nbin
	   k=k+1
	   if(k.gt.510) then
		call bell(2)
		print *, ' TOO MANY BINS'
		goto 501
	   endif
	   Xaxis(k)=Xaxis(klast)+float(j)*dx
	enddo
c   save values for printing repeated with same bins
	xsav(i,1)=float(nbin)
	xsav(i,2)=dx
	xsav(i,3)=xaxis(klast)
	xsav(i,4)=xaxis(k)
306	if(pon()) write(7,2111) (xsav(i,m),m=1,4)
	if(discprt) write(8,2111) (xsav(i,m),m=1,4)
2111	format(1x,f5.0,
     & ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
210	CONTINUE	!end of i loop
C
	nbin=k-1		!NOW NBIN=TOTAL NO OF BINS
	xwbase=dx		!IF ONLY ONE BIN WIDTH
	if(nbw.gt.1) then
	   print 107
107	   format(' Base width for frequency density = ')
	   call INPUTr(xwbase)
	endif
	if(pon()) write(7,1071) xwbase
	if(discprt) write(8,1071) xwbase
1071	format( ' Base width for frequency density = ',g13.6)
C NOW SORT Y VALUES AND CALC FREQ (DENSITY) FOR EACH FILE
	print 82
82	format(' Sorting data into bins...')
	flo=0.
	fhi=0.
	do i=1,510
	   freq(i)=0.0
	enddo
c
	do i=1,nyval
	   yv=yval1(i)
	   if(yv.lt.xaxis(1)) then
	   flo=flo+1.0
	   else if(yv.gt.xaxis(nbin+1)) then
		fhi=fhi+1.0
	   else
		do j=1,nbin		!j loop -the main bins
		   if(yv.ge.xaxis(j).and.yv.lt.xaxis(j+1)) then
			freq(j)=freq(j)+1.0
		   endif
		enddo
	   endif
c For top bin include those EQUAL to boundary (eg so Popen=1.0 not excluded
c from top bin)
	   if(yv.eq.xaxis(nbin+1)) freq(nbin)=freq(nbin)+1.0
	enddo
223	continue	!i loop (obs)
c
	if(flo.lt.0.1) flo=-2.	!do not plot if flo=0
	if(fhi.lt.0.1) fhi=-2.	!ditto
	fmax=0.
C CONVERT FREQ TO FREQ DENSITY NEXT
c N.B. FLO and FHI are abs numbers, not densities- cannot express FHI
c as a density if upper lim is infinite! FLO can be expressed as density
c once bin width has been fixed (see VHIST)
	do j=1,nbin
	   freq(j)=freq(j)*xwbase/(Xaxis(j+1)-Xaxis(j))
	   if(freq(j).gt.fmax) fmax=freq(j)
	enddo
	if(flo.gt.fmax) fmax=flo
	if(fhi.gt.fmax) fmax=fhi
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	j=ifix(0.1+fhi)
	if(fhi.lt.0.) j=0
	print 222,i,j
	if(pon()) write(7,222) i,j
	if(discprt) write(8,222) i,j
222	FORMAT(' No of values below Xlow= ',i8,
     & ' No of values above Xhigh= ',i8)
4071	print 407
407	FORMAT(' Type frequency densities [N] ? ')
	call INPUTa(ans)
	if(UC(ans).eq.'Y')then
	   print 1061
     	   if(pon()) write(7,1061)
	   if(discprt) write(8,1061)
1061 	   format(
     & ' Bin no.         X values                Frequency')
	   do i=1,nbin
		print 106,i,xaxis(i),xaxis(i+1),FREQ(I)
		if(pon()) write(7,106) i,xaxis(i),xaxis(i+1),FREQ(I)
		if(discprt) write(8,106) i,xaxis(i),xaxis(i+1),FREQ(I)
106		format(i5,3X,g13.6,' - ',g13.6,3x,G13.6)
	   enddo
	endif
	DEALLOCATE(yval1)
c
	titley='Number '
c	ict=11	!text colour for DIALOG box
	autplt=.false.		!for VHIST
	draft=.false.		!for VHIST
	doframe=.true.		!for VHIST
	landscap=.true.		!for VHIST
	fitted=.false.
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
	iask=-2
	ifitype=0
c
	lt2=2       !short dash for lo,hi bins
	ilog=0
	xlo=-1.
c
c	do i=1,1
c	   icurvc(i)=i	!in case needed
c	   iline(i)=0	!continuous
c	enddo
c histos:
	ncurvd=1
	icurvd(1)=1
	ndat(1)=nbin
	ijoin(1)=0
	do 1 i=1,nbin+1
1	 xval(i,1)=xaxis(i)
	do 21 i=1,nbin
	 yval(i,1)=freq(i)
21	continue
c
	fmin=0.
	xmin=0.
	xmax=xaxis(nbin+1)	!=xval(nbin+1,1), if no hi bin
c Round up xmax, but not if it is already an 'integer')
	if(abs(amod(xmax,1.0)).gt.1.e-4) xmax=float(1+ifix(xmax))
	if(xmax.gt.1.) then
	   xmax=float(1+ifix(1.2*xmax))	!ROUND UP
	else if(xmax.gt.0.1.and.xmax.lt.1.) then
	   xmax=0.1*float(1+ifix(12.*xmax))	!ROUND UP
	else if(xmax.gt.0.01.and.xmax.lt.0.1) then
	   xmax=0.01*float(1+ifix(120.*xmax))	!ROUND UP
	endif
	fmax=fmax*1.2     !value from SETBIN
	fmax=float(1+ifix(fmax))	!ROUND UP
	ftic=0.5*10.0**ifix(alog10((fmax-fmin)*0.5))
2102	if((fmax-fmin)/ftic.gt.10.) goto 2101
	ftic=ftic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
	xtic=xtic*5.
	ftic=ftic*5.	!smaller tics for sqrt scale for now
c
	yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
	xval(0,1)=xval(1,1)	!if no flo- it is checked in VHIST (in MINMAX)
	xval(nbin+2,1)=xval(nbin+1,1)		!ditto if no fhi bin
	if(flo.gt.0.0001) then
	   xval(0,1)=xval(1,1)-xwbase
	   xmin=xval(0,1)
	endif
	yval(nbin+1,1)=fhi
	if(fhi.gt.0.0001) then
	   xval(nbin+2,1)=xval(nbin+1,1)+xwbase
c	  keep ratio for hi bin as for other bins, so same width on log scale
	   xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
	   xmax=xval(nbin+2,1)
	 endif
c
c Curves:
c	ncalc=0
	ncurvc=0
c	if(ncalc.gt.0) then
c	  ncurvc=1		!copy first calc curve here
c	  ncal(1)=ncalc
c	  icurvc(1)=1
c	  iline(1)=0		!so calc curve is continuous
c	  do 2 i=1,ncal(1)
c		xcal(i,1)=xcalc(i)
c		ycal(i,1)=fcalc(i)
c2	  continue
c	endif
c
	iscal=0			!use input xmin,xmax etc
c	iscal=1	!scale internally 1st time
	ntx=5
	nty=5
	itx=1
	ity=1
c curves:
	ncurvc=0		!no calc curve yet
	if(debug()) then
	  print 77,xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw
77	  format(' xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw=',/,9g13.6)
	  print 771,titlex,titley
771	  format(' X axis: ',a40,/,' Y axis: ',a40)
	  pause
	endif
	ndimd=1
	ndimc=1
	ndv1=511
	ndc1=1
	kmax=20	!dimension of theta
	isetcol=0
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c
	goto 12
c
999	RETURN
	end
