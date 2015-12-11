	subroutine HYPDISP(aK,Ym,hill,nrun,aKmax,Ymmax,hillmax,
     & ndx,aKexcb,aKexcs,aKexcm,aYexcb,aYexcs,aYexcm,
     & nkboth,nkexcs,nkexcm,nyboth,nyexcs,nyexcm)
c To display distributions of estimates in hypfit
c
	allocatable yval1
	real*4 yval1(:)
	real*4 aK(nrun,6),Ym(nrun,6),hill(nrun)
	real*4 aKexcb(ndx,2),aKexcs(ndx,2),aKexcm(ndx,2),
     & aYexcb(ndx,2),aYexcs(ndx,2),aYexcm(ndx,2)
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
	character*44 title1
c
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
c
c	(1) Lineweaver-Burk
c	(2) Scatchard
c	(3) Eadie-Hofstee
c	(4) x/y versus x
c	(5) LS
101	format(a1)
	im=1
	ip=1
	xaxis(1)=0.
c
12	continue	!next distribution
	ipmax=2
	print 10,im
10	format(' Plot distribution of estimates by:',/,
     & ' (1) Lineweaver-Burk',/,
     & ' (2) Scatchard',/,
     & ' (3) Eadie-Hofstee',/,
     & ' (4) x/y versus x',/,
     & ' (5) Least squares estimates for hyperbola',/,
     & ' (6) Least squares estimates for Hill equation',/,
     & ' (7) Distributions of LS estimates of hyperbola from expts',/,
     & '     where limits exclude the true value of K',/,
     & ' (8) Distributions of LS estimates of hyperbola from expts',/,
     & '     where limits exclude the true value of Ymax',/,
     & ' (9) Plot LS estimates against each other',/,
     & ' (0) Finish',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(im)
c     & ' (6) Distributions of LS estimates from expts where',/,
c     & '     limits exclude the true value of K',/,
c     & ' (7) Distributions of LS estimates from expts where',/,
c     & '     limits exclude the true value of Ymax',/,
c     & ' (8) Plot LS estimate against each other',/,
c     & ' (9) Finish',/,
c
	if(im.eq.0) goto 999
c
	if(im.eq.7.or.im.eq.8) then
	   print 13,im1
13	   format(
     & ' (1) BOTH 2*SD limits and m=2 limits exclude true value',/,
     & ' (2) ONLY the 2*SD limits exclude true value',/,
     & ' (3) ONLY the m=2.0 limits exclude true value',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(im1)
	endif
c
c Plot of Ym vs K done by vplot5 in hypdisp1, called with irun=-1
c (args from c onwards not defined here, but not needed)
	if(im.eq.9) then
	   im2=1
83	   print 81,im2
81	   format(
     & ' (1) Ymax vs K for fit of hyperbola',/,
     & ' (2) Ymax vs K for fit of Hill equation',/,
     & ' (3) nHill vs K for fit of Hill equation',/,
     & ' (4) nHill vs Ymax for fit of Hill equation',/,
     & ' (5) Finish',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(im2)
	   if(im2.eq.5) goto 12		!another plot?
	   idum=-im2
	   call HYPDISP1(aK,Ym,hill,idum,nrun,c,yobs,w,n,
     &     aKtrue,Ymtrue,aKmax,Ymmax,hillmax)
	   if(im2.lt.5) then
		im2=im2+1
		goto 83
	   else
	      goto 12		!another plot?
	   endif
	endif
c
	if(im.ne.6) then
	   print 11,ip
11	   format(' Plot distribution of estimate of:',/,
     & ' (1) K',/,
     & ' (2) Ymax',/,
     & ' Option number [',i2,'] = ')
	else if(im.eq.6) then
	   ipmax=3
	   print 111,ip
111	   format(' Plot distribution of estimate of:',/,
     & ' (1) K',/,
     & ' (2) Ymax',/,
     & ' (3) nHill',/,
     & ' Option number [',i2,'] = ')
	endif
	call INPUTi(ip)
c
c Define labels
	itit=1
	ilabel=1
	if(ip.eq.1) then
	   titlex='K value'
	else if(ip.eq.2) then
	   titlex='Ymax value'
	else if(ip.eq.3) then
	   titlex='Hill coeff.'
	endif
	if(im.eq.1) then
	   title1='Lineweaver-Burk'
	else if(im.eq.2) then
 	   title1='Scatchard'
	else if(im.eq.3) then
 	   title1='Eadie-Hofstee'
	else if(im.eq.4) then
	   title1='x/y versus x'
	else if(im.eq.5) then
 	   title1='LS fit of hyperbola'
	else if(im.eq.6) then
 	   title1='LS fit of Hill eq.'
	else if(im.eq.7) then
	   if(im1.eq.1) then
            title1='BOTH 2*SD and m=2 limits exclude true K'
         else if(im1.eq.2) then
      	title1='ONLY the 2*SD limits exclude true K'
         else if(im1.eq.3) then
      	title1='ONLY the m=2.0 limits exclude true K'
	   endif
	else if(im.eq.8) then
	   if(im1.eq.1) then
            title1='BOTH 2*SD and m=2 limits exclude true Ymax'
         else if(im1.eq.2) then
      	title1='ONLY the 2*SD limits exclude true Ymax'
         else if(im1.eq.3) then
      	title1='ONLY the m=2.0 limits exclude true Ymax'
	   endif
	endif
c Copy data to yval1
	ALLOCATE(yval1(nrun))
	nyval=0
	if(im.ge.1.and.im.le.6) then
	 if(ip.eq.1) then
	   do i=1,nrun
		aK1=aK(i,im)
	      if(aK1.le.aKmax) then
		   nyval=nyval+1
		   yval1(nyval)=aK1
		endif
	   enddo
	 else if(ip.eq.2) then
	   do i=1,nrun
		Ym1=Ym(i,im)
	      if(Ym1.le.Ymmax) then
		   nyval=nyval+1
		   yval1(nyval)=Ym1
		endif
	   enddo
	 else if(ip.eq.3) then
	   do i=1,nrun
		hill1=hill(i)
	      if(hill1.le.hillmax) then
		   nyval=nyval+1
		   yval1(nyval)=hill1
		endif
	   enddo
	 endif
c rest is for expts where limits exclude true value
	else if(im.eq.7) then
	   if(im1.eq.1) then
		do i=1,nkboth
		   nyval=nyval+1
		   yval1(nyval)=aKexcb(i,ip)		!ip=1 for K, ip=2 for Ymax
		enddo
	   else if(im1.eq.2) then
		do i=1,nkexcs
		   nyval=nyval+1
		   yval1(nyval)=aKexcs(i,ip)		!ip=1 for K, ip=2 for Ymax
		enddo
	   else if(im1.eq.3) then
		do i=1,nkexcm
		   nyval=nyval+1
		   yval1(nyval)=aKexcm(i,ip)		!ip=1 for K, ip=2 for Ymax
		enddo
	   endif
	else if(im.eq.8) then
	   if(im1.eq.1) then
		do i=1,nyboth
		   nyval=nyval+1
		   yval1(nyval)=aYexcb(i,ip)		!ip=1 for K, ip=2 for Ymax
		enddo
	   else if(im1.eq.2) then
		do i=1,nyexcs
		   nyval=nyval+1
		   yval1(nyval)=aYexcs(i,ip)		!ip=1 for K, ip=2 for Ymax
		enddo
	   else if(im1.eq.3) then
		do i=1,nyexcm
		   nyval=nyval+1
		   yval1(nyval)=aYexcm(i,ip)		!ip=1 for K, ip=2 for Ymax
		enddo
	   endif
	endif
c
	if(nyval.eq.0) then
	   print 4132
4132	   format(' NO VALUES FOR DISTRIBUTION')
	   call BELL(4)
	   DEALLOCATE(yval1)
	   if(ip.eq.2) then
		ip=1
		if(im.lt.5) then
		   im=im+1
		else
		   im=1
		endif
	   else
		ip=2
	   endif
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
c	print 2091
c2091	format('&Histogram to start at 0.0:  O.K. [Y] ? ')
c	read 101,ans
c	if(UC(ans).eq.'N') then
c	   print 33
c33	   format('&Start histogram at x [0.0] = ')
c	   call INPUTr(xaxis(1))
c	endif
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
	read 101,ans
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
	read 101,ans
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
	xmax=float(1+ifix(1.2*xmax))	!ROUND UP
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
	ncalc=0
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
	if(ip.eq.ipmax) then
	   ip=1
	   if(im.eq.7.or.im.eq.8) then
		if(im1.lt.3) then
		   im1=im1+1
		else
		   im1=1
		   if(im.eq.7) im=8
		endif
	   else if(im.lt.6) then
		im=im+1
	   else
		im=1
	   endif
	else
	   ip=ip+1
	endif
	goto 12
c
999	RETURN
	end
