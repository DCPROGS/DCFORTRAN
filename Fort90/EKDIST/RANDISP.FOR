	subroutine RANDISP(randiff,nran,sval)
c To display randomisation distribution in EKDIST (based on RANDSP which
c is used for display in RANTEST).
c
	real XVAL(0:511,1),YVAL(0:511,1)
	real XCAL(2048,1),YCAL(2048,1)
c for histo data
	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
	integer ncal(1),icurvc(1),iline(1)
c
	real theta(1)	!for vhist call -not used
	real*4 randiff(nran)
	dimension XAXIS(510),FREQ(510),freq1(510)
c	integer*2 videotyp
	character*40 titlex,titley
	character*64 title1
	character*1 ans
	LOGICAL fitted
	logical pon,slock,autplt,draft,doframe,landscap,debug,caplock
	logical discprt
c
	common/dp/discprt
C
C
	pon()=slock()
	debug()=caplock()
c
	ans='Y'
	call DCASK('Display randomisation distribution',ans,ans)
	if(ans.eq.'N') RETURN
c
c First put into bins
81	continue
	call VARV1(randiff,nran,XBAR,VARx,xmin,xmax)
	print 413,nran,xBAR,sqrt(varx),xmin,xmax
	if(discprt) write(8,413) nran,xBAR,sqrt(varx),xmin,xmax
413	FORMAT(' Mean and SD of ',i6,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
c
501	continue
	nbmax=508	!max number of bins
	iopt=5
75	print 2092,iopt
2092	format(
     & ' (2) Distribution of durations- set bins manually',/,
     & ' (3) Distribution of durations- 20 bins',/,
     & ' (4) Distribution of durations- 40 bins',/,
     & ' (5) Distribution of durations- 60 bins',/,
     & ' (6) Skip histogram',/,
     & ' Option number [',i2,'] = ')
c     & '',/,
	call INPUTi(iopt)
	if(iopt.lt.2.or.iopt.gt.6) then
	   call BELL(1)
	   goto 75
	endif
	if(iopt.eq.6) RETURN
	xmin=float(ifixr(xmin))-1.0	!round down for default
	xmax=float(ifixr(xmax))+1.0	!round up for default
	if(iopt.ge.3) then
c auto-binwidth to give 20,40,60 bins (nbw=1, not logt)
	   nbw=1
	   nbin=20
	   if(iopt.eq.4) nbin=40
	   if(iopt.eq.5) nbin=60
	   dx=(xmax-xmin)/float(nbin)
c	   call SETTIC(dx)
	   xaxis(1)=xmin
	   xend=xmin + (dx*nbin)
	   call SETTIC(xend)
	   klast=1
	else if(iopt.eq.2) then
	   print 208
208	   FORMAT(
     &    ' Number of different bin widths (-1 to skip histo) [1]= ')
	   call INPUTi(NBW)
	   IF(NBW.EQ.0) NBW=1
28	   print 209,xmin
209	   FORMAT(
     &    '&(1) Start histogram at x = ',g10.3,/,
     &    ' (2) Specify starting value for x axis',/,
     &    ' Option number [1] = ')
	   iopt=1
	   call INPUTi(iopt)
	   if(iopt.eq.2) then
		print 27
27		format(
     &       '& Start histogram at x = ')
		call INPUTr(xmin)
	   endif
	endif
	xaxis(1)=xmin
c
C   GET BIN WIDTHS
c Calculate bin boundaries in XAXIS
	K=1		!COUNTS X AXIS ENTRIES
303	continue
	do i=1,nbw
c auto-binwidth to give 20,40,60 bins (nbw=1, not logt)
212	   continue
	   if(iopt.ge.3) then
		klast=1
	   else if(iopt.eq.2) then
		KLAST=K
		print 38,i
38		format(' (',i2,') Bin width, Last x value = ')
		call INPUT2r(dx,xend)
		if(dx.le.0.00001.or.xend.le.xaxis(1)+dx) goto 212
	   endif
32	   XN=(XEND-XAXIS(KLAST))/DX
C ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
c- also check that not more than 510 bins
	   IF(ABS(XN-FLOAT(IFIX(0.5+XN))).ge.DX*.01) then	!not OK
		xend=xaxis(klast) + float(ifixr(xn))*dx
		call bell(1)
		print 31,xend
31		format('&Last x reset to ',g13.6,' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'N') goto 212
		goto 32
	   else
		NBIN=IFIX(0.5+XN)
	   endif
311	   IF(NBIN.EQ.0) GOTO 212
	   do j=1,nbin
 		K=K+1
		if(k.gt.nbmax) then
	 	   call BELL(1)
		   print 74
74	   	   format('TOO MANY BINS')
		   goto 501
		endif
500		XAXIS(K)=XAXIS(KLAST)+FLOAT(J)*DX
	   enddo
c   save values for printing repeated with same bins
306	   print 2111,nbin,dx,xaxis(klast),xaxis(k)
	   if(discprt) write(8,2111) nbin,dx,xaxis(klast),xaxis(k)
2111	   format(1x,i5,
     &    ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
	enddo		!end of i=1,nbw loop
c
c	DO 210 I=1,NBW
c	   KLAST=K
c212	   print 104,I
c104	   FORMAT('&(',I2,') Bin width, Last x value = ')
c	   call input2r(DX,XEND)
c32	   XN=(XEND-XAXIS(KLAST))/DX
cC ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
cc- also check that not more than 101 bins
c	   IF(ABS(XN-FLOAT(IFIX(0.5+XN))).lt.DX*.01) GOTO 30	!OK
c	   xend=xaxis(klast) + float(ifixr(xn))*dx
c	   call bell(1)
c	   print 31,xend
c31	   format('&Last x reset to ',g13.6,' O.K. [Y] ? ')
c	   ans='Y'
c	   call INPUTa(ans)
c	   if(ans.eq.'N') goto 212
c	   goto 32
cc
c30	   NBIN=IFIX(0.5+XN)
c311	   IF(NBIN.EQ.0) GOTO 212
c	   DO J=1,NBIN
c	 	K=K+1
c		if(k.le.510) goto 500
c		call bell(2)
c		print *, ' TOO MANY BINS'
c		goto 501
c500		XAXIS(K)=XAXIS(KLAST)+FLOAT(J)*DX
c	   enddo
cc
c	   print 2111,nbin,dx,xaxis(klast),xaxis(k)
c	   if(discprt) write(8,2111) nbin,dx,xaxis(klast),xaxis(k)
c2111	   format(1x,i5,
c     &    ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
c210	CONTINUE	!END OF I LOOP
C
	NBIN=K-1		!NOW NBIN=TOTAL NO OF BINS
c Use absolute frequencies for RANTEST
	XWBASE=DX		!IF ONLY ONE BIN WIDTH
c	IF(NBW.EQ.1) GOTO 307
c	print 107
c107	FORMAT( ' Base width for frequency density= ')
c	input 103,XWBASE
c307	if(pon()) write(7,1071) xwbase
c	if(discprt) write(8,1071) xwbase
c	print 1071,xwbase
c1071	FORMAT( ' Base width for frequency density= ',g13.6)
c
C NOW SORT Y VALUES AND CALC FREQ (DENSITY) FOR EACH FILE
	print 82
82	format(' Sorting data into bins...')
	flo=0.
	fhi=0.
	flo1=0.	!for test of new binning method
	fhi1=0.
	do i=1,510
	   freq1(i)=0.0	!for test of new binning method
	   freq(i)=0.0
	enddo
c
	do 223 i=1,nran
	ran=randiff(i)
	if(ran.lt.xaxis(1)) then
	   flo=flo+1.0
	   goto 223
	endif
	if(ran.ge.xaxis(nbin+1)) then
	   fhi=fhi+1.0
	   goto 223
	endif
	do 221 j=1,nbin
	if(ran.ge.xaxis(j).and.ran.lt.xaxis(j+1)) freq(j)=freq(j)+1.0
221	continue	!j loop (bins)
223	continue	!i loop (obs)
C Test new binning method (for single bin width only)
	do 2231 i=1,nran
	ran=randiff(i)
	if(ran.lt.xaxis(1)) then
	   flo1=flo1+1.0
	   goto 2231
	endif
	if(ran.ge.xaxis(nbin+1)) then
	   fhi1=fhi1+1.0
	   goto 2231
	endif
	k=1 + ifix(randiff(i)-xaxis(1))/dx
	freq1(k)=freq1(k)+1.0
2231	continue	!i loop (obs)
c
	if(debug()) then
	do 2232 i=1,nbin
	print 2233,i, freq(i),freq1(i)
2233	format(i8,2g10.1)
2232	continue
	endif
C
	IF(FLO.LT.0.1) FLO=-2.	!DO NOT PLOT IF FLO=0
	IF(FHI.LT.0.1) FHI=-2.	!DITTO
	CONTINUE
	ymax=0.
c ### Use absolute frequencies for RANTEST
cC CONVERT FREQ TO FREQ DENSITY NEXT
cc N.B. FLO and FHI are abs numbers, not densities- cannot express FHI
cc as a density if upper lim is infinite! FLO can be expressed as density
cc once bin width has been fixed (see VHIST)
	DO 218 J=1,NBIN
c	FREQ(J)=FREQ(J)*XWBASE/(XAXIS(J+1)-XAXIS(J))
	IF(FREQ(J).GT.ymax) ymax=FREQ(J)
218	CONTINUE
	IF(FLO.GT.ymax) ymax=FLO
	IF(FHI.GT.ymax) ymax=FHI
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	j=ifix(0.1+fhi)
	if(fhi.lt.0.) j=0
	print 222,i,j
	if(pon()) write(7,222) i,j
	if(discprt) write(8,222) i,j
222	FORMAT(' No of values below Xlow= ',i8,
     & ' No of values above Xhigh= ',i8)
	ans='N'
	call DCASK('Type frequency densities',ans,ans)
	if(ans.eq.'N') GOTO 406
	print 1061
     	if(pon()) write(7,1061)
	if(discprt) write(8,1061)
1061 	format(
     & ' Bin no.         X values                Frequency')
	DO 220 I=1,NBIN
	print 106,I,xaxis(i),xaxis(i+1),FREQ(I)
	if(pon()) write(7,106) I,xaxis(i),xaxis(i+1),FREQ(I)
220	if(discprt) write(8,106) I,xaxis(i),xaxis(i+1),FREQ(I)
106	FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
	pause
406	continue
c
c
c For Toshiba VIDEOTYP()=18 for graphics mode, =3 for alpha mode
c Set xmax etc
	xmax=xaxis(nbin+1)+(xaxis(nbin+1)-xaxis(nbin))	!room for FHI
c284=========
	xmin=xaxis(1)
	if(sval.lt.xmin) then
	   xmin=sval-0.05*(xmax-xmin)
	   xmin=ifixr(xmin)-1.0 	!round down
	endif
	ymin=0.0
	ymax=ymax*1.3
	ymax=float(1+ifix(ymax))	!round up
	ytic=0.5*10.0**ifix(alog10((ymax-ymin)*0.5))
2102	if((ymax-ymin)/ytic.gt.5.) goto 2101
	ytic=ytic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
	xtic=xtic*5.
	call SETTIC(xtic)
	ytic=ytic*5.	!smaller tics for sqrt scale for now
	xcross=xmin
	ycross=ymin
c
	autplt=.false.		!for VHIST
	draft=.false.		!for VHIST
	doframe=.true.		!for VHIST
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=2 for op/bst histos (OBHIST)
c IFITYPE=3 for amp  histos (AHIST)
c IFITYPE=-3 for amp  histos (AHIST) if SD are constrained to be equal
	ifitype=0
	fitted=.false.
	landscap=.true.
	isval=1		!show arrow
	call flush(7)
c
108	FORMAT(/)
	lt2=2       !short dash for lo,hi bins
	cbig=2.5
	ifont=4
	ilog=0
	iscal=0     !use input xmax etc
	xlo=-1
	titlex='Randomised difference between means'
	titley='Frequency'
	ilabel=1
	itit=0
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
	yval(0,1)=flo
	yval(nbin+2,1)=fhi
	xval(0,1)=xval(1,1)-xwbase
	xval(nbin+2,1)=xval(nbin+1,1)+xwbase
c curves:
	ncurvc=0
	xw=0.0	!no addition to axis labels
	ntx=5
	nty=5
	itx=1
	ity=1
	iask=-2
	iver=1100
	ndv1=511
	ndimd=1
	ndc1=2048
	ndimd=1
	kmax=1
	isetcol=0	!use defaults

	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
C
	ans='N'
	call DCASK('Another display',ans,ans)
	if(ans.eq.'Y') goto 81
	RETURN
c
	END



