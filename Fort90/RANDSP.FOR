	subroutine RANDSP(randiff,nran,sval,paired,binomial,icrit)
	real XVAL(0:511,1),YVAL(0:511,1)
	real XCAL(2048,1),YCAL(2048,1)
c for histo data
	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
	integer ncal(1),icurvc(1),iline(1)
c
	real theta(20),randiff(500000)
	dimension XAXIS(510),FREQ(510),freq1(510)
c	integer*2 videotyp
	character*40 titlex,titley
	character*64 title1
	character*1 ans,UC
	LOGICAL fitted,paired,binomial
	logical pon,slock,autplt,draft,doframe,landscap,debug,caplock
	logical discprt
c
	common/dp/discprt
C
C
	pon()=slock()
	debug()=caplock()
c
	print 26
26	format(' Display randomisation distribution [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') RETURN
c
c First put into bins
81	continue
	call VARV1(randiff,nran,XBAR,VARx,xmin,xmax)
	print 413,nran,xBAR,sqrt(varx),xmin,xmax
	if(pon()) write(7,413) nran,xBAR,sqrt(varx),xmin,xmax
	if(discprt) write(8,413) nran,xBAR,sqrt(varx),xmin,xmax
413	FORMAT(' Mean and SD of ',i6,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
c
501	print 208
208	FORMAT(
     & ' Number of different bin widths (-1 to skip histo) [1]= ')
	call INPUTi(NBW)
	IF(NBW.EQ.0) NBW=1
	IF(NBW.LE.-1) GOTO 90		!SKIP
28	xmin=float(ifixr(xmin))-1.0	!round down
	print 209,xmin
209	FORMAT(
     & '&(1)Start histogram at x = ',g10.3,/,
     & ' (2) Specify starting value for x axis',/,
     & ' Option number [1] = ')
	call INPUTi(iopt)
	if(iopt.eq.0) iopt=1
	if(iopt.eq.1) goto 29
	print 27
27	format(
     & '& Start histogram at x = ')
	call INPUTr(xmin)
29	continue
	xaxis(1)=xmin
c
C   GET BIN WIDTHS
c Calculate bin boundaries in XAXIS
	K=1		!COUNTS X AXIS ENTRIES
303	continue
	DO 210 I=1,NBW
	KLAST=K
212	print 104,I
104	FORMAT('&(',I2,') Bin width, Last x value = ')
	call input2r(DX,XEND)
32	XN=(XEND-XAXIS(KLAST))/DX
C ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
c- also check that not more than 101 bins
	IF(ABS(XN-FLOAT(IFIX(0.5+XN))).lt.DX*.01) GOTO 30	!OK
	xend=xaxis(klast) + float(ifixr(xn))*dx
	call bell(1)
	print 31,xend
31	format('&Last x reset to ',g13.6,' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 212
	goto 32
c
30	NBIN=IFIX(0.5+XN)
311	IF(NBIN.EQ.0) GOTO 212
	DO 211 J=1,NBIN
	K=K+1
	if(k.le.510) goto 500
	call bell(2)
	print *, ' TOO MANY BINS'
	goto 501
500	XAXIS(K)=XAXIS(KLAST)+FLOAT(J)*DX
211	continue
c
	if(pon()) write(7,2111) nbin,dx,xaxis(klast),xaxis(k)
	if(discprt) write(8,2111) nbin,dx,xaxis(klast),xaxis(k)
	print 2111,nbin,dx,xaxis(klast),xaxis(k)
2111	format(1x,i5,
     & ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
210	CONTINUE	!END OF I LOOP
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
	do 83 i=1,510
	freq1(i)=0.0	!for test of new binning method
83	freq(i)=0.0
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
	print 407
407	FORMAT( ' Type frequency densities [N] ? ')
	read 101,ans
	if(UC(ans).NE.'Y') GOTO 406
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
101	format(a1)
108	FORMAT(/)
	lt2=2       !short dash for lo,hi bins
	cbig=2.5
	ifont=4
	ilog=0
	iscal=0     !use input xmax etc
	xlo=-1
	if(binomial) then
	   if(icrit.eq.1) then
		titlex='Random number of successes in set 1'
	   else
		titlex='Random value of (p1 - p2)'
	   endif
	else if(paired) then
	   titlex='Randomised mean difference'
	else
	   titlex='Randomised difference between means'
	endif
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
	kmax=20
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
90	continue
	print 80
80	format(' Another display ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 81
	RETURN
c
	END



