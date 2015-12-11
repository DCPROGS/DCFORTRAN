	Subroutine SETBIN(YVAL,FREQ,XAXIS,nyval,thist,ohist,bhist,
     & ghist,obhist,logt,tres,obmax,idtype,xsav,ybar,nbin,nbw,xwbase,
     & nbdec,fmin,fmax,flo,fhi,ymin,ymax)
c 05/28/89 08:46pm Lahey version
c
c To set bin widths in EKDIST (previously this was last part of
c CDIST1,2, from label 207 on)
	dimension YVAL(20480)
	dimension dum(1)		!for debtyp call
	dimension FREQ(510),XAXIS(510),xsav(5,4)
	character*1 ans,UC
	logical pon,thist,ohist,bhist,obhist,ghist,logt,debug
	logical sbin,shist,sres,sexp
	logical slock,caplock
	logical discprt
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
108	format(/)
207	CONTINUE
	if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
c (if data read from YVALIN thist already set true, so do not reset here)
	if(idtype.ne.50) THIST=OHIST.or.GHIST.or.BHIST	  !I.E. X AXIS IS TIME
C
	IF(NYVAL.gt.0) goto 4131
	print 4132
4132	format(' NO VALUES FOR DISTRIBUTION')
	call BELL(4)
	STOP
4131	continue
c
c	Sumy=0.
c	DO 412 I=1,NYVAL	!**
c	Sumy=Sumy+YVAL(I)
c412	CONTINUE
c	YBAR=SUMY/FLOAT(NYVAL)
	call VARV1(Yval,Nyval,YBAR,VARY,ymin,ymax)
	print 413,NYVAL,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) NYVAL,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) NYVAL,YBAR,sqrt(vary),ymin,ymax
413	FORMAT(' Mean and SD of ',I5,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
	if(obhist) obmax=ymax
C
	if(debug()) call DEBTYP(0,-1,-1,dum,dum,acal,
     & nyval,0,0,yval,pon())	!print YVAL(j)
c
c Temp section to write Yval to disc
c	if(not.debug()) goto 230
c	print 231
c231	format(' Write Yval to disc [N] ? ')
c	read 101,ans
c	if(UC(ans).ne.'Y') goto 230
c	print 232
c232	format('&Record number [0=cancel] = ')
c	read 102,irec
c	if(irec.le.0) goto 230
cc#### print what's done
c230	continue
c
C NOW GET X AXIS
	if(.not.sbin) logt=.false.
	if(OBHIST) goto 344
	if(sbin) goto 303	!same bins so XAXIS,XWBASE as before
	iopt=-1			!if NOT thist
	if(.not.thist) goto 501
 	if(idtype.eq.14.or.idtype.eq.52) then
		iopt=2
		goto 503	!burst P(open) dist (id=52 if from YVAL.DAT)
	endif
	print 2092
2092	format(
     & ' [1] Distribution of log durations',/,
     & ' (2) Distribution of durations- set bins manually',/,
     & ' (3) Distribution of durations- 20 bins',/,
     & ' (4) Distribution of durations- 40 bins',/,
     & ' (5) Distribution of durations- 60 bins',/,
     & ' (6) Skip histogram',/,
     & ' Option number [1] = ')
c     & '',/,
	read 102,iopt
	if(iopt.eq.0) iopt=1
	if(iopt.ne.6) goto 40
90	nbin=0
	RETURN		!skip histo
40	logt=iopt.eq.1
501	nbw=1
	if(logt.or.iopt.ge.3) goto 2093
503	print 208
208	FORMAT(
     & ' Number of different bin widths (-1 to skip histo) [1]= ')
	read 102,NBW
	IF(NBW.EQ.0) NBW=1
	IF(NBW.LE.-1) GOTO 90		!SKIP
102	FORMAT(I8)
2093	if(.not.thist) goto 28
	if(idtype.ge.50) goto 28	!YVAL.DAT data
	if(bhist.and.idtype.eq.14) goto 28	!dist of burst Popens
	xaxis(1)=tres
	if(iopt.ge.3) goto 29	!auto bins
	print 2091,tres
2091	format('&Histogram to start at ',f8.5,' ms.  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 29
28	print 209
209	FORMAT('&Start histogram at x (ms, pA etc) [0.0] = ')
c	read 103,XAXIS(1)
	call INPUTr(xaxis(1))
c103	FORMAT(G13.6)
c
29	continue
c
C   GET BIN WIDTHS
c Calculate bin boundaries in XAXIS
	K=1		!COUNTS X AXIS ENTRIES
303	continue
c Sep bit for dist of log(t)- have NBW bins starting at xaxis(1), up to xend
	if(.not.logt) goto 308
	if(sbin) then
	   dx=exp(alog(10.)/float(nbdec))
	   goto 73
	endif
	if(xaxis(1).le.0.) then
	   call BELL(3)
	   print 3031,xaxis(1)
3031	   format(' Can not start log scale at t = ',g11.4,/,
     &   ' New starting value (e.g. 0.01) = ')
	   call INPUTr(xaxis(1))
	endif
	if(nyval.le.500) nbdec=8	!default
	if(nyval.gt.500.and.nyval.le.1000) nbdec=10	!default
	if(nyval.gt.1000) nbdec=12	!default
1048	continue
	print 1041,nbdec
1041	format(' Number of bins/decade [',i3,'] = ')
	read 102,i
	if(i.gt.0) nbdec=i
	dx=exp(alog(10.)/float(nbdec))
c	xend=float(1+ifix(ymax))
	xend=1. + ymax - amod(ymax,1.)
	print 1046,xend
1046	format('&Last x value (ms)= ',g13.6,'  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 71
	print 72
72	format('&Last x value (ms)= ')
	call INPUTr(xend)
71	nbin=1+ifix(alog(xend/xaxis(1))/alog(dx))
	xend=(dx**nbin)*xaxis(1)
	print 1047,nbin,xend
1047	format('&',1x,i5,' bins; last x reset to ',g13.6,': O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') goto 1048
	do 1043 i=1,nbin
1043	xaxis(i+1)=xaxis(1)*(dx**i)
	klast=nbin+1
73	xwbase=alog10(dx)	  !in log10 units to scale fit
	print 1045,nbin,dx
	if(pon()) write(7,1045) nbin,dx
	if(discprt) write(8,1045) nbin,dx
1045	format(
     & ' Distribution of log(t) displayed- ',i6,' bins, factor= ',g11.4)
	goto 345	!end of logt bit
c Section done when not logt
308	continue
	DO 210 I=1,NBW
	if(sbin) goto 306
	if(iopt.lt.3) goto 309
c auto-binwidth to give 20,40,60 bins (nbw=1, not logt)
	nbin=20
	if(iopt.eq.4) nbin=40
	if(iopt.eq.5) nbin=60
	dx=(ymax-ymin)/float(nbin)
	call SETTIC(dx)
	klast=1
	goto 311
309	continue
	KLAST=K
212	print 104,I
104	FORMAT(' (',I2,') Bin width, Last x value (ms, pA etc)= ')
	call INPUT2r(dx,xend)
	if(dx.le.0.00001.or.xend.le.dx) goto 212
32	XN=(XEND-XAXIS(KLAST))/DX
C ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
c- also check that not more than 101 bins
	IF(ABS(XN-FLOAT(IFIX(0.5+XN))).lt.DX*.01) GOTO 30	!OK
C	xend=xaxis(klast) + float(ifix(0.5+xn))*dx
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
	if(k.le.101) goto 500
	call bell(2)
	print *, ' TOO MANY BINS'
	goto 501
500	XAXIS(K)=XAXIS(KLAST)+FLOAT(J)*DX
211	continue
c	if(.not.pon()) goto 210
c   save values for printing repeated with same bins
1044	xsav(i,1)=float(nbin)
	xsav(i,2)=dx
	xsav(i,3)=xaxis(klast)
	xsav(i,4)=xaxis(k)
306	if(pon()) write(7,2111) (xsav(i,m),m=1,4)
	if(discprt) write(8,2111) (xsav(i,m),m=1,4)
	if(sbin.or.iopt.ge.3) print 2111, (xsav(i,m),m=1,4)
2111	format(1x,f5.0,
     & ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
210	CONTINUE	!END OF I LOOP
C
	if(sbin) goto 307
	NBIN=K-1		!NOW NBIN=TOTAL NO OF BINS
cD	DO 213 I=1,NBIN+1	!SO NBIN+1 ENTRIES IN XAXIS
cD	print 106,I,XAXIS(I)
cD213	CONTINUE
cD	print 108
	XWBASE=DX		!IF ONLY ONE BIN WIDTH
	IF(NBW.EQ.1) GOTO 307
	print 107
107	FORMAT( ' Base width for frequency density= ')
	call INPUTr(xwbase)
307	if(pon()) write(7,1071) xwbase
	if(discprt) write(8,1071) xwbase
	if(sbin) print 1071,xwbase
1071	FORMAT( ' Base width for frequency density= ',g13.6)
	goto 345
c
C NEXT SPECIAL SECTION TO FIX XAXIS IN OBHIST CASE
344	NBW=1		!ALWAYS FOR OBHIST
	if(sbin) goto 310
	print 3481,ifixr(obmax)
3481	format(
     & ' Max number of openings/burst to show (integer) [',i3,'] = ')
	read 102,j
	if(j.eq.0) then
	   obm1=obmax
	else
	   obm1=float(j)
	endif
	print 348
348	FORMAT (
     & '&No of bins to pool ([1] unless mean is big; -1=skip histo)= ')
	read 102,J
	if(j.eq.0) j=1
	if(j.ge.1) goto 41
	nbin=0
	RETURN		!skip histo
41	DX=FLOAT(J)
	XWBASE=DX
	NBIN=2+IFIX(0.51+OBM1/DX)	!TWO EXTRA BINS FOR LOOKS
	DO 347 I=1,NBIN+1
347	XAXIS(I)=0.5 + FLOAT(I-1)*DX
C =0.5,1.5,2.5,.. FOR DX=1. =0.5,2.5,4.5.. FOR DX=2.
C =0.5,3.5,6.5 FOR DX=3 ETC
c   save values for printing when repeated with same bins
	xsav(1,1)=float(nbin)
	xsav(1,2)=dx
	xsav(1,3)=xaxis(1)
	xsav(1,4)=xaxis(nbin+1)
310	if(pon()) write(7,2111) (xsav(1,m),m=1,4)
	if(discprt) write(8,2111) (xsav(1,m),m=1,4)
	if(sbin) print 2111,(xsav(1,m),m=1,4)
C
C NOW SORT Y VALUES AND CALC FREQ (DENSITY) FOR EACH FILE
c No need to repeat this if SHIST (same histo, same bins)
345	CONTINUE
	if(shist.and.sbin.and.sexp) goto 4071
	print 82
82	format(' Sorting data into bins...')
	flo=0.
	fhi=0.
	do 83 i=1,510
83	freq(i)=0.0
c
c	if(nbw.eq.1.and.(.not.logt)) goto 84	!use fast method-NO
	do 223 i=1,nyval
	yv=yval(i)
	if(yv.lt.xaxis(1)) then
	   flo=flo+1.0
	   goto 223
	endif
c	if(yv.ge.xaxis(nbin+1)) then
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
C Test new binning method (for single bin width only)
c NO- omit for now because rounding errors make it unclear what
c happens to valuse exactly on bin boundaries
c84	continue
c	do 2231 i=1,nyval
c	yv=yval(i)
c	if(yv.lt.xaxis(1)) then
c	   flo=flo+1.0
c	   goto 2231
c	endif
cc	if(yv.ge.xaxis(nbin+1)) then
c	if(yv.gt.xaxis(nbin+1)) then
c	   fhi=fhi+1.0
c	   goto 2231
c	endif
c	k=1 + ifix((yv-xaxis(1))/dx)
c	freq(k)=freq(k)+1.0
c2231	continue	!i loop (obs)
c
c Sorting done
85	continue
c	DO 223 J=1,NBIN
c	DO 221 I=1,NYVAL	!INDEX OF YVAL
c	IF(YVAL(I).LT.XAXIS(J)) GOTO 221
c	IF(YVAL(I).GE.XAXIS(J+1)) GOTO 221
c	FREQ(J)=FREQ(J)+1.
c221	CONTINUE	!I LOOP
c223	CONTINUE	!J LOOP
cC NOW GET VALUES BELOW XAXIS(1) AND ABOVE X(NBIN+1)
c	FLO=-2.0		!DEFAULT- NO FLO PLOT
c	FHI=-2.0
c	IF(OBHIST) GOTO 400
c	FLO=0.		!always 0. for OBHIST
c	FHI=0.
c302	CONTINUE
c	DO 401 I=1,NYVAL	!INDEX OF YVAL
c	IF(YVAL(I).LT.XAXIS(1)) FLO=FLO+1.0
c	IF(YVAL(I).GE.XAXIS(NBIN+1)) FHI=FHI+1.0
c401	CONTINUE
cC
c400	CONTINUE
	IF(IDtype.EQ.11) GOTO 4061			!RETURN
c
	IF(FLO.LT.0.1) FLO=-2.	!DO NOT PLOT IF FLO=0
	IF(FHI.LT.0.1) FHI=-2.	!DITTO
	CONTINUE
	FMAX=0.
C CONVERT FREQ TO FREQ DENSITY NEXT
c N.B. FLO and FHI are abs numbers, not densities- cannot express FHI
c as a density if upper lim is infinite! FLO can be expressed as density
c once bin width has been fixed (see VHIST)
	DO 218 J=1,NBIN
	if(.not.logt) FREQ(J)=FREQ(J)*XWBASE/(XAXIS(J+1)-XAXIS(J))
	IF(FREQ(J).GT.FMAX) FMAX=FREQ(J)
218	CONTINUE
	IF(FLO.GT.FMAX) FMAX=FLO
	IF(FHI.GT.FMAX) FMAX=FHI
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
	if(UC(ans).NE.'Y') GOTO 406
	if(.not.logt) then
		print 1061
     		if(pon()) write(7,1061)
		if(discprt) write(8,1061)
1061 		format(
     & ' Bin no.         X values                Frequency')
		DO 220 I=1,NBIN
		print 106,I,xaxis(i),xaxis(i+1),FREQ(I)
		if(pon()) write(7,106) I,xaxis(i),xaxis(i+1),FREQ(I)
220		if(discprt) write(8,106) I,xaxis(i),xaxis(i+1),FREQ(I)
106		FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
	endif
c
	if(logt) then
		print 1062
     		if(pon()) write(7,1062)
		if(discprt) write(8,1062)
1062 		format(
     & ' Bin no.         X values                Frequency',/,
     & '              log(X) values              sqrt(freq)')
		DO 1063 I=1,NBIN
		print 1064,I,xaxis(i),xaxis(i+1),FREQ(I)
		if(pon()) write(7,1064) I,xaxis(i),xaxis(i+1),FREQ(I)
		if(discprt) write(8,1064) I,xaxis(i),xaxis(i+1),FREQ(I)
		if((xaxis(i).le.0.).or.(freq(i).lt.0.)) goto 1063
		print 1065,alog10(xaxis(i)),
     &	alog10(xaxis(i+1)),sqrt(freq(i))
		if(pon()) write(7,1065) alog10(xaxis(i)),alog10(xaxis(i+1)),
     &	sqrt(freq(i))
		if(discprt) write(8,1065) alog10(xaxis(i)),
     &	alog10(xaxis(i+1)),sqrt(freq(i))
1063		continue
1064		FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
1065		FORMAT(8x,   g13.6,' - ',g13.6,3x,G13.6)
	endif
c
		print 108
		if(pon()) write(7,108)
		if(discprt) write(8,108)
	pause ' Display follows'
406	CONTINUE
C
4061	CONTINUE
	call flush(7)
	if(flo.lt.0.) flo=0.
	if(fhi.lt.0.) fhi=0.
	RETURN
C
	END	!of SETBIN


