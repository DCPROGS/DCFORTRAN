	subroutine SBINCH(XAXIS,nyval,ymax,ymin,xsav,nbin,nbw,xwbase,
     & nbdec,logt,iopt,sbin)
c To set bin widths for histo display in CVFIT
	dimension XAXIS(510),xsav(5,4)
	character*1 ans,UC
	logical pon,logt,debug,sbin
	logical slock,caplock
	logical discprt
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
108	format(/)
c
501	continue
	if(sbin) goto 303
	nbw=1
	if(logt) goto 2093
	if(iopt.ge.3) goto 29	!auto bins
	print 208
208	format(
     & ' Number of different bin widths [1]= ')
	read 102,NBW
	if(NBW.eq.0) NBW=1
102	FORMAT(i8)
2093	continue
	print 209
209	FORMAT('&Start histogram at x [0.0] = ')
	read 103,XAXIS(1)
103	format(g13.6)
29	continue
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
	if(xaxis(1).le.0.) goto 2093
	if(nyval.le.500) nbdec=8	!default
	if(nyval.gt.500.and.nyval.le.1000) nbdec=10	!default
	if(nyval.gt.1000) nbdec=12	!default
1048	continue
	print 1041,nbdec
1041	format('&Number of bins/decade [',i3,'] = ')
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
	read 103,xend
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
104	FORMAT('&(',I2,') Bin width, Last x value = ')
	read 105,DX,XEND
105	FORMAT(2G13.6)
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
	DO 211 m=1,NBIN
	K=K+1
	if(k.le.501) goto 500
	call bell(2)
	print *, ' TOO MANY BINS'
	goto 501
500	XAXIS(K)=XAXIS(KLAST)+FLOAT(m)*DX
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
	XWBASE=DX		!IF ONLY ONE BIN WIDTH
	IF(NBW.EQ.1) GOTO 307
	print 107
107	FORMAT( ' Base width for frequency density= ')
	read 103,XWBASE
307	if(pon()) write(7,1071) xwbase
	if(discprt) write(8,1071) xwbase
	if(sbin) print 1071,xwbase
1071	FORMAT( ' Base width for frequency density= ',g13.6)
	goto 345
345	continue
C NEXT SORT Y VALUES AND CALC FREQ (DENSITY) FOR EACH FILE in SORTBCH
	RETURN
c
	end

