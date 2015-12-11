	subroutine SORTBCH(YVAL,FREQ,XAXIS,nyval,logt,nbin,xwbase,
     & fmax,flo,fhi)
c To set bin widths for histo display in CVFIT
	dimension YVAL(1024)
	dimension FREQ(510),XAXIS(510)
	character*1 ans,UC
	logical pon,logt,debug
	logical slock,caplock
	logical discprt
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
108	format(/)
cc No need to repeat this if SHIST (same histo, same bins)
345	CONTINUE
c	if(shist.and.sbin.and.sexp) goto 4071
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
	do 221 m=1,nbin
	if(yv.ge.xaxis(m).and.yv.lt.xaxis(m+1)) freq(m)=freq(m)+1.0
221	continue	!m loop (bins)
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
c
	IF(FLO.LT.0.1) FLO=-2.	!DO NOT PLOT IF FLO=0
	IF(FHI.LT.0.1) FHI=-2.	!DITTO
	CONTINUE
	FMAX=0.
C CONVERT FREQ TO FREQ DENSITY NEXT
c N.B. FLO and FHI are abs numbers, not densities- cannot express FHI
c as a density if upper lim is infinite! FLO can be expressed as density
c once bin width has been fixed (see VHIST)
	DO 218 m=1,NBIN
	if(.not.logt) FREQ(m)=FREQ(m)*XWBASE/(XAXIS(m+1)-XAXIS(m))
	IF(FREQ(m).GT.FMAX) FMAX=FREQ(m)
218	CONTINUE
	IF(FLO.GT.FMAX) FMAX=FLO
	IF(FHI.GT.FMAX) FMAX=FHI
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	m=ifix(0.1+fhi)
	if(fhi.lt.0.) m=0
	print 222,i,m
	if(pon()) write(7,222) i,m
	if(discprt) write(8,222) i,m
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
c	call flush(7)
	if(flo.lt.0.) flo=0.
	if(fhi.lt.0.) fhi=0.
c
	RETURN
	end

