	subroutine SCVDU(XAXIS,XMIN,XMAX,FREQ,NBIN,FMIN,FMAX,FLO,FHI,
     & FITTED,PREFIT,AHIST,OBHIST,THIST,tres,IDEST,xtic,ftic,idiskq,
     & logt,first,titlex,titley,xwbase,xw,ilabel,ncalc,xcalc,fcalc,
     & obfreq,frqcal,theta,ncomp,kmax,ntx,nty,itx,ity,idtype,
     & amean,area,sd,antrue,isg,gamdst,sdcon,lastfit,
     & nfits,thetsav,ksav,idsav,ncompsav,nfitsav,ylosav,yhisav,
     & isgsav,fitsav,mono)
c Last line of param are for CALDIS (formerly SCVD2) now called from
c here to calc values with current xmin,xmax.
c Lahey version of SCVDU3 but not now called for 'where next' which
c is now done in subroutine WNEXT
c    IJOIN(j) gives line type for main histo bars: for Hgraph types 0-8
c
c		0=solid line
c		1=dotted line
c		2-6=dashed line with increasing dash length
c		7=dash-dot
c		8=dash-dot-dot
c
c	ILINE(j) defines line type for the jth calculated curve, via ZSEQ
c         (prev numbered 1-5, but now make 0-4 so 0=continuous as in Hgraph)
c		0=continuous line
c		1=long dash
c		2=short dash
c		3=long,short dash
c		4=long,short,short dash
c
C Version of SCVDU for EKDIS3
C REDUCED VERSION to be used with SCVD1 and SCVD2 to reduce size
c of graphics overlay Apr 1987
c Apr87- modif for log(t) display
c DC version for 11-73. Aug 1986.
C SUBROUTINE FOR EKDIST TO DO ALL DISPLAY (AS SCVDU BUT
C SUBLEV IS PARAM: NEEDED FOR WRITE TO DISK)
C LAST ROW OF PARAM IS ONLY FOR COMMUNICATION TO PLOTING DISC FILE
C	IDEST IS INTEGER VAR TO SPECIFY DESTINATION AFTER DISPLAY
C	ON ENTRY IDEST=31 FOR DISPLAY OF GAUSSIAN-TRANSFORMED TIMES
C
c	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
c	real XCAL(512,ndimc),YCAL(512,ndimc)
cc for histo data
c	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
cc for calc curves
c	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real XVAL(0:511,3),YVAL(0:511,3)
	real XCAL(512,5),YCAL(512,5)
c	real XVAL(:,:),YVAL(:,:)
c	real XCAL(:,:),YCAL(:,:)
c	ALLOCATABLE::xval,yval,xcal,ycal
c for histo data
	integer ndat(3),icurvd(3),ijoin(3)
c for calc curves
	integer ncal(5),icurvc(5),iline(5)
c
	real theta(20),theta1(20)
	real thetsav(20,5),ylosav(5),yhisav(5)	!to save past fits
	integer ksav(5),idsav(5),ncompsav(5),nfitsav(5),isgsav(5) !to save past fits
	character*12 fitsav(5)			!to save past fits
	dimension XAXIS(510),FREQ(510)
	dimension xcalc(512),fcalc(512)
	dimension obfreq(510),frqcal(510)
	real amean(10),area(10),sd(10)	!for CALDIS
	real amean1(10),area1(10),sd1(10)	!for CALDIS
	character*40 titlex,titley
	character*44 title1
	character*79 note
	character*1 ans,UC
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	LOGICAL AHIST,OBHIST,THIST,FITTED,PREFIT	!PARAM
	logical pon,logt,first,slock,debug,caplock,gamdst,mono
	logical ahist1,obhist1,thist1,gamdst1
	logical landscap,autplt,draft,doframe,sdcon,lastfit
	logical sbin,shist,sres,sexp
	logical discprt
c
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
C
C
	pon()=slock()
	debug()=caplock()
c
c	real XVAL(0:511,3),
c	real XCAL(512,3),YCAL(512,3)
c	ALLOCATE(stat=ierr,xval(0:511,3),YVAL(0:511,3))
c	print*,' ierr = ',ierr
c	ALLOCATE(stat=ierr,xcal(512,3),ycal(512,3))
c	print*,' ierr = ',ierr
	ict=11	!text colour for DIALOG box
	if(mono) ict=15
	autplt=.false.		!for VHIST
	draft=.false.		!for VHIST
	doframe=.true.		!for VHIST
	landscap=.true.		!for VHIST
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
	ndimd=3
	ndimc=5
	iask=2	!do not ask before leaving display; leave graph on screen
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=2 for op/bst histos (OBHIST)
c IFITYPE=3 for amp  histos (AHIST)
c IFITYPE=-3 for amp  histos (AHIST) if SD are constrained to be equal
	ifitype=0
      if((.not.fitted).and.lastfit) then
         print 2051
2051     format('&Plot the last fitted curve on this histogram [N] = ')
         read 101,ans
         if(UC(ans).eq.'Y') fitted=.true.
      endif
	if(fitted.or.prefit) then
	   if(thist) ifitype=1
	   if(obhist) ifitype=2
	   if(ahist) ifitype=3
	   if(ifitype.eq.3.and.sdcon) ifitype=-3	!SD constant
	endif
c	pause
	call flush(7)
	first=.false.
c
	if(idest.ne.31) idest=0
101	format(a1)
108	FORMAT(/)
	lt2=2       !short dash for lo,hi bins
	ilog=0
c	if(logt) ilog=1		!plot vs log(t)
	if(logt) ilog=6		!plot sqrt(y) vs log(t)
	iscal=0			!use input xmin,xmax etc
	ixlo=-1
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
	do i=1,5
	   icurvc(i)=i	!in case needed
	   iline(i)=0	!continuous
	enddo
c Now fix lo,hi bins etc and define xmin,fmin etc (all here rather than
c in SCVD1 now)
	if(fitted.or.prefit) goto 110		!XMIN etc already fixed
c
	fmin=0.
	xmin=0.
	if(logt) xmin=xaxis(1)		!if no lo bin
	if(AHIST.or.IDEST.eq.31) XMIN=XAXIS(1)
	xmax=xaxis(nbin+1)	!=xval(nbin+1,1), if no hi bin
c Round up xmax, but not if it is already an 'integer')
	if(abs(amod(xmax,1.0)).gt.1.e-4) xmax=float(1+ifix(xmax))
	if(.not.ahist) xmax=float(1+ifix(1.2*xmax))	!ROUND UP
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
	if(.not.logt) ftic=ftic*5.	!smaller tics for sqrt scale for now
c
	yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
	xval(0,1)=xval(1,1)	!if no flo- it is checked in VHIST (in MINMAX)
	xval(nbin+2,1)=xval(nbin+1,1)		!ditto if no fhi bin
	if(flo.gt.0.0001) then
	   xval(0,1)=xval(1,1)-xwbase
	   if(logt) xval(0,1)=xval(1,1)*(xval(1,1)/xval(2,1))
	   xmin=xval(0,1)
	endif
	yval(nbin+2,1)=fhi	!NB Yval(nbin+1,1) not used- set (as flo, above)
	if(fhi.gt.0.0001) then
	   xval(nbin+2,1)=xval(nbin+1,1)+xwbase
	   if(logt) xval(nbin+2,1)=xval(nbin+1,1)*
     &	(xval(nbin+1,1)/xval(nbin,1))
c	  keep ratio for hi bin as for other bins, so same width on log scale
	   xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
	   xmax=xval(nbin+2,1)
	 endif
c	yval(0,1)=flo
c	yval(nbin+2,1)=fhi	!NB Yval(nbin+1,j) not used
cc	xval(0,1)=xval(1,1)-xw
c xw is set to 0. for log histos (to suppress 'per 10 ms'), but may
c have lo,hi bins in this case too
c	if(.not.logt) then
c	   xval(0,1)=xval(1,1)-xw
c	   if(thist) xval(0,1)=tres 		!start FLO bin at resolution
c	   xval(nbin+2,1)=xval(nbin+1,1)+xw
c	else
c	   xval(0,1)=tres
cc	  keep ratio for hi bin as for other bins, so same width on log scale
c	   xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
c	endif
c
c Curves:  (NB may have calc curve- to show exponential shape in logt
c plots) even if not fitted
	if(ncalc.gt.0) then
	  ncurvc=1		!copy first calc curve here
	  ncal(1)=ncalc
	  icurvc(1)=1
	  iline(1)=0		!so calc curve is continuous
	  do 2 i=1,ncal(1)
		xcal(i,1)=xcalc(i)
		ycal(i,1)=fcalc(i)
2	  continue
	endif
c
	if(logt) iscal=1	!scale internally 1st time
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
c curves:
	ncurvc=0		!no calc curve yet
	if(logt) then
	  ncurvc=1	!already copied in
	  iline(1)=2		!so calc curve is short dashed
	endif
	if(debug()) then
	  print 77,xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw
77	  format(' xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw=',/,9g13.6)
	  print 771,titlex,titley
771	  format(' X axis: ',a40,/,' Y axis: ',a40)
	  pause
	endif
	call VHIST2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp,sval,isval,iask)
c
	goto 201
C
110	CONTINUE
C NOW SECTION DONE ONLY WHEN RETURNING FROM FITTING,TO CALC
C AND DISPLAY FITTED CURVE
c Try calculating the curves here, after xmin etc defined, so curve
c covers whole range? But will cover whole X-range only- will not help
c if curve gets outside ymin=ymax before xmin/xmax reached- will still miss
c a bit of the curve
c NB for dist of log(t) the values of XMIN,XMAX are in log10 units
c from 1st call to VHIST so dxcalc=log10(factor)
	call CALDIS(XAXIS,XMIN,XMAX,FREQ,NBIN,XWBASE,
     & AMEAN,AREA,SD,NCOMP,ANTRUE,AHIST,OBHIST,ISG,
     & GAMDST,logt,ncalc,xcalc,fcalc,obfreq,frqcal,idtype)
	if(ncalc.gt.0) then
	  ncurvc=1		!copy first calc curve here
	  ncal(1)=ncalc
	  icurvc(1)=1
	  iline(1)=0		!so calc curve is continuous
	  do 3 i=1,ncal(1)
		xcal(i,1)=xcalc(i)
		ycal(i,1)=fcalc(i)
3	  continue
	endif
257	continue		!come here to repeat display
	IF(AHIST) GOTO 111
	if(idtype.eq.14.or.idtype.eq.52) goto 111	!gaussian for Popen distn
	IF(OBHIST) GOTO 127
c Plot data and calc curve (+calc freq density if req)
c histos:
	if(frqcal(1).gt.-0.5) then
	  ncurvd=2
	  icurvd(2)=2
	  ndat(2)=nbin
	  ijoin(2)=3      !med dashes
	  do 4 i=1,nbin+1
4	    xval(i,2)=xaxis(i)
	  do 41 i=1,nbin
41	    yval(i,2)=frqcal(i)
	  yval(0,2)=-1		!?
	  yval(nbin+2,2)=-1
c	  xval(0,2)=xval(0,1)
c	  xval(nbin+2,2)=xval(nbin+2,1)
	endif
c
c curves:  already copied
	if(debug()) then
	  print 772,xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw
772	  format('Pos#2: xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw='
     &	,/,9g13.6)
	  pause
	endif
	call VHIST2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp,sval,isval,iask)
c
	goto 206
C
C NEXT CALC CURVE FOR AHIST-CALC CURVE ONLY
111	CONTINUE
c Histo and curve already copied into Xval etc
	call VHIST2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp,sval,isval,iask)
	goto 206
C
C NOW SECTION TO CALC SINGLE OR DOUBLE GEOMETRIC AS FITTED IN MLFIT
c Section for case where 'calc curve' is a histo (ops/burst etc)
127	CONTINUE
	ncurvc=0
	ncurvd=2
	icurvd(2)=2
	ndat(2)=nbin	!same for calc and obs histos?
	ijoin(2)=3      !dashed
	do 5 i=1,nbin+1
5	xval(i,2)=xaxis(i)
	do 51 i=1,nbin
51	yval(i,2)=obfreq(i)
	yval(0,2)=-1		!?
	yval(nbin+2,2)=-1
c	xval(0,2)=xval(0,1)
c	xval(nbin+2,2)=xval(nbin+2,1)
1271	continue
	call VHIST2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp,sval,isval,iask)
	goto 206
C
201	CONTINUE
C   AFTER DISPLAY OF TRANSFORMED DIST, RETURN.
	IF(IDEST.EQ.31) GOTO 256
C GO TO MLFIT EVEN FOR OBHIST WITH ONE COMP SO THAT CAN FIT OVER LIMITED
C RANGE AND SO THAT ERRORS CAN BE CALC. N.B. AS ARRANGED NOW CANNOT PLOT
C A NON-FITTED DISPLAY IF ASK FOR CORRELATION TESTS
	IDEST=0
c	call LOCATE(0,0)		!start typing at top of page
c	print 205
c205	FORMAT('&Fit this [Y] ? ')
c	read 101,ans
c	if(UC(ans).ne.'N') GOTO 301
c Now still in graphics mode, so write to the dialog box defined in VHIST
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Fit this display',defolt,ict,cans)
	call GETINPa(cans,ans)
	if(ans.ne.'N') goto 301
c
	IF(IDEST.EQ.21) then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   RETURN
	else
	   goto 206
	endif
301	continue
	IF(IDEST.NE.21) IDEST=2		!FIT ONLY
	IF(IDEST.EQ.21) IDEST=22	!CORRELATIONS AND FIT
c note that if display of transformed variables requested in CORCAL
c cannot do fit even if requested here
	RETURN
C
206	continue
c20	continue	!JUMP TO HERE IF ALREADY FITTED
	call VIDEOMOD(3)
	autplt=.false.		!in case set true below
	call LOCATE(0,0)		!start typing at top of page
207	continue
	iopt=1
c
	if(nfits.gt.1) then
	   print 40
40	   FORMAT(
     &    ' (1) Carry on',/,
     &    ' (2) Type in a note to go on the printout',/,
     &    ' (3) Repeat the same display',/,
     &    ' (4) Display precalculated curve and print parameters',/,
     &    ' (5) Fit the same histogram again',/,
     &    ' (6) Superimpose previous fitted curve(s) on graph',/,
     &    ' Option number [1] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.6) goto 207
	else
	   print 2063
2063	   FORMAT(
     &    ' (1) Carry on',/,
     &    ' (2) Type in a note to go on the printout',/,
     &    ' (3) Repeat the same display',/,
     &    ' (4) Display precalculated curve and print parameters',/,
     &    ' (5) Fit the same histogram again',/,
     &    ' Option number [1] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.5) goto 207
	endif
	if(iopt.eq.2) then
	   call TITENT1(' Type in the note:',note,79,.true.)	!blank=true
	   if(pon()) write(7,4011) note
	   if(discprt) write(8,4011) note
4011	   format(' =============================================',/,
     &    1x,a79,/,
     &    ' =============================================')
	   goto 207		!redisplay?
	else if(iopt.eq.3) then		!redisplay
	   autplt=.true.
	   itit=1
	   goto 257
	else if(iopt.eq.4) then
	   IDEST=4		!IF PRE-CALC CURVE WANTED
	   RETURN
	else if(iopt.eq.5) then
	   IDEST=2		!FIT ONLY
c	   if(VIDEOTYP().ne.18) call INIPLT(0,.false.,1.0)
	   goto 999
	else if(iopt.eq.6) then
	   autplt=.true.
	   itit=1
	   print 46
46	   format(' Previous fits:')
	   do j=1,nfits
	      print 42,j,fitsav(j),ncompsav(j)
42		format(' (',i2,') Fit of ',a12,':',i2,' components')
	   enddo
	   nsuper=1
	   print 43
43	   format(' Number of these to be superimposed [1] = ')
	   call INPUTi(nsuper)
	   jsup=nfits-1	!previous fit=default
	   do i1=1,nsuper
		print 44,i1,nfits
44		format(' (',i2,') Superimpose fit number [',i2,'] = ')
		call INPUTi(jsup)
c	     call ptyp1 (with errors=false) to calc area, amean etc
		do i=1,20
		   theta1(i)=thetsav(i,jsup)
		enddo
		if(fitsav(jsup).eq.'Gaussians') then
		   ahist1=.true.
		else if(fitsav(jsup).eq.'geometrics') then
		   obhist1=.true.
		else if(fitsav(jsup).eq.'exponentials') then
		   thist1=.true.
		else if(fitsav(jsup).eq.'gammas') then
		   gamdst1=.true.
		endif
		if(obhist1) then
		   ncurvd=ncurvd+1
		   j=ncurvd
		else
		   ncurvc=ncurvc+1
		   j=ncurvc
		endif
	      print 45,jsup,j
      	if(pon()) write(7,45) jsup,j
	      if(discprt) write(8,45) jsup,j
45		format(' Superimpose fit # ',i2,' as curve # ',i3,':')
c Calculate the required curve. First calc amean etc (don't change current vals)
		call PTYP1(theta1,area1,amean1,sd1,ksav(jsup),
     &	ncompsav(jsup),yhisav(jsup),ylosav(jsup),nfitsav(jsup),
     &	antrue1,.false.,gamdst1,isgsav(jsup),idsav(jsup),
     &	ahist1,thist1,obhist1)
		call CALDIS(xaxis,xmin,xmax,freq,nbin,xwbase,
     &	 amean1,area1,sd1,ncompsav(jsup),antrue1,ahist1,obhist1,
     &	 isgsav(jsup),
     &	 gamdst1,logt,ncalc,xcalc,fcalc,obfreq,frqcal,idsav(jsup))
c	now put results in xcal,ycal (or xval,yval for geometrics)
c 	 (j defined above as nurvc or ncurvd as appropriate)
		if(obhist1) then
		   ndat(j)=nbin
		   icurvd(j)=j
		   ijoin(j)=4
		   do i=1,nbin+1
			xval(i,j)=xaxis(i)
		   enddo
		   do i=1,nbin
			yval(i,j)=obfreq(i)
		   enddo
		   yval(0,j)=-1
		   yval(nbin+2,j)=-1
		else
		   ncal(j)=ncalc
		   icurvc(j)=j
		   iline(j)=0		!so calc curve is continuous
		   do i=1,ncalc
			xcal(i,j)=xcalc(i)
			ycal(i,j)=fcalc(i)
		   enddo
		endif
	   enddo		!end of jsup loop
	   if(obhist) goto 1271
	   goto 257
	else
	   goto 2064	!carry on
	endif
c
c This analysis ended so choose where next
256	continue
	idest=-1		!options 5-8 only in WNEXT
	goto 999
2064	continue
	IDEST=0
c	RETURN
c
999	continue
cc If fitted, copy the calc curve into xcal(i,3),ycal(i,3) so it can be
cc superimposed on next fit
c	if(fitted) then
c	   do i=1,ncal(1)
c		xcal(i,3)=xcal(i,1)
c		ycal(i,3)=ycal(i,1)
c	   enddo
c	   ncal(3)=ncal(1)
c	   fitdef=.true.
c	endif
	RETURN
c
	END



