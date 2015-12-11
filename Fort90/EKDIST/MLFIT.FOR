	subroutine MLFIT(THETA,YBAR,AHIST,THIST,OBHIST,FITTED,nfix,
     & k,kfit,id,jfix,errfac,ndisp,nbin,sdcon,tres,ymin,ymax,mono)
c NB id=idtype
c Modif 07/06/93 11:06am to put text into the dialog box that was defined
c  in VHIST2 (stll in graphics mode up to call VIDEOMOD(3))
c 06/03/89 02:22pm Lahey version of mlfit3
c
C Version for MLFITS for EKDIS3
c DC modif for 11-73 AUG 1986. Add Xaxis,Freq for fast fit (May 87)
c
C VERSION OF MLFIT1 WITH SIMPLEX RATHER THAN PATTERNSEARCH. TOO BIG SO
C CALLS TO SIMPLEX REMOVED TO SEP OVERLAY (CSIMP.FOR). IF CALLED WITH
C NFIX=-1 DOES EVERYTHING UP TO THESE CALLS, THEN RETURNS TO CALL CSIMP,
C THEN CALL MLFITS AGAIN-WHEN NFIX =>0 GOES STRAIGHT TO END TO FINISH OFF
C LAST 4 PARAM (JFIX-NDISP) ADDED FOR TRANSMISSION TO CSIMP.
C
C VERSION OF MLFIT WITH YVAL AS PARAM RATHER THAN IN COMMON. ALSO PARAM
C FOR PTSCH5,EXPLIK ETC
C SUBLEV,NFILE,IDISK,NVALUE,NEEDED ONLY FOR WRITING TODISC FOR CHAIN TO
C ERROR,PROG (FOR USE ON RETURN TO SCDIST)
	REAL THETA(20)		!PARAM
	INTEGER JFIX(20)
	integer*2 videotyp
C AREA=FRACTIONAL AREA,AMEAN=TAU (FOR THIST) OR MEANS
C (FOR AHIST),SD=STANDARD DEV (FOR AHIST)
C NOW DECLARE LOCAL ARRAYS FOR FITTING
	character*1 ans,UC
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum0*11,cnum1*11,cnum2*11		!for dialog
	LOGICAL AHIST,THIST,DEBUG,FITTED,OBHIST,AMPOS,
     & GAMDST,fastf,logt,mono,graphic
	logical discprt,pon,slock,caplock,sdcon,pon1
	common/dp/discprt
	COMMON/FITBLK/yval(81920),xaxis(510),freq(510),Nyval,
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2
C SUBROUTINE FOR MAX LIKELIHOOD FITTING OF DISTRIBUTION CURVES
C FOR OVERLAY IN SCDIST.FOR
c
	debug()=caplock()
c	pon()=slock()
	pon1=slock()
C
101	format(a1)
108	FORMAT(/)
C
cc		do 1099 i=1,nyval
cc1099		type*,i,' ',yval(i)
	GAMDST=.FALSE.
	fastf=.false.
	ict=11	!text colour for DIALOG box
	graphic=VIDEOTYP().eq.18	!may not be in graphics for refitting
	if(mono) ict=15
	if(.not.logt) goto 50
c
	if(graphic) then
	   ans='N'
	    call DEFOLTa(ans,defolt)
	    call QDIALOG(1,'Use fast (log-binned) ML fitting',
     &     defolt,ict,cans)
	   call GETINPa(cans,ans)
	   fastf=ans.eq.'Y'
	else
	   print 501
501	   format('&Use fast (log-binned) ML fitting [N] ? ')
	   read 101,ans
	   fastf=UC(ans).eq.'Y'
	endif
	if(fastf.and.pon1) write(7,502)
	if(fastf.and.discprt) write(8,502)
502	format(' Fast (log-binned) fitting used')
c
50	continue
	if(thist) then
	 ylow=tres
	 yhigh=10.*ymax
	 if(graphic) then
	   call REALTOCH(ylow,cnum1,11)
	   call REALTOCH(yhigh,cnum2,11)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Fit between Tres, 10*Ymax = '//
     &      CHARNB(cnum1)//' and '//CHARNB(cnum2)//' (inc)',
     &	defolt,ict,cans)
	   call GETINPa(cans,ans)
	 else
	   print 207,ylow,yhigh
207	   format(
     &'&Fit between Tres, 10*Ymax = ',2g13.6,' (inclusive) O.K. [Y] ? ')
	   read 101,ans
	 endif
	 if(UC(ans).eq.'N') then
	   if(graphic) then
		call QDIALOG(1,
     &	 'Fit intervals between Ylow, Yhigh (inclusive) (ms)',
     &	 ' ',ict,cans)
		call GETINP2r(cans,ylow,yhigh)
	   else
		print 208
208		format(
     &	'&Fit intervals between Ylow, Yhigh (inclusive) (ms) = ')
	      call INPUT2r(ylow,yhigh)
	   endif
	 endif
	else if(ahist) then
	   if(graphic) then
		call QDIALOG(1,
     &	 'Fit amplitudes between Ylow, Yhigh (inclusive) (pA)',
     &	 ' ',ict,cans)
		call GETINP2r(cans,ylow,yhigh)
	   else
		print 2081
2081		format(
     &	'&Fit amplitudes between Ylow, Yhigh (inclusive) (pA) = ')
	      call INPUT2r(ylow,yhigh)
	   endif
	else if(obhist) then
	   ylow=1.0
	   yhigh=2.*ymax
	   if(graphic) then
	      call REALTOCH(ylow,cnum1,11)
	      call REALTOCH(yhigh,cnum2,11)
	      ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Fit ops/burst between'//
     &       CHARNB(cnum1)//' and '//CHARNB(cnum2)//' (incl)',
     &	 defolt,ict,cans)
	      call GETINPa(cans,ans)
	   else
	      print 2072,ylow,yhigh
2072	      format(
     &   '&Fit ops/burst between = ',2g13.6,' (inclusive) O.K. [Y] ? ')
	      read 101,ans
	   endif
	   if(UC(ans).eq.'N') then
	      if(graphic) then
		   call QDIALOG(1,
     &	   'Fit ops/burst between Ylow, Yhigh (incl)',
     &	     ' ',ict,cans)
		   call GETINP2r(cans,ylow,yhigh)
		else
		   print 2082
2082		   format(
     &	'&Fit ops/burst between Ylow, Yhigh (inclusive) (ms) = ')
	         call INPUT2r(ylow,yhigh)
		endif
	   endif
	endif
	if(yhigh.gt.ylow) goto 2031
	t=ylow		!swap
	ylow=yhigh
	yhigh=t
2031	continue
c NB for FASTF these must be bin boundaries!
	if(.not.fastf) goto 65
	nb1=1
	nb2=nbin
c
	if(ylow.lt.xaxis(1)) goto 681
	do 66 i=1,nbin
	if(xaxis(i).le.ylow.and.xaxis(i+1).gt.ylow) goto 67
66	continue
67	nb1=i
c
681	if(yhigh.gt.xaxis(nbin+1)) goto 69
	do 661 i=1,nbin
	if(xaxis(i).le.yhigh.and.xaxis(i+1).gt.yhigh) goto 682
661	continue
682	nb2=i
c
69	ylow=xaxis(nb1)
	yhigh=xaxis(nb2)
	if(graphic) then
	   call INTCONV(nb1,cnum1)
	   call INTCONV(nb2,cnum2)
	   call WDIALOG(1,'Fit bins '//CHARNB(cnum1)//' to '
     &    //CHARNB(cnum2),ict)
	else
	   print 71,nb1,nb2
71	   format(' Fitted bins= ',2i6)
	endif
c	if(nb1.ge.1.and.nb2.le.nbin) goto 65	!ylow,yhigh reset succesfully
	if(nb1.lt.1.or.nb2.gt.nbin) then
	   call BELL(1)
	   if(graphic) then
		call OPENDIALOG(2,12,.true.)		!draw dialog box #2
		call WDIALOG(2,'Not in range',12)
	   else
		print 70
70		format(' Values not within range of bins')
	   endif
	   goto 50
	endif
65	continue	!end of fastf ection
c
	IF(OBHIST.AND.YLOW.LT.1.0) YLOW=1.0
	if(pon1) write(7,2071) ylow,yhigh
	if(discprt) write(8,2071) ylow,yhigh
2071	format(' Fitted between ',g13.6,' and ',g13.6)
C 1.0 IS MIN VALUE ALLOWABLE FOR OBHIST- CALC OF PROB(Y<YLOW) IN OBLIK1
C WILL BE NONSENSE IF SMALLER YLOW GIVEN
	IF(.NOT.(THIST.AND.ISG.GT.0.and.(id.eq.7.or.id.eq.8)))
     & GOTO 53
c
	if(graphic) then
	   call QDIALOG(1,
     &    'Fit Gamma distribution (param=OPEN TIME)',
     &     ' ',ict,cans)
	   ans='Y'
	   call GETINPa(cans,ans)
	else
	   print 52
52	   FORMAT('&Fit Gamma distribution (PARAM=OPEN TIME PDF PARAM)? ')
	   read 101,ans
	endif
	if(UC(ans).EQ.'Y') GAMDST=.TRUE.
	if(gamdst.and.pon1) write(7,521) isg+1
	if(gamdst.and.discprt) write(8,521) isg+1
	if(graphic.and.gamdst) then
	   call WDIALOG(1,
     &    'Specify ''one component'' if open time pdf is 1 exp',ict)
	   call WDIALOG(1,
     &    'Specify ''two components'' if open time pdf is 2 exp',ict)
	   call WDIALOG(1,
     &    '   (for sum of two openings only)',ict)
	else if(gamdst) then
	   print 521,isg+1
521	   format(' Gamma distribution for sum of ',i3,'''openings''.',/,
     &'    Parameters for this are the param for the ''open time''',/,
     &' distribution. Specify ''one component'' if the latter is',/,
     &' single-exponential (can be used for sum of ANY number of'/
     &' openings); specify ''two components'' if it is double',/,
     &' exponential (can be used only for the sum of two openings)'/)
	endif
C N.B. 2-COMPONENTS MEANS THAT OPEN TIME PDF IS SUM OF 2 EXPONENTIALS
C WITH TIME CONST AND AREAS THAT ARE PARAM FOR THIS FITTING. FOR
C NCOMP=1 CONVOLUTION OF ANY NUMBER (ISG+1) OF THESE IS A GAMMA
C DISTRIBUTION. IF OPEN TIME IS SUM OF TWO EXP THEN CONVOLUTION
C OF SEVERAL OF THEM IS MORE COMPLEX AND PROG ALLOWS FIT ONLY FOR
C CONV OF TWO (ISG=1)
C
53	continue
	if(ncomp.lt.1) ncomp=1
	if(graphic) then
 	   call DEFOLTi(ncomp,defolt)
	   call QDIALOG(1,'Number of components to be fitted',
     &    defolt,ict,cans)
	   call GETINPi(cans,ncomp)
	else
	   print 1,ncomp
1	   FORMAT('&Number of components to be fitted [',i2,'] = ')
	   call INPUTi(ncomp)
	endif
	IF(GAMDST.AND.(NCOMP.GT.2.OR.(NCOMP.EQ.2.AND.ISG.NE.1)))
     & GOTO 53
c2	FORMAT(I8)
C REDEFINE YVAL SO IT CONTAINS ONLY THE NFIT VALUES BETWEEN
C YLOW AND YHIGH. REDEFINE YBAR AS MEAN OF THESE.
404	NFIT=0
C FOR  OBHIST MAKE SURE THAT VALUES 'EXACTLY' EQUAL TO YLOW,YHIGH
C ARE NOT EXCLUDED
	s=0.
	if(obhist) s=1.0e-5
	YLO=YLOW-s
	YHI=YHIGH+s
c
c	S=0.			!FOR SUM
	DO 30 I=1,NYVAL
c	print 303,i,yval(i)
c303	format(' **',i8,g13.6)
	IF((YVAL(I).LT.YLO).OR.(YVAL(I).GT.YHI)) GOTO 30
	NFIT=NFIT+1
	YVAL(NFIT)=YVAL(I)
c	S=S+YVAL(NFIT)
30	CONTINUE
c	ybar=S/FLOAT(NFIT)
	call VARV1(Yval,Nfit,YBAR,VARY,ymin,ymax)
	if(pon1) write(7,301) NFIT,YBAR,sqrt(vary)
	if(discprt) write(8,301) NFIT,YBAR,sqrt(vary)
	if(graphic) then
	   call INTCONV(nfit,cnum0)
	   call REALTOCH(round1(ybar,2),cnum1,11)
	   call REALTOCH(round1(sqrt(vary),3),cnum2,11)
	   call WDIALOG(1,'Mean and SD of '//CHARNB(cnum0)
     &    //' values in range = '
     &    //CHARNB(cnum1)//', '//CHARNB(cnum2),ict)
	else
	   print 301,NFIT,YBAR,sqrt(vary)
301	   FORMAT(
     &    ' Mean and SD of',I5,' values between YLOW,YHIGH (inc)= ',
     &    2g13.6)
	endif
C
	IF(AHIST) GOTO 3
	if(id.eq.14.or.id.eq.52) goto 3	!fit gau to Popen dist
	IF(OBHIST) GOTO 400
C
C SECTION FOR INIT GUESSES FOR TIME HISTOGRAMS
	K=2*NCOMP-1	!NO OF PARAMETERS
	theta(1)=YBAR-YLOW		!TAU(1) GUESS
93	continue
	if(graphic) then
	   call WDIALOG(1,'Initial guesses:',ict)
	   call QDIALOG(1,'(Param 1): Tau(1) (ms)',
     &     ' ',ict,cans)
	   call GETINPr(cans,theta(1))
	else
	   print 6
6	   FORMAT( ' Initial guesses:',/,' (Param 1): Tau(1) (ms)= ')
         call INPUTr(theta(1))
	endif
	S=0.
	IF(NCOMP.EQ.1) GOTO 4
C
	DO 5 I=2,NCOMP
	IX=2*(I-1)	!=2,4,6..
	if(graphic) then
	   call INTCONV(ix,cnum1)
	   call INTCONV(i-1,cnum2)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &     '): Fractional area of component '//CHARNB(cnum2),
     &     ' ',ict,cans)
	   call GETINPr(cans,theta(ix))
	else
	   print 9,IX,I-1
9	   FORMAT(
     & '&(Param',I2,'): Fractional area of component ',I2,' = ')
         call INPUTr(theta(ix))
	endif
	S=S+theta(ix)
	if(S.ge.1.0) then
	   call BELL(2)
	   if(graphic) then
		call OPENDIALOG(2,12,.true.)		!draw dialog box #2
		call WDIALOG(2,'AREA > 1',12)
	   else
		print 92
92		FORMAT(' *** TOTAL AREA GREATER THAN ONE. TRY AGAIN'/)
	   endif
	   goto 93
	endif
	if(graphic) then
	   call INTCONV(2*i-1,cnum1)
	   call INTCONV(i,cnum2)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &     '): Tau('//CHARNB(cnum2)//') (ms)',' ',ict,cans)
	   call GETINPr(cans,theta(2*i-1))
	else
	   print 7,2*I-1,I
7	   FORMAT('&(Param',I2,'): Tau(',I2,') (ms)= ')
         call INPUTr(theta(2*i-1))
	endif
c TAU=THETA 1,3,5,7,9 (1 SET ABOVE)
c8	FORMAT(G13.6)
5	CONTINUE
C NOW CALC INIT GUESSES FOR A(I) FROM REL AREA GUESSES. S CONTAINS
C SUM FROM I=1,NCOMP-1, OF AREA(I)=1.0-AREA(NCOMP)
	theta(2*NCOMP)=1.0-S	!==SURELY NOT NEEDED!
	GOTO 4		!INIT GUESSES SET
C
3	CONTINUE
C NOW SECTION FOR AMPLITUDE HISTOGRAM GUESSES
C ORDER OF PARAMETERS IS MEAN(1),SD(1),AREA(1),MEAN(2),SD(2)....
C ..SD(NCOMP),AREA(NCOMP)
	K=3*NCOMP-1
	if(ncomp.gt.1) then
	   if(graphic) then
		call DEFOLTa(' ',defolt)
		call QDIALOG(1,' Constrain all SD to be equal',
     &       defolt,ict,cans)
		call GETINPa(cans,ans)
	   else
		print 941
941		format('  Constrain all SD to be equal ? ')
		read 101,ans
	   endif
	   sdcon=.false.
	   if(UC(ans).eq.'Y') then
		k=k-(ncomp-1)
		sdcon=.true.
	   endif
	endif
C
94	S=0.
	J=0
	theta(3)=1.0	!AREA OF COMPONENT 1 UNLESS REDEFINED
	DO 35 I=1,NCOMP
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call INTCONV(i,cnum2)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Mean amplitude('//CHARNB(cnum2)//') (pA)',' ',ict,cans)
	   call GETINPr(cans,theta(j))
	else
	   print 39,J,I
39	   format('&Param(',I2,')  Mean amp(',I2,') (pA)= ')
         call INPUTr(theta(j))
	endif
	if(i.gt.1.and.k.lt.3*ncomp-1) goto 371		!SD equal
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Standard dev('//CHARNB(cnum2)//') (pA)',' ',ict,cans)
	   call GETINPr(cans,theta(j))
	else
	   print 37,J,I
37	   FORMAT('&Param(',I2,') Standard dev(',I2,')= ')
        call INPUTr(theta(j))
	endif
371	IF(NCOMP.EQ.1.OR.I.EQ.NCOMP) GOTO 35
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Fraction with amplitude '//CHARNB(cnum2),' ',ict,cans)
	   call GETINPr(cans,theta(j))
	else
	   print 38,J,I
38	   FORMAT('&Param(',I2,'): Fraction with amplitude(',I2,')= ')
         call INPUTr(theta(j))
	endif
	S=S+theta(J)
c	IF(S.LT.1.0) GOTO 35	!OK
	if(s.ge.1.0) then
	   call BELL(2)
	   if(graphic) then
	      call OPENDIALOG(2,12,.true.)		!draw dialog box #2
	      call WDIALOG(2,'AREA > 1',12)
	   else
	     print 92		! *** TOTAL AREA GREATER THAN ONE. TRY AGAIN
	   endif
	   goto 94
	endif
35	CONTINUE
C
	IF(NCOMP.EQ.1) GOTO 4
c	theta(3*NCOMP)=1.0-S		!=AREA(NCOMP)
	theta(j+1)=1.0-S		!=AREA(NCOMP)
	GOTO 4
C
C NOW SECTION FOR INIT GUESSES FOR GEOMETRIC DISTNS
400	CONTINUE
	k=2*ncomp-1
c	K=1	!NO OF PARAM
212	continue
	if(graphic) then
	   call WDIALOG(1,'Initial guesses:',ict)
	   call QDIALOG(1,'(Param 1): Mean openings/bst for component 1',
     &    ' ',ict,cans)
	   call GETINPr(cans,theta(1))
	else
	   print 200
200	   FORMAT( ' Initial guesses:',/,
     &   ' Param (1): Mean no of openings/burst for component 1= ')
        call INPUTr(theta(1))
	endif
	IF(NCOMP.EQ.1) GOTO 4
	s=0.0
	do 210 i=2,ncomp
	ix=2*(i-1)	!=2,4,6..
	if(graphic) then
	   call INTCONV(ix,cnum1)
	   call INTCONV(i-1,cnum2)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &   '): Fractional area of component '//CHARNB(cnum2),' ',ict,cans)
	   call GETINPr(cans,theta(ix))
	else
	   print 9,ix,i-1	!(Param',I2,'): Fractional area of component
         call INPUTr(theta(ix))
	endif
	S=S+theta(ix)
	if(S.ge.1.0) then
	   call BELL(2)
	   if(graphic) then
		call OPENDIALOG(2,12,.true.)		!draw dialog box #2
		call WDIALOG(2,'AREA > 1',12)
	   else
	 	print 92
c92		FORMAT(' *** TOTAL AREA GREATER THAN ONE. TRY AGAIN'/)
	   endif
	   goto 212
	endif
	if(graphic) then
	   call INTCONV(2*i-1,cnum1)
	   call INTCONV(i,cnum2)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Mean no of openings/burst for component'
     &    //CHARNB(cnum2),' ',ict,cans)
	   call GETINPr(cans,theta(2*i-1))
	else
	   print 201,2*i-1,i
201	   FORMAT(
     &   '&(Param',I2,'): Mean no of openings/burst for component',I2,
     &   '  = ')
         call INPUTr(theta(2*i-1))
	endif
210	CONTINUE
C NOW CALC INIT GUESSES FOR A(I) FROM REL AREA GUESSES. S CONTAINS
C SUM FROM I=1,NCOMP-1, OF AREA(I)=1.0-AREA(NCOMP)
	theta(2*NCOMP)=1.0-S	!==SURELY NOT NEEDED!
c	GOTO 4		!INIT GUESSES SET
C
4	CONTINUE
	ERRFAC=1.0E-4		!DEFAULT
	FITTED=.TRUE.
	NFIX=0
	ndisp=20
	do j=1,20
	   jfix(j)=0	!INITIALISE
	enddo
	if(graphic) then
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Fix any parameters',
     &     defolt,ict,cans)
	   call GETINPa(cans,ans)
	else
	   print 41
41	   format('&Fix any parameters [N] ? ')
	   read 101,ans
	endif
	if(UC(ans).eq.'Y') then
	   if(graphic) then
		call QDIALOG(1,' Number to be fixed [0]',
     &	' ',ict,cans)
		call GETINPi(cans,nfix)
	   else
	      print 411
411		format('& -how many: n= ')
		call INPUTi(nfix)
	   endif
	   do i=1,nfix
		if(graphic) then
		   call INTCONV(i,cnum1)
		   call QDIALOG(1,' '//CHARNB(cnum1)//
     & 	    ': Fix parameter #',' ',ict,cans)
		   call GETINPi(cans,j)
		else
		   print 43,i
43		   FORMAT('&',1x,i3,'. Parameter number= ')
		   call INPUTi(j)
		endif
		if(pon1) write(7,431) j,theta(j)
		if(discprt) write(8,431) j,theta(j)
431		format(' Param no ',i3, ' fixed at ',g13.6)
		JFIX(J)=1
	   enddo
	endif
C
42	KFIT=K-NFIX	!NUMBER OF PARAM TO BE FITTED
c
c suppress graph after guesses in
c	call ENDPLT		!needs <ENTER> so try
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	call LOCATE(0,0)
c NOW IN ALPHA MODE
c
	AMPOS=.TRUE.
	IF(NCOMP.EQ.1) GOTO 142
	print 302
302	FORMAT('&Constrain amplitudes to be positive [Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') AMPOS=.FALSE.
c	ans='Y'
c	call DEFOLTa(ans,defolt)
c	call QDIALOG(1,'Constrain amplitudes to be positive',
c     &  defolt,ict,cans)
c	call GETINPa(cans,ans)
c	if(ans.eq.'N') AMPOS=.FALSE.
	if(pon1.and.ampos) write(7,3021)
	if(discprt.and.ampos) write(8,3021)
3021	format(' Amplitudes constrained to be positive')
	PEN=0.
	IF(AMPOS.or.(.not.THIST)) GOTO 142
1431	print 143
143	FORMAT( ' Penalty constant (type -1. for help)= ')
      call INPUTr(pen)
	if(pen.gt.0.) goto 1422
	print 1421
1421	format(
     & ' If some amplitudes are negative the p.d.f. could become',/,
     & ' negative which is not allowed! The p.d.f. at t=0, f(0),',/,
     & ' can be forced to be non-negative by artificially decreasing',/,
     & ' the likelihood steeply if it goes negative. The steepness',/,
     & ' depends on the penalty factor (e.g.try 1.and 10. and 50.)')
	goto 1431
1422	if(pon1) write(7,1423) pen
	if(discprt) write(8,1423) pen
1423	format(' Penalty factor= ',g13.6)
142	continue
	print 15,ERRFAC
15	FORMAT('&ERROR= ',G8.1,'*init guess',/
     & ,' Type every Nth iteration [20]: N= ')
	call INPUTi(ndisp)
      print 32
      if(discprt) write(8,32)
32    FORMAT(' Initial guesses: ')
      do 12 i=1,k
      print 13,i,theta(i)
12    if(discprt) write(8,13) i,theta(i)
13    FORMAT(I8,G13.6)
	print 108
c	if(pon1) write(7,108)
	if(discprt) write(8,108)
C
	IF(.NOT.debug()) GOTO 31
	print 311
311	FORMAT( ' ERROR=F*initial guess (e.g.1.e-4):  F= ')
      call INPUTr(errfac)
342   print 340
340   FORMAT(' List some values of variable between YLO,YHI? ')
      read 101,ans
	if(UC(ans).EQ.'N') GOTO 31
	print 3421
3421	FORMAT(' Between values N1,N2= ')
	call INPUT2i(n1,n2)
	DO 33 I=N1,N2
c	if(pon()) write(7,13) I,YVAL(I)
	if(pon1) write(7,13) I,YVAL(I)
	if(discprt) write(8,13) I,YVAL(I)
33	print 13,I,YVAL(I)
	print 108
c	if(pon()) write(7,108)
	if(pon1) write(7,108)
	if(discprt) write(8,108)
	GOTO 342
31	CONTINUE
	RETURN		!TO CALL CSIMP
C
	END



