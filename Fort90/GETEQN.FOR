	subroutine GETEQN(IW,titw,titlep,xgt0,iflag,
     & jmiss,juse,njset,ndth,imod,nodata,iesc)
c
c To define which equation to be fitted in CVFIT2
c
c NB nmod=1, -1 exist only for ifitmode=4,5; if ifitmode.le.3 then they no longer
c    (except transiently, here) -in this case Langmuir/Hill are now nmod=26,27
c
c 03/20/96 10:01am Add nmod=28 (imod=23) for power function
c Modif 09/18/94 06:08pm for graphics mode input (not finished yet!)
c Modif 09/17/94 11:38am Add nmod=25 =straight lines with common
c   intercept on Y axis
c 04/13/92 07:08pm nmod=23,24 added = Langmuir/Hill (resp) with common
c   maximum when more than one set being fitted.  These are defined in ycalcv
c   and nmod set to 23,24 in neweqcv (not here); used only if ifitmode=4,5
c   and isepfit=3.
c 04/11/92 06:12pm ncomp now in common
c
	character*20 TITLEP(ndth)     !names of params for fit
	character*60 titmod(30)		!names of models
	character*60 titw(5)		!names of weighting methods
	character*10 T1(50)		!'standard' param names
c	character*4 tt1(10)
	integer*2 videotyp
C THESE ARE ARRAYS TO HOLD TITLES (UP TO 60 CHAR) FOR DATA SETS
C (IN TITLED) AND FOR PARAMETERS (TITLEP)(10 CHAR)
c Also include option to put axis titles in here, and store them with the
c data
	character*1 ans,UC
	LOGICAL CONSTR,FLINE,alpha
	logical discprt,slock,pon,pprt,xgt0,titprint
	character cnum*11,cnum1*11
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical help,nodata
	integer jmiss(njset),juse(njset)
	common/hlp/help				!for QDIALOG
	common/dp/discprt
	COMMON/BLOCK1/constr,nset,nfit,nsfit,
     & Xv,alfa,kmax,ncomp,nmod,fline,nomit,jomit(20),jset,ifitmode
	COMMON/BLOCK2/ castar,X1,X2,iequiv,ip1,ip2
	common/potrat/jfirst,iset,kmax1			!for pot ratios
c (kmax=actual no of param to be estimated; kmax1=number before extra param
c  added for ifitmode=4,5)
	logical logyfit,norm
	COMMON/BLOCK3/logyfit,norm,xnorm		!CVIN2,YCALCV,CVDISP,GETEQN
c
c Oct 86- two new variables added to common/block2/
c IEQUIV=0 for equiv subunits; =1 for non-equiv (prev signalled via castar)
c IP1,IP2:
c  (1) mods 10,11 IP1=1 if param are K1,K2; =2 if param are K1, K2/K1.
c  (2) mods 9,11  IP2=1 if param is K2 or (K2/K1 if IP1=2)
c		  IP2=2 if param is k(-2) (must have ip2=1 if ip1=2)
c  (3) mods 9,11  IP2 set negative if k(+2) is a param  to be estimated
c  (4) nmod=99 =polynomial. Degree of poly=ip1 (and if ip2>0 then degree
c	is ip2 for x>x1 )
c
c	DATA tt1/' b1 ',' b2 ',' b3 ',' b4 ',' b5 ',' b6 ',' b7 ',
c     & ' b8 ',' b9 ','b10 '/
c
	pon()=slock()
c
101	format(a1)
c
	alpha=VIDEOTYP().eq.3		!alpha mode
c
	pprt=pon()		!to reduce number of pon() calls (interrupt 11 problems)
	titprint=.false.
	T1(1)='       K1 '
	t1(2)='    Ymax1 '
	t1(3)='       K2 '
	t1(4)='    Ymax2 '
	t1(5)='    Slope '
	t1(6)='    Y1(0) '
	t1(7)='   Y(inf) '
	t1(8)='    k(-1) '
	t1(9)='     k(1) '
	t1(10)='     Bmax '
	t1(11)='      B/A '
	t1(12)='     Beta '
	t1(13)='       KB '
	t1(14)='     Y(0) '
	t1(15)='  N(Hill) '
	t1(16)='    K2/K1 '
	t1(17)=' 	k(-2) '
	t1(18)='     k(2) '
	t1(19)='   B2 /A2 '
	t1(20)='   Non-eq '
	t1(21)='     Coop '
	t1(22)=' Erev(mV) '
	t1(23)='   g(E=0) '
	t1(24)='    H (mV)'
	t1(25)=' B/A (E=0)'
	t1(26)='   g(max) '
	t1(27)='   conc/K '
	t1(28)='   tau(0) '
	t1(29)='g(Na) (pS)'
	t1(30)='g(Cs) (pS)'
	t1(31)='E(off)(mV)'
	t1(32)='H(K1) (mV)'
	t1(33)='H(K2) (mV)'
	t1(34)='     nH1  '
	t1(35)='    Y2(0) '
	t1(36)='     nH2  '
	t1(37)='  IC50(1) '
	t1(38)='  IC50(2) '
c
	titmod(1)='(1) Polynomial (inc. straight line)'			!nmod=99
	titmod(2)='(2) Langmuir hyperbola(s) (inc. or dec.) '		!nmod=1/26
	titmod(3)='(3) Hill equation(s) (inc. or dec./common K or max)'!nmod=-1/27
	titmod(4)='(4) Hyperbola plus straight line'			!nmod=2
	titmod(5)='(5) Two hyperbolas '					!nmod=3
	titmod(6)='(6) Two hyperbolas plus straight line'		!nmod=4
	titmod(7)='(7) Binding inhibition curve (Hill form)'		!nmod=5
	titmod(8)='(8) Exponential binding onset curves'
c	titmod(8)='(8) Exponential binding onset (common overall max)'
c	titmod(9)='(9) Exponential binding onset (separate max)'
	titmod(9)='(9) General fit of sums of exponentials/geometrics'
	titmod(10)='(10) Popen FOR KM2 MODEL+ BLOCK. K1=K2'
	titmod(11)='(11) Popen FOR KM2 + BLOCK (BETA as param). K1=K2'
	titmod(12)='(12) Popen for KM2 at equilibrium + block: SEP K1,K2'
	titmod(13)='(13) Popen for KM2 + block (BETA as param)'
	titmod(14)=
     & '(14) Total current (I/V) for KM2 at equil + block (K1=K2)'
	titmod(15)='(15) Popen for 2 independent KM1 subunits'
	titmod(16)='(16) Popen for KM2 (Coop and nonequiv) + block'
	titmod(17)=
     & '(17) Popen for KM2 (Coop and nonequiv)+block (beta as param)'
	titmod(18)=
     & '(18) I/V with exponential voltage-dependence of current'
	titmod(19)=
     & '(19) Ditto with saturation (beta/alpha V-dep)'
	titmod(20)=
     & '(20) Ditto with saturation (affinity V-dep)'
	titmod(21)=
     & '(21) I/V for GHK with Mg block'
	titmod(22)=
     & '(22) Exponential voltage dependence of rate constant'
	titmod(23)=
     & '(23) Power function y=a*(x^n) (linear on log-log plot)'
	ntmod=23		!total number of models defined
c	titmod(0)=
c     & ''
c
      if(iflag.eq.1) then             !2nd, 3rd,... time
	   call INTCONV(imod,cnum)
	   if(alpha) then
	      call DCASK(
     &	'Fit same equation (#'//charnb(cnum)//')','Y',ans)
	   else
		ans='Y'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Fit same equation (#'//charnb(cnum)//')',
     &	   defolt,11,cans)
		call GETINPa(cans,ans)
	   endif
c=         if(UC(ans).eq.'Y') goto 316
         if(UC(ans).eq.'Y') goto 317
	endif
64    continue
c
	logyfit=.false.
c
	if(alpha) then
         print 1151,imod
1151	   FORMAT(
     &' Number of equation to be fitted (-1=abort; 0=HELP) [',i3,'] = ')
	   call INPUTi(imod)
	   if(imod.eq.0) then
		do i=1,ntmod
		   print 66,titmod(i)
66		   format(1x,a60)
		enddo
	      goto 64	!ask for mod # again
	   endif
	else
	   if(imod.lt.1) imod=1
	   call DEFOLTi(imod,defolt)
25	   call QDIALOG(1,
     &     'No. of equation to be fitted (-1=abort) [F1=HELP]',
     &	defolt,11,cans)
	   if(help) then				!F1 hit in qdialog
		call CVHELP(1,titmod,imod)		!outputs imod
	   else
		call GETINPi(cans,imod)
	   endif
	endif
	if(imod.lt.0) then
	   iesc=99
	   RETURN
	endif
	if(imod.gt.ntmod) goto 64
c
c Define original model number (nmod) from imod to save extensive changes
c later
317	continue			!jump here if 'same equation'
c	if(nmod.eq.0) nmod=99
	if(imod.eq.1) then
	   nmod=99				!polynomial
	else if(imod.eq.2) then
	   nmod=1				!hyperbola
	else if(imod.eq.3) then
	   nmod=-1				!Hill eqn
	else if(imod.eq.8) then       !old imod=8,9; nmod=6,7; exponential onset
24	   continue
	   if(alpha) then
		print 23
23		format(
     &     ' (1) Fit common overall maximum',/,
     &     ' (2) Fit separate maximum for each curve',/,
     &     ' Option number = ')
		call INPUTi(iopt)
	   else
		call WDIALOG(1,'(1) Fit common overall maximum',11)
		call WDIALOG(1,'(2) Fit separate maximum for each curve',11)
		iopt=1
		call DEFOLTi(iopt,defolt)
		call QDIALOG(1,'Option number',defolt,11,cans)
		call GETINPi(cans,iopt)
	   endif
	   if(iopt.lt.1.or.iopt.gt.2) goto 24
	   if(iopt.eq.1) nmod=6
	   if(iopt.eq.2) nmod=7
	else if(imod.eq.9) then		!new general exponentials
	   nmod=19
	   if(alpha) then
	      print 38
38		format(' Fit geometrically decaying curve [N] ')
		read 101,ans
	   else
		ans='N'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Fit geometrically decaying curve',
     &		defolt,11,cans)
		call GETINPa(cans,ans)
	   endif
	   if(UC(ans).ne.'Y') then
		nmod=19		!exponentials
	   else
		nmod=20		!geometrics
	   endif
	else if(imod.ge.4.and.imod.le.20.and.imod.ne.8.and.imod.ne.9) then
	  nmod=imod-2		!nmod=2,..,18 for imod=4,...,20 (exc imod=8,9)
	else if(imod.eq.23) then
	   nmod=28	!power function
	else
	  nmod=imod			!new ones nmod=21,22 (no nmod=20!)
	endif
	if(nodata) then
	   if(pprt) write(7,131) titmod(imod)
	   if(discprt) write(8,131) titmod(imod)
131	   format(/,' Calculated curves:',/,' Equation = ',a60)
	endif
c
c Common maximum for Hill/Langmuir when more than one set fitted?
c (omit this question if ifitmode=4,5 because these already specified
c to do fit with all param except pot ratio/dose ratio in common)
c For ifitmode=3, choose which param are in common
c For ifitmode=2, no params in common -sep fit
c Use ip1,ip2 to signal options concerning common parameters when ifitmode=3
c ip1=0 -common max, separate K
c ip1=1 -common K, separate max
c ip2=0 -separate nH
c ip2=1 -common nH
c/=======nmod=26/27 here if ifitmode.le.3? -not yet!
	if((nmod.eq.1.or.nmod.eq.-1).and.ifitmode.le.3) then
	   if(ncomp.lt.1) ncomp=1
	   if(alpha) then
		print 541,ncomp
541		format(' Number of components for each set [',i2,'] = ')
		call INPUTi(ncomp)
	   else
		call DEFOLTi(ncomp,defolt)
		call QDIALOG(1,
     &	'Number of components for each set',
     &	defolt,11,cans)
c	      if(help) then				!F1 hit in qdialog
c		   call CVHELP(3,titmod,imod)
c		   goto 28
c	      else
		   call GETINPi(cans,ncomp)
c		endif
	   endif
241	   continue
	   if(ifitmode.eq.3) then	!one eqn, several sets
	    if(alpha) then
		print 31
31		format(
     &     ' (1) Fit common maximum, separate K values',/,
     &     ' (2) Fit common K value, separate maxima',/,
     &     ' Option number = ')
		call INPUTi(iopt)
		if(iopt.lt.1.or.iopt.gt.2) goto 241
		print 33
33		format(' Y increasing with X [Y] ? ')
		read 101,ans
		iequiv=0		     		!increasing
		if(UC(ans).eq.'N') iequiv=1	!decreasing
		if(nmod.eq.-1) then
		   print 32
32		   format(' Hill slope same for all sets [Y] ? ')
		   read 101,ans
		   ip2=0		     		!separate nH
		   if(UC(ans).ne.'N') ip2=1	!common nH
		endif
	    else
		call WDIALOG(1,
     &	  '(1) Fit common maximum, separate K values',11)
		call WDIALOG(1,
     &	  '(2) Fit common K value, separate maxima',11)
		iopt=1
		if(ncomp.eq.1) then
		  call WDIALOG(1,
     &	    '(3) ditto using fast fit to log(y)',11)
		  iopt=3
		endif
		call DEFOLTi(iopt,defolt)
		call QDIALOG(1,'Option number',defolt,11,cans)
		call GETINPi(cans,iopt)
		if(iopt.lt.1.or.iopt.gt.3) goto 241
		ans='Y'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Y increasing with X',
     &		defolt,11,cans)
		call GETINPa(cans,ans)
		iequiv=0		     		!increasing
		if(UC(ans).eq.'N') iequiv=1	!decreasing
		if(nmod.eq.-1) then
		   ans='Y'
		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,'Hill slope same for all sets',
     &		defolt,11,cans)
		   call GETINPa(cans,ans)
		   ip2=0			!separate nH
		   if(UC(ans).eq.'Y') ip2=1
		endif
	    endif
	   endif	!end of ifitmode=3 only bit
c Following bit applies for normalised responses too
c IFITMODE=2 -FITTING ONLY ONE SET AT A TIME so no question of common
c anything -just fit ncomp components separately to each set
	   if(ifitmode.eq.1.or.ifitmode.eq.2) then
c/=========ifitmode=2 =======fix next bit (and ycalcv!)
c=		iopt=1
c=		ip1=0		!set below -not needed in ycalcv for ifitmode=1,2
c=		ip2=1		!so params located correctly in ycalcv
		iopt=4	!no common parameters!
		if(alpha) then
		   print 33
c33		   format(' Y increasing with X [Y] ? ')
		   read 101,ans
		   iequiv=0		     		!increasing
		   if(UC(ans).eq.'N') iequiv=1	!decreasing
		else
		   ans='Y'
		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,'Y increasing with X',
     &		defolt,11,cans)
		   call GETINPa(cans,ans)
		   iequiv=0		     		!increasing
		   if(UC(ans).eq.'N') iequiv=1	!decreasing
		endif
	   endif		!end of ifitmode=1,2
	   if(iw.ge.1.and.iw.le.5) then
		if(pprt) write(7,1301) titw(iw),titmod(imod)
	      if(discprt) write(8,1301)titw(iw),titmod(imod)
c1301		format(/,' FITTING:',/,' Weighting = ',a60,/,' Equation = ',a60)
		titprint=.true.
	   endif
	   if(iopt.eq.1) then		!common max, sep K
		if(pprt) write(7,35)
	      if(discprt) write(8,35)
35		format(' Common maximum, separate K values for each set',/)
		ip1=0
		titlep(1)='     Y(0) ' 			    !same for all
		if(iequiv.eq.1) titlep(1)='   Y(inf) '  !same for all
		k=1
		do i=1,ncomp	!ymax(i) for component #i (same for all sets)
		   call INTCONV(i,cnum)
		   n=nblank(cnum)
		   k=k+1
		   titlep(k)='    Ymax'//charnb(cnum)//' '	!='Ymax1','Ymax2',...
		enddo
		do j=1,nsfit
		   j1=juse(j)		!set number used
		   call INTCONV(j1,cnum)
		   n=nblank(cnum)
c		   chj=char(j1+48)
		   do i=1,ncomp
		      call INTCONV(i,cnum1)
		      n1=nblank(cnum1)
c			chi=char(i+48)
			k=k+1
			titlep(k)='K'//cnum1(1:n1)//' (set '//cnum(1:n)//')'
		   enddo
		enddo
	   else if(iopt.eq.2) then	!common K, sep max
		if(pprt) write(7,36)
	      if(discprt) write(8,36)
36		format(' Common K value, separate maxima for each set',/)
		ip1=1
		titlep(1)='     Y(0) '  !same for all
		if(iequiv.eq.1) titlep(1)='   Y(inf) '  !same for all
		k=1
		do j=1,nsfit
		   j1=juse(j)		!set number used
		   call INTCONV(j1,cnum)
		   n=nblank(cnum)
		   do i=1,ncomp
		      call INTCONV(i,cnum1)
		      n1=nblank(cnum1)
c		      chi=char(i+48)
			k=k+1
		     titlep(k)='Ymax'//cnum1(1:n1)//' (set '//cnum(1:n)//')'
		   enddo
		enddo
		do i=1,ncomp	!K(i) for component #i (same for all sets)
		   call INTCONV(i,cnum1)
		   n1=nblank(cnum1)
c		   chi=char(i+48)
		   k=k+1
		   titlep(k)='       K'//cnum1(1:n1)//' '	!='K1','K2',...
		enddo
	   else if(iopt.eq.3) then	!common K, sep max by fitting log(y)
		if(pprt) write(7,37)
	      if(discprt) write(8,37)
37		format(' Common K value, separate maxima for each set',/,
     &	' Fit to log(y) values with equal weight for log values',/)
		ip1=1
		titlep(1)='     Y(0) '  !same for all
		if(iequiv.eq.1) titlep(1)='   Y(inf) '  !same for all
		if(alpha) then
		   print 34,titlep(1)(4:9)
34		   format(' This method assumes ',a6,
     &		' =0: [Y] ')
		   read 101,ans
		else
		   ans='Y'
		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,'This method assumes '//
     &		titlep(1)(4:9)//' = 0: O.K.',
     &		defolt,11,cans)
		   call GETINPa(cans,ans)
		endif
		if(UC(ans).eq.'N') goto 241
		logyfit=.true.
		titlep(1)='     K1   '
		k=1
c NB in logy case all the Ymax are linear and are calc in YCALCV
	   else if(iopt.eq.4) then	!for ifitmode=1, 2: no common params
		if(pprt) write(7,39)
	      if(discprt) write(8,39)
39		format(' Separate fit to each set: no common parameters',/)
		if(nmod.eq.1) then
		   nmod=26	!Langmuir
		else
		   nmod=27	!Hill
		endif
		titlep(1)='     Y(0) '  !same for all
		if(iequiv.eq.1) titlep(1)='   Y(inf) '  !same for all
c	fitting only set #jset at each pass for ifitmode=2
		call INTCONV(jset,cnum)
		n=nblank(cnum)
		k=1
		do i=1,ncomp
		   call INTCONV(i,cnum1)
		   n1=nblank(cnum1)
		   k=k+1
		   titlep(k)='Ymax'//cnum1(1:n1)//' (set '//cnum(1:n)//')'
		   k=k+1
		   titlep(k)='   K'//cnum1(1:n1)//' (set '//cnum(1:n)//')'
		   if(nmod.eq.27) then
			k=k+1
			titlep(k)='nH'//cnum1(1:n1)//' (set '//cnum(1:n)//')'
		   endif
		enddo
		kmax=k
		goto 316
	   endif
c Add nH parameters if Hill (skipped if ifitmode=1,2 -set above)
	   if(nmod.eq.1) then
		nmod=26	!Langmuir
		kmax=k
	   else
		nmod=27	!Hill
		if(ip2.eq.1) then		!common nH (1 for each component)     		!separate nH
		   do i=1,ncomp	!ymax(i) for component #i (same for all sets)
		      call INTCONV(i,cnum)
		      n=nblank(cnum)
c			chi=char(i+48)
			k=k+1
			titlep(k)='     nH'//cnum(1:n)//' '	!='nH1','nH2',...
		   enddo
		else				!sep nH for each set
		   do j=1,nsfit
			j1=juse(j)		!set number used
		      call INTCONV(j1,cnum)
		      n=nblank(cnum)
c			chj=char(j1+48)
			do i=1,ncomp
		         call INTCONV(i,cnum1)
		         n1=nblank(cnum1)
c			   chi=char(i+48)
			   k=k+1
			 titlep(k)='nH'//cnum1(1:n1)//' (set '//cnum(1:n)//')'
			enddo
		   enddo
		endif
		kmax=k
	   endif
	   goto 316
	endif
c
c General exponentials
	if(nmod.eq.19) then
	   xgt0=.true.
	   if(alpha) then
		print 531
531		format('&Allow fitted curves to go below x=0 [N] ? ')
		read 101,ans
		if(UC(ans).eq.'Y') xgt0=.false.
		print 53
53		format(
     &	  '&Number of exponential components to be fitted [1] = ')
		call INPUTi(ncomp)
	   else
		ans='N'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Allow fitted curves to go below x=0',
     &		defolt,11,cans)
		call GETINPa(cans,ans)
		if(ans.eq.'Y') xgt0=.false.
		call DEFOLTi(ncomp,defolt)
		call QDIALOG(1,
     &	'Number of exponential components to be fitted',
     &	defolt,11,cans)
		call GETINPi(cans,ncomp)
	   endif
	   if(ncomp.eq.0) ncomp=1
	   kmax=2*ncomp+1		!no of parameters
	   do 5 i=1,ncomp
		ix=2*i-1		!=1,3,5,7..
	      titlep(ix)='  tau('//char(i+48)//') '
	      titlep(ix+1)='  amp('//char(i+48)//') '
5	   continue
	   titlep(kmax)='  Y(inf) '
	   goto 316
	endif
	if(nmod.eq.20) then
	   xgt0=.true.
	   if(alpha) then
		print 55
55		format(
     &	  '&Number of geometric components to be fitted [1] = ')
		call INPUTi(ncomp)
	   else
		call DEFOLTi(ncomp,defolt)
		call QDIALOG(1,
     &	'Number of geometric components to be fitted',
     &	defolt,11,cans)
		call GETINPi(cans,ncomp)
	   endif
	   if(ncomp.eq.0) ncomp=1
	   kmax=2*ncomp+1		!no of parameters
	   do i=1,ncomp
		ix=2*i-1		!=1,3,5,7..
	      titlep(ix)='  ''tau''('//char(i+48)//') '
	      titlep(ix+1)='  amp('//char(i+48)//') '
	   enddo
	   titlep(kmax)='  Y(inf) '
	   goto 316
	endif
c Power function
	if(nmod.eq.28) then
	   kmax=2		!no of parameters
	   titlep(1)=' a (=y at x=1)     '
	   titlep(2)=' n (=log-log slope)'
	   goto 316
	endif
c
	IF(NMOD.EQ.9.OR.NMOD.EQ.11) print 3141
3141	FORMAT(' CALC OF OP TIME/BST CORRECTED 25-JAN-85')
c
	ip1=1
	ip2=1
	if(iw.ge.1.and.iw.le.5) then
	   if(pprt) write(7,1301) titw(iw),titmod(imod)
         if(discprt) write(8,1301)titw(iw),titmod(imod)
c1301	  format(/,' FITTING:',/,' Weighting = ',a60,/,' Equation = ',a60)
	   titprint=.true.
	endif
	if(nmod.eq.23.or.nmod.eq.24) then
	   if(pprt) write(7,1302)
         if(discprt) write(8,1302)
1302	   format(' (common maximum for all sets fitted)')
	endif
c	goto(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
c     & 19,20,21,22),iabs(nmod)
c NMOD now defined
	if(nmod.eq.21) goto 21
	if(nmod.eq.22) goto 22
	if(nmod.eq.16.or.nmod.eq.17.or.nmod.eq.18) goto 329
	if(nmod.ne.99) goto 981		!not polynomial
	ip2=0		!use to signal disjoint polynomial (ip2.ne.0)
	if(alpha) then
	   print 48
48	   format('&Use a disjoint polynomial [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
   		print 50
50   		format(' Make junction at x1 = ')
		call INPUTr(x1)
	      print 471
471	      format(
     &   '&Degree of polynomial below x1 (1=line, 2=quadratic,..) = ')
	      call INPUTi(ip1)
   		print 51,x1
51		format(' Degree of polynomial above x1 = ',g12.4,' = ')
		call INPUTi(ip2)
	   else	!not disjoint
	      print 47
47	      format(
     &	'&Degree of polynomial (1=line, 2=quadratic,...,10) = ')
	      call INPUTi(ip1)
	   endif
	else
	   ans='N'
	   call DEFOLTa(ans,defolt)
27	   call QDIALOG(1,'Use a disjoint polynomial (F1=help)',
     &	defolt,11,cans)
	   if(help) then				!F1 hit in qdialog
		call CVHELP(2,titmod,imod)
		goto 27
	   else
	      call GETINPa(cans,ans)
	   endif
	   if(UC(ans).eq.'N') then
		ip1=1
		call DEFOLTi(ip1,defolt)
26		call QDIALOG(1,
     &	'Degree of polynomial (F1=help)',
     &	 defolt,11,cans)
		if(help) then				!F1 hit in qdialog
		   call CVHELP(2,titmod,imod)
		   goto 26
		else
	         call GETINPi(cans,ip1)
		endif
	   else		!disjoint
		x1=0.0
		call DEFOLTr(x1,defolt)
		call QDIALOG(1,
     &	  'Make junction at x1',
     &	   defolt,11,cans)
		call GETINPr(cans,x1)
		ip1=1
		call DEFOLTi(ip1,defolt)
261		call QDIALOG(1,
     &	'Degree of polynomial below x1 (F1=help)',
     &	 defolt,11,cans)
		if(help) then				!F1 hit in qdialog
		   call CVHELP(2,titmod,imod)
		   goto 261
		else
	         call GETINPi(cans,ip1)
		endif
		ip2=2
		call DEFOLTi(ip2,defolt)
		call QDIALOG(1,
     &     	  'Degree of polynomial above x1',
     &	   defolt,11,cans)
		call GETINPi(cans,ip2)
	   endif
	endif
	if(pprt) write(7,52)ip1
      if(discprt) write(8,52)ip1
52	format(' Polynomial of degree= ',i3,' fitted')
c	kmax=2				!polynomial
	kmax=ip1+1			!polynomial
c Can fit a different degree of polynomial ( eg a line) above x=x1
c as follows. Coeffs are the same for both polynomials up to coeff
c of X**ip1 (or X**ip2 if ip2<ip1)
	if(ip2.gt.0) then
	   if(ip2.gt.ip1) kmax=ip2+1	!to get right no of params
	   if(pprt) write(7,521)x1,ip2
         if(discprt) write(8,521)x1,ip2
521	   format(
     & ' Above x= ',g12.4,' polynomial of degree= ',i3,' fitted')
	endif
	titlep(1)=t1(14)		!Y(0) (=a0)
	do k=2,kmax
	   call INTCONV(k-1,cnum)
c==	   TITLEP(k)='    '//tt1(k)	!b1,b2,...
	   TITLEP(k)='    a'//charnb(cnum)	!a1,a2,...
	enddo
c  Lines with common X intercept (eg reversal potential).  For n lines
c must estimate n slopes + 1 intercept=n+1 parameters
c (only if ifitmode=3)
	if(ip1.eq.1.and.ip2.eq.0.and.nsfit.gt.1.and.ifitmode.eq.3) then
	   if(alpha) then
	      print 40
40	      format(
     &	' Constrain lines to common intercept on x axis [N] ? ')
	      read 101,ans
	   else
	      ans='N'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,
     &	'Constrain lines to common intercept on x axis',
     &	 defolt,11,cans)
	      call GETINPa(cans,ans)
	   endif
	   if(UC(ans).eq.'Y') then
		nmod=25		!new model #
		kmax=nsfit+1
		do i=1,nsfit
		   call INTCONV(i,cnum)
		   n=nblank(cnum)
		   titlep(i)='slope('//cnum(1:n)//')'	!slope(i)
		enddo
		titlep(kmax)='X (at Y=0)'
	   endif
	endif
	GOTO 316	!MODEL DEFINED-START FITTING
c
981	if(nmod.eq.13) goto 315
	if(nmod.eq.14.or.nmod.eq.15) goto 424
	if(nmod.ne.10.and.nmod.ne.11) goto 350
351	continue
c  (1) mods 10,11 IP1=1 if param are K1,K2; =2 if param are K1, K2/K1.
c  (2) mods 9,11  IP2=1 if param is K2 or (K2/K1 if IP1=2)
c		  IP2=2 if param is k(-2) (must have ip2=1 if ip1=2)
c  (3) mods 9,11  IP2 set negative if k(+2) is a param  to be estimated
	if(alpha) then
	   print 3184
3184	   format(' Use K1,K2 (Y), [or K1, K2/K1 (N)] as parameters ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') ip1=1
	   if(UC(ans).ne.'Y') ip1=2
	   print 3183
3183	   format(' Fit for non-equiv subunits (no coop) ? ')
C YES MEANS K1,K2 ARE FOR SUBUNITS print 1,2- ASSUMED SAME
C REGARDLESS OF ORDER OF BINDING (NO COOP). ALSO FORWARD RATE
C FOR NMOD=11 TAKEN AS SAME FOR BOTH SUBUNITS=X2
	   read 101,ans
	else
	   call DEFOLTi(ip1,defolt)
	   call QDIALOG(1,
     &     '(1) Use K1, K2; (2) use K1, K2/K1 as parameters: n',
     &	defolt,11,cans)
	   call GETINPi(cans,ip1)
	   if(ip1.ne.1.and.ip1.ne.2) goto 351
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Fit for non-equiv subunits (no cooperativity)',
     &     defolt,11,cans)
	   call GETINPa(cans,ans)
	endif
	if(UC(ans).ne.'Y'.and.UC(ans).ne.'N') goto 351
	if(UC(ans).eq.'Y') goto 352
c
c	CASTAR=-1.
	iequiv=0
      if(pon()) write(7,353)
      if(discprt) write(8,353)
353	format(' Cooperative binding  (equivalent sites)',/)
	goto 350
c
352	iequiv=1
c352	CASTAR=1.		!USE CASTAR AS FLAG
      if(pon()) write(7,354)
      if(discprt) write(8,354)
354	format(' Nonequivalent sites (non-cooperative binding)',/)
350	continue
c
	IF(NMOD.GT.4) GOTO 300
c32	continue
	kmax=nmod+1		!for nmod=1-4
	ncomp=kmax/2	!for nmod=1-4
	if(nmod.eq.-1) then
	   ncomp=1
	   kmax=4		!Hill equation
	endif
	FLINE=.false.
	if(nmod.eq.2.or.nmod.eq.4) fline=.true.	!for nmod=2,4 ie imod=4,6
C
	titlep(1)=t1(1)
	titlep(2)=t1(2)
	titlep(3)=t1(5)
	IF(NMOD.NE.-1) GOTO 119
	titlep(3)=t1(14)
	titlep(4)=t1(15)
119	IF(NCOMP.NE.2) GOTO 316
	titlep(3)=t1(3)
	titlep(4)=t1(4)
	titlep(5)=t1(5)
	GOTO 316
C
300	CONTINUE
	if(nmod.eq.5) then
	   ncomp=1		!default
	   if(alpha) then
		print 54,ncomp
54		format(' Number of components (1 or 2) [',i2,'] = ')
		call INPUTi(ncomp)
            print 57
	      print 318
	      call INPUTr(castar)
	   else
		call DEFOLTi(ncomp,defolt)
28		call QDIALOG(1,
     &	'Number of components to be fitted (F1=help)',
     &	defolt,11,cans)
	      if(help) then				!F1 hit in qdialog
		   call CVHELP(3,titmod,imod)
		   goto 28
	      else
		   call GETINPi(cans,ncomp)
		endif
		castar=-1.
		call DEFOLTr(castar,defolt)
29		call QDIALOG(1,
     &	'Normalized conc of labelled ligand (F1=help)',
     &	defolt,11,cans)
	      if(help) then				!F1 hit in qdialog
		   call CVHELP(3,titmod,imod)
		   goto 29
	      else
		   call GETINPr(cans,castar)
		endif
	   endif
	   if(ncomp.lt.1.or.ncomp.gt.2) ncomp=1
	   kmax=4
	   if(ncomp.eq.2) kmax=7
	   titlep(1)=t1(37)	!IC50(1)
	   titlep(2)=t1(6)	!Y1(0)
	   titlep(3)=t1(7)	!Y(inf)
	   titlep(4)=t1(34)	!nH1
	   titlep(5)=t1(38)	!IC50(2)
	   titlep(6)=t1(35)	!Y2(0)
	   titlep(7)=t1(36)	!nH2
         if(pon()) write(7,57)
         if(discprt) write(8,57)
57	   format(
     &' NB Y1(0) and Y2(0) are amplitudes of components after ',
     &'subtracting Y(inf)')
318	   format(
     &' NB It is possible to estimate equilibrium constants only',/,
     &   ' when Hill coefficient(s) are fixed at 1.0; in this case',/,
     &   ' the normalised concentration of labelled ligand must be',/,
     &   ' supplied (hit ENTER if not relevent):',/,
     &   ' Normalised conc of labelled ligand = ')
	   GOTO 316	!MODEL DEFINED-START FITTING
	endif
C
506	IF(NMOD.NE.6.AND.NMOD.NE.7) GOTO 508
	KMAX=3
	IF(NMOD.EQ.7) KMAX=2+NSET	!K1,K2 + NSET VALUES OF BMAX
	titlep(1)=t1(8)
	titlep(2)=t1(9)
	titlep(3)=t1(7)
	GOTO 316
C
508	CONTINUE	! MODELS 8-12 HERE
	KMAX=3
	IF(NMOD.EQ.10.OR.NMOD.EQ.11) GOTO 510
	titlep(1)=t1(11)
	IF(NMOD.EQ.9) titlep(1)=t1(12)
	titlep(2)=t1(1)
	titlep(3)=t1(13)
	if(nmod.eq.12) titlep(4)=t1(2)
	if(nmod.eq.12) KMAX=4
	GOTO 3161
510	KMAX=4
	IF(NMOD.EQ.10) titlep(1)=t1(11)
	IF(NMOD.EQ.11) titlep(1)=t1(12)
	titlep(2)=t1(1)
	if(ip1.eq.1) titlep(3)=t1(3)
	if(ip1.eq.2) titlep(3)=t1(16)
	titlep(4)=t1(13)
c
c3161	IF(NMOD.EQ.8.OR.NMOD.EQ.10.or.nmod.eq.12) GOTO 316
3161	if(nmod.ne.9.and.nmod.ne.11) goto 316
c Section for NMOD=9 or 11
c  (1) mods 10,11 IP1=1 if param are K1,K2; =2 if param are K1, K2/K1.
c  (2) mods 9,11  IP2=1 if param is K2 or (K2/K1 if IP1=2)
c		  IP2=2 if param is k(-2) (must have ip2=1 if ip1=2)
c  (3) mods 9,11  IP2 set negative if k(+2) is a param  to be estimated
c==============graphics not done from here on
	print 3181
3181	FORMAT(' Mean burst length (ms)= ')
	call INPUTr(x1)
	if(pprt) write(7,3188)x1
      if(discprt) write(8,3188)x1
3188	format(' Mean burst length (ms)= ',g13.6)
	X1=X1/1000.	!SEC
	ip2=1
	if(ip1.eq.2) goto 3191
	print 3190
3190	format(' Use K2 (Y), [or k(-2), (N)] as parameter? ')
	read 101,ans
	if(UC(ans).eq.'Y') ip2=1
	if(UC(ans).ne.'Y') ip2=2
	if(ip2.eq.1) goto 3191
	i1=2
	if(nmod.eq.11) i1=3
	titlep(i1)=t1(17)
3191	print 3185
3185	format(' Treat k(+2) as a parameter to be estimated? ')
	read 101,ans
	if(UC(ans).eq.'Y') ip2=-ip2
	if(ip2.gt.0) goto 3186
	kmax=kmax+1
	titlep(kmax)=t1(18)
	goto 316
3186	print 3182
3182	FORMAT(' k(+2)= ')
	call INPUTr(x2)
	if(pprt) write(7,3189)x2
      if(discprt) write(8,3189)x2
3189	format(' k(+2) fixed at ',g13.6)
	GOTO 316
C
c model 13
315	print 323
323	format(' Same affinity for both subunits? ')
	read 101,ans
	ip1=0
	if(UC(ans).eq.'Y') ip1=1
	print 324
324	format(' Same beta/alpha for both subunits? ')
	read 101,ans
	ip2=0
	if(UC(ans).eq.'Y') ip2=1
	titlep(1)=t1(11)
	titlep(2)=t1(1)
	kmax=2
	if(ip1.eq.1) goto 325
	kmax=kmax+1
	titlep(kmax)=t1(3)
325	if(ip2.eq.1) goto 326
	kmax=kmax+1
	titlep(kmax)=t1(19)
326	kmax=kmax+1
	titlep(kmax)=t1(13)
	goto 316
c
c Model 14 & 15
c Kij= ith subunit type, jth (sequential) binding
424	kmax=5
	titlep(1)=t1(1)
	titlep(2)=t1(20)
	titlep(3)=t1(21)
	if(nmod.eq.14) titlep(4)=t1(11)
	if(nmod.eq.15) titlep(4)=t1(12)
	titlep(5)=t1(13)
	if(nmod.eq.14) GOTO 316
c
	print 3181	!' Mean burst length (ms)= ')
	call INPUTr(x1)
	if(pprt) write(7,3188)x1		!Mean burst lengt
      if(discprt) write(8,3188)x1		!Mean burst length (ms)= ,rec= h (ms)= '
	X1=X1/1000.	!SEC
	print 428	!Treat k(+2) as a parameter to be estimated?
428	format(
     & ' Treat k(+2) (assumed to be same for both subunit types)',/,
     & ' as a parameter to be estimated? ')
	read 101,ans
	x2=-1.		!k+2 not to be estimated
	if(UC(ans).eq.'N') goto 426
	kmax=kmax+1
	titlep(kmax)=t1(18)
	goto 316
426	print 3182	!' k(+2)= '
	call INPUTr(x2)
	if(pprt) write(7,429)x2
      if(discprt) write(8,429)x2
429	format(' k(+2) fixed at ',g13.6,/,
     & '   (assumed to be same for both subunit types)')
	goto 316
c
c Model 16
329	continue
	kmax=3
	titlep(1)=t1(22)
	titlep(2)=t1(23)
	titlep(3)=t1(24)
	if(nmod.eq.16) goto 316
	kmax=5		!nmod=17
	titlep(2)=t1(26)
	titlep(4)=t1(27)
	titlep(5)=t1(25)
	if(nmod.eq.18) titlep(5)=t1(11)
	GOTO 316	!MODEL DEFINED-START FITTING
c
c Model 21 ' (21) I/V for GHK with Mg block'
21	continue
	kmax=7
c29	gNa=theta(1)		!conductance in pS
c30	gCs=theta(2)		!conductance in pS
c31	Eoff=theta(3)	!offset in Erev, relative to GHK value
c1	aK1=theta(4)		!in mM
c3	aK2=theta(5)		!in mM
c32	H1=theta(6)
c33	H2=theta(7)
	titlep(1)=t1(29)
	titlep(2)=t1(30)
	titlep(3)=t1(31)
	titlep(4)=t1(1)
	titlep(5)=t1(3)
	titlep(6)=t1(32)
	titlep(7)=t1(33)
	goto 316
c Model 22 ' (22) Exponential voltage dependence of rate constant'
22	continue
	kmax=2
	titlep(1)=t1(28)  !tau
	titlep(2)=t1(24)  !H (mV)
	goto 316
c
c
C Model now defined- start fitting
316	continue
	if(iw.ge.1.and.iw.le.5.and.(.not.titprint)) then
	   if(pprt) write(7,1301) titw(iw),titmod(imod)
         if(discprt) write(8,1301)titw(iw),titmod(imod)
1301	  format(/,' FITTING:',/,' Weighting = ',a60,/,' Equation = ',a60)
	endif
c Before leaving add extra params for ifitmode=4 (nset-1 potency ratios)
c and for ifitmode=5 (KB)
c For ifitmode=4 jset=set # for the 1st set to be fitted so this is reference
c set for other potency ratios; title of pot ratios = 'rn' where n=data set
c number (n>jset) to which the ratio applies
	kmax1=kmax		!keep number of params before adding extra ones!
	if(ifitmode.eq.4) then
	   i=0
	   do j=jset+1,nset
c	   do 317 j=jset+1,nset
c	    if(jmiss(j).eq.1) goto 317	!set skipped
	    if(jmiss(j).eq.0) then	!set not skipped
	      i=i+1
	      titlep(kmax+i)='       r'//char(j+48)//' '	!='r3','r5',...
	      if(j.eq.10) titlep(kmax+i)='      r10 '
	    endif
	   enddo
c317	   continue
c	   kmax=kmax+i                      !new kmax for simult fit
	   kmax=kmax+nsfit-1                !new kmax for simult fit
	else if(ifitmode.eq.5) then
	   kmax=kmax+1
	   titlep(kmax)=t1(13)		!='       KB '
	endif
c	call flush(7)
	RETURN
	END


