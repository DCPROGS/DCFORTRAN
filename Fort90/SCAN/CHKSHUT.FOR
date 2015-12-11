	subroutine CHKSHUT(y1,base,scrit,k,i1,delt,t0,dxf,dtf,
     & tshift,ibk2,calfac2,rms1,avamp,SHUT,sets,autoset,
     & opendown,reject,n,ixn,imode)
c To check if the level Y1 is close enough to BASE (both in intermed units)
c that the channel should be deemed SHUT
c NB i1=1,..,ntrans-1 (CHKSHUT not called for i1=ntrans)
c
c imode=0 to ask about each dubious 'shut' level separately (as original)
c imode=1 to just draw arrows at each dubious level so can ask whether
c	 to set ALL shut (returns position of arrows in ixn() for erasure;
c      n=number drawn -initialise before the call). In this case setall
c	 is ignored. In this case 'shut' and 'sets' are not returned
c imode=2 is used is setall=true in fitsub, and in this case chkshut
c 	is used as before to select the dubious amplitudes but they are
c	all set shut without asking
c imode=3 used if setall false but only one doubtful interval, so
c	no need to ask which
c Modif 02/21/95 08:07am by adding opendown to arguments, and querying
c any level that (for opendowm=true) is ABOVE the baseline by more
c than rms1
	real*4 delt(0:50)		!temp storage for transition search results
	integer ixn(25)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*70 string		!to put result in dialog box
	character*11 cnum1,cnum2,cnum3
	character UC,ans
	logical shut,sets,autoset,opendown,reject
c
	reject=.false.
	sets=.false.		!true if set as shut 'manually' below
c	if(abs(y1-base).lt.0.4*scrit) then
c	   SHUT=.true.		!final level is SHUT
c	else if(abs(y1-base).gt.2.0*scrit) then
c	   SHUT=.false.		!final level is OPEN
	upper=2.0*scrit		!above this level, assumed to be OPEN
	a=0.3*abs(avamp)
	if(upper.gt.a) upper=a
	if(abs(y1-base).lt.rms1) then  	!rms1 in intermed units
	   SHUT=.true.		!final level is SHUT
	   autoset=.true.		!so fitted trace redrawn to show new value
c	else if(abs(y1-base).gt.upper) then
c	   SHUT=.false.		!final level is OPEN
	else if(opendown.and.(base-y1.gt.upper)) then
	   SHUT=.false.		!final level is OPEN
	else if(.not.opendown.and.(y1-base.gt.upper)) then
	   SHUT=.false.		!final level is OPEN
	else		!borderline case!
c Mark this 'shutting' on graph with arrow.  Time to middle of shutting is
c sum of the delt(i) up to the transition before the shutting, plus half
c the length of the shutting
	   t=0.
	   do i=1,i1-1
		t=t+delt(i)
	   enddo
	   t=t + 0.5*delt(i1) + t0	!microsec
	   t=t + tshift 		!t0 is too early to get arrow in middle of event
	   n=n+1
	   ixn(n)=ifixr(t*dxf/dtf)
	   if(imode.eq.0) then	!ask for each
	      call VARROW(ixn(n),445,380,12)
            call INTCONV(k,cnum1)	!transition #
	      t=delt(i1)*1.e-3		!duration (ms)
		if(t.lt.1000.) then
	         call DCFORMAT(t,7,3,cnum2)		!duration
		else if((t.ge.1000.).and.(t.lt.10000.)) then
	         call DCFORMAT(t,7,2,cnum2)		!duration
		else
	         call DCFORMAT(t,7,1,cnum2)		!duration
		endif
		amp=calfac2*(y1-base)
		call DCFORMAT(amp,9,5,cnum3)		!amplitude
		n1=NBLANK(cnum1)
		n2=NBLANK(cnum2)
		n3=NBLANK(cnum3)
		string=cnum1(1:n1)//' '//cnum2(1:n2)//'ms'//
     &	 ' '//cnum3(1:n3)//'pA'
		ans='Y'
		call TONE(352,20)
c		call BELL(1)
	 	call DEFOLTa(ans,defolt)
		call QDIALOG1(1,CHARNB(string)//' is SHUT (2=reject): O.K.'
     &	,defolt,1,12,cans)	!read nchar=1 character only then leave
		call GETINPa(cans,ans)
		call VARROW(ixn(n),445,380,ibk2)	!erase arrow
		if(UC(ans).eq.'2') then
		   reject=.true.
		else if(ans.eq.'Y') then
		   SHUT=.true.
		   sets=.true.
		else
		   SHUT=.false.
		   sets=.false.
		endif
	   else if(imode.eq.1) then	!just draw
	      call VARROW(ixn(n),445,380,12)
	   else if(imode.eq.2) then	!setall=true in FITSUB
		SHUT=.true.
		sets=.true.
	   else if(imode.eq.3) then	!setall=false, but n=1 in FITSUB
		SHUT=.false.
		sets=.false.
	   endif
	endif
	RETURN
	end

