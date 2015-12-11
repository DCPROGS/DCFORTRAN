	subroutine MISSEV(data,nt,ix,dx,ibk2,vard,dispderiv,ylevel,
     & itriglst,kfix,delt,opening,base,avamp,finter,derivfac,
     & inext,t0,tprev,shutprev,dtrigm,itrigm,timamax,opendown)
c To insert an extra transition in SCAN
c If we appear to get two transitions in same direction that are
c 'close together' there may well actually be an
c 'short opening-short gap-long(ish) opening' rather than a sublevel.
c Thus, if iautosub=1 ('avoid sublevels if possible') look again using
c first derivative method for transitions
c
c Modif 09/15/94 12:20pm so never sets kfix()=2 in MISSEV, to fix short
c openings to average amp -this causes problems particularly when fitting
c an opening larger than AVAMP (but NOT doing it may cause problems too?)
c
c Modif 07/07/94 10:54am so when DERIVSCH is successful, the distance between
c stat points is no longer used as estimate of duration (unless it is too
c long for the timamax method to be used), but use timamax
c method as elsewhere (orig method=distance between inflection points becomes
c indep of duration -converges to 0.79*trise for impulse function -see
c GAUFILT.MCD) (NB for manual options, GETALL, which calls this, is called
c only for sublevel case, so don't need fixed amp option as in NEWLEN).
c NB Range for DERIVSCH now starts at n1=itriglst-4 rather than n1=itriglst-1
c NB When open 'initially', and re-opening inserted the first open level,
c   ylevel(nt-1), is used rather than avamp (see avamp1 below)
c
c Modified 02/02/94 10:21am to define dtrigm() for the new transition
c if one is inserted, ie the extreme point between itrig an itrig+npfilt.
c MISSEV is called after dtrigm() has been defined for the two transitions
c that were found (two close openings or two close shuttings) so that
c for the first of them (#nt-1) should be OK, and that for the second (#nt)
c should also be OK but now moved up to next (#nt+1), and new dtrigm(nt)
c calculated here.
c
c Modified 01/06/94 09:14am so that if channel was initially shut, brief
c shuttings are fixed to level of initial baseline, ylevel(1), rather
c than to 'base' which has still not been updated.
	real*4 data(2048)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 dtrigm(50)
	real*4 timamax(49)
	integer itrigm(50)	!for debug only
	integer*2 ix(2048)
	integer*2 istat(20)	!for search of dy/dt
	integer kfix(0:50)
	character cnum*11		!for debug
	logical opening(50),dispderiv,shutprev,opendown
c
	if(shutprev) then
	   base1=ylevel(1)
	else
	   base1=base
	endif
c
c	n1=itriglst-1		!define region to be searched
	n1=itriglst-4		!define region to be searched
	if(n1.lt.1) n1=1
	n2=inext
	if(n2.gt.2048) n2=2048
	call DERIVSCH(data,ix,dx,n1,n2,ibk2,vard,
     & dispderiv,nstat,istat,derivfac)
c
	if(nstat.ge.3) then        !use 1st 3 trans only
c        Look for min/max between 1st 2 stationary points
	   n1=int4(i2abs(istat(1)))
	   n2=int4(i2abs(istat(2)))
	   if(n1.lt.1) n1=1
	   if(n2.gt.2048) n2=2048
	   call GETMAX(data,n1,n2,dmin,dmax,imin,imax)
	   if(opendown) then
	     dm11=dmin
	     dm21=dmax
	   else
	     dm11=dmax
	     dm21=dmin
	   endif
c        Look for min/max between 2nd 2 stationary points
	   n1=int4(i2abs(istat(2)))
	   n2=int4(i2abs(istat(3)))
	   if(n1.lt.1) n1=1
	   if(n2.gt.2048) n2=2048
	   call GETMAX(data,n1,n2,dmin,dmax,imin,imax)
	   if(opendown) then
	     dm12=dmin
	     dm22=dmax
	   else
	     dm12=dmax
	     dm22=dmin
	   endif
	   if(opening(nt)) then	!2 close openings
c 2 CLOSE OPENINGS -REPLACE with open-shut-open
c (1) Trans #(nt-1) is still opening, so need only change duration estimate
c 	and fix its amplitude
c (2) trans #nt is now shutting (fix its amp=0)
c (3) add extra trans #(nt+1)=opening = previous trans #nt
c Note that last cycle was for 2nd opening, which remains the last transition
c so curlev, shut etc do not need to be changed, but indexed values just
c defined will have index 1 greater -set these first
c NB nt not yet incremented
c ylevel(nt-1) is unchanged -still shut before 1st trans
c (Do not use ISTAT! -see note at top)
c First opening
c=		delt(nt-1)=
c=     &		float(i2abs(istat(2))-i2abs(istat(1)))*finter   !1st op
		frac=abs((dm11-base)/avamp)
		if(frac.le.0.95) then	!new criterion
		   i=ifixr(frac/0.02)
		   if(i.lt.1) i=1		!A/Amax=2%
		   if(i.gt.49) i=49		!A/Amax=98%
		   delt(nt-1)=timamax(i)
		else
		   delt(nt-1)=
     &		float(i2abs(istat(2))-i2abs(istat(1)))*finter   !1st op
		endif
		ylevel(nt)=avamp+base1	!open before 2nd trans
c Remove next line for now -has bad effects if opening larger than
c avamp is being fitted!
c==		kfix(nt-1)=2  	!fix parameter later at amplitude=avamp
c
c now short shut
c=		delt(nt)=
c=     &		float(i2abs(istat(3))-i2abs(istat(2)))*finter !short shut
	 	frac=abs((dm22-(avamp+base1))/avamp)
		if(frac.le.0.95) then	!new criterion
		   i=ifixr(frac/0.02)
		   if(i.lt.1) i=1		!A/Amax=2%
		   if(i.gt.49) i=49		!A/Amax=98%
		   delt(nt)=timamax(i)
		else
		   delt(nt)=
     &		float(i2abs(istat(3))-i2abs(istat(2)))*finter !short shut
		endif
		ylevel(nt+1)=base1	!shut before 3rd trans now
		kfix(nt)=1  		!fixed at baseline -use to fix parameter later
		opening(nt)=.false.	!inserted transition is shutting
		opening(nt+1)=.true.	!last trans=opening still
		dtrigm(nt+1)=dtrigm(nt)	!move up one (see above)
		itrigm(nt+1)=itrigm(nt)	!move up one (see above) -debug
c now define dtrigm() for the new transition
		dtrigm(nt)=dmax
		itrigm(nt)=imax		!for debug
		if(dispderiv) then
      	   call INTCONV(nt,cnum)	!transition #
		   call WDIALOG(1,
     &'Trans #'//cnum(1:2)//'(reshutting) inserted by MISSEV/DERIVSCH',
     &	  12)
		endif	!====end debug
c now increment nt for the extra transition inserted
		nt=nt+1
c --now nt refers to last opening -but don't know how long this is yet!
	   else 				!2 close shuttings
c 2 CLOSE SHUTTINGS =REPLACE with shut-open-shut
c		delt(nt)=delt(nt-1)		!not defined yet anyway!
c ylevel(nt-1) is unchanged -still open before 1st trans
c Try using this value as open level, rather than using avamp below
		avamp1=ylevel(nt-1)-base1
c First shutting
c=		delt(nt-1)=
c=     &	  float(i2abs(istat(2))-i2abs(istat(1)))*finter  !1st shutting
	 	frac=abs((dm21-(avamp1+base1))/avamp1)
		if(frac.le.0.95) then	!new criterion
		   i=ifixr(frac/0.02)
		   if(i.lt.1) i=1		!A/Amax=2%
		   if(i.gt.49) i=49		!A/Amax=98%
		   delt(nt-1)=timamax(i)
		else
		   delt(nt-1)=
     &		float(i2abs(istat(2))-i2abs(istat(1)))*finter !short shut
		endif
		ylevel(nt)=base1	!shut before 2nd trans
		kfix(nt-1)=1  	!fix parameter later at amplitude=base
c then short opening
c=		delt(nt)=
c=     &	  float(i2abs(istat(3))-i2abs(istat(2)))*finter !short open
		frac=abs((dm12-base1)/avamp1)
		if(frac.le.0.95) then	!new criterion
		   i=ifixr(frac/0.02)
		   if(i.lt.1) i=1		!A/Amax=2%
		   if(i.gt.49) i=49		!A/Amax=98%
		   delt(nt)=timamax(i)
		else
		   delt(nt)=
     &		float(i2abs(istat(3))-i2abs(istat(2)))*finter   !short op
		endif
		ylevel(nt+1)=avamp1+base1	!open before 3rd trans now
c Remove next line for now -has bad effects if opening larger than
c avamp is being fitted!
c==		kfix(nt)=2  		!fixed at open lev -use to fix parameter later
		opening(nt)=.true.	!inserted transition is opening
		opening(nt+1)=.false.	!last trans=shutting still
c now define dtrigm() for the new transition, as above
		dtrigm(nt+1)=dtrigm(nt)	!move up one (see above)
		itrigm(nt+1)=itrigm(nt)	!move up one (see above) -debug
		dtrigm(nt)=dmin
		itrigm(nt)=imin		!for debug
		if(dispderiv) then
      	   call INTCONV(nt,cnum)	!transition #
		   call WDIALOG(1,
     &'Trans #'//cnum(1:2)//'(reopening) inserted by MISSEV/DERIVSCH',
     &	  12)
		endif	!====end debug
c now increment nt for the extra transition inserted
		nt=nt+1
	   endif
c Adjust tprev to end of inserted transition
	   tprev=t0
	   do i=1,nt-1
	      tprev=tprev+delt(i)
	   end do
	endif
c
	RETURN
	end

