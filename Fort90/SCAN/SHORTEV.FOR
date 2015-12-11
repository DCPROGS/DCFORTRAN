	subroutine SHORTEV(data,i1,i2,nt,ylevel,kfix,delt,timamax,opening,
     & base,avamp,disptran,inext,curav,open,shut,opensav,downprev,
     & tprev,shutprev,tmin,itrig,npfilt,finter,dtrigm,itrigm,
     & rms1,opendown)
c Version of MISSTRIG that looks for missed event after EVERY transition,
c regardless of apparent level before and after the transition.
c MISSTRIG: To insert an extra transition in SCAN when level after a trigger
c appears to be 'same' as level before trigger.
c Modified 01/08/94 09:11pm so that duration of missed event is estimated
c  first, and if estimated duration is less than tmin then NO extra
c  event is inserted.
c Modified 01/06/94 09:14am so that if channel was initially shut, brief
c shuttings are fixed to level of initial baseline, ylevel(1), rather
c than to 'base' which has still not been updated.
c (1) If at open level before and after last transition (opensav=true) then
c	(a) if last transition was shutting (opening(nt)=false on entry) then
c		assume there was a brief shutting from the current open
c		level, the return of which to current open level was missed
c		so insert brief shutting
c	(b) if last transition was opening (opening(nt)=true on entry) then
c		assume there was a brief opening on top of the current open
c		level, the return of which to current open level was missed
c		so insert brief opening
c (2) If at shut level before and after last transition (opensav=false) then
c	only option is to insert short opening
c
	real*4 data(2048)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 dtrigm(50)
	integer itrigm(50)	!for debug only
	integer kfix(0:50)
	logical opening(50),open,opensav,shut,downprev,shutprev
	logical curop,opdec,opendown,disptran
	character cnum*11		!===debug
c
	ycrit=2.5*rms1	!for 'real' difference (intermed units)
c
	if(curav.lt.ylevel(nt)) then
	   dlo=curav
	   dhi=ylevel(nt)
	else
	   dhi=curav
	   dlo=ylevel(nt)
	endif
c
	call GETMAX(data,i1,i2,dmin,dmax,imin,imax)
c
	if(shutprev) then
	   base1=ylevel(1)
	else
	   base1=base
	endif
c
c if 'up-then-down', dtrigm for the 'up', which was detected, is already
c set in GETALL, as dmax, so now record dtrigm() for down as dmin (range
c of points is same for both at present, namely the npfilt points following
c the transition that was detected)
	if(dmax.gt.dhi+ycrit) then	!must have up-then-down transitions
	   dtrigm(nt+1)=dmin		!record extreme point for down trans
	   itrigm(nt+1)=imin		!for debug
	else if(dmin.lt.dlo-ycrit) then	!prob down-then-up transitions
	   dtrigm(nt+1)=dmax		!record extreme point for up trans
	   itrigm(nt+1)=imax		!for debug
	else				!prob only one transition
	   RETURN
c==	   goto 201			!no transition to be inserted
	endif
c
c If curav is an open level, use it (rather than avamp) to estimate denominator
c of frac.
	if(nt.eq.1) then
	   curop=shutprev.and.opensav
	else
	   curop=(.not.opening(nt-1)).and.opensav
	endif
	if(curop) then
	    frac=abs((dtrigm(nt)-curav)/(curav-base1))
	else
	    frac=abs((dtrigm(nt)-curav)/avamp)
	endif
c
c timamax(i) is length corresp. to frac=i*0.02 (see INSCAN)
	i=ifixr(frac/0.02)
	if(i.lt.1) i=1
	if(i.gt.49) i=49
	if(timamax(i).lt.tmin) then
	   RETURN			!misstrig abandoned
	else
	   delt(nt)=timamax(i)
	   tprev=tprev+delt(nt)
	   itrig=itrig+1+ifix(delt(nt)/finter) 	!redefine itrig
	   inext=itrig+npfilt				!redefine inext
	endif
c
	nt=nt+1		!insert the missed transition
c
c If the transition detected in findall was downwards (downprev=true) then
c it must represent an opening (if opendown=true), so we need to insert
c a (short) re-shutting
	opdec=downprev.eqv.opendown	!opening detected by findall
c
c	if(opensav) then		!open BEFORE (and after) last trigger
c	if(opensav.and.(.not.opening(nt-1))) then   !note: nt already incremented
	if(.not.opdec) then
	   ylevel(nt)=base1	!ie shutting was missed so fix amplitude=baseline
	   kfix(nt-1)=1  		!fixed at baseline -use to fix parameter later
	   open=.true.
	   opening(nt)=.true.	!inserted transition is reopening
c        ========debug
	   if(disptran) then
      	call INTCONV(nt,cnum)	!transition #
		call WDIALOG(1,
     &	'Trans #'//cnum(1:2)//'(reopening) inserted by SHORTEV',12)
	   endif	!====end debug
	else			!shut BEFORE last trigger
	   ylevel(nt)=avamp+base1		!in intermed units
	   kfix(nt-1)=2  		!fix parameter later at amplitude=avamp
	   open=.false.
	   opening(nt)=.false.	!inserted transition is reshutting
	   if(disptran) then
      	call INTCONV(nt,cnum)	!transition #
		call WDIALOG(1,
     &	'Trans #'//cnum(1:2)//'(reshutting) inserted by SHORTEV',12)
	   endif	!====end debug
	endif
c
	SHUT=.not.open		!redefine SHUT for new final level
c
	RETURN
	end

