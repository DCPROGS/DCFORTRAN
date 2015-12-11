	subroutine MISSTRIG(data,nt,ylevel,kfix,delt,timamax,opening,
     & base,avamp,inextprv,inext,curav,open,shut,opensav,downprev,
     & tprev,shutprev,tmin,itrig,npfilt,finter,i1,i2)
c MISSTRIG NOT USED IN CURRENT VERSION OF SCAN
c To insert an extra transition in SCAN when level after a trigger appears
c to be 'same' as level before trigger.
c Modified 01/31/94 10:50am so that data(i1) to data(i2) searched, and
c these no specified in FITSUB(6). NB must ensure that we never search
c beyond data(ndfit) as data() is not defined beyond this point.
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
	integer kfix(0:50)
	logical opening(50),open,opensav,shut,downprev,shutprev
	logical newshut
c
c   First get estimate for duration of missed event
	if(.not.downprev) then	    !last transition was upwards on screen
c					    !i.e. subsequent down transition missed
	   dext=-1.e36         !extreme value=maximum
c	   do i=inextprv,inext
	   do i=i1,i2
	      if(data(i).gt.dext) then
		   dext=data(i)		!highest point on screen
		endif
	   enddo
	else
	   dext=1.e36          !extreme value=minimum
	   do i=i1,i2    !i.e. subsequent down transition missed
		if(data(i).lt.dext) then
		   dext=data(i)		!lowest point on screen
		endif
	   enddo
	endif
c
	if(shutprev) then
	   base1=ylevel(1)
	else
	   base1=base
	endif
c Problem with opening(nt-1) if nt=1 e.g if initially open (shutprev=.true.)
c and then get short shutting as 1st transition
c	if(opensav.and.(.not.opening(nt-1))) then   !open before and after last trig
	if(nt.eq.1) then
	   newshut=shutprev.and.opensav
	else
	   newshut=(.not.opening(nt-1)).and.opensav
	endif
	if(newshut) then
	    frac=abs((dext-curav)/(curav-base1))
	else
	    frac=abs((dext-curav)/avamp)
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
c	if(opensav) then		!open BEFORE (and after) last trigger
	if(opensav.and.(.not.opening(nt-1))) then   !note: nt already incremented
	   ylevel(nt)=base1	!ie shutting was missed so fix amplitude=baseline
	   kfix(nt-1)=1  		!fixed at baseline -use to fix parameter later
	   open=.false.
	   opening(nt)=.true.	!inserted transition is reopening
	else			!shut BEFORE last trigger
	   ylevel(nt)=avamp+base1		!in intermed units
	   kfix(nt-1)=2  		!fix parameter later at amplitude=avamp
	   open=.true.
	   opening(nt)=.false.	!inserted transition is reshutting
	endif
c
	SHUT=.not.open		!redefine SHUT
c
	RETURN
	end

