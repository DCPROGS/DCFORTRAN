	subroutine GETONE(data,ix,nt,ntm,if1,if2,inext,itrig,it1,
     & scrit,ntrig,shut,shutprev,opendown,delt,ylevel,base,
     & t0,opening,trise,timamax,avamp,finter,npfilt,disptran,openlev)
c
c Subroutine to run the entire 'do while' loop to find transitions
c with FINDALT -called only form SETLEV (single pre-fixed open level,
c with alternating open/shut transitions (SO must not change pre-set ylevel()).
c
	real*4 data(2048)
	integer*2 ix(2048)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	integer kfix(0:50)	!needed for NEWLEN call but not used
	logical opendown,down,trigfound,open,shut,shutprev,fixa
	logical opening(50),disptran

c
	nt=0			!counts # of transitions found
	inext=if1		!start search at beginning of fitted region
	trigfound=.true.	!so doesn't jump out at next line
	do while (nt.le.ntm.and.trigfound)
c	  NB Findalt uses value of shut as input ONLY; does not set it
	   if(nt.gt.0) itriglst=itrig
c Want to avoid trying to fit any incomplete transition close to right
c edge of screen; previously search went only up to ndfit-npfilt=if2, but
c need to exclude any such incomplete transition from fit range too, but
c also want to use points right up to last for fitting if there is no
c transition too close to end.  Therefore search right up to point #ndfit,
c but correct if1,if2 below if a transition is found too close to right
c edge of screen.
	   call FINDALT(data,inext,if2,openlev,base,scrit,ntrig,
     &	 shut,opendown,curav,itrig,down,trigfound)
c
c
c Now process the transition found on this cycle
c At this point we define the transition located by FINDTRANS (even if an
c extra transition has been inserted above when short gap/op located), and
c DOWN still has the value output by FINDTRANS.
	   if(trigfound) then       !so nt=ntm after increment
		nt=nt+1
c===		ylevel(nt)=curav	!level before trigger (intermed units)- NOW FIXED!
c For each transition, record whether it is an opening or shutting
	      opening(nt)=(down.eqv.opendown)
c==		open=abs(curav-base).gt.scrit		!open before trigger
		open=.not.shut		!open before trigger
		if(nt.eq.1) then
		   it1=itrig	!'time' of transition #nt
		   t0=float(it1-1)*finter   !=0 when it1=1 ie first point on display
		   tprev=t0
		else
		   t=float(itrig-1)*finter  !=0 when itrig=1 ie first point on display
		   delt(nt-1)=t-tprev
		   tprev=t
		endif
c -set inext, and mark trigger points
	      inext=itrig+npfilt	!leave time for filter to complete
c
		if(inext.le.if2) then
		   if(disptran) then
			call VLINE(ix(itrig),235,450,11) !mark transition light blue
			call VLINE(ix(inext),235,450,13)	!mark inext purple
		   endif
		else
		   inext=if2
		   if(disptran) then
			call VLINE(ix(itrig),235,450,11) !mark transition light blue
			call DVLINE(ix(inext),235,450,13,44)	!dashed purple
		   endif
		endif
c		if(disptran) then
c		   call VLINE(ix(itrig),235,450,11) !mark transition light blue
c		   call VLINE(ix(inext),235,450,13)	!mark inext purple
c		endif
c
c Decide whether open AFTER trigger, ready for next FINDALT.
c Easy in this case, because shut/open alternate.
		if(.not.open) then		!shut before trigger
		   open=.true.			!open AFTER trigger
		else if(open) then
		   open=.false.
		endif
		SHUT=.not.open		!shut AFTER current transition
c
c Cannot have two consec transitions in same direction here, so MISSEV removed
c
c SPECIAL CASE WHEN TWO TRANSITIONS IN OPPOSITE DIRECTIONS ARE 'CLOSE'
c   Short openings or shuttings for which 2nd trigger 'only just'
c found have estimated durations that are rather too long -fix now
c by re-estimating from max amplitude (as done above when 2nd trans
c missed altogether) -do it only of max amp is substantially less
c than avamp!  If max excursion is clearly below avamp, then
c (a) use it to revise guess of duration, and (b) FIX the amplitude
c (to zero or avamp).  This ONLY changes initial guess for duration
c of an event: does not insert new transitions
c (should these be in 'if trigfound' loop?)
c Use 3*trise as criterion to try this method?; but use max excursion (and
c fix amp), only if duration estimated here is < 2*trise
c
c==========use same tshort here (2*trise at present) -used to fix amplitude
		if(nt.gt.1.and.(opening(nt-1).neqv.opening(nt)).and.
     &		   delt(nt-1).lt.2.*trise) then
		   fixa=.true.		!so ylevel not changed by NEWLEN
		  call NEWLEN(data,nt,itriglst,inext,avamp,base,trise,
     & 	 tprev,delt,ylevel,timamax,kfix,down,opendown,shutprev,fixa)
		endif
c
	   endif	!end of 'if trigfound'
	enddo		!end of 'do while' loop
c
	RETURN
	end

