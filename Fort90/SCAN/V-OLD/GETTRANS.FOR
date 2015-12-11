	subroutine GETTRANS(data,ix,dx,nt,ntm,if1,if2,inext,itrig,it1,
     & scrit,curlev,ntrig,navtest,shut,shutprev,open,opendown,
     & kfix,delt,ylevel,base,t0,opening,trise,iautosub,timamax,
     & avamp,ibasevga,tmin,finter,ibk2,vard,dispderiv,iampcrit,
     & npfilt,disptran,ampdone)
c
c Subroutine to run the entire 'do while' loop to find transitions
c with FINDALL
c
	real*4 data(2048)
	integer*2 ix(2048)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	integer kfix(0:50)
	logical opendown,down,trigfound,open,shut,shutprev
	logical ampdone,opensav,downprev,dispderiv,opening(50),disptran

c
	nt=0			!counts # of transitions found
c===	inext=1		!start search at data(inext)
	inext=if1		!start search at beginning of fitted region
	trigfound=.true.	!so doesn't jump out at next line
	do while (nt.le.ntm.and.trigfound)
c	  NB Findtrans uses value of shut as input ONLY; does not set it
	   if(nt.gt.0) itriglst=itrig
c Want to avoid trying to fit any incomplete transition close to right
c edge of screen; previously search went only up to ndfit-npfilt=if2, but
c need to exclude any such incomplete transition from fit range too, but
c also want to use points right up to last for fitting if there is no
c transition too close to end.  Therefore search right up to point #ndfit,
c but correct if1,if2 below if a transition is found too close to right
c edge of screen.
c	   iflast=if2-npfilt
c	   call FINDALL(data,inext,iflast,curlev,scrit,ntrig,
	   downprev=down
	   call FINDALL(data,inext,if2,curlev,scrit,ntrig,
     &	navtest,shut,opendown,itrig,curav,nav,down,trigfound)
c
c Now ylevel(nt)=current level (intermed units) BEFORE transition found on
c previous cycle, and curav=amplitude before transition found on this cycle,
c ie AFTER trans found on previous cycle, so if these similar then transition
c found on previous cycle was probably actually a double transition, so check
c this now, before processing the transition just found. Update inextprv
c only after this has been done.
c Also define kfix(0,..ntrans) here, for use in fixing params (via jfix()) later
c Note that interval #k refers to interval AFTER transition #k
	   if(nt.gt.0.and.ampdone) then
c	    if(abs(curav-ylevel(nt)).lt.scrit) then	!'same' amp before & after trans
	    if(abs(curav-ylevel(nt)).lt.float(iampcrit)) then
c		'same' amp before & after trans so call misstrig
c -debug -mark the region from inextprv to inext that is searched
c to determine amplitude
		call HLINE(ix(inextprv),ix(inext),ibasevga-3,15)
		call HLINE(ix(inextprv),ix(inext),ibasevga-4,15)
		call HLINE(ix(inextprv),ix(inext),ibasevga-5,15)
c==end debug
		call MISSTRIG(data,nt,ylevel,kfix,delt,timamax,opening,
     &     base,avamp,inextprv,inext,curav,open,shut,opensav,downprev,
     &	tprev,shutprev,tmin)
	    endif
	   endif
c
	   inextprv=inext
c
c Now process the transition found on this cycle
c At this point we define the transition located by FINDTRANS (even if an
c extra transition has been inserted above when short gap/op located), and
c DOWN still has the value output by FINDTRANS.
	   if(trigfound) then       !so nt=ntm after increment
		nt=nt+1
		ylevel(nt)=curav	!level before trigger (intermed units)
c For each transition, record whether it is an opening or shutting
	      opening(nt)=(down.eqv.opendown)
		open=abs(curav-base).gt.scrit		!open before trigger
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
		if(disptran) then
		   call VLINE(ix(itrig),235,450,11) !mark transition light blue
		   call VLINE(ix(inext),235,450,13)	!mark inext purple
		endif
c Average next 2 points as start on search for next transition
	      curnew=(data(inext)+data(inext+1))/2.
c Decide whether open AFTER trigger, ready for next FINDTRANS.
c If shut before trigger then MUST be open afterwards, but if open
c before trigger not so easy to tell whether shut after it: can go
c only on curnew value which is mean of only two points -would be easier
c to wait until after next FINDTRANS to get better value, but FINDTRANS needs
c to know whether open or shut initially -well needs to know if SHUT (only to
c prevent transitions in wrong direction) but OK to enter with 'not known'
c in which case transitions in either direction allowed
		opensav=open			!open BEFORE trigger
		if(.not.open) then		!shut before trigger
		   open=.true.			!open AFTER trigger
		else if(open) then
		   open=(abs(curnew-base).gt.scrit)
c========add dual criterion here also for SHUT=?
c -or set 'unknown' for now (eg enter FINDTRANS with ishut=-1 (unknown)
c or ishut=1 (shut), or ishut=0 (open)
c=======revise this value of OPEN after next findtrans?
		endif
		SHUT=.not.open		!shut AFTER current transition
c
c
c SPECIAL CASE WHEN TWO TRANSITIONS IN SAME DIRECTION ARE 'CLOSE'
c   When Findtrans finds two consecutive openings then free fit
c will either (a) insert an extra shutting (above) if level is 'same' before
c and after the transition, OR (b) fit a sublevel (if level is not 'same').
c In former case opening(nt-1) and opening(nt) will be different at this
c point so next bit will be skipped. But in latter case, if transitions
c are 'close' together there may well actually be an
c 'short opening-short gap-long(ish) opening' rather than a sublevel.
c Thus, if iautosub=1 ('avoid sublevels if possible') look again using
c first derivative method for transitions
c Define here the time (tclose, microsec) below which transitions
c are deemed to be 'close'
		tclose=4.0*trise
		if(nt.gt.1.and.(opening(nt-1).eqv.opening(nt)).and.
     &		   delt(nt-1).lt.tclose.and.iautosub.eq.1) then
		   call MISSEV(data,nt,ix,dx,ibk2,vard,dispderiv,ylevel,
     & 		itriglst,kfix,delt,opening,base,avamp,finter,
     &		inext,t0,tprev,shutprev)
		endif
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
		   call NEWLEN(data,nt,itriglst,inext,avamp,base,trise,
     & 	     tprev,delt,ylevel,timamax,kfix,down,opendown,shutprev)
		endif
c
c Must now reset curlev before looking for next trans!
		curlev=curnew
	   endif	!end of 'if trigfound'
	enddo		!end of 'do while' loop
c
c Lastly set amplitude after last transition
	ylevel(nt+1)=curlev	!set amplitude after last transition
c
	RETURN
	end

