	subroutine GETALL(data,ix,dx,nt,ntm,if1,if2,inext,itrig,it1,
     & scrit,curlev,ntrig,navtest,shut,shutprev,open,opendown,
     & kfix,delt,ylevel,base,t0,opening,trise,iautosub,timamax,
     & avamp,ibasevga,tmin,finter,ibk2,vard,dispderiv,iampcrit,
     & npfilt,disptran,ampdone,tclose,dtrigm,rms1,nscale,iyoff,
     & derivfac)
c
c Subroutine to run the entire 'do while' loop to find transitions
c with FINDALL
c Modif by adding definition of dtrigm(), for use later in helping to define
c shut periods in DEF_ONE later.
c Modif 08/23/94 09:05am by adding derivfac as parameter (see DERIVSCH).
c
c VERSION IN WHICH, RATHER THAN CALLING MISSTRIG ONLY WHEN LEVEL IS 'SAME'
C BEFORE AND AFTER A TRANSITION, SEARCHES THE REGION AROUND TRIGGER FOR
C EVERY TRANSITION FOR MAX/MIN FOUND TO SEE WHETHER THE TRANSTION FROM
C PRE- TO POST-TRANSITION LEVEL IS 'SMOOTH' OR WHETHER IT CONTAINS A
C BRIEF EVENT
c
	real*4 data(2048)
	integer*2 ix(2048),iy2
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 dtrigm(50)
	integer itrigm(50)	!for debug only
	integer kfix(0:50)
	logical opendown,down,trigfound,open,shut,shutprev
	logical ampdone,opensav,downprev,dispderiv,opening(50),disptran

c
	nt=0			!counts # of transitions found
c===	inext=1		!start search at data(inext)
	inext=if1		!start search at beginning of fitted region
	trigfound=.true.	!so doesn't jump out at next line
c==	ampcrit=float(iampcrit)
	iy=240		!=========debug

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
	   navlast=nav
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
c!!!!!!!!!!!!!!!!!
c   PROBLEM WITH FOLLOWING CRITERION: does not work when there is a short
c event with different levels on each side of it!! -really need to look
c for extreme point when ANY transition is found! -ie check whether current
c goes DIRECTLY from the 'pre-trig' to 'post-trig' level (rather than having
c an intervening brief opening or shutting).- but no need to search from as
c far back as inextprv -range could be chosen better
c	NB Next bit could go at end of loop, but findall gives average level
c BEFORE transition (-curav), so better put it 'start of next loop;, but after
c next FINDALL call, so have levels before AND after transition found on prev
c cycle.  The last cycle (only) has trigfound=false, and curav gives level
c after the last transition.
c    NB trigfound will be true every time this is done (first 'false' ends
c the do-while loop), but must put this after Findall, so we have value
c of curav (after the final call to findall, returning trigfound=false,
c curav will estimate the value after the last transition)
c!!!!!!!!!!!!!!!!!
c============
c NEW VERSION:
c 	On EVERY transition, search the data between initial and final
c level (eg itrig-2 to inext+2) to see whether there is a big enough
c min/max to suggest that a short opening/shutting has been missed, and
c there are actually two rather than one transition.
c   If transition from pretrig level (ylevel(nt)), and posttrigger level
c (curav) was smooth and noise-free, then all points between them should
c lie BETWEEN these two levels.
c NOTE: works badly with single short opening when BOTH transitions are (just)
c found, i.e. when 2nd trigger immediately follows the first, because in this
c case the level after the 1st transition (curav, =level before 2nd) will be
c mean of only one or two points after inext  (and these points likely to be
c well past peak of the opening) so they differ from dmin (in shortev) and
c an extra transition is, incorrectly, inserted).  Try to fix this by
c using SHORTEV only if curav is mean of a decent number of points, say nav>4
c or so (better to use a time, rel to trise, rather than a number of points!)
c Try SHORTEV also if orig criterion (similar amp before and after trigger)
c -this may help to catch some short events in which only the first transition
c was found but which are so close to another transition that nav<4.
c Do next bit even for the first fit -problem is that need estimate of avamp
c which is not yet defined for first fit, so fudge it below
c	   if(nt.gt.0.and.ampdone) then
	   if(nt.gt.0) then
          if(nav.gt.3.or.abs(curav-ylevel(nt)).lt.scrit) then
		i1=itriglst
		i2=itriglst+npfilt+2
c 		NB can search only up to data(ndfit)=data(if2); the array data() is not
c 		defined beyond this point!!
		if(i2.gt.if2) i2=if2
		if(disptran) then
		   ix1=itriglst
c 		   mark curav,ylev in yellow
		   iy2=ifixr(ylevel(nt))
		   call SCALINT(iy2,1,nscale,iyoff)
		   call HLINE(ix(ix1)-6,ix(ix1)+2,iy2,14)
		   iy2=ifixr(curav)
		   call SCALINT(iy2,1,nscale,iyoff)
		   call HLINE(ix(ix1)-6,ix(ix1)+2,iy2,14)
		   call HLINE(ix(i1),ix(i2),iy,15)           !=======debug
		   call HLINE(ix(i1),ix(i2),iy-1,15)         !=======debug
		   iy=iy-2						!=======debug
		endif
		if(.not.ampdone) then
		   if(opendown) then
			cmax=1.e37
			do i=if1,if2
			   if(data(i).lt.cmax) cmax=data(i)
			enddo
			avamp=cmax-base
		   else
		   endif
		endif
		call SHORTEV(data,i1,i2,nt,ylevel,kfix,delt,timamax,opening,
     & 	base,avamp,disptran,inext,curav,open,shut,opensav,downprev,
     &      tprev,shutprev,tmin,itrig,npfilt,finter,dtrigm,itrigm,
     &	rms1,opendown)
	    endif
	   endif
c
c  NB iampcrit seems to be too small sometimes, and causes some obvious
c short events to be missed -go back to using Scrit for now, but very
c arbitrary, and better search after EVERY transition,
c as above- DONE IN THIS VERSION
c	   if(nt.gt.0.and.ampdone) then
c	    if(abs(curav-ylevel(nt)).lt.scrit) then	!'same' amp before & after trans
cc	    if(abs(curav-ylevel(nt)).lt.float(iampcrit)) then
cc		'same' amp before & after trans so call misstrig
cc -debug -mark the region from inextprv to inext that is searched
cc to determine amplitude
c		call HLINE(ix(inextprv),ix(inext),ibasevga-3,15)
c		call HLINE(ix(inextprv),ix(inext),ibasevga-4,15)
c		call HLINE(ix(inextprv),ix(inext),ibasevga-5,15)
cc==end debug
c		call MISSTRIG(data,nt,ylevel,kfix,delt,timamax,opening,
c     &     base,avamp,inextprv,inext,curav,open,shut,opensav,downprev,
c     &	tprev,shutprev,tmin,itrig,npfilt,finter)
cc If trigfound=false then need to calculate here the value if dtrigm()
cc following the inserted transition (if one was inserted by misstrig,
cc ie if inext>inextprv)
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
		   t0=float(it1-2)*finter   !=0 when it1=1 ie first point on display
		   tprev=t0
		else
		   t=float(itrig-2)*finter  !=0 when itrig=1 ie first point on display
		   t=t - 0.1*trise   !empirical shift -trans is bit too far to right
		   delt(nt-1)=t-tprev
		   tprev=t
		endif
c -set inext, and mark trigger points
	      inext=itrig+npfilt	!leave time for filter to complete
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
c Record the extreme point between itrig and inext (or inext+2) for use
c later; dtrigm(nt)=extreme point following nt'th trigger
		inext1=inext+1
		if(inext1.gt.if2) inext1=if2
		call GETMAX(data,itrig,inext1,dmin,dmax,imin,imax)
		if(down) then
		   dtrigm(nt)=dmin		!lowest point on screen
		   itrigm(nt)=imin		!for debug
		else
		   dtrigm(nt)=dmax	!highest point on screen
		   itrigm(nt)=imax		!for debug
		endif
c =========mark for debug
c		call HLINE(ix(itrig),ix(inext1),ibasevga-4,12)	!debug
c		call HLINE(ix(itrig),ix(inext1),ibasevga-5,12)	!debug
c		call HLINE(ix(itrig),ix(inext1),ibasevga-6,12)	!debug
c		call VLINE(ix(im),235,450,12)
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
c==		tclose=4.0*trise		!now param
		if(nt.gt.1.and.(opening(nt-1).eqv.opening(nt)).and.
     &		   delt(nt-1).lt.tclose.and.iautosub.eq.1) then
		   call MISSEV(data,nt,ix,dx,ibk2,vard,dispderiv,ylevel,
     & 	    itriglst,kfix,delt,opening,base,avamp,finter,derivfac,
     &	    inext,t0,tprev,shutprev,dtrigm,itrigm,timamax,opendown)
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
		  call NEWLEN(data,nt,itriglst,inext,avamp,base,trise,tprev,
     & 	   delt,ylevel,timamax,kfix,down,opendown,shutprev,.false.)
		endif
c
c Must now reset curlev before looking for next trans!
		curlev=curnew
	   endif	!end of 'if trigfound'
	enddo		!end of 'do while' loop
c
c Lastly set amplitude after last transition.  NB if trigger not found
c then use CURAV from last call to FINDALL (that which returned trigfound=false),
c which will be average of all points from inext to if2, which should be better
c estimate of the final level than curlev, which is mean of only the first
c two points
	if(trigfound.or.nav.le.2) then
	   ylevel(nt+1)=curlev	!set amplitude after last transition
	else
	   ylevel(nt+1)=curav	!set amplitude after last transition
	endif
c
c DEBUG: display dmin,dmax for all the transitions finally located
c=======debug
	if(disptran) then
	   do n=1,nt
c mark dmin,dmax in green
		ix1=itrigm(n)
		iy2=ifixr(dtrigm(n))
		call SCALINT(iy2,1,nscale,iyoff)
		call HLINE(ix(ix1)-4,ix(ix1)+4,iy2,10)
c		iy2=ifixr(dmax)
c		call SCALINT(iy2,1,nscale,iyoff)
c		call HLINE(ix(ix1)-4,ix(ix1)+4,iy2,10)
	   enddo
c Insert here a horizontal line, moved by arrow keys, that displays the
c corresponding value of data()
	   call WDIALOG(1,
     &	'Hit arrows to display data() value cursor: ESC to end',-10)
	   iy2=ifixr(base)
	   call SCALINT(iy2,1,nscale,iyoff)
	   call BUGLINE(iy2,nscale,iyoff,10)
	endif
c=====  end debug
	RETURN
	end

