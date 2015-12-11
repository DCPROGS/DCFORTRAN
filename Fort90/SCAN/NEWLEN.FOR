	subroutine NEWLEN(data,nt,itriglst,inext,avamp,base,trise,
     & tprev,delt,ylevel,timamax,kfix,down,opendown,shutprev,fixa)
c SPECIAL CASE WHEN TWO TRANSITIONS IN OPPOSITE DIRECTIONS ARE 'CLOSE' in SCAN
c   Short openings or shuttings for which 2nd trigger 'only just'
c found have estimated durations that are rather too long -fix now
c by re-estimating from max amplitude (as done above when 2nd trans
c missed altogether) -do it only of max amp is substantially less
c than avamp!  If max excursion is clearly below avamp, then
c (a) use it to revise guess of duration, and (b) FIX the amplitude
c (to zero or avamp).  This ONLY changes initial guess for duration
c of an event: does not insert new transitions
c
c NB this version also changes ylevel() to fix the amplitude of the
c short event at base (if shut) or base+avamp (if open)
c MODIF 06/10/94 10:42am by adding extra parameter=fixa; set this true
c for call from GETONE (manual-one-amp mode) so ylevel(nt) is not changed
c and open level is taken as ylevel(2), shut level is taken as ylevel(1)
c (GETONE is not called if initially open). Otherwise set false.
c
c Modified 01/06/94 09:14am so that if channel was initially shut, brief
c shuttings are fixed to level of initial baseline, ylevel(1), rather
c than to 'base' which has still not been updated.
c
	real*4 data(2048)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	integer kfix(0:50)
	logical firstdown,down,opendown,shutprev,fixa
c
		 firstdown=.not.down
		 i1=itriglst-1
		 if(i1.lt.1) i1=1
		 if(.not.firstdown) then	    !1st trans=up; 2nd trans=down
		   dext=-1.e36         !extreme value=maximum
		   do i=i1,inext
		      if(data(i).gt.dext) then
			   dext=data(i)	!highest point on screen
			endif
		   enddo
		 else			     !1st trans=down; 2nd trans=up
		   dext=1.e36          !extreme value=minimum
		   do i=i1,inext    !i.e. subsequent down transition missed
			if(data(i).lt.dext) then
			   dext=data(i)	!lowest point on screen
			endif
		   enddo
		 endif
		 if((firstdown.and.opendown).or.
     &	    (.not.firstdown.and.(.not.opendown))) then	!SHORT OPENING
		 	frac=abs((dext-base)/avamp)
			if(fixa) then
			   a=ylevel(2)
			else
			   a=avamp+base			!fixed amplitude (if req)
			endif
			if=2				!value for kfix (if req)
		 else								!SHORT SHUTTING
		 	frac=abs((dext-(avamp+base))/avamp)
c===			a=base			!fixed amplitude (if req)
		   	if(shutprev.or.fixa) then
			   a=ylevel(1)		!initial level
			else
			   a=base
			endif
			if=1				!value for kfix (if req)
		 endif
c timamax(i) is length corresp. to frac=i*0.02 (see INSCAN)
c If duration small then fix the amplitude -but can't
c use 'if(d.lt.2.0*trise) then' as criterion because d=<timamax(19) which
c is ALWAYS < 2*trise (95%=1.56*trise!).  So use 'frac' as criterion -if
c frac>0.95 then interval is too long to be estimated from max amp.
		 if(frac.le.0.95) then	!new criterion
		   i=ifixr(frac/0.02)
		   if(i.lt.1) i=1		!A/Amax=2%
		   if(i.gt.49) i=49		!A/Amax=98%
c==		   delt(nt-1)=timamax(i)
		   d=timamax(i)			!provisional new duration estimate
c===		 if(d.lt.2.0*trise) then
c           NB if delt() is altered here, tprev must be changed accordingly!
		   tprev=tprev - (delt(nt-1) - d)
		   delt(nt-1)=d
		   if(.not.fixa) ylevel(nt)=a		!in intermed units
		   kfix(nt-1)=if  	!fix parameter later
		 endif
c
	RETURN
	end


