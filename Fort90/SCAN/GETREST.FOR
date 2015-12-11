	subroutine GETREST(nt,ntrans,ndfit,finter,shutprev,imode,
     &  ylevel,base,avamp,prevlevel,inext,t0,delt,opening)
c
c What if the requested number of transitions is not found? Must add more
c transitions to make up the requested number -make alternately open/shut
c -must define the necessary extra ylevel(), t0 and delt() values
c INPUT: imode=0 when called from SETSUBL (sublevel mode), so have no
c			idea what remaining amps should be -use base, avamp+base
c	   imode=1 when called from SETLEV so have only one amp, and when
c			this defined, use it for the rest of the transitions
c
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 delt(0:50)		!temp storage for transition search results
	logical shutprev,opening(50)
	logical cjumpfst
	common/jump/cjumpfst
c
	tmax=float(ndfit)*finter	!length of display
	if(nt.eq.0) then
	   if(shutprev.or.cjumpfst) then		!shut before first transition
		ylevel(1)=base
		ylevel(2)=avamp+base
	   else				!open before first transition
		ylevel(1)=prevlevel
		ylevel(2)=base
	   endif
	   do i=3,ntrans+1
		ylevel(i)=ylevel(i-2)      !set alternating levels
	   enddo
	   t0=0.2*tmax
	   do i=1,ntrans-1
		delt(i)=0.3*tmax/float(ntrans)
	   enddo
	else if(nt.eq.1) then
c    	case where at only one transition found (so t0 and ylevel(1) defined)
	   if(shutprev) then		!shut before first transition
		ylevel(2)=avamp+base
	   else				!open before first transition
		ylevel(2)=base
	   endif
	   do i=3,ntrans+1
		ylevel(i)=ylevel(i-2)      !set alternating levels
	   enddo
	   t1=float(inext-1)*finter  !time for point after last trigger found
	   tleft=tmax-t1			!time left to end of display
	   do i=nt,ntrans-1
		delt(i)=0.4*tleft/(float(ntrans-nt))
	   enddo
	else
c    	case where at least two transitions found (ylevel(1) and (2) defined)
	   t1=float(inext-1)*finter  !time for point after last trigger found
	   tleft=tmax-t1			!time left to end of display
	   if(imode.eq.0) then
		if(opening(nt)) then		!last trans found was opening
		   ylevel(nt+1)=avamp+base		!set open level
		   ylevel(nt+2)=base
		else				!last trans found was shutting
		   ylevel(nt+1)=base
		   ylevel(nt+2)=avamp+base		!set open level
		endif
		if(ntrans.ge.nt+2) then
		   do i=nt+3,ntrans+1
		      ylevel(i)=ylevel(i-2)      !set alternating levels for rest
		   enddo
		endif
	   else if(imode.eq.1) then
		do i=nt+3,ntrans+1
		   ylevel(i)=ylevel(i-2)      !set alternating levels for rest
		enddo
	   endif
c
	   t1=float(inext-1)*finter  !time for point after last trigger found
	   tleft=tmax-t1			!time left to end of display
	   do i=nt,ntrans-1
	      delt(i)=0.4*tleft/(float(ntrans-nt))
	   enddo
	endif
	RETURN
	end
