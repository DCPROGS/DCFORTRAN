	subroutine FINDALT(data,i1,i2,openlev,base,scrit,ntrig,
     & shut,opendown,curav,itrig,down,trigfound)
c Searches data(i1) to data(i2) for a transition.
c Version for FITSUB4 in which amplitudes are already fixed manually before
c calling AND which expects openings and shuttings to alternate
c (cf FINDTRAN which has preset amplitudes but allows open-open transitions)
c Thus, this routine outputs only the position of the transition as itrig
c (FINDALL is version that locates transition AND amplitudes)
c
c=========curav calc removed; OK??
c Try leaving CURAV in, for use in MISSEV etc, but not
c used now for transition detection which uses only preset amp levels, and
c not calc as running average -it is just the preset level before the transition
c
c  NB uses value of shut as input ONLY; does not set it
c
c Input:
c openlev,base=prefixed open and shut levels (intermed units)
c idata,ndfit,scrit (always positive), ntrig, navtest
c shut (=shut at start point=idata(i1))
c opendown=openings are downwards in data (on screen)
c
c Output:
c itrig=index of first point to exceed trigger level
c==removed OK?c curav=mean of NAV points from i1 to point before trigger found
c trigfound=false only if no trigger located
c down=transition was downwards (on screen)
c
c NB if SHUT on entry then upward transition is illegal (if opendown)
c  and if open on entry then downward transition is illegal (if opendown)
c
	real*4 data(2048)
	logical down,trigfound,shut,opendown,uplegal,downleg
	logical caplock

	trigfound=.true.	!false if no trigger found
c
	if(shut) then
	   uplegal=.not.opendown
c	   downleg=opendown	!crazy error: this not obeyed on DELL 450!
	   downleg=.not.uplegal
	   curav=base			!initial level
	else
	   uplegal=opendown
	   downleg=.not.uplegal
	   curav=openlev
	endif
c
	ntup=0	!counts number of consecutive points > trigger level
	ntdown=0	!counts number of consecutive points < -trigger level
c=	curav=data(i1)	!used only if navtest=0
c=	curtot=0.0	!for running average
c=	nav=0		!number in running average
	do i=i1,i2
	   curtest=curav
	   ytest=data(i)-curtest
c==	   if(caplock()) print*,i,scrit,ytest
	   if((ytest.ge.scrit).and.uplegal) then	!possible upward transition
		if(ntup.eq.0.or.(i.gt.1.and.i.eq.ilastup+1)) then
	         ntup=ntup+1
	         if(ntup.eq.1) itrig=i	!record first value past trigger
	         ilastup=i
	         if(ntup.eq.ntrig) goto 2	!upward transition confirmed
		endif
		if(i.gt.ilastup+1) ntup=0   !cancel if points not consec
	   else if((ytest.le.-scrit).and.downleg) then !pos downward transition
		if(ntdown.eq.0.or.(i.gt.1.and.i.eq.ilastdn+1)) then
	         ntdown=ntdown+1
	         if(ntdown.eq.1) itrig=i	!record first value past trigger
	         ilastdn=i
	         if(ntdown.eq.ntrig) goto 2	!upward transition confirmed
		endif
		if(i.gt.ilastdn+1) ntdown=0   !cancel if points not consec
c=	   else		!running average
c=		if(ntup.eq.0.and.ntdown.eq.0) then
c=		   nav=nav+1
c=		   curtot=curtot+data(i)
c=		endif
	   endif
c=	if(nav.gt.0) curav=curtot/float(nav)
	enddo

c arrive here if no trigger found
	trigfound=.false.
	goto 99

c arrive here when trigger confirmed
2	continue
	down=ntdown.eq.ntrig
c	if(caplock()) then
c	   if(down) then
c		print*,' downward trans at itrig = ',itrig
c	   else
c		print*,'   upward trans at itrig = ',itrig
c	   endif
c	endif
99	continue
	RETURN
	end


