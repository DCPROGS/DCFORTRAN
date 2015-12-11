	subroutine SETPARAM(ntrans,delt,t0,y0,kfix,shutprev,
     & prevlevel,stepamp,dt,dtsav,kmax,theta,jfix)
c Input=ntrans, delt(), t0, y0, kfix, shutprev,prevlevel, stepamp
c Output=dt(),dtsav(),kmax,theta(1,..,kmax), kfix, jfix
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 stepamp(50),dt(49),dtsav(49)
	real*4 theta(50)
	integer jfix(50),kfix(0:50)
	logical shutprev
c
c Define time intervals for CONV in DT()
	do i=1,ntrans-1
	   if(i.eq.1) then
		dt(i)=delt(i)
	   else
		dt(i)=dt(i-1) + delt(i)
	   endif
	   dtsav(i)=dt(i)		!save in guesses need to be altered
	enddo
c
c Define theta()
	theta(1)=t0
	if(ntrans.gt.1) then
	   do i=2,ntrans
		theta(i)=t0 + dt(i-1)
	   enddo
	endif
c Amplitude params (units as for data()='intermed units') (some may be fixed)
	j1=ntrans+1
	theta(j1)=y0	!initial level before trans #1
c Define jfix(), eg from kfix() values set above to fix amplitudes
c of short events
	if(kfix(0).eq.0) then
	   jfix(j1)=0
	else
	   jfix(j1)=1	!first amp level fixed
	endif
c If prev fitting ended in OPEN state (y=prelevel), then fix level before
c first transition to this value (=method 1, below) (but do not set IPROPS()
c to indicate fixed value since this value WAS estimated, from the first part
c of the opening).
	if(.not.shutprev) then 		!open initially
	   theta(j1)=prevlevel
	   jfix(j1)=1
	   kfix(0)=2	!define so print out correct below
	endif
	do j=1,ntrans
	  j1=j1+1			!=ntrans+2,...,2*ntrans+1
	  theta(j1)=theta(j1-1)+stepamp(j)
	  if(kfix(j).eq.0) then
	    jfix(j1)=0
	  else
	    jfix(j1)=1	!fixed
	  endif
	enddo
c
	kmax=j1		!total # of parameters (inc fixed ones)
c
	RETURN
	end

