	subroutine SETVALS(iampl,ntmax,kt,ntrans,base,scrit,delt,dt,
     & ylevel,t0,dxf,dtf,tshift,ibk2,kfix,calfac2,rms1,avamp,
     & SHUT,setshut,stepamp,opendown)
c Called in FITSUB6 when 'use this guess' is chosen, to check which
c levels are SHUT and set iampl() etc.
	integer*2 iampl(0:ntmax)
	integer kfix(0:50)
	real*4 dt(49),stepamp(50)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 delt(0:50)
	logical shut,sets,setshut(0:50),autoset,opendown
	integer ixn(25)
	logical reject
c
c Modify ylevel, amd set setshut,iampl, and stepamp, on basis of values
c that were set shut (via kfix) in GETTRANS, using value of 'base' (which
c may have been altered by 'adjust baseline' in FITSUB6
c Also set as in CHKSHUT (which is called before leaving to check again)
	do i=0,ntrans
	   setshut(i)=.false.
	   y1=ylevel(i+1)
	   if(kfix(i).eq.1.or.abs(y1-base).lt.0.4*scrit) then
c	   if(kfix(i).eq.1) then	!use kfix, from GETTRANS to define shut
		setshut(i)=.true.
		ylevel(i+1)=base
		iampl(kt+i)=0
	   endif
	enddo
	do i=1,ntrans
	   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	enddo
c set non-zero iampl()
	do i=1,ntrans
	   if(setshut(i)) then
		iampl(kt+i)=0
	   else
      	iampl(kt+i)=int2(ifixr(ylevel(i+1)-base))
	   endif
	enddo
c
c  Define delt (used to be in FITSUB)
	do i=1,ntrans-1
	   if(i.eq.1) then
		delt(i)=dt(i)
	   else
		delt(i)=dt(i)-dt(i-1)
	   endif
	enddo
c
c Before leaving, check any levels that are not already set shut, to see
c if they should be.
	imode=0	!not yet fixed to check all at once as in FITSUB
	n=0
	do i=1,ntrans
	   k=kt+i		!index for iampl()
	   y1=ylevel(i+1)
	   call CHKSHUT(y1,base,scrit,k,i,delt,t0,dxf,dtf,
     &	  tshift,ibk2,calfac2,rms1,avamp,SHUT,sets,autoset,
     &	   opendown,reject,n,ixn,imode)
	   if(shut) iampl(k)=0
	   if(sets) then
	      setshut(i)=.true.
	      ylevel(i+1)=base
	   endif
	enddo
c
	RETURN
	end


