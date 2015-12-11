	subroutine GETSH1(shut1,k,shutprev,shut,t0,dt,i1,i2,kt,
     &  finter,ntrans,iampl,ntmax)
c To identify the iampl() value that corresponds to each flat bit located
c in the calculated trace, so that corresp data values, data(i1) to data(i2),
c can be allocated to shut point amp histo (if iampl=0) or open point
c histo (if not), in FITSUB.
c    The points t0, t0+dt(1),... are at start of filter func for each transition
c (see debug with VLINE in FITSUB).  A short shutting gives 2 closely spaced
c lines both of which appear to precede the blip in the data.  However any flat
c section on calc curve is ENDED by the line marking the NEXT transition,
c and this can be used to identify which iampl() belongs to which flat bit.
c
c Modified 12/01/96 05:55pm to return, as well as shut1, the index (k) such
c that iampl(k) is the amplitude that corresponds to each flat bit.
c
c Modif 03/22/94 05:27pm because GETSH1 now called BEFORE kt updated
c
c Start and end bits, which are fitted if FITSUB (but not FITSUB6) must
c be dealt with separately
c
	real*4 dt(49)
	integer*2 iampl(0:ntmax)
	logical shut1,shutprev,shut
c
	t1=float(i1-1)*finter	!time to first point in flat section
	t2=float(i2-1)*finter	!time to last point in flat section
	t=0.5*(t1+t2)		!time in middle of flat bit
	if(t.lt.t0) then
	   shut1=shutprev
	   k=kt
	   RETURN
	endif
	ttrans=t0		!in case ntrans=1
c
c	i0=kt-ntrans	!iampl(i0)=amplitude before 1st transtion
	i0=kt			!GETSH1 now called BEFORE kt updated
	if(ntrans.gt.1) then
	   do i=1,ntrans-1
		ttrans=t0 + dt(i)		!time at END of interval #i
		if(t.lt.ttrans) then
		   k=i0+i
		   shut1=iampl(k).eq.0
		   RETURN
		endif
	   enddo
	endif
c  Finally the bit after the last transition (shut when SHUT=true)
	if(t.gt.ttrans) then
	   k=kt+ntrans
	   shut1=shut
	endif
c
	RETURN
	end

