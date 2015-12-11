	subroutine GETLAST(kt,iampl,iprops,ntmax,ialast,ialast1)
c To identify 'last', 'last but one' amplitude for use in SETLEV, SETSUBL
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
c
	ialast=0			!not yet def
c==?	if(iampsv.ne.-9999) ialast=iampsv
	ialast1=0			!not yet def
	ikt=kt
604	if(ikt.le.0) goto 603	!no value found
	itest=iampl(ikt)
	if(itest.ne.0.and.iprops(ikt).eq.0) then
	   ialast=itest		!ialast found
	   ikt=ikt-1		!go back one more
	else
	   ikt=ikt-1
	   if(ikt.le.0) goto 603	!no value found
	   goto 604				!look at prev amplitude
	endif
c look for last but one
605	if(ikt.le.0) goto 603	!no value found
	itest=iampl(ikt)
	if(itest.ne.0.and.iprops(ikt).eq.0) then
	   ialast1=itest			!last but one found
	else
	   ikt=ikt-1
	   if(ikt.le.0) goto 603	!no value found
	   goto 605				!look at next prev amp
	endif
603	continue
c
	RETURN
	end


