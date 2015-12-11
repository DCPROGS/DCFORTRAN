	subroutine CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
	integer NSC(n0),icyc(n0),icycsav(n0,n0)
	allocatable ivec
	integer ivec(:)
	logical same,second
c Check if the cycle just found (in icyc)is same as any of the ncyc already
c identified by
c (1) check if it contains same state as an existing one
c (2) if so rotate so this state is 1st element
c (3) compare with existing one- if same, discard
c (4) After (1) reverse order of elements and then do (2),(3),(4)
c
	ndim=n0	!dimension
	same=.false.
	if(ncyc.eq.0) RETURN	!no existing cycles!
	allocate(ivec(n0))
	second=.false.
c Two cycles can be the same only if they have the same number of elements
c so first check if any of the old ones have mc-1 elements:
	ns=mc-1	!size of new one
	do 10 i1=1,ncyc
	if(nsc(i1).ne.ns) goto 10	!new one not same size
c at this point an existing cycle, in icycsav(i1,j), has been found
c that is same size as the new one in icyc(); check if they have any
c state in common
	do 1 i=1,ns		!go through all elements of new one
	is=icyc(i)
	do 2 j=1,ns
	j1=j			!in prep for jumping out of loop
2	if(icycsav(i1,j).eq.is) goto 3	!common element found in posn j1
	goto 1
c
3	continue
c Found state #is in an old cycle, in element #j. Copy the old cycle
c into ivec, rotate it j-1 places to left, to bring state #is to 1st
c position, and check if old one is same as new
	do 4 k=1,ns
4	ivec(k)=icycsav(i1,k)
	if(second) call IVECREV(ivec,ns,ndim)	!reverse elements of ivec
	nr=-(j1-1)
	call IVECROT(ivec,ns,nr,ndim)
	same=.false.
	do 5 k=1,ns
5	if(ivec(k).ne.icyc(k)) goto 6		!not same
c if get here then all elements the same
	same=.true.
	deallocate(ivec)
	RETURN
c
6	continue
c not same- so now reverse order of elements and try again
	if(second) then
	   second=.false.
	   goto 1
	endif
c
	second=.true.
	goto 3
1	continue
10	continue
c
	deallocate(ivec)
	RETURN
	end


