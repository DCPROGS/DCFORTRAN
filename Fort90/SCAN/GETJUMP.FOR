	subroutine GETJUMP(kt,ijump,njump,ns1,jumps,kjumps,nsweep)
c To return ijump (1,2,...,nsweep) and njump (numbered as in CJUMP.DAT),
c the number of the jump that contains tint(kt)
c Looks through sweeps #ns1 to nsweep-1, so call with ns1=1 if there
c is no knowledge of what result may be.  On exit ns1 is set to ijump
c so if it is known that tint(kt) is in jump #ijump or higher the
c next call to GETJUMP can use this ns1 value.
	integer*2 jumps(nsweep)
	integer*4 kjumps(nsweep)
c
	do j=ns1,nsweep-1
	   ktj=kjumps(j)
	   ktj1=kjumps(j+1)
c	   if(kt.ge.kjumps(j).and.kt.lt.kjumps(j+1)) then
	   if(kt.ge.ktj.and.kt.lt.ktj1) then
		ijump=j
		njump=int4(jumps(j))
		goto 1
	   endif
	enddo
	if(kt.ge.kjumps(nsweep)) then
	   ijump=nsweep
	   njump=int4(jumps(nsweep))
	   goto 1
	endif
c Should never get here!
	call BELL(3)
	print 2,kt,nsweep,kjumps(nsweep)
2	format(' ERROR in GETJUMP: kt, njump, kjumps(njump) = ',3i7)
c
1	continue
	ns1=ijump
	RETURN
	end


