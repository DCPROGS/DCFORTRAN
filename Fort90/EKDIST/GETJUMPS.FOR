	subroutine GETJUMPs(kt,ijump,njump,ns1,jumps,kjumps,nsweep,
     & jset,nswpmax,nset)
c To return ijump (1,2,...,nsweep) and njump (numbered as in CJUMP.DAT),
c the number of the jump that contains tint(kt)
c Looks through sweeps #ns1 to nsweep-1, so call with ns1=1 if there
c is no knowledge of what result may be.  On exit ns1 is set to ijump
c so if it is known that tint(kt) is in jump #ijump or higher the
c next call to GETJUMP can use this ns1 value.
c
c Modif GETJUMP modified for 2D arrays and renamed GETJUMPs (original
c version still used in CJFIT and SCAN- j=jset is the set number to be
c used (if more than 1)
c
c	integer*2 jumps(nsweep)
c	integer*4 kjumps(nsweep)
	integer*2 jumps(nswpmax,nset)
	integer*4 kjumps(nswpmax,nset)
	integer*4 nsweep(10)
c
	j=jset
	do ij=ns1,nsweep(j)-1
	   if(kt.ge.kjumps(ij,j).and.kt.lt.kjumps(ij+1,j)) then
		ijump=ij
		njump=int4(jumps(ij,j))
		goto 1
	   endif
	enddo
	if(kt.ge.kjumps(nsweep(j),j)) then
	   ijump=nsweep(j)
	   njump=int4(jumps(nsweep(j),j))
	   goto 1
	endif
c Should never get here!
	call BELL(3)
	print 2,kt,j,nsweep(j),kjumps(nsweep(j),j)
2	format(' ERROR in GETJUMP: kt,jset,njump, kjumps(njump,j) = ',4i7)
c
1	continue
	ns1=ijump
	RETURN
	end


