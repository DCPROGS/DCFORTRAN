	logical function CONCDEP(i,j)
c Returns true if q(i,j) is concentration dependent in theory progs
c (call GETIJ(IQ,k,i,j,m) first to get i,j for rate constant #m)
	COMMON/CPAR/NCDEP,IX(10),JX(10),X
c
	if(ncdep.gt.0) then
	   do n=1,ncdep
		if(ix(n).eq.i.and.jx(n).eq.j) then
		   CONCDEP=.true.	
		   RETURN
		endif
	   enddo
	endif
	CONCDEP=.false.	
	RETURN
	end

