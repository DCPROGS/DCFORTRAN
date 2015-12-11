	logical function CONSET(i,j)
c Returns true if q(i,j) is set via comstraint in theory progs
c (call GETIJ(IQ,k,i,j,m) first to get i,j for rate constant #m)
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
c
	if(neq.gt.0) then
	   do n=1,neq
		if(i.eq.ie(n).and.j.eq.je(n)) then
	         CONSET=.true.
		   RETURN
		endif
	   enddo
	endif
	CONSET=.false.
	RETURN
	end


