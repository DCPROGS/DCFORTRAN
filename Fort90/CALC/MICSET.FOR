	logical function MICSET(i,j)
c Returns true if q(i,j) is set via microscopic rev in theory progs
c (call GETIJ(IQ,k,i,j,m) first to get i,j for rate constant #m)
	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
c
	if(ncyc.gt.0) then
	   do n=1,ncyc
		if(im(n,1).eq.i.and.jm(n,1).eq.j) then
		   MICSET=.true.	
		   RETURN
		endif
	   enddo
	endif
	MICSET=.false.
	RETURN
	end


