	logical function JOINED(i,j)
c
	COMMON/QPAR/NCON,IC(2,200)
c
c joined=true if state i and state j are connected
	joined=.false.
	do m=1,ncon
	   if((ic(1,m).eq.i.and.ic(2,m).eq.j).or.
     &	(ic(1,m).eq.j.and.ic(2,m).eq.i)) then
		joined=.true.
		goto 9
	   endif
	enddo
9	continue
	RETURN
	end

