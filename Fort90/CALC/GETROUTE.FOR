	subroutine GETROUTE(i1,kA,kF,ix,jx,il,ncdep,
     &	xazero,xbzero,is_route)
	logical route,is_route,xazero,xbzero
	integer IX(100),JX(100)
	integer IL(100)		!for ligand type
	ALLOCATABLE::ic1,ic2,idone
	integer*2 ic1(:),ic2(:),idone(:)
	common/QPAR/ncon,IC(2,200)     !for connections
c
c Routine to find out whether there is ANY route leading from a
c specified shut state (#i), i=kA+1,...,kF, to an open state
c (state 1,..,kA), for SCJUMP.
c Returns is_route=true if route exists, false if it does not exist.
c   This routine could easily be modified to look at ANY two subsets, set 1
c and set 2 say, rather than shut, open.  Need only check that state #i is
c in set 1 on entry, and change 'j.le.kA' to test of whether state j is
c is in set 2.
c   Array IC contains connections, as defined in QGEN.  Arrays ix,jx,il
c contain details of concentration-dependent routes as in QGEN.
c If xazero=true in the call, then conc of agonist (ligand 1) is zero
c	so some route will not be valid
c If xbzero=true in call then conc of ligand 2=0
c
	is_route=.false.
	k=kA+kF
	if(i1.le.kA.or.i1.gt.k) then
	   call BELL(2)
	   print 10,i1
10	   format(' GETROUTE called with state',i3,' that is not shut')
	   STOP
	endif
c Allocate arrays needed in this routine
	ALLOCATE(ic1(k),ic2(k*(k-1)),idone(k))
c
c Now start loop that identifies, in ic2(), all the states to which the
c shut states in ic1(i), i=1,..,kc1, are connected.  If an open state is
c found, jump out of the 'do n=' loop and reurn with is_route=true. Once
c a particular starting shut state (#n say) has been tested in this loop,
c mark it as 'done' by setting idone(n)=1 (no need to test this one again).
c For the first time round the loop find states to which #i1 is connected
	kc1=1
	ic1(1)=i1
c Initialise idone()
	do i=1,k
	   idone(i)=0
	enddo
c
c Return to 3 to repeat the 'do n=' loop
3	continue
	kc2=0		!zero here, so get ALL the shut states for loop in ic2()
	do n=1,kc1
	   i=ic1(n)		!start at each shut state in ic1()
	   do j=1,ncon
		if(ROUTE(i,j)) then
c check (a) whether this route is invalid because of zero agonist conc
c  and (b) if valid, whether j is an open state
		   do m=1,ncdep
			if(ix(m).eq.i.and.jx(m).eq.j) then	!i1->j is conc dep
			   if(il(m).eq.1.and.xazero) goto 1	!zero conc so no route
			   if(il(m).eq.2.and.xbzero) goto 1	!zero conc so no route
			endif
		   enddo
		   if(j.le.kA) then
			is_route=.true.	!if j is open state then done
			goto 99		!exit
c			RETURN
		   endif
		   kc2=kc2+1
		   ic2(kc2)=j		!record this shut state
1		   continue
		endif
	   enddo
	enddo
c Now have list of all the kc2 shut states that are connected to each of
c the shut states tested in the 'do n=' loop (i.e. those connected to #i1
c for the first time round this loop).  After the first time round, ic2()
c may contain the same shut state # more than once, so eliminate duplicates.
c Also eliminate any starting shut states that have already been tested
c ('done') on previous 'do n=' loops.  If there any further starting shut
c states to be tested, put them in ic1() and do the 'do n=' loop again.
c Subroutine finishes when either a route to an open state is found, or when
c all possible starting shut states have been done without finding a
c route to an open state.
c First note as 'done' the shut states tested on last 'do n=' loop
	do i=1,kc1
	   idone(ic1(i))=1
	enddo
c Make a new ic1() array
	kc1=0
	do m=1,kc2
	   m1=ic2(m)	!current shut state # (for brevity)
	   if(idone(m1).eq.1) goto 2	!this state already done
	   if(m1.eq.1) then
		kc1=kc1+1
		ic1(kc1)=m1		!copy first ic2() into ic1()
	   else
		do j=1,kc1	  !check for duplicates in ic2() (kc1 not updated yet)
		   if(ic1(j).eq.m1) goto 2	!#m1 is already in ic1()
		enddo
		kc1=kc1+1
		ic1(kc1)=m1		!copy next ic2() into ic1()
	   endif
2	   continue
	enddo
c
	if(kc1.gt.0) goto 3	!repeat 'do n=' loop
c
99	continue
	DEALLOCATE(ic1,ic2,idone)
	RETURN
	end


