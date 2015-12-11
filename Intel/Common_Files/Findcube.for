	subroutine FINDCUBE(ncube,cubedef,cubecyc,cubext,extcyc)
c Called in getqd, qdefine, \hjcfit\getqd1.  All args in commons
c To locate cubic structures in a reaction mechanism, by looking for two
c different cycles that are joined, and then checking that the rest of the
c connections needed to make a cube are present.  This finds the face
c opposite face to the current face being tested.  For a single cube any given
c cycle has only one opposite face, but for a cycle that forms the interface
c between two cubes, there are two opposite faces and this cycle form a part
c of two cubes.  Any given cycle can be part of two cubes at most.
c has two opposite faces, one in each cube
c
c The output, in common, defines cube #m (m=1,...,ncube) via following arrays.
c  The numbering of the cubes is such that cubes with the smallest number of
c  external faces come first, and those with largest number (corner cubes) last.
c
c cubedef(m,1:8)= the eight states that form the cube, the first 4 and last 4
c			being opposite faces of the cube (no routes in common)
c Cubecyc(m,1:6)= the 6 cycles that form the cube (cycle numbering as in im() at
c			input to findcube).  The first 2 are cycles that form opposite
c			faces of the cube, the next 2 and last two are adjacent faces,
c			as required for setting MR in a single cube
c			NEW ADDITION: 09/25/03 12:28pm -it seems to be essential for
c			stacked (more than one) cube that not only are the last two
c			faces adjacent not opposite, but also that internal faces are
c			set before external faces.  This may not always be possible:
c			for example in a 3*3*1 cube arrangement (eg PB's desens model)
c			the central cube has two external faces and they are opposite
c			('front' and 'back') not adjacent, so the two external faces
c			cannot come last. However since the central cube is set first,
c			it should not matter if there is an internal cycle that is set
c			next to last.  So precedence rules seem to be
c
c			(1) For any cube ALWAYS make the last two faces to be set a
c			    pair of adjacent faces, not opposite.
c			(2) When passible set internal faces before externl, but if
c			    the only two external faces are opposite, then make one
c			    external face last.
c			    (a) If there is only one external face set it last, preceded
c				by any adjacent face
c			    (b) If there are 2 or more external faces,
c				 (i) if any pair of them are adjacent then set these
c				 last, and internal faces first
c
c				 (ii) if there is no pair of adjecent external faces
c				then make one of the external faces last, and precede
c				 it with an adjacent face.
c
c cubext(m) =	the number of external faces for cube #m
c
	logical joined,found,duplic,opp
	integer*4 icyc(4),icyc1(4)
	integer NSC(50),IM(50,100),JM(50,100)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/QPAR/NCON,IC(2,200)
c     integer*4 cubecon(100,8),ncons
c	common/KBLK/KA,KB,KC,KD
	ALLOCATABLE::index,itemp
	integer*4 index(:),itemp(:,:)
	integer*4 istat(8),istat1(8)	!temp
c for definitions of cubes
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
	!common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
	logical extcyc(50)
	
c
	logical discprt
	common/dp/discprt
c
	logical debug,caplock

c Added 16-03-07 DC
	

c
	debug=.false.
c
	ncube=0
c For one cube need 6 cycles (faces)
	if(ncyc.lt.6) RETURN
c Define max possible number of cubes
	ncmax=1+(ncyc-1)/5
c
c	if(debug) then		!print cycles for reference
c###	   print 83
	   if(discprt) write(7,83)
83	   format(' cycle #    states')
	   do n=1,ncyc
c###		print 84,n,(im(n,i),i=1,4)
		if(discprt) write(7,84) n,(im(n,i),i=1,4)
84		format(2x,i3,5x,4i3)
	   enddo
c	endif

c Any given cycle, #nc. can be part of two cubes at most.
	do nc=1,ncyc
	   if(nsc(nc).ne.4) then
c###		call BELL(1)
c###		print 1,nc,nsc(nc)
1		format(' Cycle ',i3,' has ',i2,' states',/,
     & 	' FINDCUBE works only for cycles with 4 states')
		goto 99
	   endif
	   is=im(nc,1)	!start with any state (1st say) in cycle #nc
	   if(debug) then
c###		print 70,is,nc
		if(discprt) write(7,70) is,nc
70		format(' Test connections to state ',i3,' in cycle ',i3)
	   endif
	   do nc1=1,ncyc	!is state #nc joined to a state in a different cycle?
		if(nsc(nc1).ne.4) then
c###		   call BELL(1)
c###		   print 1,nc1,nsc(nc1)
c1		   format(' Cycle #',i2,' has ',i2, 'states',/,
c      &	' FINDCUBE works only for cycles with 4 states')
		   goto 99
		endif
		if(nc1.ne.nc) then
		   do m=1,nsc(nc1)
			is1=im(nc1,m)
			if(debug) then
c###			   print 71,is1,nc1
			   if(discprt) write(7,71) is1,nc1
71		         format('       - state ',i3,' in cycle ',i3)
	   		endif
c If join between cycles found then record the putative cube in cubedef(ncube+1)
c but to ensure two opposite faces, must check that state is1 (in cycle nc1) is
c not also a member of cycle nc (as will be the case if cycles nc and nc1 are
c adjacent)
			do i=1,4
			   if(is1.eq.im(nc,i)) then
				if(debug) then
c###				   print 79,is1,nc,nc1
				   if(discprt) write(7,79) is1,nc,nc1
79				   format(' state ',i3,' is in both cycles ',i3,
     &	   ' and ',i3,' so cycles are adjacent, not opposite')
				endif
c				goto 9	!next nc value
				goto 8	!next nc1 value
			   endif
			enddo
c
			found=.false.
			if(joined(is,is1)) then	!state is in cycle nc joined to is1 in nc1
			   if(debug) then
c###				print 72,is,nc
				if(discprt) write(7,72) is,nc
72		      	format(
     &		'         is connected to ',i3,' in cycle ',i3,/,
     &		'         so check rest of connections')
	   		   endif
			   found=.true.
c two cycles (nc, nc1) joined so define them as faces of a putative cube
c (ncube+1 will be overwritten if cube incomplete or faces not opposite)
			   do i=1,4
				cubedef(ncube+1,i)=im(nc,i)
			  	cubedef(ncube+1,4+i)=im(nc1,i)
			   enddo
			   cubecyc(ncube+1,1)=nc
			   cubecyc(ncube+1,2)=nc1
c NB when checking if rest of states connected, must test both directions for
c one of the cycles ('clockwise' and 'anticlockwise')
			   do i=2,4		!check if rest of states connected to form cube
				iss=im(nc,i)	!started from state 1 in cycle #nc
				j=m+i-1
				if(j.gt.4) j=j-4
				iss1=im(nc1,j)	!next state in cycle nc1
				if(.not.joined(iss,iss1)) then
				   found=.false.	!cube incomplete
			 	   if(debug) then
c####					print 74,iss,nc,iss1,nc1
					if(discprt) write(7,74) iss,nc,iss1,nc1
74		      		format(
     &		'          state ',i3,',cycle ',i3,
     &	   			' NOT joined to state ',i3,',cycle ',i3)
	   			   endif
				else
			 	   if(debug) then
c###					print 75,iss,nc,iss1,nc1
					if(discprt) write(7,75) iss,nc,iss1,nc1
75		      		format(
     &		'          state ',i3,',cycle ',i3,
     &	   			' joined to state ',i3,',cycle ',i3)
	   			   endif
				endif
			   enddo
c                   repeat above but going other way round cycle #nc, if nec
			   if(.not.found) then
			     if(debug) then
c###				  print 78,nc
				  if(discprt) write(7,78) nc
78		      	  format(
     &		'         Reverse direction round cycle ',i3)
	   		     endif
			     found=.true.
			     do i=2,4		!check if rest of states connected to form cube
				i1=6-i	!=4,3,2
				iss=im(nc,i1)	!started from state 1 in cycle #nc
				j=m+i-1
				if(j.gt.4) j=j-4
				iss1=im(nc1,j)	!next state in cycle nc1
				if(.not.joined(iss,iss1)) then
				   found=.false.	!cube incomplete
			 	   if(debug) then
c###					print 74,iss,nc,iss1,nc1
					if(discprt) write(7,74) iss,nc,iss1,nc1
				   endif
				else
			 	   if(debug) then
c###					print 75,iss,nc,iss1,nc1
					if(discprt) write(7,75) iss,nc,iss1,nc1
	   			   endif
				endif
			     enddo
			   endif	!end of 'anticlockwise' test if 'clockwise' fails
			else
			   if(debug) then
c###				print 73,is,nc
				if(discprt) write(7,73) is,nc
73		      	format(
     &		'       - NOT connected to ',i3,' in cycle ',i3)
	   		   endif
			endif
			if(found) then	!cube found -check if it duplicates previous cube
			   if(debug) then
c###				print 76,ncube+1,(cubedef(ncube+1,i),i=1,8)
				if(discprt) write(7,76) ncube+1,
     &				(cubedef(ncube+1,i),i=1,8)
76		      	format('  cube ',i3,' found: states = ',8i3)
	   		   endif
c NB 8 states define a cube, regardless of order, so simplest way to check if
c two cubes are the same is to arrange states in ascending order
			   if(ncube.eq.0) then	!ncube not yet incremented
				duplic=.false.
			   else if(ncube.gt.0) then 	!check against previous cubes, if any
				do icube=1,ncube		!all previous confirmed cubes
				   do i=1,8
					istat(i)=cubedef(icube,i)	!prev cycle
					istat1(i)=cubedef(ncube+1,i)	!current putative cycle
				   enddo
c				   subroutine SORTI(IA,n,up,ndim)
				   call SORTi(istat,8,.true.,8)
				   call SORTi(istat1,8,.true.,8)
				   if(debug) then
c###				      print 2,ncube+1,(istat1(i),i=1,8)
				      if(discprt) write(7,2) ncube+1,
     &						(istat1(i),i=1,8)
2				      format(/,' Putative cube ',i3,': ordered states = ',8i3)
c###				      print 21,icube,(istat(i),i=1,8)
				      if(discprt) write(7,21) icube,
     &						(istat(i),i=1,8)
21				      format(/,' Previous cube ',i3,': ordered states = ',8i3)
				   endif
				   duplic=.true.
				   do i=1,8
					if(istat(i).ne.istat1(i)) then
					   duplic=.false.
					endif
				   enddo
				   if(duplic) then
					icb=icube	!record for debug print
					goto 5	!jump out before duplic is reset true
				   endif
				enddo		!end of check or previous cube #icube
			   endif	!end of 'if(ncube.eq.0) then'
5			   continue
			   if(.not.duplic) then
c Now need to check again that NONE of the states in cycle #nc also occur in
c cycle #nc1
				do n=1,4
				   i=im(nc1,n)
				   do n1=1,4
					if(i.eq.im(nc,n1)) then
					   if(debug) then
c###						print 79,i,nc,nc1
						if(discprt) write(7,79) i,nc,nc1
					   endif
c					   goto 9	!next nc value
					   goto 8	!next nc1 value
					endif
				   enddo
				enddo
c If not an opposite face then will have jumped to 9, so not accepted below
			      ncube=ncube+1	!states, and 2 cycles, already recorded
				if(debug) then
c###				   print 77,ncube,(cubedef(ncube,i),i=1,8)
				   if(discprt) write(7,77) ncube,
     &	   		     (cubedef(ncube,i),i=1,8)
77				   format(/,' Cube ',i3,
     &			   ' confirmed: states = ',8i3)
				endif
				if(ncube.gt.ncmax) goto 91	!all found
c cube found so locate the other 4 cycles (apart from #n and #nc1) that form its faces
			  else	!present cube duppicates earlier one so don't increment ncube
				if(debug) then
c###				   print 3,ncube+1,icb
				   if(discprt) write(7,3) ncube+1,icb
3				   format(' Cube ',i3,
     &			  ' duplicates cube ',i3,' so eliminated')
				endif
			  endif
c			else 		!cube not found on this cycle
c===
			endif
		   enddo
		endif
8		continue	!jump here for next nc1
	   enddo	!end of nc1=1,ncyc
9	   continue    !jump here for next nc
	enddo	!end of nc=1,ncyc
c
91	continue
c
c
c Have found two opposite faces (cycles) in each cube at this point.  Now
c add the other four faces (cycles) in cubecyc(icube,3:6)
c Already have 2 cycles (cycle numbers in cubecyc(icube,1:2)) and states
c in cubedef(icube,1:4) and cubedef(icube,5:8)
c Problems arise if opp faces are listed in opp directions (clock/anticlock)
c so best way seems to be to take routes from 1st cycle (1-2; 2-3; 3-4; 4-1)
c and combine with all possible pairs from 2nd cycle (eg if 2nd is 5-6-7-8
c then 5-6, 5-7, 5-8, 6-7, 6-8, 7-8) and then test wich of these combos is
c a valid cycle.  Output should be the 4 other cycles in the cube, returned
c in istat1(1:4)
	do n=1,ncube
	   do i=1,8
		istat(i)=cubedef(n,i)		!1,2
	   enddo
c Check that these really are cycles, and which cycle # they are
	   call CHECKCYC(istat,istat1,8)
	   do i=1,4
		cubecyc(n,2+i)=istat1(i)
	   enddo
c The two cycles in cubecyc(n,1:2) are already opposite faces
c and the two cycles in cubecyc(n,3:4) opposite and
c     the two cycles in cubecyc(n,5:6) opposite
	enddo		!n=1,ncube
c
c Count how many external faces each cube -result in cubext(n), n=1,ncube
c and for each cycle set extcyc(i), i=1,ncyc, =true if cycle #1 is an
c external face, false it it is an internal face.
c Note that any face (cycle) that is shared between two adjacent
c cubes is an internal face, and cycle that occurs in only one cube is an
c external face. For each cube, go through all 8 faces and see how many of
c these faces (cycles) occur in another cube
c Now add extcyc(i)=true if cycle #i is external, false if internal
	do n=1,ncube
	   nint=0
	   do i=1,6
		ic1=cubecyc(n,i)	!does cycle ic1 occur in a different cube?
		extcyc(ic1)=.true.
		do n1=1,ncube
		   if(n1.ne.n) then
			do i1=1,6
			   if(ic1.eq.cubecyc(n1,i1)) then
				nint=nint+1		!shared face so internal
				extcyc(ic1)=.false.
				goto 11
			   endif
			enddo
		   endif
		enddo
11		continue
	   enddo
	   cubext(n)=6-nint	!number of external faces for cube n
	enddo		!n=1,ncube
c
c Re-order the cycles within a cube, so (a) last pair adjacent not opposite
c and (b) where possible, internal faces (cycles) are set before external (see
c notes at top). First move internal cycles to beginning, then if last two
c are not adjacent, swap 4th and 5th
c
	do n=1,ncube
	   do i=1,6
		istat(i)=cubecyc(n,i)	!the 6 cycle numbers for cube n in istat(i)
	   enddo
	   nex=cubext(n)	!number of external faces
	   nin=6-nex	!number of internal faces
c Put the internal ones into into positions 1 to nin
	   m=0
	   do i=1,6
		i1=istat(i)	!cycle number in position i
		if(.not.extcyc(i1)) then
		   m=m+1
		   cubecyc(n,m)=i1
		endif
	   enddo
c###	   if(m.ne.nin) print 70,' Error 1 in FINDCUBE'
c Put the external ones in positions nin+1 to 6
	   m=0
	   do i=1,6
		i1=istat(i)	!cycle number in position i
		if(extcyc(i1)) then
		   m=m+1
		   cubecyc(n,nin+m)=i1
		endif
	   enddo
c###	   if(nin+m.ne.6) print 70,' Error 2 in FINDCUBE'
c Lastly check that last two cycles are adjacent (share states) not opposite
	   nc=cubecyc(n,5)	!5th cycle
	   do j=1,nsc(nc)		!must always be 4 states/cycle
		icyc(j)=im(nc,j)
	   enddo
	   nc1=cubecyc(n,6)	!last cycle
	   do j=1,nsc(nc1)		!must always be 4 states/cycle
		icyc1(j)=im(nc1,j)
	   enddo
c    have icyc and icyc1 any states in common?
	   opp=.true.
	   do i=1,nsc(nc)
		is=icyc(i)
		do j=1,nsc(nc1)
		   if(is.eq.icyc1(j)) opp=.false.	!state in common so not opposite
		enddo
	   enddo
	   if(opp) then
		i=cubecyc(n,4)
		cubecyc(n,4)=cubecyc(n,5)
		cubecyc(n,5)=i
	   endif
	enddo		!n=1,ncube
c
c Now reorder the cubes so those with fewest external faces are first (want
c to set MR in these ones first)
c Need to reorder cubedef(100,8),cubecyc(100,6),cubext(100) according to
c number of external faces in cubext(), used to make pointer array in index().
c	subroutine SORTINDI(IA,index,n,ndim)
c Sorts integer array IA(1)-IA(n) into ascending order, and also outputs
c an array, index, that gives rank order of sort (initially index set
c to 1,2,...,n internally)
c Shellsort method
c	dimension IA(ndim),index(ndim)
	ndim=100
	ALLOCATE(index(ndim),itemp(ndim,8))
	call SORTindi(cubext,index,ncube,ndim)
	do n=1,ncube
	   do i=1,8    	!Copy cubedef to itemp
		itemp(n,i)=cubedef(n,i)
	   enddo
	enddo		!n=1,ncube
	do n=1,ncube
	   n1=index(n)
	   do i=1,8    	!copy back in new order
		cubedef(n,i)=itemp(n1,i)
	   enddo
	enddo		!n=1,ncube
c    repeat for cubecyc
	do n=1,ncube
	   do i=1,6    	!Copy cubecyc to itemp
		itemp(n,i)=cubecyc(n,i)
	   enddo
	enddo		!n=1,ncube
	do n=1,ncube
	   n1=index(n)
	   do i=1,6		!copy back in new order
		cubecyc(n,i)=itemp(n1,i)	!copy back in new order
	   enddo
	enddo		!n=1,ncube
	DEALLOCATE(index,itemp)
c
c	if(debug) then
	   do n=1,ncube
c###	      print 80,n,(cubedef(n,i),i=1,8),(cubecyc(n,i),i=1,6),
c###     &	   cubext(n)
	      if(discprt) write(7,80) n,(cubedef(n,i),i=1,8),
     &		(cubecyc(n,i),i=1,6),cubext(n)
80    	format(/,' OUTPUT: cube ',i3,' = states ',8i3,/,
     &	'    cycles  = ',6i3,':  number of external faces = ',i3,/,
     &	'  cycle #     states (in order)')
		do j=1,6
		   icb=cubecyc(n,j)
c####		   print 82,icb,(im(icb,i),i=1,4)
		   if(discprt) write(7,82) icb,(im(icb,i),i=1,4)
82		   format(4x,i3,5x,4i3)
		enddo
	   enddo
c	endif
c
c
99	continue
	RETURN
	end

	subroutine CHECKCYC(istat,nc,ndim)
c Input istat(1:4) = first cycle that defines a cube, istat(5:8) = opposite
c face of cube
c Output nc(1:4) = the cycle numbers of the 4 other cycles in the cube
c  such that nc(1) and nc(2) are cycle numbers of opposite faces and
c            nc(3) and nc(4) are cycle numbers of opposite faces
c
c Best way seems to be to take routes from 1st cycle (1-2; 2-3; 3-4; 4-1)
c and combine with all possible pairs from 2nd cycle (eg if 2nd is 5-6-7-8
c then 5-6, 5-7, 5-8, 6-7, 6-8, 7-8) and then test which of these combos is
c a valid cycle.
	integer*4 istat(ndim),nc(ndim),icyc(4),icyc1(4)
	logical match,opp
	logical discprt
	common/dp/discprt
c
	integer NSC(50),IM(50,100),JM(50,100)
	COMMON/MPAR/NCYC,NSC,IM,JM
c
	ncf=0		!number of cycles found
c
	do ix=1,4	!the 4 routes in 1st cycle
	   i1=ix+1-1
	   i2=i1+1
	   if(i2.eq.5) i2=1
	   icyc(1)=istat(i1)
	   icyc(2)=istat(i2)
c
	   do j1=1,6		!the 6 pos pairs of states in 2nd cycle
		if(j1.eq.1) then
		   i3=5
		   i4=6
		else if(j1.eq.2) then
		   i3=5
		   i4=7
		else if(j1.eq.3) then
		   i3=5
		   i4=8
		else if(j1.eq.4) then
		   i3=6
		   i4=7
		else if(j1.eq.5) then
		   i3=6
		   i4=8
		else if(j1.eq.6) then
		   i3=7
		   i4=8
		endif
		icyc(1)=istat(i1)		!reset for each j1 as changed by sorting
		icyc(2)=istat(i2)
		icyc(3)=istat(i3)
		icyc(4)=istat(i4)
c	Now test whether icyc contains a valid cycle
		call SORTi(icyc,4,.true.,4)
c Put the 'real' cycles in icyc1

		do n=1,ncyc
		   do j=1,nsc(n)		!must always be 4 states/cycle
			icyc1(j)=im(n,j)
		   enddo
		   call SORTi(icyc1,4,.true.,4)
		   match=.true.
		   do i=1,4
			if(icyc(i).ne.icyc1(i)) then
			   match=.false.
			endif
		   enddo
		   if(match) then
			nc1=n
			goto 1	!jump out before match is reset
		   endif
		enddo		!n=1,ncyc
1		continue
		if(match) then
		   ncf=ncf+1
		   nc(ncf)=nc1
		endif
	   enddo		!do j=1,6
	enddo		!do i=1,4
	if(ncf.ne.4) then
c###	   call BELL(2)
c###	   print 81,ncf
	   if(discprt) write(7,81) ncf
81	   format(' ncf = ',i3,': should be 4 in checkcyc')
	endif
c
c The two cycles in cubecyc(n,1:2) are already opposite faces
c Re-order cycles here so that they are in right order for setting MR,
c ie last two cycles (5 and 6) must be adjacent (e.g. front and right
c in microrev2.mcd, in which satisfactory order of cycles is
c top-bottom-left-back-front-right)
c
c Make the two cycles in cubecyc(n,3:4) opposite and
c	 the two cycles in cubecyc(n,5:6) opposite
c If cycles 3 and 4 (# in nc(1:2)) are NOT opposite (share a state) then swap
c nc(2) and nc(3)
	do j=1,nsc(n)		!must always be 4 states/cycle
	   icyc(j)=im(nc(1),j)
	   icyc1(j)=im(nc(2),j)
	enddo
c    have icyc and icyc1 any states in common?
	opp=.true.
	do i=1,4
	   is=icyc(i)
	   do j=1,4
		if(is.eq.icyc1(j)) opp=.false.	!state in common so not opposite
	   enddo
	enddo
	if(.not.opp) then
	   i=nc(2)
	   nc(2)=nc(3)
	   nc(3)=i
	endif
cc and finally swap 4th and 6th  (nc(2) and nc(4))to ensure that
cc last two are adjacent (nc(2) and nc(4))
c	i=nc(2)
c	nc(2)=nc(4)
c	nc(4)=i
c
	RETURN
	end

