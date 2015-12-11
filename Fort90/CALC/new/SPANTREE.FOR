	subroutine SPANTREE(JCON,Jtree,is1,k,ncon,ICspec,nc,ndim)

c To find the spanning trees for an mechanism specified by IC() connection
c array already defined
c The routes that must be in the tree, nc in number, are specified as input
c in the form of an an IC(1:2,nm) matrix, ICspec -on output this comtains
c IC() that defines whole tree (as does JCON).
c If nc=0 then a spanning tree is found starting the search from state #is1
c (is1 ignored if nc>0)
c
c NB a spanning tree must contain all k states, and having no loops it must
c therefore have k-1 connections.
c In SPANTREE it was simple to count the states that were 'not yet in' the tree,
c and use the fact that all k states are in to indicate that the tree is
c complete. But when routes are pre-specified, they could involve all 8 states
c yet not be connected to form a tree. Therefore it seems better to count
c connections rather than states -when k-1 connections have been specified
c the tree is complete
c
c If no links pre-specified then can start from any state (#is1 say), but if
c connections are pre-specified, then to join up the preseocified connections
c to form a complete spanning tree, the search must start from any of the
c states in the pre-specified conenctions, using as many of them as is
c necessary to get a complete tree (k-1 connections)
c
c If states i and j are connected then IC(1,m)=i, IC(2,m)=j, m=1,ncon
c IC is IC(2,ncon).
c May be easier to work with JCON(i,j)=0 if i,j not connected, =1 if connected
c where JCON(k,k), k=number of states (symmetric -only lower triangle needed)
c notin(i)=states not yet in the tree, i=1,nit
c istep(1),istep1(i) = states added to tree at the last step, i=1,ist
c
c Modif 10/21/03 08:03am: ncon added to parameters (though could be found
c  internally from entries in JCON). And icadd added to SRCHTREE.
c===could be integer*1
	integer*4 ICspec(2,100) !input routes to be in tree
	integer*4 JCON(ndim,ndim),Jtree(ndim,ndim)
c local
c	allocatable::notin,istep,istep1
c	integer*4 notin(:),istep(:),istep1(:)
	allocatable::istart,iend,itemp
	integer*4 istart(:),iend(:),itemp(:)
	allocatable::istree,nstree,iadd
	integer istree(:,:),nstree(:),iadd(:)
	allocatable::icadd
	integer icadd(:,:)	!for srchtree
	logical join,debug,caplock,dup
	logical discprt
	common/dp/discprt
c
	debug()=caplock()
c
c	allocate(notin(k),istep(k),istep1(k))
	allocate(istart(2*k),iend(k),itemp(k))
	allocate(istree(k,2*k),nstree(k),iadd(k))
	allocate(icadd(2,ncon))
c
c Start in any state, say is1, as starting point
c Start looking from any state in the specified conditions -if none specified
c then start from state is1
c NB may need two stage process
c (1) Connect up the prespecified routes (without making any cycles)
c (2) if there are still not k-1 connections then add the rest exactly as in
c  the simple case, but with start state that can be any of those in the
c  part chain that exists up to this point

c List all the states in the pre-specified connections -these are all potential
c starting points in search for rest of the connections.  Also need to know
c how many are already joined: number of subtrees specified <= nc.
c Identify the number and make up of the specified subtrees -the number will
c be <nc if some of the specified routes are connected to each other.  For
c each specified route, see if it is connected to any of the other specified
c routes to form a subtree
c
c Define a subtree in istree(k,2*k)
c istree(n,is) where is=1,2 are the states in the first link in
c subtree #n, is=3,4 are states in 2nd link, is=2*(j-1)+1 or +2 are states
c in jth link (n=1,...,nst where nst=number of subtrees).
c

	ndim=100	!dimension of jcon,jtree
	do i=1,k
	   do j=1,k
		Jtree(i,j)=0
	   enddo
	enddo
c
	if(nc.eq.0) goto 100
c
	nadd=0	!number of predef links that are concatenated with others
			!to make a subtree
c start istree(1) with the first specified link
	ist=1		!first subtree, with one link
	nst=1		!number of subtrees defined
	nstree(1)=2		!number of STATES in subtree #n
	is1=ICspec(1,1)	!the 2 states in link n1
	is2=ICspec(2,1)
	istree(1,1)=is1
	istree(1,2)=is2
	Jtree(is1,is2)=1
	Jtree(is2,is1)=1
	nadd=1	!number of predef links that are concatenated with others
			!to make a subtree
	iadd(1)=1	!first link added to a subtree (istree(1))
c
c Any other links that have a state in common with any in current istree(nst)
c are next added to istree(nst) without incrementing nst (and these links marked
c as 'added')
	do n=1,nc
	   do i=1,nadd
		if(n.eq.iadd(i)) goto 9	!skip to next n
	   enddo
	   is1=ICspec(1,n)	!the 2 states in link n
	   is2=ICspec(2,n)
c are either is2 or is1 present in the current istree(nst)?
c	   join=.false.
c	   do n1=1,nstree(nst)
c		is3=istree(nst,n1)
c		if(is1.eq.is3.or.is2.eq.is3) then	!join is1,is2 to istree(nst)
c		   join=.true.
c		endif
c	   enddo
c Above is OK if individual steps are specified in 'joined-up' order, e.g.
c 1,2; 2,3; 3,4; 7,8 so first 6 go in istree(1), last two in istree(2), but if
c for example they are given as  1,2; 2,3; 7,8; 3,4 then must be able to tell
c that the last one, 3,4, should be attached to istree(1), so need to look
c at all istree defined so far to determine which one (which nst) the next
c link is joined to. If none, then start a new istree()
	   join=.false.
	   do m=1,nst	!look at all trees started so far
		do n1=1,nstree(m)
		   is3=istree(m,n1)
		   if(is1.eq.is3.or.is2.eq.is3) then	!join is1,is2 to istree(nst)
			join=.true.
			ist1=m	!record which tree current link is joined to
		   endif
		enddo
	   enddo
	   if(join) then
		ist=ist1
		nstree(ist)=nstree(ist)+2	!total number of states (some duplicated)
		m=nstree(ist)/2		!total number of connections
		j=2*(m-1)+1
		j1=j+1
		istree(ist,j)=is1
		istree(ist,j1)=is2
		Jtree(is1,is2)=1
		Jtree(is2,is1)=1
c     record that link n has been added to a subtree so that it isn't
c     used again
		nadd=nadd+1
		iadd(nadd)=n
	   else
		nst=nst+1		!go on to next subtree, with one link
		nstree(nst)=2		!number of STATES in subtree #n
		j=1
		j1=2
		istree(nst,j)=is1
		istree(nst,j1)=is2
		Jtree(is1,is2)=1
		Jtree(is2,is1)=1
	   endif
9	   continue 	!next n
	enddo
c
	if(debug()) then
	   print 21,nst
	   if(discprt) write(8,21) nst
21	   format(/,' Initially specified ''forest'' has ',i3,
     &	' subtrees (components)')
	   do n=1,nst
		print 22,n,(istree(n,i),i=1,nstree(n))
		if(discprt) write(8,22) n,(istree(n,i),i=1,nstree(n))
22	   	format(' ',i4,':   ',20i3)
	   enddo
	endif
c
c
c Now have nst subtrees that are not connected to each other.  Next join
c them up by finding nst-1 links.
c For NCONS to work, JTREE must contain a connected graph (at this point
c it contains all the disconnected subtrees) so zero again and add only
c the first subtree.
	do i=1,k
	   do j=1,k
		Jtree(i,j)=0
	   enddo
	enddo
	do n=1,nstree(1),2
	   i=istree(1,n)
	   j=istree(1,n+1)
	   jtree(i,j)=1
	   jtree(j,i)=1
	enddo
c
	ns=0		!number of states in istart (and so not in notin())
	nadd=0	!number of connections added by call to SRCHTREE
	do is=1,nst-1
c start from any state in subtree #is
c stop when any state in subtree #is+1 reached
c The states in subtree #is are already 'in' the tree. All others, including
c those in subtree #is+1, must be counted as 'notin'
c After first subtree has been added, istart for the next should have states
c from first time, as well.
c
	   nslast=ns
	   ns=ns+nstree(is)		!number of states in subtree #is
	   do n=1,nstree(is)
		istart(n+nslast)=istree(is,n)	!add to previous istart values
	   enddo
c  add to istart() any extra states introduced by last call to SRCHTREE (if
c  a direct link was added between subtree is and subtree is+1 then no new
c  states will be added, but duplicate states removed in REMDUP
	   do n=1,nadd
		ns=ns+1
		istart(ns)=icadd(1,n)
		ns=ns+1
		istart(ns)=icadd(2,n)
	   enddo
	   call REMDUP(istart,ns,ns,2*k)	!remove duplicate entries eg (2,3;4,3)->(2,3,4)
c
	   ne=nstree(is+1)		!number of states in subtree #is+1
	   do n=1,ne
		iend(n)=istree(is+1,n)
	   enddo
	   call REMDUP(iend,ne,ne,k)	!remove duplicate entries
c If any of the iend states are in istart, they should be removed from istart()
c or they will be excluded as end states
	   nstemp=0
	   do m=1,ns
		i1=istart(m)
		dup=.false.
		do n=1,ne
		   if(i1.eq.iend(n)) dup=.true.	!istart(m) occurs in iend()
		enddo
		if(.not.dup) then
		   nstemp=nstemp+1
		   itemp(nstemp)=istart(m)
		endif
	   enddo
	   ns=nstemp
	   do n=1,ns
		istart(n)=itemp(n)
	   enddo
c
c check states in tree already
	   if(ns.eq.k) then
		if(debug()) then
		   print 18,k
		   if(discprt) write(8,18) k
18		   format(/,' All ',i3,' states are in tree, so finished')
		endif
		goto 101
	   endif
c check connections in tree already
	   call NCONS(Jtree,k,ncont,nstate,ndim)
	   if(ncont.eq.k-1.and.nstate.eq.k) then
		if(debug()) then
		   print 19,ncont,nstate
		   if(discprt) write(8,19) ncont,nstate
19		   format(/,
     &		' Tree has ',i3,' connections and ',i3,
     &		' connections, so finished')
		endif
		goto 101
	   endif
	   if(debug()) then
		print 14,(istart(n),n=1,ns)
		if(discprt) write(8,14) (istart(n),n=1,ns)
14		format(/,' Start states, already ''in'':',20i3)
		print 17,is+1,(iend(n),n=1,ne)
		if(discprt) write(8,17) is+1,(iend(n),n=1,ne)
17		format(/,' Add subtree ',i3,'= states:',20i3)
	   endif
	   iconv=1
	   ndim=100
c debug printing
	   if(debug()) then
		print 11,is+1
		if(discprt) write(8,11) is+1
11		format(/,' Before adding subtree ',i3)
		do i=1,k
		   print 1,(jtree(i,j),j=1,k)
	 	   if(discprt) write(8,1) (jtree(i,j),j=1,k)
1		   format(20i3)
		enddo
	   endif
c
	   call SRCHTREE(JCON,Jtree,istart,ns,iconv,iend,ne,
     &	icadd,nadd,ncon,k,ndim)
c
	   if(debug()) then
		print 15,nadd
		if(discprt) write(8,15) nadd
15		format(/,' ',i3,' Connections added')
		do n=1,nadd
		   print 16,icadd(1,n),icadd(2,n)
		   if(discprt) write(8,16) icadd(1,n),icadd(2,n)
16		   format(' ',i3,' -',i3)
		enddo
		print 12,is+1
		if(discprt) write(8,12) is+1
12		format(' After adding subtree ',i3)
		do i=1,k
		   print 1,(jtree(i,j),j=1,k)
	 	   if(discprt) write(8,1) (jtree(i,j),j=1,k)
c1		   format(20i3)
		enddo
	   endif
	enddo
c
c Subtrees now joined to make a single partial tree -if this tree contains
c fewer than k states, then finish off the spanning tree, starting at any
c of the points in the existing tree and continuing until all states are in
c so iconv=0
101	continue
	ns=0
	do i=1,k
	   join=.false.
	   do j=1,k
		if(jtree(i,j).eq.1) join=.true.	!state i joined to one or more others
	   enddo
	   if(join) then
		ns=ns+1
		istart(ns)=i		!partial tree contains state #i
	   endif
	enddo
	if(ns.lt.k) then	!complete the tree
	   iconv=0		!so iend(), ne are ignored
	   call SRCHTREE(JCON,Jtree,istart,ns,iconv,iend,ne,
     &	icadd,nadd,ncon,k,ndim)
	endif
c
	if(debug()) then
	   print 13
	   if(discprt) write(8,13)
13	   format(/,' Complete tree')
	   do i=1,k
		print 1,(jtree(i,j),j=1,k)
		if(discprt) write(8,1) (jtree(i,j),j=1,k)
c1		format(20i3)
	   enddo
	endif
	goto 99
c Now the part done if nc=0, not links specified to be in tree
100	continue

c Initialise, state is1 already in tree
	iconv=0
	ns=1
	istart(1)=is1
	call SRCHTREE(JCON,Jtree,istart,ns,iconv,iend,ne,
     &	icadd,nadd,ncon,k,ndim)
c
c	do i=1,k
c	   do j=1,k
c		Jtree(i,j)=0
c	   enddo
c	enddo
cc
c	j=0
c	do i=1,k
c	   if(i.ne.is1) then
c		j=j+1
c		notin(j)=i
c	    endif
c	enddo
c	nit=k-1		!number of states not yet it
c	ncont=0		!number of connections in tree
c	ist=1
c	istep(1)=is1	!one state in
cc now loop until all in
c	do while(nit.gt.0)
cc   Check through states not yet in -if any of them is connected to
cc		 a state added in last step then add it
c	   ist1=0	!number added in following loop
c	   in=1	!index for notin()
cc	   do i=1,nit
c	   do while(in.le.nit)
c		do j=1,ist		!states added in last step
c		   ni=notin(in)		!next state that is not yet in
c		   ji=istep(j)
c		   if(jcon(ni,ji).eq.1) then	!ni,ji connected so add ni
c			Jtree(ni,ji)=1
c			Jtree(ji,ni)=1
c			ncont=ncont+1	!one connection added
c			ist1=ist1+1
c			istep1(ist1)=ni
c			n1=0
c			do n=1,nit		!remove state ni from notin()
c			   if(notin(n).ne.ni) then
c				n1=n1+1
c				notin(n1)=notin(n)
c			   endif
c			enddo
c			nit=nit-1
c			if(nit.eq.0) goto 99
c		   endif	!end of addition of state ni to tree
c		enddo		!j=1,ist
cc  increment in here
c		in=in+1
c	   enddo	!i=1,nit
cc     prepare for next loop
c	   ist=ist1		!number added in last loop
c	   do i=1,ist1
c		istep(i)=istep1(i)
c	   enddo
c	enddo		!end of do while()
c
99	continue
c	deallocate(notin,istep,istep1)
	deallocate(istart,iend,itemp)
	deallocate(istree,nstree,iadd)
	deallocate(icadd)
	return
	end

	subroutine SRCHTREE(JCON,Jtree,istart,ns,iconv,iend,ne,
     &	icadd,nadd,ncon,k,ndim)
c To find whole or part of spanning tree
c INPUT
c JCON(i,j)
c istart(1:ns) =ns starting states, all of which are tried.  In simplest
c    case ns=1 and istart(1)=is1
c iconv=0 to find complete tree
c iconv=1 to stop as soon as any state in iend(1:ne) is included (eg to join
c   subtrees)
c iend(1:ne) =state numbers of ne states
c
c INTERNAL
c notin(1:nit) = states that are not yet in the tree in the sense that only
c			the states in notin() are looked at when extending the tree
c                 This is set internally to include initially all states
c			except the current start state
c
c OUTPUT (added 10/21/03 04:37am)
c icadd(1:2,1:nadd), The connections that are added to the tree in current call
c   to subroutine (having these need to be added to istart for subsequent
c   additions so they are not revisited, forming loops
c
c Note that if a partial tree is being completed, there may be no connection
c between a particular start state, and a state in notin(i) so can get
c infinite loop.  So if no states added after ncon steps give up and try a
c different start state (nsteps=number of steps attempted)
c
c Modif 10/23/03 06:38am so that nothing is added to JTREE during loop for
c each start state, but number of steps needed for each start state recorded
c and the one with fewest steps used. Add an extra dimansion to arrays istep etc
c so values can be kept for each starting state, before deciding which one to
c add to jtree
c
	integer*4 JCON(ndim,ndim),Jtree(ndim,ndim)
	integer*4 istart(2*k),iend(k)
	integer*4 icadd(2,ncon),nadd		!arguments
	allocatable::icad,nad			!internal
	integer*4 icad(:,:,:),nad(:)
	allocatable::istep,istep1,nsteps
	integer*4 istep(:,:),istep1(:,:),nsteps(:)

	allocatable::notin,notins
	integer*4 notin(:),notins(:)
	logical statin
	logical debug,caplock
	logical discprt
	common/dp/discprt
c
	debug()=caplock()
c
	allocate(notin(k),notins(k))
	allocate(istep(k,ncon),istep1(k,ncon),nsteps(ncon),
     &  icad(2,ncon,ncon),nad(ncon))
c
c notin(i) should include all states that are not in istart(1:is)
c -no need to set notin on entry?
	j=0			!notin(1:nit) set in call
	do i=1,k
	   statin=.false.
	   do ii=1,ns
		is1=istart(ii)		!current start state for search
		if(i.eq.is1) statin=.true.	!state #i is in istart
	   enddo
	   if(.not.statin) then
		j=j+1
		notins(j)=i		!save so can be restored for each stating state
	   endif
	enddo
	nits=j		!number of states not yet it
c
c Loop to try each start state in turn
	do ii=1,ns
	   is1=istart(ii)		!current start state for search
c restore notin()
	   nit=nits
	   do i=1,nit
		notin(i)=notins(i)
	   enddo
	   nad(is1)=0		!number of connections added here
	   ist=1
	   istep(1,is1)=is1	!one state in
	   nsteps(is1)=0		!counts no of steps when starting from state is1
c nsteps=number of states away from start state that have been tested
c
c Search using current starting state
	   do while(nit.gt.0)
c   Check through states not yet in -if any of them is connected to
c		 a state added in last step then add it
		ist1=0	!number added in following loop
		in=1	!index for notin()
c		do i=1,nit
		do while(in.le.nit)
		   do j=1,ist		!states added in last step
			ni=notin(in)		!next state that is not yet in
			ji=istep(j,is1)
			if(jcon(ni,ji).eq.1) then	!ni,ji connected so add ni
c			   Jtree(ni,ji)=1
c			   Jtree(ji,ni)=1
c test whether this addition produces a cycle -if so reverse it
c Add to jtree only when shortest route found -can't use ncons until jtree
c completed
c			   call NCONS(Jtree,k,ncon1,nstate,ndim)
c			   if(ncon1.gt.nstate-1) then
c				Jtree(ni,ji)=0	!reverse
c				Jtree(ji,ni)=0	!reverse
c				if(debug()) then
c				   print 20,ni,ji
c				   if(discprt) write(8,20) ni,ji
c20				   format(/,' Addition of ',i3,',',i3,
c     &			    ' would make a cycle so rejected')
c				endif
c				goto 91
c			   endif
			   nad(is1)=nad(is1)+1		!one connection added
			   icad(1,nad(is1),is1)=ni	!record which connections are added
			   icad(2,nad(is1),is1)=ji	!record which connections are added
			   ist1=ist1+1
			   istep1(ist1,is1)=ni
			   n1=0
			   do n=1,nit		!remove state ni from notin()
				if(notin(n).ne.ni) then
				   n1=n1+1
				   notin(n1)=notin(n)
				endif
			   enddo
			   nit=nit-1
			   if(iconv.eq.0) then		!end when all added
c				if(nit.eq.0) goto 99	!finished
				if(nit.eq.0) goto 98	!next start state
			   else if(iconv.eq.1) then	!end when a state in iend is added
				do n=1,ne
c				   if(ni.eq.iend(n)) goto 99	!finish
				   if(ni.eq.iend(n)) goto 98	!next start state
				enddo
			   endif
			endif	!end of addition of state ni to tree
91			continue
		   enddo		!j=1,ist
c  increment in here
		   in=in+1
	      enddo	!i=1,nit
c     prepare for next loop
		nsteps(is1)=nsteps(is1)+1
c test ALL start states
c		if(nsteps(is1).gt.ncon) go to 98	!try another start state
		ist=ist1		!number added in last loop
		do i=1,ist1
		   istep(i,is1)=istep1(i,is1)
		enddo
	   enddo		!end of do while()
98	   continue		!try next start state
	enddo 	!end of loop for each start state do ii=1,ns
99	continue
c Now take the shortest route from the start state that gets to end in the
c fewest steps and put connections into JTREE (including destination states
c in IEND)
	min=1000
	do ii=1,ns
	   if(nsteps(ii).lt.min) min=ii
	enddo
c shortest route is that from start state is1=istart(min), so add this route
c to jtree
	is=istart(min)
	nadd=nad(is)
	do j=1,nadd
	   icadd(1,j)=icad(1,j,is)	!copy for output
	   icadd(2,j)=icad(2,j,is)
	enddo
	do ii=1,nsteps(min)
	   i=icadd(1,nadd)
	   j=icadd(2,nadd)
	   jtree(i,j)=1
	   jtree(j,i)=1
	enddo
c and add the end states to jtree if iconv=1, eg if joining to iend=6,7,8
c need to add 6-7 and 7-8
	if(iconv.eq.1) then
	   j=1
	   do i=1,ne/2
		i1=iend(j)
		i2=iend(j+1)
		jtree(i1,i2)=1
		jtree(i2,i1)=1
		j=j+1
	   enddo
	endif
c
	deallocate(notin,notins)
	deallocate(istep,istep1,nsteps,icad,nad)
	RETURN
	end

	subroutine REMDUP(iarray,n,nout,ndim)
c To take an array of n integers and remove duplicate entries from it
c and return the reduced n
	integer iarray(ndim),iout(100)
	logical dup
c
	nout=n
	n1=0
	do i=1,n
	   is=iarray(i)
	   dup=.false.
	   if(n1.ge.1) then
		do i1=1,n1	!check all prev for duplicates
		   if(is.eq.iout(i1)) then
			dup=.true.
			nout=nout-1
		   endif
		enddo
	   endif
	   if(.not.dup) then
		n1=n1+1
		iout(n1)=is
	   endif
	enddo
	if(n1.ne.nout) then
	   pause 'error in remdup'
	endif
	do i=1,nout
	   iarray(i)=iout(i)
	enddo
	return
	end

	subroutine NCONS(Jcon,k,ncon,nstate,ndim)
c counts the number of connections in any Jcon type matrix by counting
c the number of 1s in lower triangular part of it
c And the number of states in the tree=number of rows (or cols) that
c contain a non-zero entry
	integer Jcon(ndim,ndim)
	logical sflag
c
	ncon=0
	do i=2,k	!check lower triangle only
	   do j=1,i-1
	     if(jcon(i,j).eq.1) ncon=ncon+1
	   enddo
	enddo
	nstate=0
	do i=1,k
	   sflag=.false.
	   do j=1,k
		if(jcon(i,j).eq.1) sflag=.true.	!at least one non-zero in row i
	   enddo
	   if(sflag) nstate=nstate+1
	enddo
	RETURN
	end

