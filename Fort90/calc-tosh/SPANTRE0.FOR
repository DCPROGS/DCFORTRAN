	subroutine SPANTRE0(JCON,Jtree,is1,k,ndim)
c To find the spanning trees for an mechanism specified by IC() connection
c array already defined
c If states i and j are connected then IC(1,m)=i, IC(2,m)=j, m=1,ncon
c IC is IC(2,ncon).
c May be easier to work with JCON(i,j)=0 if i,j not connected, =1 if connected
c where JCON(k,k), k=number of states (symmetric -only lower triangle needed)
c notin(i)=states not yet in the tree, i=1,nit
c istep(1),istep1(i) = states added to tree at the last step, i=1,ist
c===could be integer*1
c	integer*4 IC(2,100),
	integer*4 JCON(ndim,ndim),Jtree(ndim,ndim)
c local
	allocatable::notin,istep,istep1
	integer*1 notin(:),istep(:),istep1(:)
	logical discprt
	common/dp/discprt
c
	allocate(notin(k),istep(k),istep1(k))
c Start in any state, say is1, as starting point
c Initialise, state is1 already in tree
	do i=1,k
	   do j=1,k
		Jtree(i,j)=0
	   enddo
	enddo
c
	j=0
	do i=1,k
	   if(i.ne.is1) then
		j=j+1
		notin(j)=i
	    endif
	enddo
	nit=k-1		!number of states not yet it
	ist=1
	istep(1)=is1	!one state in
c now loop until all in
	do while(nit.gt.0)
c   Check through states not yet in -if any of them is connected to
c		 a state added in last step then add it
	   ist1=0	!number added in following loop
	   in=1	!index for notin()
c	   do i=1,nit
	   do while(in.le.nit)
		do j=1,ist		!states added in last step
		   ni=notin(in)		!next state that is not yet in
		   ji=istep(j)
		   if(jcon(ni,ji).eq.1) then	!ni,ji connected so add ni
			Jtree(ni,ji)=1
			Jtree(ji,ni)=1
			ist1=ist1+1
			istep1(ist1)=ni
			n1=0
			do n=1,nit		!remove state ni from notin()
			   if(notin(n).ne.ni) then
				n1=n1+1
				notin(n1)=notin(n)
			   endif
			enddo
			nit=nit-1
			if(nit.eq.0) goto 99
		   endif	!end of addition of state ni to tree
		enddo		!j=1,ist
c  increment in here
		in=in+1
	   enddo	!i=1,nit
c     prepare for next loop
	   ist=ist1		!number added in last loop
	   do i=1,ist1
		istep(i)=istep1(i)
	   enddo
	enddo		!end of do while()
c
99	deallocate(notin,istep,istep1)
	return
	end
