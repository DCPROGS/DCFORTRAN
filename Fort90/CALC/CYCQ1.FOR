	subroutine CYCQ1(k,ncon,ic,ncyc,nsc0,im,jm,ncmax)
c To get cycles automatically from connections IC() as found in CHARQ
c
c CYCQ1 is modified version that has no printout, and max number in
c a cycle as a parameter (ncmax)
c
c Corrected 02/03/93 08:36am because this algorithm finds a cycle with
c k=2 states! -now skips search when k=2 (see '==')
c
	INTEGER IC(2,200)		!for connections
	integer IM(50,100),JM(50,100)	!for cycles (final output)
	character*1 ans
c make arrays for cycles bigger, NSC particularly, so can collect many
c cycles before rejecting the repeated ones; make ICYC 2-D internally
c to store several cycles
	integer nsc0(50)			!for output of nsc()
c Some models have more than 50 cycles during search so make internal arrays
c bigger
	allocatable::nsc,icyc,icycsav,nsi,ns,nck,nc
	allocatable::last,nod
	integer NSC(:),icyc(:),icycsav(:,:)
	integer nsi(:),ns(:,:),nck(:),nc(:,:)	!for search for cycles
	integer last(:),nod(:)
	logical caplock,debug,same
c
	debug()=caplock()
c To get cycles automatically from connections IC() as found in CHARQ
c Now find CYCLES
c Define cycles (if ncyc>0 then define NSC,ICYC,IM,JM here
c (but ask which steps to be calc by micro rev in QDEF1)
c Example from QMOD1
c 1st cycle
c	ncyc=3
c	nsc(1)=4	!4 states in cycle 1
cc	icyc(1)=2	!i first route= 2,5 to be calc by micro rev
cc	icyc(2)=5	!j
cc	icyc(3)=6	!i
cc	icyc(4)=3	!j
c	im(1,1)=2
c	jm(1,1)=5
c	im(1,2)=5
c	jm(1,2)=6
c	im(1,3)=6
c	jm(1,3)=3
c	im(1,4)=3
c	jm(1,4)=2
c States with 1 connection cannot be in a cycle
c States with 2 connections may be in 0 or 1 cycles
c States with n connections may be in 0,1,...n-1 cycles
c (actually states with 2 con could be part of both a large cycle and
c a smaller one nested within it-but larger one is superfluous)
c Allocate arrays
c	n0=500
	n0=1000
	n2=1000
	n3=1000
	ALLOCATE(NSC(n0),icyc(n0),icycsav(n0,n0))
	ALLOCATE(nsi(n0),ns(n2,n0),nck(n2),nc(n2,n0))	!for search for cycles
	ALLOCATE(last(n3),nod(n3))
c	ALLOCATE(nsc1(n2),ind(n2),melim(n2),mcyc(n2))
c
c First find number of connections for each state, and what states these
c connections are to.
	do 34 i=1,k		!go through all states
	nck(i)=0		!number of connections for state i=1,2,...,k
	do 35 m=1,ncon
	if(ic(1,m).eq.i) then
	   nck(i)=nck(i)+1	!no of con for ith state
	   nc(i,nck(i))=ic(2,m)	!the 1,2,..nck states to which state i is connected
	else if(ic(2,m).eq.i) then
	   nck(i)=nck(i)+1	!no of con for ith state
	   nc(i,nck(i))=ic(1,m)	!the 1,2,..nck states to which state i is connected
	endif
35	continue
c	print 300,i,nck(i),(nc(i,j),j=1,nck(i))
c300	format(' State # ',i5,' has ',i5,' connections to states:',/,10i5)
34	continue
c States with only one con can be skipped- look for the first state with
c 2 con and look for cycle starting at that state. Record now the number
c of states (and which states they are) that have 1,2,3,4 connections
c NB in middle of a cube, could have up to 6 connections
c	do m=1,4
	do m=1,10
	   nsi(m)=0
	   do i=1,k
		if(nck(i).eq.m) then
		   nsi(m)=nsi(m)+1	!# of states with m=1,2,3,4 connections
		   ns(nsi(m),m)=i		!the 1,2,..nsi(m) states with m connections
		endif
	   enddo
c	   if(nsi(m).gt.0) then
c		print 301,nsi(m),m,(ns(i,m),i=1,nsi(m))
c301		format(1x,i3,' states have ',i5,' connections, viz states:',
c     &	/,10i5,/,10i5,/,10i5)
c	   endif
	enddo
c
c
c Now start search for cycles. Start with states with 2 con (if any)
c State # ks is connected to states nc(ks,j) for j=1,2 (nck(ks)=2 connections)
c Go through each state in turn; for 2-con states there is only one way
c out (since way in is excluded) so no choice. For n-con states, n>2, there
c are n-1 (=2,3,..) ways out so have a choice=node. Search through states ends
c when (a) a state already in the sequence (other than the initial one)
c		 is encountered
c or	 (b) a dead-end (a 1-con state) is encountered
c or	 (c) a cycle is successfully completed
c When any of these occurs, go back to the node that was last encountered
c and choose the next way out of it- if all ways out of it have been explored
c then go back to previous node, and so on until all routes explored
c Keep a pututive cycle in icyc(20); record it permanently in IM,JM if
c it is successfully completed
	ncyc=0
	if(k.eq.2) goto 91		!no cycles possible==
c Make more general by doing 2-con,3-con and 4-con states all in same loop
c Say jc=number of connections
c
c	do 400 jc=2,4		!outer loop
	do jc=2,10		!outer loop
	   if(nsi(jc).eq.0) goto 400       !no jc-con states -try next
cc section for case where 1st state is a 2-con state
c	   do 40 i1=1,nsi(jc)	!go through the jc-con start states
	   do i1=1,nsi(jc)	!go through the jc-con start states
		ks=ns(i1,jc)		!state # to start search for cycle
c		print 325,ks
c325		format(' Starting search at state #',i5)
c		pause
c first state is different from others in that it is a node even when
c it is a 2-con state
		k1=ks	!current state that we are searching from   [eg ks=1]
c State k1 is connected to nck(k1) states, viz states # nc(k1,1),
c nc(k1,2),...,nc(k1,nck(k1)) (2 states the 1st time when k1=ks)
		do nd=1,n3
		   last(nd)=0	!=last exit route tried for node #nd
		enddo
		nd=0        !starting state is first node (updated below)
		mc=1
		icyc(mc)=ks		!=first state in putative cycle
		nflag=0		!so next node located is counted as a new one
c
41		continue	!return here to continue search
c		if(nd.ge.1) then
c		   print 310,mc,mw,k1,nd,nod(nd),last(nd),klast,(icyc(i2),i2=1,mc)
c310		   format(' After 41: mc,mw,k1,nd,nod(nd),last(nd),klast = ',/,7i5,/,
c 	    &    ' ICYC= ',20i5)
c 		endif
c
		nout=nck(k1)		!# of ways out of current state
		if(k1.ne.ks) nout=nout-1	!exclude arrival route
		if(nout.eq.0) then	!dead end
c 	   Code for when state k1 is a dead end
c		   print 313,mc,mw,k1,nd,nod(nd),last(nd),klast,(icyc(i2),i2=1,mc)
c313 		  format(' Dead end: mc,mw,k1,nd,nod(nd),last(nd),klast = ',/,7i5,
c	     & /,' ICYC= ',20i5)
		   goto 44	!dead end: return to prev node
		else if(nout.eq.1) then
c	Only one way out- use it
c	Should never arrive here when at initial state (k1=ks)
		   klast=icyc(mc-1)
		   mc=mc+1		!next element if ICYC; =2,3,...
		   icyc(mc)=nc(k1,1)	!1st way out=next state in putative cycle
		   if(icyc(mc).eq.klast) icyc(mc)=nc(k1,2)	!use other one
c		   print 321,k1,mc,icyc(mc),klast
c321		   format(' One way out: k1,mc,icyc(mc),klast = ',4i5)
		else
c     If nout>1 then current state is a node, so check which routes out
c     of it have already been used, and go to next unused one (if any)
c     (will get here while at 1st state even if it has 2-con)
c	Count this node as a new one if it is reached while searching
c	forward (nflag=0), but not if it has been returned to.
		   if(nflag.eq.0) then
		      nd=nd+1		!count number of nodes encountered
		      nod(nd)=mc		!index in icyc where node is
		   endif
		   nflag=0			!will be reset to 1 if returned to again
c		   klast=icyc(mc)		!mc not yet updated
		   if(mc.gt.1) then
			klast=icyc(mc-1)		!mc not yet updated
		   else
			klast=0
		   endif
c		   print 322,nd,nod(nd),last(nd),klast
c322		   format(' Node found: nd,nod(nd),last(nd),icyc(mc) = ',4i5)
		   if(nck(k1).gt.last(nd)) then
		      do m1=last(nd)+1,nck(k1)	!check all ways out not yet used
			   mw=m1
			   inext=nc(k1,m1)
c			   print 323,m1,k1,inext,klast
c323			   format(' Look for unused exit: m1,k1,inext,klast= ',4i5)
			   if(inext.ne.klast) goto 43 !unused way out (not=entry route) found
			enddo
		   endif
c        If get here, all ways out used at node #nd, so go back to next
c earliest node (or, if already at first node=starting state, then finish)
		   nd=nd-1
		   do i2=nd+1,n3
			last(i2)=0	!reset all exits for all later nodes usable again
		   enddo
c		   if(nd.eq.0) goto 90	!finish
		   if(nd.eq.0) goto 40	!try next starting state
c	         print 311,mc,mw,k1,nd,nod(nd),last(nd),klast,(icyc(i2),i2=1,mc)
c311 		   format(' #2: mc,mw,k1,nd,nod(nd),last(nd),klast = ',/,7i5,/,
c		     & ' ICYC= ',20i5)
		   goto 44	!back to next prev node
c
43	  	   continue
c		   print 324,mw,inext
c324	         format(' exit #',i4,' found, to state #',i4)
		   mc=mc+1
		   icyc(mc)=inext
		   last(nd)=mw		!record last exit used from node #nd
		endif
c Test whether this route ended, via repeat, or finding good cycle
c check if state,other than first, has been repeated
		if(mc.ge.3) then
c		   print 314,mc,mw,k1,nd,nod(nd),last(nd),klast,(icyc(i2),i2=1,mc)
c314 		   format(' At 45: mc,mw,k1,nd,nod(nd),last(nd),klast = ',/,7i5,/,
c 		    & ' ICYC= ',20i5)
		   do i2=2,mc-1
			if(icyc(i2).eq.icyc(mc)) goto 44	!repeat: return to prev node
		   enddo
c version to include debug statement
c		   do 45 i2=2,mc-1
c		   if(icyc(i2).ne.icyc(mc)) goto 45
c 		   print 317
c317		   format(' End with repeated state')
c 		   goto 44		!repeat: return to prev node
c45		   continue
		endif
c check if cycle found
		if(icyc(mc).eq.ks) then
c   yes- cycle found so record it in im,jm (and also in ICYCSAV for now)
c Check if the cycle just found is same as one already identified by
c (1) check if it contains same state as an existing one
c (2) if so rotate so this state is 1st element
c (3) compare with existing one- if same, discard
c (4) After (1) reverse order of elements and then do (2),(3),(4)
c
		   call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
		   if(same) goto 44	!same as existing cycle:return to last node
c
c keep only non-duplicated ones (if ncyc=0 then SAME set false so 1st one
c always kept in icycsav)
c Modified 07/03/03 07:44pm so that cycles with more than ncmax are rejected at
c this stage, not stored
		   if(mc-1.le.ncmax) then
			if(ncyc+1.eq.n0) goto 90	!n0=array size
			ncyc=ncyc+1
			do j1=1,mc
			   icycsav(ncyc,j1)=icyc(j1)
			enddo
			nsc(ncyc)=mc-1
c			print 318,(icyc(i2),i2=1,mc)
c318			format(' Cycle found: ICYC=',20i3)
c 			pause
			goto 44			!return to last node
		   endif
		endif
c If arrive here then search not ended, so carry on
		k1=icyc(mc)		!start point for next search
c		print 315
c315		format(' Carry on search')
		goto 41
c
c Section at 44 done to return to most recent node
44		continue
c		nd=nd-1		!NO- nd=last node #
		mc=nod(nd)		!index in icyc of node
		k1=icyc(mc)		!new start state for search
		nflag=1		!so this not counted as a new node
c 		print 316,nd,mc,nod(nd),k1
c316		format(' Return to last node: nd,mc,nod(nd),k1 = ',4i5)
		goto 41			!continue search
c
40		continue		!end of loop for jc-con states
	   enddo
400	   continue		!loop for jc=2,3,4,...,10
	enddo
c
c
90	continue
c Finally, eliminate any superfluous cycles. If a 2-con state occurs
c in more than one cycle, then take only the smaller cycle.
c In general if any n-con state occurs in more than n-1 cycles then
c take only the n-1 smallest cycles.
c Modified 07/02/03 05:53pm -in a cube a 3-connection state
c (all have 3 connections) are all part of 3 cycles, not 2
c The n-1 rule holds only for 2D mechanisms but not for 3D -any way to
c to automate the decision of whether a model is 3D?  Or rewrite this section
c in terms of routes (edges) not states (vertices)?
c
c Simplest solution is to ASK what maxiumum number of states in a cycle should
c be and eliminate any cycles with more than this


c	if(ncyc.le.1) goto 999	!no elim needed
91	continue
	if(ncyc.eq.0) then		!if no cycles found==
c	  print 302,ncyc
	  goto 999		!return
	endif
	ncyc0=ncyc
c NB output is nsc0()
	ncyc=0
	do m=1,ncyc0
	   if(nsc(m).le.ncmax) then
		ncyc=ncyc+1
		nsc0(ncyc)=nsc(m)		!output of nsc()
		do j1=1,nsc(m)-1
		   im(ncyc,j1)=icycsav(m,j1)
		   jm(ncyc,j1)=icycsav(m,j1+1)
		enddo
c last im,jm
		im(ncyc,nsc(m))=icycsav(m,nsc(m))
		jm(ncyc,nsc(m))=icycsav(m,1)
	   endif
	enddo
c	print 302,ncyc
c302	format(' number of cycles found = ',i5)
c	do i=1,ncyc
c	   print 304,i,nsc0(i)
c304	   format(' Cycle # ',i5,' has ',i5,' states-')
c		do j=1,nsc0(i)
c		print 305,im(i,j),jm(i,j)
c305		format(' im(i,j),jm(i,j) = ',2i8)
c	   enddo
c	enddo
c
c Original elimination did not need ncmax to be specified, but does
c not work with 3D mechanisms -all commented out
c
cc	do 410 jc=2,4		!outer loop for 2,3,4-con states
c	do jc=2,10		!outer loop for 2,3,4, . . .,10 -con states
c	   if(nsi(jc).eq.0) goto 410       !no jc-con states -try next
cc
c	   do i1=1,nsi(jc)	!go through the jc-con start states,jc=2,3,4
c		ks=ns(i1,jc)		!the state #
cc how many cycles does each state #ks occur in
cc
c		njc=0		!number of cycles that the jc-con state occurs in
c		do n1=1,ncyc
c		   do j1=1,nsc(n1)
c	 		if(icycsav(n1,j1).eq.ks) goto 414
c		   enddo
c		   goto 412	!next cycle
c414		   njc=njc+1
c		   nsc1(njc)=nsc(n1)	!keep # of states in each cycle with current jc-con state
c		   mcyc(njc)=n1	!cycle (row) number in ditto
c412		   continue
c		enddo
cc
c		if(njc.le.jc-1) goto 411	!correct # of cycles so no action needed
cc
cc Sort the sizes in nsc1. IND=index for sorted array.
cc No need to go through the jc-1 smallest ones at all- just eliminate
cc the jc,jc+1,..,njc largest ones from ICYCSAV so at end latter contains
cc only the required cycles
c		call SORTINDI(nsc1,ind,njc,n2)
cc Remove any superfluous cycles from ICYCSAV before next cycle
cc -move all cycles above that removed down one place in icycsav (and nsc)
c		nelim=njc-jc+1
c		do i2=jc,njc
c		   m=mcyc(ind(i2))
c		   melim(i2-jc+1)=m	!store cycle # to be removed
c		   print 452,m,(icycsav(m,j1),j1=1,nsc1(i2))
c452		   format(/' Superfluous cycle #',i3,' eliminated: ',/,20i3)
c		enddo
cc remove icycsav(m,*) (not necessary if m=last row=row #ncyc- just decrement
cc ncyc in this case
c		print 456
c456		format(' ICYCSAV before elimination')
cc		do 457 i3=1,ncyc
cc457		print 458,(icycsav(i3,j1),j1=1,nsc(i3))
cc458		format(1x,20i3)
cc
cc care needed with elimination- if rows 2,4 to be elim then after row
cc 2 is elim, row 4 will have become row 3, but if 4,2 elim in that
cc order then there is no problem, so reorder the rows to be eliminated,
cc stored in melim(), in descending order
c		call SORTI(melim,nelim,.false.,n2)
c		do 461 i2=1,nelim
c		   m=melim(i2)
c		   if(m.lt.ncyc) then
c			do 453 m1=m,ncyc-1
c			   nsc(m1)=nsc(m1+1)
c			    do 453 j1=1,nsc(m1)
c			       icycsav(m1,j1)=icycsav(m1+1,j1)
c453			continue
c		   endif
c		   ncyc=ncyc-1
c461		   continue
cc
cc check icycsav OK after elim of superfluous cycles
cc		  print 4561
cc4561		  format(' ICYCSAV after elimination')
cc		  do 4571 i3=1,ncyc
cc4571		  print 458,(icycsav(i3,j1),j1=1,nsc(i3))
cc
c411 		continue	!i1 loop to go through each jc-con state
c	   enddo
c410	   continue		!loop for jc=2,3,4
c	enddo
cc
c91	continue
cc
cc Set final values, and get param to be fixed by micro rev (these must
cc be the 1st two states in icyc, and in im,jm (see QSETD,QDEF1,GETCYC)
cc define im,jm
c	if(ncyc.eq.0) then		!if no cycles found==
c	  print 302,ncyc
c	  goto 999		!return
c	endif
c	do 52 m=1,ncyc
c	do 47 j1=1,nsc(m)-1		!nsc1 already sorted
c	   im(m,j1)=icycsav(m,j1)
c	   jm(m,j1)=icycsav(m,j1+1)
c47	continue
cc last im,jm
c	im(m,nsc(m))=icycsav(m,nsc(m))
c	jm(m,nsc(m))=icycsav(m,1)
c52	continue
cc
c	print 326
c326	format(' All cycles now found as follows')
c	pause
c	print 302,ncyc
c302	format(' number of cycles found = ',i5)
c	do 303 i=1,ncyc
c	print 304,i,nsc(i)
c304	format(' Cycle # ',i5,' has ',i5,' states-')
c	do 303 j=1,nsc(i)
c	print 305,im(i,j),jm(i,j)
c305	format(' im(i,j),jm(i,j) = ',2i8)
c303	continue
cc put nsc() in array of same dimension (=8) as in QDEF1 for output
c	do 3031 i=1,ncyc
c3031	nsc0(i)=nsc(i)
c
999	continue
	DEALLOCATE(NSC,icyc,icycsav)
	DEALLOCATE(nsi,ns,nck,nc)
	DEALLOCATE(last,nod)
c	DEALLOCATE(last,nod,nsc1,ind,melim,mcyc)
	RETURN
	end


