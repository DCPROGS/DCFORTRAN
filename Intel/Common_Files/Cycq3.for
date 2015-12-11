	subroutine CYCQ(k,ncon,ic,ncyc,nsc0,im,jm,ncmax,ncyc0)
c To get cycles automatically from connections IC() as found in CHARQ
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
c	allocatable::nsc1,ind,melim,mcyc
c	integere nsc1(:),ind(:),melim(:),mcyc(:)	!ditto
c	integer NSC(50),icyc(50),icycsav(50,50)
c	integer nsi(49),ns(100,40),nck(100),nc(100,40)	!for search for cycles
c	integer last(200),nod(200),nsc1(100),ind(100),melim(100),mcyc(100)	!ditto
	logical same


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

34	continue

	do m=1,10
	   nsi(m)=0
	   do i=1,k
		if(nck(i).eq.m) then
		   nsi(m)=nsi(m)+1	!# of states with m=1,2,3,4 connections
		   ns(nsi(m),m)=i		!the 1,2,..nsi(m) states with m connections
		endif
	   enddo
	  
	enddo

	ncyc=0
	if(k.eq.2) goto 91		!no cycles possible==

	do jc=2,10		!outer loop
	   if(nsi(jc).eq.0) goto 400       !no jc-con states -try next

	   do i1=1,nsi(jc)	!go through the jc-con start states
		ks=ns(i1,jc)		!state # to start search for cycle

		k1=ks	!current state that we are searching from   [eg ks=1]

		do nd=1,n3
		   last(nd)=0	!=last exit route tried for node #nd
		enddo
		nd=0        !starting state is first node (upDATEWd below)
		mc=1
		icyc(mc)=ks		!=first state in putative cycle
		nflag=0		!so next node located is counted as a new one
c
41		continue	!return here to continue search
		nout=nck(k1)		!# of ways out of current state
		if(k1.ne.ks) nout=nout-1	!exclude arrival route
		if(nout.eq.0) then	!dead end

		   goto 44	!dead end: return to prev node
		else if(nout.eq.1) then

		   klast=icyc(mc-1)
		   mc=mc+1		!next element if ICYC; =2,3,...
		   icyc(mc)=nc(k1,1)	!1st way out=next state in putative cycle
		   if(icyc(mc).eq.klast) icyc(mc)=nc(k1,2)	!use other one
c		   print 321,k1,mc,icyc(mc),klast
c321		   format(' One way out: k1,mc,icyc(mc),klast = ',4i5)
		else

		   if(nflag.eq.0) then
		      nd=nd+1		!count number of nodes encountered
		      nod(nd)=mc		!index in icyc where node is
		   endif
		   nflag=0			!will be reset to 1 if returned to again
c		   klast=icyc(mc)		!mc not yet upDATEWd
		   if(mc.gt.1) then
			klast=icyc(mc-1)		!mc not yet upDATEWd
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
			indcyc=indcyc+1
			if(indcyc.eq.10000) goto 90
		   if(ncyc.gt.n0) goto 90	!n0=array size
		   if(mc-1.le.ncmax) then
				ncyc=ncyc+1
		   
				do j1=1,mc
					icycsav(ncyc,j1)=icyc(j1)
				enddo
				nsc(ncyc)=mc-1
c		   
		   endif
		   goto 44			!return to last node
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



	if(ncyc.le.1) goto 999	!no elim needed
91	continue
c

	if(ncyc.eq.0) then		!if no cycles found==
	 
	  goto 999		!return
	endif
	ncyc0=ncyc
	if(ncyc0.gt.1000) ncyc0=1000
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
	

999	continue
	DEALLOCATE(NSC,icyc,icycsav)
	DEALLOCATE(nsi,ns,nck,nc)
	DEALLOCATE(last,nod)
c	DEALLOCATE(last,nod,nsc1,ind,melim,mcyc)
	RETURN
	end


