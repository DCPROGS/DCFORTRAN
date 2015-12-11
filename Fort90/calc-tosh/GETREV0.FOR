	subroutine GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini)
c To alter, if req, the state in each cycle that is calculated by
c micro ref (see also GETCYC, in QDEF)
c
c Modif 09/20/03 05:26pm for calculation of MR in mechanisms with cubes,
c the cubes having been defined in FINDCUBE, and ordered so that cubes
c with the fewest external faces come first -results in common/cube/
c
c================
c Finally sort the cycles within each cube so that 5 of the 6 can be set
c in the normal way.  I think all that matters is that the last (6th) side
c (which will automatically obey MR if the rest do, should be adjacent to
c the 5th (last one to be set) rather than opposite to it.  The cycles were
c ordered above (in checkcyc) so that the last two were opposite, so all that
c should be needed is to swap the last two.
c===thus far, cycles in each cube have been identified, and cycle numbers (m)
c===recorded as cubecyc(icube,m), but the definition of cycle #m has not
c=== changed from that read in as im(),jm().  Since MR is set via a loop
c=== m=1,ncyc, and since order in which they are set matters, then before
c=== starting calculations the definition of cycle 1, cycle2 in terms of
c=== im(m,*), jm() must be changed so the cycle to be set first comes first.
c
c=== Can all this be skipped if ncube=0?

c=== treatment of default cycles?

c=== probably this re-ordering should be done not here, but in GETREV
c=== if ncube>0 (via common/cube/cubedef,cubecyc,cubext, and ncube (put
c=== ncube in common too?). When nbew model is defined the it should be saved
c=== with correct ordering anyway in which case need little printout from
c=== any of this, but for existing models, re-order here AND make write
c=== back to qmechs mandatory -or do it automatically.
c===
c=================

c  Modif 06/24/03 08:38am by adding CALCMR to allow MR to be forced
c in cases where TESTMR reports that there are NO rate constants in a cycle
c that are not part of a previously set cycle, and therefore available
c to be set in the usual way.  The presence of even one such cycle means
c that MR must be set by solving all the MR constraint equations for all
c of the free rates and this is done in CALCMR if MRCALC is set true here.
c The initial guesses fopr this calculation are the rate constants
c found by setting MR by the normal method for all the cycles for which
c this can be done.  Therefore we need a new array, like obeymr, say
c ISETMR(50)=nmr where nmr is the number of rate constants that are
c available to be set in each cycle. If ANY isetmr=0 then mrcalc=true
c and CALCMR used.
c Modified again 07/01/03 08:02am to automatically re-order the cycles so
c as to maximise the number of cycles that can be set in the ordinary way.
c E.g for cubic mechanism, it the last two cycles to be set are left and
c right faces (or any two non-adjacent faces) these two cycles have no rates
c in common, and so contain 8 pairs of rate constants (8 edges), all different,
c between them. This means that every rate constant in them occurs in one of
c the other cycles (4 other cycles=8 routes/edges), so no free rates to set.
c but if cycles with no common rates are set FIRST then usual method can be
c used to set MR for 5 cycles, which gurantees 6th.  Note
c (1) each pair of rates in a cycle occurs either 0 or 1 time in another cycle
c	(0 if the cycles have no edge in common, 1 if they have an edge in
c	common -two squares cannot share more than one edge). Pairs of cycles
c	with no edge in common (0) should be set before those with (1).
c (2) One edge (pair of rates) can be shared between 0, 1, 2, 3, or 4 different
c	cycles (in a cube, max=2 but the central cube in 3x3 cube stack would have
c	edges shared with 4 different cycles). Probably sensible to set first
c	the 'innermost' cycles -those shared with the most other cycles)
c
c  Modif 06/19/03 09:53am by adding TESTMR
c
c Modif 11/22/01 09:43am so ncyc,nsc,im,jm are parameters, rather than
c in common/mpar, so can be called either with values from prog.ini or values
c from qmechs.dat.  Now sets obeymr(i)=true if cycle #i is constrained by
c MR, and moves the constrained rate to first postion in im(),jm(). If no
c MR constraint then obeymr(i)=false and IM(), JM() returned unchanged
c (im1 etc are used only for defaults)
c im() etc are values for QMECHS.DAT; im1() etc are values
c from prog.ini
c
c Modif 11/16/01 09:41am so that if im(L,1)=0 then microscopic reversibility
c check is omitted for cycle #L -so this way can have some cycles that
c do not obey MR and some that do.
c
	integer icyc(50)
	character*1 ans
	logical discprt
c  Local arrays to get valid MR routes if selected onne is not valid
	integer iOK(10),jOK(10)
	integer nsc(50),im(50,100),jm(50,100)
	integer nsc1(50),im1(50,100),jm1(50,100)
c=	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	common/dp/discprt
	logical obeymr(50),allmr,useini,mrcalc
	integer isetmr(50)
	common/mr/obeymr
	common/mr1/isetmr,mrcalc
c Fore re-ordering cycles
	logical done
	allocatable::icord
	integer icord(:)
	allocatable::nsct,imt,jmt,imt1,jmt1
	integer nsct(:),imt(:,:),jmt(:,:),imt1(:,:),jmt1(:,:)
	allocatable::ncedge
	integer ncedge(:)
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
c for definitions of cubes
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
	common/cube/ncube,cubedef,cubecyc,cubext	!from findcube
c
	ndim=50	!for call to ivecrev
	allmr=.false.	!local
	do i=1,50
	   obeymr(i)=.false.
	enddo
108	format(/)
	print 1,ncyc
1	format(/,' Number of cycles in mechanism [',i2,'] = ')
	call INPUTi(ncyc)
	if(ncyc.eq.0) then
	   RETURN
	endif
c
c Assume that the cycles are already defined at this point, and re-order
c the cycles to ensure, if possible, that the last cycles to be set have
c one or more states in common.  This is important in case of cubic
c mechanisms, to allow as many cycles as possible to be set to obey MR
c by the simple usual way (rather than needing numerical solution
c in CALCMR).
c (1) First check all pairs of cycles to find out whether each pair has 0 or 1
c edges in common.  Set those with 0 in common first. For n cycles there
c are n(n-1)/2 different pairs.  Probably OK to start with ANY cycle, then look
c for any other with 0 in common to set next. The start with 2nd cycle and look
c for another (exc first) that has 0 edges in common, and so on.
c Define icord(i)=order in which cycles to be set e.g =(3,5,4,1,6,2) for cube
	ALLOCATE(icord(ncyc),ncedge(npar))
	do i=1,ncyc
	   icord(i)=0
	enddo
	icord(1)=1
	nset=1	!number of cycles set in icord()
	m=1	!start with cycle 1
10	continue		!return here for next cycle
	do n=1,nsc(m)	!go through states in cycle m
	   i=im(m,n)
	   j=jm(m,n)
	   do m1=1,ncyc  !go through all the other cycles that are  not yet chosen
		done=.false.
		do n1=1,nset
		   if(m1.eq.icord(n1)) done=.true.	!cycle m1 already done
		enddo
		ndone=0
		if(.not.done) then
		   ndone=ndone+1
		   icom=0	!no edge in common between cycle m and m1
		   do n1=1,nsc(m1)
			if(im(m1,n1).eq.i.and.jm(m1,n1).eq.j.OR.
     &		  im(m1,n1).eq.j.and.jm(m1,n1).eq.i) then
				icom=1	!edge in common between cycle m and m1
			endif
		   enddo
		   if(icom.eq.0) then
	      	nset=nset+1
	      	icord(nset)=m1
		   endif
		endif
	   enddo
	   m=m1
	   if(nset.lt.ncyc) goto 10
	enddo
c Any cycles not already in icord() can now be added at the end (set last)
	if(nset.lt.ncyc) then
	   do m=1,ncyc
		done=.false.
		do n1=1,nset
		   if(icord(n1).eq.m) done=.true. 	!cycle m ale
		enddo
		if(.not.done) then
	         nset=nset+1
	         icord(nset)=m
		endif
	   enddo
	endif
c Make copy of original
	ALLOCATE(nsct(50),imt(50,100),jmt(50,100),
     &	imt1(50,100),jmt1(50,100))
	do m=1,ncyc
	   nsct(m)=nsc(m)
	   do n=1,nsc(m)
		imt(m,n)=im(m,n)
		jmt(m,n)=jm(m,n)
		imt1(m,n)=im1(m,n)	!default
		jmt1(m,n)=jm1(m,n)
	   enddo
	enddo
c Copy back to nsc,im,jm in new order (also reorder defaults in im1, jm1)
	do m=1,ncyc
	   m1=icord(m)
	   nsc(m)=nsct(m1)
	   do n=1,nsc(m)
		im(m,n)=imt(m1,n)
		jm(m,n)=jmt(m1,n)
		im1(m,n)=imt1(m1,n)
		jm1(m,n)=jmt1(m1,n)
	   enddo
	enddo
c Small problem -don't want to save altered im1,jm1 in .ini file unless
c altered order of cycles is also stored in qmechs file
c Print what was done if cycles re-ordered
	done=.false.
	do m=1,ncyc
	   if(icord(m).ne.m) done=.true.
	enddo
	if(done) then
	   print 24,(icord(i),i=1,ncyc)
	   if(discprt) write(8,24) (icord(i),i=1,ncyc)
24	   format(
     & ' Cycles reordered as follows (to allow setting setting of MR)',/
     & ,10(2x,i3),/,10(2x,i3),/,10(2x,i3),/,10(2x,i3),/,10(2x,i3))
	   pause
	endif
	DEALLOCATE(icord,nsct,imt,jmt,imt1,jmt1,ncedge)

c End of optimum reordering of cycles
c
c Find out how many cycles each edge belongs to (info not used yet)
c List pairs of rates (edges) as in print of 'values of rate constants'
c Values are in pairs -values are same edge in opp directions so need only
c every 2nd value
	print 108			!new line
	if(discprt) write(8,108)
	do n=1,npar,2
	   i=irate(n)
	   j=jrate(n)
	   ncedge(n)=0
c How many cycles contain i,j or j,i?
	   do m=1,ncyc
		do n1=1,nsc(m)	!go through all states in cycle m
		   if(im(m,n1).eq.i.and.jm(m,n1).eq.j.OR.
     &		  im(m,n1).eq.j.and.jm(m,n1).eq.i) then
			ncedge(n)=ncedge(n) + 1
		   endif
		enddo
	   enddo
	   ncedge(n+1)=ncedge(n)	!define for j,i as well as i,j
	   print 26,i,j,j,i,ncedge(n)
	   if(discprt) write(8,26) i,j,j,i,ncedge(n)
26	   format(
     &  	' route q(',i2,',',i2,') and q(',i2,',',i2,
     &	') is a member of ',i3,' cycles')
	enddo
	print 108			!new line
	if(discprt) write(8,108)
c
c Back to original code
	ans='Y'
	call DCASK('Should ALL cycles be constrained by microscopic rev',
     &	ans,ans)
	allmr=ans.eq.'Y'
	do i=1,ncyc
17	   print 123,i,nsc(i)
123	   format(' Cycle #',i3,/,
     &     ' Number of states [',i2,'] = ')
	   call INPUTi(nsc(i))
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!states for current cycle
	   enddo
	   print 13,(icyc(j),j=1,nsc(i))
13	   format('   states: ',20i3)
	   if(useini) then
		i1=im1(i,1)		!default
		j1=jm1(i,1)
	   else
		i1=im(i,1)		!default
		j1=jm(i,1)
	   endif
	   if(allmr) then
		obeymr(i)=.true.
	   else
		if(obeymr(i)) then	!set default
		   print 65,i
65		   format(
     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
		   ans='Y'
		else
		   print 66,i
66		   format(
     &	' Should cycle #',i2,' be constrained by micro rev [N] ? ')
		   ans='N'
		endif
		call INPUTa(ans)
		obeymr(i)=ans.eq.'Y'
	   endif
25	   continue		!jump here to try again if invalid values given
	   if(obeymr(i)) then
		print 67,i1,j1
67		FORMAT(
     &'  Define the route, q(i,j), to be calculated by micro rev:',/,
     &'   Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
	   else
		goto 9	!go to next cycle
	   endif
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	   call IVECREV(icyc,nsc(i),ndim)
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c if reach here the specified i,j are not found in the cycle
	   goto 17
15	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1-m2,ndim)
	   goto 18
151	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1,ndim)
c and redefine im,jm in correct order
18	   continue
	   do m=1,nsc(i)-1
		im(i,m)=icyc(m)
		jm(i,m)=icyc(m+1)
	   enddo
c and the last im,jm
	   im(i,nsc(i))=icyc(nsc(i))
	   jm(i,nsc(i))=icyc(1)		!complete the cycle
c Now check that the rate specified to be set by MR does not occur as
c part of any cycle that has been previously set by MR (if it did the previous
c cycle would be changed and would no longer obey MR)
	   if(i.eq.1) isetmr(1)=nsc(1) 	!all setable in first cycle
	   if(i.ge.2) then
		i11=im(i,1)
		j11=jm(i,1)
		call TESTMR(i,i11,j11,ncycp,nOK,iOK,jOK)
		isetmr(i)=nOK
		mrcalc=.false.
		if(ncycp.ne.0) then
		   call BELL(2)
		   print 20,i11,j11,ncycp,i
20		   format(
     & ' Q(',i2,',',i2,') is a member of cycle ',i3,
     & ' so cannot be reset in cycle ',i3)
		   if(nOK.eq.0.and.i.eq.ncyc) then
			print 23,i
23			format(
     & ' All rates in last cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  set not to obey MR but should do so anyway, if others do.')
			pause
			obeymr(i)=.false. 	!set false
		   else if(nOK.eq.0.and.i.lt.ncyc) then
			call BELL(3)
			print 21,i,i
21			format(
     & ' All rates in cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  cycle ',i3,' MR set globally for all rates.')
			mrcalc=.true.	!one or more with nOK=0 so use CALCMR
		   else
			print 22, (iOK(n),jOK(n),n=1,nOK)
22			format(
     & ' The following routes are NOT a member of any previous cycle,',
     & '  so OK ',/, 5(i3,1x,i3,',  '))
			i1=iOK(1)		!default=first good route
			j1=jOK(1)
c	 Try again')
			goto 25
		   endif
		endif
	   endif
9	   continue			!skip here if no m.r. constraint for current cycle
	enddo		!next cycle
c
c Print the values
	do L=1,ncyc
	   print 126, L
	   if(discprt) write(8,126)L
126	   format(' Cycle # ',i3)
	   if(obeymr(L)) then
	      if(mrcalc) then
		   print 1271,(im(L,m),jm(L,m),m=1,nsc(L))
		   if(discprt) write(8,1271)(im(L,m),jm(L,m),m=1,nsc(L))
1271		   format(' Micro rev imposed iteratively',/,
     &		2(5(2i3,4x),/))
		else 	!MR set in the normal way
		   print 1311,im(L,1),jm(L,1)
		   if(discprt) write(8,1311)im(L,1),jm(L,1)
1311		   format(2i3,'  (calc by micro rev)')
		   print 127,(im(L,m),jm(L,m),m=2,nsc(L))
		   if(discprt) write(8,127)(im(L,m),jm(L,m),m=2,nsc(L))
127		   format(2(5(2i3,4x),/))
		endif
	   else
		print 1312,im(L,1),jm(L,1)
		if(discprt) write(8,1312)im(L,1),jm(L,1)
1312		format(2i3,'  (no micro rev constraint)')
		print 127,(im(L,m),jm(L,m),m=2,nsc(L))
		if(discprt) write(8,127)(im(L,m),jm(L,m),m=2,nsc(L))
	   endif
	enddo
c
	RETURN
	end

