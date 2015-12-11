
	subroutine GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini,nccub,
     &  ncube,cubedef,cubecyc,cubext,extcyc)
c To alter, if req, the state in each cycle that is calculated by
c micro ref (see also GETCYC, in QDEF)
c
c Modif again 10/06/03 08:55am so that when there are no cubes, or for cycles
c that are not part of cubes, isetmr() is defined so that cycles with the
c greatest number of edges in common with other cycles are set first.
c For example in a 2-dimensional 3x3 grid (9 cycles), the central cycle
c shares edges with 4 of the 8 outer cycles (the ones that are not at corners)
c and this must clearly be set first.
c NB in any 2D mechanism there seem to be no cycles that obey MR automatically
c because of neighbouring cycles so automr(i) always false (see mucro-rev4.mcd)
c
c Modif 09/22/03 08:43am
c (1) to remove much of what was added last time.
c Probably CALCMR not needed at all -all MR can be set in the usual way
c as long as order of setting is correct.  Problems arise only in mechanisms
c with cubes and these are now defined in FINDCUBE, and ordered so that cubes
c with the fewest external faces come first -results in common/cube/
c Query? Does order still matter in, say, a cube with one connection absent?
c (See microrev3.mcd -this reduces number of cycles to 5, four with 4 members as
c for complete cube, and one with 6 members, the lats obeying MR if the first
c four obey it)
c
c (2) Revised ordering of cycles, done only if ncube>0.  Need to set AUTOMR
c for cycles that obey MR automatically becuase others have been set to
c obey MR already.  This can be automated for cubic mechanisms, but for cases
c like cube with a missing link there are also automr cycles -now attempt to
c detect these as cycles, every route in which is part of another cycle that
c is set to obeymr.
c
c (3) Rather than re-ordering cycles, the cycle numbers are kept as
c  as in qmechs.mec, but instead a pointer array is defined to specifiy
c  the order in which cycles must be set during calculations
c     ISETMR(i)=cycle # of the ith cycle to be set eg isetmr=32,3,7 means
c	set cycles 32, then 3, then 7.  The array isetmr() contains ncyc elements,
c	though setting will be skipped for those for which automr=true (or obeymr=false)
c     (NB earlier verion used isetmr() for different purpose, now defunct)
c
c (4) Code for getting MR rates as first two in cycle, and for checking
c     which rates can be set, is moved out into FINDMR (which calls TESTMR)
c
c
c  Modif 06/24/03 08:38am by adding CALCMR to allow MR to be forced
c in cases where TESTMR reports that there are NO rate constants in a cycle
c that are not part of a previously set cycle, and therefore available
c to be set in the usual way.  The presence of even one such cycle means
c that MR must be set by solving all the MR constraint equations for all
c of the free rates and this is done in CALCMR if MRCALC is set true here.
c The initial guesses for this calculation are the rate constants
c found by setting MR by the normal method for all the cycles for which
c this can be done.  Therefore we need a new array, like obeymr, say
c ISETMR(50)=nmr where nmr is the number of rate constants that are
c available to be set in each cycle. If ANY isetmr=0 then mrcalc=true
c and CALCMR used.
c
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
	integer*4 icyc(50),itemp(50)	!local
	logical done(50)	!local
	character*1 ans
cc  Local arrays to get valid MR routes if selected one is not valid
c	integer iOK(10),jOK(10)		!now in FINDMR
c
	integer nsc(50),im(50,100),jm(50,100)
	integer nsc1(50),im1(50,100),jm1(50,100)
c=	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	logical discprt
	common/dp/discprt
	logical allmr,useini,set5
	logical obeymr(50),automr(50),automr1(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
cc For re-ordering cycles
c	logical done
c	allocatable::icord
c	integer icord(:)
c	allocatable::nsct,imt,jmt,imt1,jmt1
c	integer nsct(:),imt(:,:),jmt(:,:),imt1(:,:),jmt1(:,:)
	allocatable::nedge
	integer nedge(:)
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
c for definitions of cubes
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
!	common/cube/ncube,cubedef,cubecyc,cubext	!fron findcube
c
	allmr=.false.	!local
	ndim=50		!max no of cycles
	do i=1,ndim
	   obeymr(i)=.false.
	   automr(i)=.false.
	   automr1(i)=.false.
	   done(i)=.false.  !set true when cycle when mr route set for cycle #i
	enddo
108	format(/)

	if(ncyc.eq.0) then
	   RETURN
	endif
c
c Assume that the cycles are already defined at this point, and re-order
c the cycles to ensure, if possible, that the last cycles to be set have
c one or more states in common.
c In this version, cycles are reordered ONLY if the mechanism contains
c cubes (ncube>0, from findcube).  But it is possible that there are other
c cases in which order matters. In particular, the case of a cube that has
c one link missing (see above) will give ncube=0 (order in which the first 4
c cycles are set probably does not matter in this case, but the fact that the
c 5th cycle is 'autoMR' does matter, and would not be detected by present
c version).
c
c Apart from systematic treatment of cubes, the main change is to introduce
c new variable AUTOMR for cycles that obey MR automatically,, because others do
c (so OBEYMR=true) but which do not need (indeed must not be) set
c For each cycle may have
c OBEYMR=true or false
c AUTOMR=true or false (if true, OBEYMR is ignored)
c A cycle is set to obey MR in the usual way IF obeymr=true AND automr=false
c
c
	if(discprt) write(7,108)
c
c Back to original code

	allmr=.true.
	ncyc1=ncyc
c
	IF(NCUBE.GT.0) GOTO 100	!separate section!
c
c NORMAL SECTION DONE WHEN NO CUBES PRESENT
c First order cycles such that those with most shared edges are set first
c  For each cycle, see how many of the others have a rate in common with
c  current cycle
c  There is a problem here -model that has a cube with one edge missing
c  will count as having no cubes, yet one cycle is automr() (micro-rev3.mcd)
c  though this can also be regarded as an 'outer cycle' that should be eliminated
c  altogether (see cubic.ppt)
	ALLOCATE(nedge(ndim))
c First order the cycles in decreasing number of shared edges
	do i=1,ncyc
	   automr(i)=.false.
c Define icyc() to contain states in the current cycle, and check if all of
c these links are members of other cycles that are set to obey MR
	   nedge(i)=0	!number of shared rates (edges) for cycle i
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!define states for current cycle
		is1=im(i,j)
		js1=jm(i,j)
		do m=1,ncyc
		   if(m.ne.i) then	!exclude current cycle
			do n1=1,nsc(m)	!go through all states in cycle m
			   if(im(m,n1).eq.is1.and.jm(m,n1).eq.js1.OR.
     &		    im(m,n1).eq.js1.and.jm(m,n1).eq.is1) then
				nedge(i)=nedge(i)+1	!number of shared rates for cycle i
			   endif
			enddo
		   endif
		enddo
	   enddo

	   if(discprt) write(7,13) (icyc(j),j=1,nsc(i))
13	   format('&   states: ',20i3)

	   if(discprt) write(7,21) i,nedge(i)
21	   format(
     & ' Number of shared edges cycle # ',i3,' = ',i3,/)
	enddo
	call SORTindi(nedge,isetmr,ncyc,ndim)
	call IVECREV(isetmr,ncyc,ndim)	!so cyc with most shared edges set first
	if(discprt) write(7,18) (isetmr(j),j=1,ncyc)
18	format(
     & ' Cycles set in order of decreasing number of shared edges:',/,
     & 10(2x,5i4))

	if(discprt) write(7,108)

	do L=1,ncyc
	   i=isetmr(L)
	   nsc1(l)=nsc(i)		!cycle number in order set above
	   do j=1,nsc(i)
	     
		 im1(l,j)=im(i,j)
		 jm1(l,j)=jm(i,j)	!define states for current cycle
	   enddo
	
	
	    allmr=.true.
		obeymr(i)=.true.
		automr(i)=.false.
	
		i1=im1(l,1)		!default
		j1=jm1(l,1)
		icy=l 
		do j=1,nsc(i)
		   icyc(j)=im(i,j)	!states for current cycle
		enddo
	!	call FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest)
		done(i)=.true.		!everything set for cycle #i

		imode=1
		icube=0	!ignored when imode=1
	enddo		!next cycle
	DEALLOCATE(nedge)
	goto 99	!print and return
c
c
c NOW SECTION TO DEFINE MR RATES WHEN MECHANISM CONTAINS CUBES
c (1) Are there cycles in addition to those faces of cubes?
c    List all cycles in cubes (removing duplicates!) -can't be more
c    than 50 if duplicates removed on the fly so use itemp(50) to store them
100	continue
	itemp(1)=cubecyc(1,1)
	nccub=1		!number of cycles in cubes
	do n=1,ncube	!set cube with fewest external faces first
	   do i=1,6
		i1=cubecyc(n,i)
		do j=1,nccub	!prev entries in itemp (nccub not yet upDATEWd)
		   if(i1.eq.itemp(j)) goto 10	!already in itemp
		enddo
		nccub=nccub+1
		itemp(nccub)=i1
10		continue
	   enddo
	enddo	!n=1,ncube
	call SORTi(itemp,nccub,.true.,50)

	if(nccub.lt.ncyc.and.nccub.gt.0) then

	   if(discprt) write(7,11) ncyc,nccub,ncyc-nccub
11	   format(' Number of cycles = ',i3,' of which ',i3,
     & ' form faces of cubes, and ',i3,' are not part of cubes')
	else if(nccub.eq.ncyc) then
	   if(discprt) write(7,12) ncyc
12	   format(' Number of cycles = ',i3, ' all of which ',
     & ' form faces of cubes')
	endif
c
c First do those that are NOT part of a cube (inc manual setting of automr)
c  The cycle numbers that ARE in cubes are sorted in ascending order in itemp()
c  so look for cycles that are not there
c
	is=0	!index for isetmr()
c
	if(ncyc.eq.nccub) goto 101	!all cycles are in cubes
c
c CODE FOR CYCLES THAT ARE NOT IN CUBES (in case where some cubes present)
c (modified 10/06/03 03:43pm as for non-cube case)
	if(discprt) write(7,14) ncyc-nccub
14	format(/,' The ',i3,' cycles that are NOT faces of cubes ')

	if(discprt) write(7,16)
16	format(' ------------------------------------------------',/)
c Locate the cycles that are not part of cubes
	do i=1,ncyc
	   do j=1,nccub	!is cycle #i in a cube?
		if(i.eq.itemp(j)) goto 94	!next cycle
	   enddo
	   is=is+1
	   isetmr(is)=i
94	   continue
	enddo
c
	ALLOCATE(nedge(ndim))
c Order the non-cube cycles in decreasing number of shared edges
	do is=1,ncyc-nccub
	   i=isetmr(is)
	   automr(i)=.false.
c Define icyc() to contain states in the current cycle, and check if all of
c these links are members of other cycles that are set to obey MR
	   nedge(i)=0	!number of shared rates (edges) for cycle i
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!define states for current cycle
		is1=im(i,j)
		js1=jm(i,j)
		do m=1,ncyc
		   if(m.ne.i) then	!exclude current cycle
			do n1=1,nsc(m)	!go through all states in cycle m
			   if(im(m,n1).eq.is1.and.jm(m,n1).eq.js1.OR.
     &		     im(m,n1).eq.js1.and.jm(m,n1).eq.is1) then
				   nedge(i)=nedge(i)+1	!number of shared rates for cycle i
			   endif
			enddo
		   endif
		enddo
	   enddo
	  
	   if(discprt) write(7,13) (icyc(j),j=1,nsc(i))
	   if(discprt) write(7,21) i,nedge(i)
	enddo
	call SORTindi(nedge,isetmr,ncyc-nccub,ndim)
	call IVECREV(isetmr,ncyc-nccub,ndim)	!so cyc with most shared edges set first
	if(discprt) write(7,181) (isetmr(j),j=1,ncyc-nccub)
181	format(
     & ' Non-cube cycles set in order of dec number of shared edges:',/,
     & 10(2x,5i4))

	if(discprt) write(7,108)
c
c
c Now set the mr values for the non-cube cycles
	do L=1,ncyc-nccub
	   i=isetmr(L)		!cycle number in order set above
	   nsc1(l)=nsc(i)
	   do j=1,nsc(i)
		im1(l,j)=im(i,j)
		jm1(l,j)=jm(i,j)	!define states for current cycle
	   enddo
	
	   allmr=.true.
	   obeymr(i)=.true.
	   automr(i)=.false.
	
	   
		i1=im1(l,1)		!default
		j1=jm1(l,1)
		icy=l 
		do j=1,nsc(i)
		   icyc(j)=im(i,j)	!states for current cycle
		enddo
	!	call FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest)
		done(i)=.true.		!everything set for cycle #i

		imode=1
		icube=0	!ignored when imode=1
	enddo		!end of defining MR for cycles not in cubes
c
c NOW DEFINE MR FOR ALL CYCLES THAT ARE PART OF CUBES
c
101	continue !jump here if all cycles are parts of cubes
	if(discprt) write(7,15) nccub
15	format(/,' The ',i3,' cycles that are faces of cubes ')
	if(discprt) write(7,16)
	do n=1,ncube
		do m1=1,6
 		i=cubecyc(n,m1)	!i=cycle number
		allmr=.true.
		obeymr(i)=.true.
c		automr(i)=.false.	!for 1st 5 faces
ccc
		if(m1.eq.6) automr(i)=.true.
		do k=1,is
			if(isetmr(k).eq.i) goto 111
		enddo
		is=is+1
		isetmr(is)=i	!define pointer
		if(m1.eq.6) automr(is)=.true.
c		
		nsc1(is)=nsc(i)
		do j=1,nsc(i)
		   im1(is,j)=im(i,j)	!states for current cycle
		   jm1(is,j)=jm(i,j)
		enddo
		
		i1=im1(is,1)		!default
		j1=jm1(is,1)
		icy=is 
		do j=1,nsc(i)
		   icyc(j)=im(i,j)	!states for current cycle
		enddo
	!	call FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest)
		done(i)=.true.		!everything set for cycle #i

		imode=2
		icube=n	!ignored when imode=1
111	    continue
	    enddo		!next cycle, i=1,6
	continue
	enddo		!n=1,ncube
c
c Print the values
c

99	continue

	RETURN
	end


	subroutine PRTCYC(icyc,ncyc,nsc,im,jm,icube,imode)
c To print cycles in GETREV
c Print one cycle at a time, in the loop
c icyc=current cycle number
c icube=current cube number (ignored if imode=1)
c
c imode=1 if ncube=0 then as before
c imode=1 and ncube>0 for cycles that are not part of cubes
c imode=2 and cube>0 when cycles that are faces of cubes
c
c for definitions of cubes
	integer nsc(50),im(50,100),jm(50,100)
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
	common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	logical extcyc(50)
	
	logical discprt
	common/dp/discprt
c
c Print the values
c Printing done in order in which cycles are set (desirable?) in this version
c but cycle numbering not changed
	n=icyc	!actual cycle number
c==	do L=1,ncyc
	   if(discprt) write(7,108)
108	format(/)
	   if(imode.eq.2.and.ncube>0) then
	      if(discprt) write(7,124) icube,cubext(icube)
124		format(' Cube #',i3,'(',i3,' external faces)')
		if(extcyc(n)) then
	         if(discprt) write(7,125) n
125	         format(' Cycle # ',i3,' external')
		else
	         if(discprt) write(7,126) n
126	         format(' Cycle # ',i3,' internal')
		endif
	   else
	      if(discprt) write(7,127) n
127	      format(' Cycle # ',i3)
	   endif
	   if(obeymr(n)) then
	      if(automr(n)) then
		   if(discprt) write(7,1271)(im(n,m),jm(n,m),m=1,nsc(n))
1271		   format(' Micro rev guaranteed by adjacent cycles',/,
     &		2(5(2i3,4x),/))
		else 	!MR set in the normal way
		   if(discprt) write(7,1311)im(n,1),jm(n,1)
1311		   format(2i3,'  (calc by micro rev)')
		   if(discprt) write(7,128)(im(n,m),jm(n,m),m=2,nsc(n))
128		   format(2(5(2i3,4x),/))
		endif
	   else
		if(discprt) write(7,1312)im(n,1),jm(n,1)
1312		format(2i3,'  (no micro rev constraint)')
		if(discprt) write(7,127)(im(n,m),jm(n,m),m=2,nsc(n))
	   endif
c==	enddo		!end of L=1,ncyc
c
	RETURN
	end
	

	
	
	subroutine order_states(i,i1,j1,icyc,nsc,ndim,im,jm)

	integer icyc(50),im(50,100),jm(50,100)

	do m1=1,nsc-1
	   m2=m1		!for skip-out
	   if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	enddo
	if(icyc(nsc).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	call IVECREV(icyc,nsc,ndim)
	do m1=1,nsc-1
	   m2=m1		!for skip-out
	   if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	enddo
	if(icyc(nsc).eq.i1.and.icyc(1).eq.j1) goto 151
c if reach here the specified i,j are not found in the cycle
c	goto 17
	idest=17		!start mr setting again in GETREV
15	continue
c now bring state in element m2 of icyc into element #1
	call IVECROT(icyc,nsc,1-m2,ndim)
	goto 18
151	continue
c now bring state in element m2 of icyc into element #1
	call IVECROT(icyc,nsc,1,ndim)
c and redefine im,jm in correct order
18	continue
	do m=1,nsc-1
	   im(i,m)=icyc(m)
	   jm(i,m)=icyc(m+1)
	enddo
c and the last im,jm

	im(i,nsc)=icyc(nsc)
	jm(i,nsc)=icyc(1)		
	
	return
	end
