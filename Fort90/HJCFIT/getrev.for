	subroutine GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini)
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
	logical obeymr(50),automr(50)
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
	common/cube/ncube,cubedef,cubecyc,cubext	!fron findcube
c
	allmr=.false.	!local
	ndim=50		!max no of cycles
	do i=1,ndim
	   obeymr(i)=.false.
	   automr(i)=.false.
	   done(i)=.false.  !set true when cycle when mr route set for cycle #i
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
	print 108			!new line
	if(discprt) write(8,108)
c
c Back to original code
	ans='Y'
	call DCASK('Should ALL cycles be constrained by microscopic rev',
     &	ans,ans)
	allmr=ans.eq.'Y'
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
c	   isetmr(i)=i	!set actual order later
c17	   print 123,i,nsc(i)
	   print 123,i,nsc(i)
123	   format(' Cycle #',i3,/,
     &     '   Number of states [',i2,'] = ')
	   call INPUTi(nsc(i))
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
	   print 13,(icyc(j),j=1,nsc(i))
	   if(discprt) write(8,13) (icyc(j),j=1,nsc(i))
13	   format('&   states: ',20i3)
	   print 21, i,nedge(i)
	   if(discprt) write(8,21) i,nedge(i)
21	   format(
     & ' Number of shared edges cycle # ',i3,' = ',i3,/)
	enddo
	call SORTindi(nedge,isetmr,ncyc,ndim)
	call IVECREV(isetmr,ncyc,ndim)	!so cyc with most shared edges set first
	print 18,(isetmr(j),j=1,ncyc)
	if(discprt) write(8,18) (isetmr(j),j=1,ncyc)
18	format(
     & ' Cycles set in order of decreasing number of shared edges:',/,
     & 10(2x,5i4))
	print 108			!new line
	if(discprt) write(8,108)
c
c	enddo
c	do i=1,ncyc
c	   isetmr(i)=i	!can set in original order if no cubes
c17	   print 123,i,nsc(i)
c123	   format(' Cycle #',i3,/,
c     &     '   Number of states [',i2,'] = ')
c	   call INPUTi(nsc(i))
cc Define icyc() to contain states in the current cycle, and check if all of
cc these links are members of other cycles that are set to obey MR
cc -if so default=automr
cc Potential problem with this (and similar code below) -if applied
cc to a simple cube it would say that ALL cycles have all edges in common
cc with other cycles that are set to MR so would give default=automr
cc for all of them, whereas in fact only one of them is automr
cc Do the following lines fix this problem?
cc				else
cc				   automr(m)=.false.	!other cycles NOT automr, only last
c	   automr(i)=.true.
c	   do j=1,nsc(i)
c		icyc(j)=im(i,j)	!define states for current cycle
c		do m=1,ncyc
c		   if(m.ne.i) then	!exclude current cycle
c			do n1=1,nsc(m)	!go through all states in cycle m
c			   if(im(m,n1).eq.i.and.jm(m,n1).eq.j.OR.
c     &		     im(m,n1).eq.j.and.jm(m,n1).eq.i) then
c				if(.not.obeymr(m)) then	!route im(i,j) belongs also to cycle m
c				   automr(i)=.false.
c				   goto 23	!not all adjacent cycles obey mr
c				else
c				   automr(m)=.false.	!other cycles NOT automr, only last
c				endif
c			   endif
c			enddo
c		   endif
c		enddo
c	   enddo
c23	   print 13,(icyc(j),j=1,nsc(i))
c13	   format('&   states: ',20i3)
c	   if(automr(i)) then
c		print 21, i,i
c		if(discprt) write(8,21) i,i
c21		format(
c     & ' All routes in cycle # ',i3,' are part of other cycles that',/,
c     & '  obey MR so MR is guaranteed in cycle # ',i3)
c	   endif
c
	do L=1,ncyc
	   i=isetmr(L)		!cycle number in order set above
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!define states for current cycle
	   enddo
17	   print 1231,i
1231	   format(/,' Cycle #',i3)
	   print 13,(icyc(j),j=1,nsc(i))
c	   if(discprt) write(8,13) (icyc(j),j=1,nsc(i))
c13	   format('&   states: ',20i3)
	   if(useini) then
		i1=im1(i,1)		!default
		j1=jm1(i,1)
	   else
		i1=im(i,1)		!default
		j1=jm(i,1)
	   endif
c
	   if(allmr) then
		obeymr(i)=.true.
		ans='N'
		if(automr(i)) ans='Y'
		call DCASK(
     & ' Is MR guaranteed by constraints on adjacent cycles',ans,ans)
		automr(i)=ans.eq.'Y'
	   else
		if(obeymr(i).and.(.not.automr(i))) then	!set default
		   ians=1
		else if(automr(i)) then
		   ians=2
		else
		   ians=3
		endif
		if(ncyc.lt.4) then	!automr not possible
		   print 65,i
65		   format(
     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   obeymr(i)=ans.eq.'Y'
		   automr=.false.
		else
		   print 651,i,ians
651		   format(
     &    ' Cycle ',i3,':',/,
     &    '  (1) set to obey micro rev',/,
     &    '  (2) obeys micro rev via constraints on adjacent cycles ',/,
     &    '  (3) does NOT obey micro rev',/,
     &    '  Option number [',i2,'] = ')
		   call INPUTi(ians)
		   if(ians.eq.1) then
			obeymr=.true.
			automr=.false.
		   else if(ians.eq.2) then
			obeymr=.true.
			automr=.true.
		   else if(ians.eq.3) then
			obeymr=.false.
			automr=.false.
		   endif
		endif
	   endif
25	   continue		!jump here to try again if invalid values given
	   if(obeymr(i).and.(.not.automr(i))) then
		print 67,i1,j1
67		FORMAT(
     &'    Define the route, q(i,j), to be calculated by micro rev:',/,
     &'     Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
	   else
		goto 9	!go to next cycle
	   endif
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
c	subroutine FINDMR(i1,j1,ncyc,nsc,icyc,im,jm,obeymr,idest) replaces code below
c	   icy=i		!current cycle number (isetmr(i)=i in this case)
	   icy=L		!current cycle number (isetmr(L)=i in this case)
	   call FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest) !replaces code below
	   if(idest.eq.17) goto 17
	   if(idest.eq.25) goto 25

c	   do m1=1,nsc(i)-1
c		m2=m1		!for skip-out
c		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
c	   enddo
c	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
cc also check if they occur in reverse order
c	   call IVECREV(icyc,nsc(i),ndim)
c	   do m1=1,nsc(i)-1
c		m2=m1		!for skip-out
c		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
c	   enddo
c	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
cc if reach here the specified i,j are not found in the cycle
c	   goto 17
c15	   continue
cc now bring state in element m2 of icyc into element #1
c	   call IVECROT(icyc,nsc(i),1-m2,ndim)
c	   goto 18
c151	   continue
cc now bring state in element m2 of icyc into element #1
c	   call IVECROT(icyc,nsc(i),1,ndim)
cc and redefine im,jm in correct order
c18	   continue
c	   do m=1,nsc(i)-1
c		im(i,m)=icyc(m)
c		jm(i,m)=icyc(m+1)
c	   enddo
cc and the last im,jm
c	   im(i,nsc(i))=icyc(nsc(i))
c	   jm(i,nsc(i))=icyc(1)		!complete the cycle
cc Now check that the rate specified to be set by MR does not occur as
cc part of any cycle that has been previously set by MR (if it did the previous
cc cycle would be changed and would no longer obey MR)
c	   if(i.eq.1) isetmr(1)=nsc(1) 	!all setable in first cycle
c	   if(i.ge.2) then
c		i11=im(i,1)
c		j11=jm(i,1)
c		call TESTMR(i,i11,j11,ncycp,nOK,iOK,jOK)
c		isetmr(i)=nOK
c		if(ncycp.ne.0) then
c		   call BELL(2)
c		   print 20,i11,j11,ncycp,i
c20		   format(
c     & ' Q(',i2,',',i2,') is a member of cycle ',i3,
c     & ' so cannot be reset in cycle ',i3)
c		   if(nOK.eq.0) then
c			call BELL(3)
c			print 21,i,i
c21			format(
c     & ' All rates in cycle ',i3,'  are part of previous cycles',/,
c     & '  -NONE available to set by microscopic reversibility')
c		   else
c			print 22, (iOK(n),jOK(n),n=1,nOK)
c22			format(
c     & ' The following routes are NOT a member of any previous cycle,',
c     & '  so OK ',/, 5(i3,1x,i3,',  '))
c			i1=iOK(1)		!default=first good route
c			j1=jOK(1)
cc	 Try again')
c			goto 25
c		   endif
c		endif
c	   endif
c ===end of code now moved to FINDMR
c
9	   continue			!skip here if no m.r. constraint for current cycle
	   imode=1
	   icube=0	!ignored when imode=1
	   call PRTCYC(i,ncyc,nsc,im,jm,icube,imode)
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
		do j=1,nccub	!prev entries in itemp (nccub not yet updated)
		   if(i1.eq.itemp(j)) goto 10	!already in itemp
		enddo
		nccub=nccub+1
		itemp(nccub)=i1
10		continue
	   enddo
	enddo	!n=1,ncube
	call SORTi(itemp,nccub,.true.,50)
c
	if(nccub.lt.ncyc.and.nccub.gt.0) then
	   print 11,ncyc,nccub,ncyc-nccub
	   if(discprt) write(8,11) ncyc,nccub,ncyc-nccub
11	   format(' Number of cycles = ',i3,' of which ',i3,
     & ' form faces of cubes, and ',i3,' are not part of cubes')
	else if(nccub.eq.ncyc) then
	   print 12,ncyc
	   if(discprt) write(8,12) ncyc
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
	print 14,ncyc-nccub
	if(discprt) write(8,14) ncyc-nccub
14	format(/,' The ',i3,' cycles that are NOT faces of cubes ')
	print 16
	if(discprt) write(8,16)
16	format(' ------------------------------------------------',/)
c Locate the cycles that are not part of cubes
	do i=1,ncyc
	   do j=1,nccub	!is cycle #i in a cube?
		if(i.eq.itemp(j)) goto 94	!next cycle
	   enddo
c      cycle #i is not in a cube so define MR for it
	   print 123,i,nsc(i)
c123	   format(' Cycle #',i3,/,
c     &  ' Number of states [',i2,'] = ')
	   call INPUTi(nsc(i))
c
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
c	   isetmr(i)=i	!set actual order later
c17	   print 123,i,nsc(i)
	   print 123,i,nsc(i)
c123	   format(' Cycle #',i3,/,
c     &     '   Number of states [',i2,'] = ')
	   call INPUTi(nsc(i))
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
	   print 13,(icyc(j),j=1,nsc(i))
	   if(discprt) write(8,13) (icyc(j),j=1,nsc(i))
c13	   format('&   states: ',20i3)
	   print 21, i,nedge(i)
	   if(discprt) write(8,21) i,nedge(i)
c21	   format(
c     & ' Number of shared edges cycle # ',i3,' = ',i3,/)
	enddo
	call SORTindi(nedge,isetmr,ncyc-nccub,ndim)
	call IVECREV(isetmr,ncyc-nccub,ndim)	!so cyc with most shared edges set first
	print 181,(isetmr(j),j=1,ncyc-nccub)
	if(discprt) write(8,181) (isetmr(j),j=1,ncyc-nccub)
181	format(
     & ' Non-cube cycles set in order of dec number of shared edges:',/,
     & 10(2x,5i4))
	print 108			!new line
	if(discprt) write(8,108)
c
c
c Now set the mr values for the non-cube cycles
	do L=1,ncyc-nccub
	   i=isetmr(L)		!cycle number in order set above
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!define states for current cycle
	   enddo
171	   print 1231,i
c1231	   format(/,' Cycle #',i3)
	   print 13,(icyc(j),j=1,nsc(i))
c	   if(discprt) write(8,13) (icyc(j),j=1,nsc(i))
c13	   format('&   states: ',20i3)
	   if(useini) then
		i1=im1(i,1)		!default
		j1=jm1(i,1)
	   else
		i1=im(i,1)		!default
		j1=jm(i,1)
	   endif
c
	   if(allmr) then
		obeymr(i)=.true.
		ans='N'
		if(automr(i)) ans='Y'
		call DCASK(
     & ' Is MR guaranteed by constraints on adjacent cycles',ans,ans)
		automr(i)=ans.eq.'Y'
	   else
		if(obeymr(i).and.(.not.automr(i))) then	!set default
		   ians=1
		else if(automr(i)) then
		   ians=2
		else
		   ians=3
		endif
		if(ncyc.lt.4) then	!automr not possible
		   print 65,i
c65		   format(
c     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   obeymr(i)=ans.eq.'Y'
		   automr=.false.
		else
		   print 651,i,ians
c651		   format(
c     &    ' Cycle ',i3,':',/,
c     &    '  (1) set to obey micro rev',/,
c     &    '  (2) obeys micro rev via constraints on adjacent cycles ',/,
c     &    '  (3) does NOT obey micro rev',/,
c     &    '  Option number [',i2,'] = ')
		   call INPUTi(ians)
		   if(ians.eq.1) then
			obeymr=.true.
			automr=.false.
		   else if(ians.eq.2) then
			obeymr=.true.
			automr=.true.
		   else if(ians.eq.3) then
			obeymr=.false.
			automr=.false.
		   endif
		endif
	   endif
251	   continue		!jump here to try again if invalid values given
	   if(obeymr(i).and.(.not.automr(i))) then
		print 67,i1,j1
c67		FORMAT(
c     &'    Define the route, q(i,j), to be calculated by micro rev:',/,
c     &'     Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
	   else
		goto 90	!go to next cycle
	   endif
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
c	subroutine FINDMR(i1,j1,ncyc,nsc,icyc,im,jm,obeymr,idest) replaces code below
c	   icy=i		!current cycle number (isetmr(i)=i in this case)
	   icy=L		!current cycle number (isetmr(L)=i in this case)
	   call FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest) !replaces code below
	   if(idest.eq.17) goto 171
	   if(idest.eq.25) goto 251
91	   continue	   !next cycle: skip here if no m.r. constraint for current cycle
c
	   done(i)=.true.		!everything set for cycle #i
c
	   imode=1
	   icube=0	!ignored when imode=1
	   call PRTCYC(i,ncyc,nsc,im,jm,icube,imode)
c
90	   continue	   !next cycle: skip if cycle is part of cube and so done below
	enddo		!end of defining MR for cycles not in cubes
c
c NOW DEFINE MR FOR ALL CYCLES THAT ARE PART OF CUBES
c
101	continue !jump here if all cycles are parts of cubes
	print 15,nccub
	if(discprt) write(8,15) nccub
15	format(/,' The ',i3,' cycles that are faces of cubes ')
	print 16
	if(discprt) write(8,16)
c16	format(' ------------------------------------------------',/)
	do n=1,ncube	!set cube with fewest external faces first
	   print 129,n
129	   format(/,' Cube #',i3)
	   set5=.true.	!if 1st 5 cycles all obey MR
	   do m1=1,6
		i=cubecyc(n,m1)	!i=cycle number
		if(done(i)) then	!cycle i already set in previous cube
		   print 128,m1,i,n
128		   format(
     & ' (',i3,'): cycle #',i3,' (cube #',i3,') already set',/)
		   goto 93
		endif
c
		is=is+1
		isetmr(is)=i	!define pointer
c
		if(m1.eq.6) then
		   if(set5) then
			obeymr(i)=.true.
			automr(i)=.true.	!for 6th face
			print 125,m1,i,n
125			format(' (',i3,'): cycle #',i3,' (cube #',i3,')',/,
     &    ' MR defined automatically by first five cycles in this cube')
			goto 93 	!next cycle ie end
		   endif
		endif
172		print 124,m1,i,n,nsc(i)
124		format(' (',i3,'): cycle #',i3,' (cube #',i3,')',/,
     &     ' Number of states [',i2,'] = ')
		call INPUTi(nsc(i))
		do j=1,nsc(i)
		   icyc(j)=im(i,j)	!states for current cycle
		enddo
		print 13,(icyc(j),j=1,nsc(i))
c13		format('   states: ',20i3)
		if(useini) then
		   i1=im1(i,1)		!default
		   j1=jm1(i,1)
		else
		   i1=im(i,1)		!default
		   j1=jm(i,1)
		endif
		if(allmr) then
		   obeymr(i)=.true.
		   automr(i)=.false.	!for 1st 5 faces
		else
		   if(obeymr(i)) then	!set default
			print 652,i
652			format(
     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
			ans='Y'
		   else
			print 66,i
66			format(
     &	' Should cycle #',i2,' be constrained by micro rev [N] ? ')
			ans='N'
		   endif
		   call INPUTa(ans)
		   obeymr(i)=ans.eq.'Y'
		   automr(i)=.false.	!for 1st 5 faces
		endif
252		continue		!jump here to try again if invalid values given
		if(obeymr(i)) then
		   print 67,i1,j1
c67		   FORMAT(
c     &'  Define the route, q(i,j), to be calculated by micro rev:',/,
c     &'   Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
		else
		   goto 92	!go to next cycle
		endif
c
c Must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
c===		icy=i		!current cycle number
		icy=is 	!index, such that cycle number=isetmr(is)
		call FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest)
		if(idest.eq.17) goto 172
		if(idest.eq.25) goto 252
92	      continue		!skip here if no m.r. constraint for current cycle
c Check if first 5 all obey MR, despite allmr not being true
		if(m1.le.5) then
		   if(.not.obeymr(i)) set5=.false.	!false if ANY if 1st 5 not mr
		endif
93		continue
c
		done(i)=.true.		!everything set for cycle #i
c
		imode=2
		icube=n	!ignored when imode=1
		call PRTCYC(i,ncyc,nsc,im,jm,icube,imode)
	   enddo		!next cycle, i=1,6
	enddo		!n=1,ncube
c
c Print the values
c
99	continue
c Printing now done in PRTCYC, below
cc Print the values
cc Printing done in order in which cycles are set (desirable?) in this version
cc but cycle numbering not changed
c	do L=1,ncyc
c	   print 108			!new line
c	   if(discprt) write(8,108)
c	   n=isetmr(L)	!cycle number
c	   print 126, L,n
c	   if(discprt) write(8,126) L,n
c126	   format(' (',i3,'): cycle # ',i3)
c	   if(obeymr(n)) then
c	      if(automr(n)) then
c		   print 1271,(im(n,m),jm(n,m),m=1,nsc(n))
c		   if(discprt) write(8,1271)(im(n,m),jm(n,m),m=1,nsc(n))
c1271		   format(' Micro rev guaranteed by adjacent cycles',/,
c     &		2(5(2i3,4x),/))
c		else 	!MR set in the normal way
c		   print 1311,im(n,1),jm(n,1)
c		   if(discprt) write(8,1311)im(n,1),jm(n,1)
c1311		   format(2i3,'  (calc by micro rev)')
c		   print 127,(im(n,m),jm(n,m),m=2,nsc(n))
c		   if(discprt) write(8,127)(im(n,m),jm(n,m),m=2,nsc(n))
c127		   format(2(5(2i3,4x),/))
c		endif
c	   else
c		print 1312,im(n,1),jm(n,1)
c		if(discprt) write(8,1312)im(n,1),jm(n,1)
c1312		format(2i3,'  (no micro rev constraint)')
c		print 127,(im(n,m),jm(n,m),m=2,nsc(n))
c		if(discprt) write(8,127)(im(n,m),jm(n,m),m=2,nsc(n))
c	   endif
c	enddo		!end of L=1,ncyc
cc
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
	common/mr3/extcyc
	logical discprt
	common/dp/discprt
c
c Print the values
c Printing done in order in which cycles are set (desirable?) in this version
c but cycle numbering not changed
	n=icyc	!actual cycle number
c==	do L=1,ncyc
	   print 108			!new line
	   if(discprt) write(8,108)
108	format(/)
	   if(imode.eq.2.and.ncube>0) then
		print 124,icube,cubext(icube)
	      if(discprt) write(8,124) icube,cubext(icube)
124		format(' Cube #',i3,'(',i3,' external faces)')
		if(extcyc(n)) then
	         print 125, n
	         if(discprt) write(8,125) n
125	         format(' Cycle # ',i3,' external')
		else
	         print 126, n
	         if(discprt) write(8,126) n
126	         format(' Cycle # ',i3,' internal')
		endif
	   else
	      print 127, n
	      if(discprt) write(8,127) n
127	      format(' Cycle # ',i3)
	   endif
	   if(obeymr(n)) then
	      if(automr(n)) then
		   print 1271,(im(n,m),jm(n,m),m=1,nsc(n))
		   if(discprt) write(8,1271)(im(n,m),jm(n,m),m=1,nsc(n))
1271		   format(' Micro rev guaranteed by adjacent cycles',/,
     &		2(5(2i3,4x),/))
		else 	!MR set in the normal way
		   print 1311,im(n,1),jm(n,1)
		   if(discprt) write(8,1311)im(n,1),jm(n,1)
1311		   format(2i3,'  (calc by micro rev)')
		   print 128,(im(n,m),jm(n,m),m=2,nsc(n))
		   if(discprt) write(8,128)(im(n,m),jm(n,m),m=2,nsc(n))
128		   format(2(5(2i3,4x),/))
		endif
	   else
		print 1312,im(n,1),jm(n,1)
		if(discprt) write(8,1312)im(n,1),jm(n,1)
1312		format(2i3,'  (no micro rev constraint)')
		print 127,(im(n,m),jm(n,m),m=2,nsc(n))
		if(discprt) write(8,127)(im(n,m),jm(n,m),m=2,nsc(n))
	   endif
c==	enddo		!end of L=1,ncyc
c
	RETURN
	end

