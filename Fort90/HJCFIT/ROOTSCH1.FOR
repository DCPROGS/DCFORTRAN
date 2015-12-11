ROOTSCH1 has original code for grouped roots -not very successful!
===============
	subroutine ROOTSCH(DETWX,kX,eigen,rootX,s1,s2,nroot,kXm)
c To locate roots of det[W(s)]=0 by searching through graph, and outputting
c suitable starting guesses for each root in s1(m),s2(m), the upper and
c lower limits for BISECTion, done after this call.
c 	In the call the function DETWX is replaced by DETWA or DETWF, to
c calculate det[W(s)] for open, shut respectively, and kX is kA or kF resp.
c	 On exit nroot=number actually found.
c
c Modified 02/13/03 03:59pm by adding
c
c (1) npmax, np0max to prevent extreme slowness (and rounding errors) when
c number of points gets too big
c
c (2) Try index of stability -e.g. stable if no call to rootsch has been
c made in the last 100 iterations?
c
c (3) Once stable, the width of the guesses could be made much narrower
c than at present (when roots well separated) -should save time in bisection
c
c (4) Problem of repeated roots was serious in first attempt -fastest 4 became
c     near-identical.  In this case must define a criterion for 'identical'
c	and then treat these  4 roots as identical -find rootF(1) say, and
c	then set rootF(2), (3) and (4) to same value without further searching
c	-just go on to look for the other 12 (when kF=16).  If another 12
c	not found then must redo whole search without assuming any identical.
c	Problem is that in course of iteration, these roots could drift apart
c	again, so need to repeat the full search every 100 (or so) iterations
c	anyway.
c
c========
c
c Modified 02/07/03 05:13pm for new sort of search based on local estimate
c of slope (grouped search does not work well enough at least with the numbers
c and model used -just abandon it?).
c Do the first coarse search using pairs of closely-spaced points
c rather than single s values, so have good estimate of the slope at each
c search point as well as of function value (how close can pairs be without
c getting rounding error problems?  Check interval between slast and s in
c coarse search
c (1) If sign of detw(s) changes, then there must be an odd number of roots
c	between slast and s (possibly including one or more pairs of duplicate
c	roots.  This is true whether or not there slope has different sign
c	at slast and s, so slope irrelavant -set ODD=true, =>1 root present
c (2) If  slope has DIFFERENT
c	sign at slast and s, this gives useful info only when sign of detw(s)
c	is SAME at slast and s.  In this case there must be an even number of
c	roots between slast and s (possibly including one or more pairs of
c	duplicate roots) -set EVEN=true BUT guarantees only => 0 roots present
c	(the interval between slast and s might contain only a min or max
c NB if odd=true then even=false and vice versa, but both may be false
c
c If either of these occurs then do fine search over region from slast and s
c to look for roots (detw changes sign). If slope changes sign but
c no change of sign of function can be detected then assign a double root
c at position of the min value of function which must just touch the
c x axis in this case).
c This method use when SLOPSCH=true
c
c
c Modified 01/31/03 08:37am for special search when roots occure in groups
c of similar values (as judged by values found in last iteration in which all
c kX values were found).
c
c Modified 01/21/02 08:57pm so that if determinant overflows then leave
c  at once with nerr=10
c
c Modif 01/25/00 08:16am so that s values used for search aare equally spaced
c on a log scale if logsch=T, rather that on arithmetic scale
c (see rootsch.mcd)
c
c   Modif 10/09/92 12:00pm so that arguments include both EIGEN (eigenvalues
c of current QXX) AND rootX=roots found on previous call.  Former are used
c for initial guesses the first time (do this by calling with eigen in call for
c both eigen AND rootX). Latter used subsequently for the first
c search, but if have to re-search with reduced step size then use EIGEN
c again -in early stages, where param are changing substantially, the previous
c roots may be poor guesses, though they should be good in later stages.
c Problem with this procedure is that occasionally an eigenvalue may become
c huge (e.g. if micro-rev parameter is calc with a rate in the denom that
c simplex sets to near-zero).  If this happens, search may become very slow.
c
c   Modif 10/09/92 12:00pm so that calculates slope of the graph too, and
c looks for maxima/minima.  Idea was that in cases where it cannot find all
c the roots there might be some (putatively) complex roots indicated e.g. by
c a minimum that occurred at Y>0.  No such examples found yet though, but
c max/min printed out (if no help, all code relating to slope can be removed
c to speed it up a bit).  Also prints value of s0 at which search starts
c (option to alter this removed for now)
c At present likilihood is set to 0 in HJCLIK if all roots not located (but
c could also restart SIMPLEX at its prev best value, as is now done if QXX gives
c ridiculous eigenvalues in HJCASYMP)
c   Modif 03/21/92 12:30pm so doesn't step back too far if s reset to zero
c (records slast).  Also fixed so works with kX=1
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c	real*4 xs
	real*8 s1(100),s2(100)	!lower and upper guesses for subsequent bisection
	allocatable::x1,x2,n1,n2,rootG
	integer*4 n1(:),n2(:)
	real*8 x1(:),x2(:),rootG(:)	!ditto for grouped roots
	real*8 eigen(kXm)		!=eigAA,eigFF, or previous rootA,rootF in call
	real*8 rootX(100)
	logical discprt,last,caplock,debug,logsch
c	character*1 ans,UC
	common/detw2/tres,km,nerr				   !for DETWA,DETWF
	logical debprt,dprt
	common/deb1/debprt
	common/dp/discprt
c for search when roots are grouped
	logical checkgrp,grouped
	integer ngrp(100)		!number of roots in the ith group
	real*8 rgmin(100),gmean(100)
	common/grp/checkgrp,grouped,ngrp,ngroup,rcrit,rgmin,gmean
c For new search method based on pairs of point to get accurate local slope
	logical slopsch,odd,even,odd0,even0
	logical bothpos,bothneg,min,max,bothpos0,bothneg0,min0,max0
c
	EXTERNAL detwx
c
c Start search at fastest (most negative) s value, say at 1.5*eigen(1) (ie
c eigenvalue of -QAA or -QFF, the roots for tres=0).  Base the step size
c for s on the intervals between eigen(m) values (if these closely spaced then
c use smaller steps because the roots for tres>0 will probably be closely
c spaced too).  Divide this interval into FAC parts to get step size.
c If the full number of roots, kX (=kA or kF in the call) is not found then
c reduce the step size by a factor REDFAC and begin again.
c
	debug=caplock()	!not function -set on entry to ROOTSCH
c
c Modif 01/30/03 04:33pm to check if shut time roots are occurring
c in groups of closely spaced values (only if indmod=true ?)
c Set checkgrp=true (earlier) if this is to be attempted
c

	zero=0.0d0
	slopsch=.true.		!for now
c	checkgrp=.true.		!for now
	checkgrp=.false.		!for now
c
c check for groups as in tgrp.for
c rgmin(n) smallest ratio between roots in each group (if ngrp(n) > 1)
c to fix step size for search in rootsch
	if(checkgrp) then
	   call GETGROUP(rootX,kX,kXm)	!most args in common/grp/
	endif
c	if(grouped) logsch=.true.
	logsch=.true.	!always true now
c	logsch=.false.
	dprt=discprt.and.debprt	 !put problems in root-finding on disc as well as screen
	if(grouped) allocate(x1(100),x2(100),n1(100),n2(100),rootG(100))
	if(.not.logsch) goto 100	!sep section
c
c START OF SLOPE SEARCH
c TO DO
c (1) check for root in the small step between s and s+delta
c (2) check for duplicate root during fine search

	if(slopsch) then
	   fac2=2.0d0
	   fac3=10.d0
	   slo=fac2*rootX(1)		!start very negative
	   if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
	   if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
c	   shi=-0.00001d0			!100 000 sec
	   shi=rootX(kX)/fac3		!slowest root
	   ired=0	!counts no of times step size reduced for whole range search
c===	   nredmax=3	!gives up if still can't find all roots after n reductions
c===	   nredmax=5	!gets too slow after 5th reduction
	   nredmax=4	!gives up if still can't find all roots after n reductions
	   nredmax0=3	!ditto for fine search
	   npmax=100000	!upper limit of number of search points for coarse search
	   np0max=30000	!ditto for fine search
	   npfac=5		!factor by which # of points increased after failure
	   ratmin=1.d238
	   do i=1,kX-1
		rat=rootX(i)/rootX(i+1)		!ratio is postitive
		if(rat.lt.ratmin) ratmin=rat
	   enddo
c     calc number of points needed
c	   np=1 + int4(dlog10(-slo)-dlog10(-shi))/ratmin
c Corrected 02/03/03 04:40pm
	   rat=ratmin
	   if(rat.lt.1.1) rat=1.1	!so np not too huge first time
	   np=1 + int4((dlog10(-slo)-dlog10(-shi))/dlog10(rat))
	   if(np.lt.50) np=50	!for safety
	   if(np.gt.npmax) np=npmax	!for safety
c          calculate increment in log(-s) needed to achieve this
c	   dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)
c
	   s=slo
	   slast=s
	   istep=0			!to count steps
	   debug=caplock()	!not function -set on entry to ROOTSCH
c SLOPESCH is called to do search from slo to shi on log scale, with np points
c spaced logarithmically, using local estimation of slope
c NB need not start with istep=0 so can resume search from specified step
c Covers range istep+1, istep+2, . . . ,np
c Returns with
c
c Calculate the current slope by getting another point close to the current
c value of s (rather than using the next s value which is much further away)
c Try separation of points by 0.1 ms? But this is too much for fastest rates
c so make proportional to s?
c eg 1 mus at tau=10 mus
c    10 mus at tau=100 mus
c    0.1 mus at tau=1 ms
c    0.1 mus at tau=1 ms . . .
c    100 ms  at tau= 1 s
c sign=sign of last detw(s) (-1, 0 or +1)
c sign1=sign of current detw(s)
c slope=sign of last slope
c slope1=sign of current slope
c Check for change in sign of detw(s)
c If sign changes must have odd number of roots since last s value (and poss
c some pairs of identical roots too). So do fine search between these s values
c to define s1(nroot) and s2(nroot) as guesses for bisection in hjcasymp
c
	   nroot=0	!number of roots found
81	   continue		!return here to continue or restart coarse search
c	   print 82,s,shi,np,ired		!print now in  SLOPESCH
c	   if(dprt) write(8,82) s,shi,np,ired
c82	   format(
c     & ' Coarse search from s = ',g13.6,' to ',g13.6,
c     &  ' ( ',i5,' points; step red ',i2,')')
c code with LOG-SPACED s VALUES -NORMAL SEARCH
	   call SLOPESCH(DETWX,slo,shi,np,istep,
     &      slast,s,detlast,det,sloplast,slope,
     &	slast1,slast2,dmin,dmax,dprt,ired,'Coarse')
	   if(nerr.eq.10) then	!determinant overflowed
		goto 999		!deallocate and RETURN
	   endif
c odd=true if sign of detw(s) changes between slast and s
	   odd=((detlast.gt.zero).and.(det.lt.zero)).or.
     &	  ((detlast.lt.zero).and.(det.gt.zero)).or.
     &	  (detlast.eq.zero.or.det.eq.zero)
	   bothpos=(detlast.gt.zero).and.(det.gt.zero)	!det(slast) and det(s) both pos
	   bothneg=(detlast.lt.zero).and.(det.lt.zero)	!det(slast) and det(s) both pos
	   min=(sloplast.lt.zero).and.(slope.gt.zero)
	   max=(sloplast.gt.zero).and.(slope.lt.zero)
	   even=(bothpos.and.min).or.(bothneg.and.max)
c NB if odd=true then even=false and vice versa, but both may be false (if
c neither detw nor slope changes sign -though sadly this does not guarantee
c there are no roots between slast and s)
c If slope changes sign between s1 and s2, there must must be (one or more)
c min/max between s1, s2.  If there is a minimum and det(s1) and det(s2) are
c both positive, then the minimum MUST cross, or at least touch, the x axis
c  If odd=t there is a change in sign of detws(s) so at least one root found
c  If bothpos or bothneg= true there is NO change in sign of detw(s) (so
c   neither can be true if odd0 is true)
c  If even0 is true then there MUST be an even number of roots (could be
c   be one or more pairs of duplicate roots.
c
c  If SLOPESCH returns that change in sign of detw(s) or slope has been
c  found then do FINE SEARCH between slast and s now
c       Initial fine step size=***, then reduce until an odd/even number
c       of roots is found (not nec all though, a pair could be missed)
c       If sign change in slope is found with no roots in between, then
c       assign two identical roots at the min.
c NB don't want to change s,slo,shi, np, dlogs for main search while
c  doing local search
c
	   nrootsav=nroot		!save current value, before fine search started
	   if(odd.or.even) then
		slo0=slast	!interval for local search
		shi0=s
		np0=1 + int4((dlog10(-slo0)-dlog10(-shi0))/dlog10(ratmin))
		if(np0.lt.100) np0=100 	!on first trial got np0=1 here!
		if(np0.gt.np0max) np0=np0max	!for safety
		istep0=0
		nr=0		!count number of roots found between slast and s
		ired0=0		!counts step reductions for local search
80		continue	!return to continue search
		call SLOPESCH(DETWX,slo0,shi0,np0,istep0,
     &       slast0,s0,detlast,det,sloplast,slope,
     &	 slast1,slast2,dmin,dmax,dprt,ired0,'  Fine')
		if(nerr.eq.10) then	!determinant overflowed
		   goto 999		!deallocate and RETURN
		endif
c odd0=true if sign of detw(s) changes between slast0 and s0
		odd0=((detlast.gt.zero).and.(det.lt.zero)).or.
     &	     ((detlast.lt.zero).and.(det.gt.zero)).or.
     &	     (detlast.eq.zero.or.det.eq.zero)
		bothpos0=(detlast.gt.zero).and.(det.gt.zero)	!det(slast) and det(s) both pos
		bothneg0=(detlast.lt.zero).and.(det.lt.zero)	!det(slast) and det(s) both pos
		min0=(sloplast.lt.zero).and.(slope.gt.zero)
		max0=(sloplast.gt.zero).and.(slope.lt.zero)
		even0=(bothpos0.and.min0).or.(bothneg0.and.max0)
c
c  If a root found, define s1() and s2() now and then continue fine search until
c   all np0 points searched -if an even/odd number of roots is not found
c   then make fine search finer.
c  If odd=t there is a change in sign of detws(s) so at least one root found
c  If bothpos or bothneg= true there is NO change in sign of detw(s) (so
c   neither can be true if odd0 is true)
c  If even0 is true then there MUST be an even number of roots (could be
c   be one or more pairs of duplicate roots.
c  Thus the three possibilities below for root detection are mutually
c  exclusive
c
c If fine search is restarted with small step size, then must reset nroot
c to what it was before fine search started (nrootsav), or some of the same
c roots will be found again
c
	      if(odd0) then
		   nr=nr+1
		   nroot=nroot+1
		   s1(nroot)=slast0
		   s2(nroot)=s0
		   if(detlast.eq.zero) then
			s1(nroot)=slast0 - 0.001d0*dabs(slast0)
		   endif
		   if(det.eq.zero) then
			s2(nroot)=s0 + 0.001d0*dabs(s0)
		   endif
		   print 84,nroot,kX,s1(nroot),s2(nroot),detlast,det
		   if(dprt) write(8,84) nroot,kX,s1(nroot),s2(nroot),
     &		detlast,det
84		   format(
     &    '    Found root ',i3,' out of ',i3,' between slast = ',g13.6,
     &    ' and s = ',g13.6,/,
     &    '       detW(slast) = ',g13.6,', detW(s) = ',g13.6)
		   if(nroot.eq.kX) goto 1	!all roots found
		else if(bothpos0.and.min0) then
		   print 86,slast0,s0,dmin,detlast,det,sloplast,slope
		   if(dprt) write(8,86) slast0,s0,dmin,detlast,det,
     &		sloplast,slope
86		   format(
     &  ' Minimum between s = ',g13.6,' and ',g13.6,': dmin = ',g13.6,/,
     &   '    detW(s) = ',2g13.6,', slope = ',2g13.6)
c===   assign duplicate root?
		else if(bothneg0.and.max0) then   !root(s) found between slast0 and s0
		   print 83,slast0,s0,dmax,detlast,det,sloplast,slope
		   if(dprt) write(8,83) slast0,s0,dmax,detlast,det,
     &		sloplast,slope
83		   format(
     &  ' Maximum between s = ',g13.6,' and ',g13.6,': dmax = ',g13.6,/,
     &   '    detW(s) = ',2g13.6,', slope = ',2g13.6)
c===   assign duplicate root?
		endif
c	    Carry on with fine search after point where root found, if not
c		reached shi0
c         NB next value of s in slopesch is calculated from istep and slo
c         so will carry on to next s if istep not zeroed and slo not changed
		if(istep0.lt.np0) then
		   goto 80	!carry on with fine search
		endif
c     Reach here when fine search completed between slast and s (coarse
c	searc), and all the roots in this region have been found
c	(all that can be with current step size). Check if the number found is
c	odd and =>1 shen odd=true.  Check if even (nad =>0) when even=true,
c	as expected. If not, reduce step size and repeat same fine search (between
c	same slast,s. Otherwise go on with coarse search
c
c    If fine search left before all np0 points searched, return to finish it
c    from the current istep0 (that is what dictates first s value used for search)
		if(istep0.lt.np0) then
c		   s=shi0		!carry on with course search from this s value
		   goto 80 		!carry on with  search
		endif
c  If number of roots found in fine search is not odd/even as expected
c  then reduce step size for fine search and start again at beginning
		if((odd.and.(mod(nr,2).ne.1.or.nr.eq.0)).OR.
     &	  (even.and.mod(nr,2).ne.0)) then
		   ired0=ired0+1
		   if(ired0.le.nredmax0.and.np0.lt.np0max) then
			np0=np0*npfac
			istep0=0
			nroot=nrootsav		!so same roots not found again
			goto 80
		   endif
		endif
	   endif	!end of if(odd.or.even)
c End of fine search between slast and s
c
c Carry on with coarse search if istep < np, otherwise redo coarse search
c with smaller steps
	   if(nroot.eq.kX) goto 1	!all roots found
	   if(istep.lt.np) then
		s=shi0		!carry on with course search from this s value
		goto 81	!carry on with course search
	   else
	      ired=ired+1
		if(ired.le.nredmax.and.np.lt.npmax) then
		   np=np*npfac
		   if(np.gt.npmax) np=npmax	!for safety
		   istep=0
		   nroot=0	!start all over again!
c            next line not needed -next s calculated from slo and istep
c		   s=slo	!start from most neg value again
		   slo=slo*fac2	!make lowest point more neg, in case
		   shi=shi/fac2	!make highest point less neg, in case
		   goto 81
		else
		   print 74,nredmax
	         if(dprt) write(8,74) nredmax
c74		   format(' Gave up after ',i3,' reductions')
		   goto 999		!deallocate and RETURN
		endif
	   endif
	endif		!end of slopesearch method
c What happens if we reach here without finding all roots?
	print 85
	if(dprt) write(8,85)
85	format(
     & ' ***Reached end of slopesch section without finding all roots')
	goto 999 	! ?or revert to 'crude' search in next sec?
c END OF SLOPE SEARCH
c
c  Usual (mow) plain search on log scale
c	fac2=1.5d0
	fac2=2.0d0
	fac3=10.d0
	slo=fac2*rootX(1)		!start very negative
	if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
c	shi=-0.00001d0			!100 000 sec
	shi=rootX(kX)/fac3		!slowest root
	ired=0	!counts no of times step size reduced
c===	nredmax=3	!gives up if still can't find all roots after n reductions
c===	nredmax=5	!gets too slow after 5th reduction
	nredmax=4	!gives up if still can't find all roots after n reductions
	npmax=30000	!upper limit of number of search points
c===	if(debug) then
        print 53,(eigen(j),j=1,kX)
        if(discprt) write(8,53) (eigen(j),j=1,kX)
53	  format(' Eigs of QXX:',25g12.5)
        print 531,(rootX(j),j=1,kX)
        if(discprt) write(8,531) (rootX(j),j=1,kX)
531	  format(' Prev roots:',25g12.5)
c===	endif
c Calculate step size. For log-spaced values, calculate the  initial
c step size so that values would encompass all the values in current
c rootX, i.e. base it on the smallest ratio between succesive roots
c (see rootsch.mcd)
	npfac=5		!factor by which # of points increased after failure
	if(grouped) then
	   nroot=0
	   goto 30		!special section for grouped roots
	endif
c
	if(kX.eq.1) then
	   np=100	!arbitrary
	   dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)
	else
	   ratmin=1.d238
	   do i=1,kX-1
		rat=rootX(i)/rootX(i+1)		!ratio is postitive
		if(rat.lt.ratmin) ratmin=rat
	   enddo
c     calc number of points needed
c	   np=1 + int4(dlog10(-slo)-dlog10(-shi))/ratmin
c Corrected 02/03/03 04:40pm
	   np=1 + int4((dlog10(-slo)-dlog10(-shi))/dlog10(ratmin))
	   if(np.lt.50) np=50	!for safety
c          calculate increment in log(-s) needed to achieve this
	   dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)
	endif
c
5	continue		!return here to restart with smaller steps
	nroot=0	!number of roots found
	if(grouped) goto 30
c
	if(.not.logsch) goto 100	!sep section
c
c code with LOG-SPACED s VALUES -NORMAL SEARCH
c
	if(ired.gt.0) then
c===         print 72, slo,shi,np
c===         if(dprt) write(8,72) slo,shi,np
72	   format(
     & ' Search from s = ',g13.6,' to ',g13.6,' # of points = ',i8)
	endif
	s=slo
	istep=0			!to count steps
	det=DETWX(s)
	if(nerr.eq.10) then	!determinant overflowed
	   goto 999		!deallocate and RETURN
	endif
c
	if(det.gt.0.0d0) then
	   isign=1
	else if(det.eq.0.0d0) then
	   isign=0
	else if(det.lt.0.0d0) then
	   isign=-1
	endif
c For debug (only?) calculate slope also to look for max/min.  For this
c purpose take one step more before starting the loop, to get an initial
c value for the slope
	slast=s
	detlast=det
	istep=istep+1
	s=slo/(10**(dfloat(istep)*dlogs))
	if(s.gt.-0.00001d0) s=-0.00001d0   		!100 000 sec
	det=DETWX(s)
	if(nerr.eq.10) then	!determinant overflowed
	   goto 999		!deallocate and RETURN
	endif
	slop=(det-detlast)/(s-slast)
c Now start loop
	last=.false.
	debug=caplock()	!not function -set on entry to ROOTSCH
c
2	continue		!return here for next step in s value
	slast=s		!record value of previous s value
	sloplast=slop		!record value of previous slope
	detlast=det		!record value of previous determinant
c
	istep=istep+1
	s=slo/(10**(dfloat(istep)*dlogs))
	if(last) then	!last step in s for current step length
         print 4,nroot,kX,ired+1
         if(dprt) write(8,4) nroot,kX,ired+1
4	   format(
     & 	' Only ',i2,' roots out of ',i3,
     & 	' found -step size reduction # ',i2)
	   if(debug) then
		do m=1,nroot
		   print 7,m,s1(m),s2(m),DETWX(s1(m)),DETWX(s2(m))
		   if(dprt) write(8,7) m,s1(m),s2(m),DETWX(s1(m)),
     &		DETWX(s2(m))
		enddo
	   endif
	   ired=ired+1
	   if(ired.gt.nredmax) then
	      print 74,nredmax
	      write(8,74) nredmax
74		format(' Gave up after ',i3,' reductions')
		print 532,nroot,(s1(m),m=1,nroot)
		if(discprt) write(8,532) nroot,(s1(m),m=1,nroot)
532		format(' Found ',i2,/,' lower guess:',25g12.5)
		print 533,(s2(m),m=1,nroot)
		if(discprt) write(8,533) (s2(m),m=1,nroot)
533		format(' upper guess:',25g12.5)
		goto 999		!deallocate and RETURN
	   endif
	   np=np*npfac		!more points
	   dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)	!smaller steps
c=====is next bit sensible?
c=== leave for now, for comparison purpose
c====REMOVED 02/03/03 10:27am
c	   if(ired.eq.1) then
c		slo=eigen(1)		!use eigs rather than prev root after 1st try
c		if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
c	   endif
	   slo=fac2*slo	!start MORE negative if restart with smaller steps
	   goto 5			!restart from beginning (s=slo)
	endif
	if(istep.eq.np.or.s.gt.0.d0) then
c===	   s=-1.0d-50	!reset to near-zero
	   last=.true.
	endif
	det=DETWX(s)
	if(nerr.eq.10) then	!determinant overflowed
	   goto 999		!deallocate and RETURN
	endif
	slop=(det-detlast)/(s-slast)
	if(debug.and.ired.gt.1) then
	   print 8,istep,s,det,slop
8	   format(' step = ',i8,' s, det(s), slope = ',3g13.6)
	endif
	if(det.gt.0.0d0) then
	   isign1=1
	else if(det.eq.0.0d0) then
	   isign1=0
	else if(det.lt.0.0d0) then
	   isign1=-1
	endif
c slop*sloplast can overflow in extreme cases so use DSIGN(1.,slop)=-1 if slop
c  s is negative, +1 if slop is 0 or positive
	sl1=dsign(1.d0,slop)
	sl2=dsign(1.d0,sloplast)
	if(.false..and.sl1*sl2.lt.0.0d0) then	!suppress print of min/max
c===	if(dprt.and.slop*sloplast.lt.0.0d0) then
	   if(sloplast.lt.0.d0.and.slop.gt.0.d0) then
      	print 731
      	write(8,731)
731		format(/,' Minimum found')
	   else
      	print 732
      	write(8,732)
732		format(/,' Maximum found')
	   endif
         print 73,istep-1,slast,sloplast,detlast,
     &	istep,s,slop,det
         write(8,73) istep-1,slast,sloplast,detlast,
     &	istep,s,slop,det
73	   format(
     &	' step # ',i8,': s, slope, det(s) = ',3g13.6,/,
     &	' step # ',i8,': s, slope, det(s) = ',3g13.6)
c	   pause
	endif
	if(isign.ne.isign1.or.isign1.eq.0) then	!crossed, or hit, a root
	   nroot=nroot+1
	   s1(nroot)=slast
	   s2(nroot)=s
	   if(isign1.eq.0) then
c=		s2(nroot)=s+step        !move past correct root, and bisect anyway!
		s2(nroot)=s + 0.05d0*dabs(s)
c=		s2(nroot)=slo/(10**(dfloat(istep+1)*dlogs))
	      if(s2(nroot).gt.0.d0) s2(nroot)=-1.0d-30	!reset to near-zero
	   endif
	   if(nroot.eq.kX) goto 1	!all roots found
	   isign=isign1			!prepare for next root
	endif
	goto 2		!next step in s
c end of code for log-spaced s values
c
c SEARCH FOR GROUPED ROOTS (with log-spaced s values)
30	continue
c
	facg=5.0d0	!factor by which smallest gap-bet roots is divided for init step
	facr=1.5d0	!factor by which range is multipied to define search region
	npfac=5	!factor by which # of points increased after failure
c First try to find one root (any) in each group by bisection with
c guess set halfway (log scale) between groups (as judged by previous
c iteration). This will get only one root per group so numbering won't
c correspond with the actual roots so keep them in x1,x2 rather than s1,s2
c Define n1(n), n2(n) = index of 1st and last root in nth group
	n1(1)=1
	do n=1,ngroup
	   n2(n)=0
	   do j=1,n
		n2(n)=n2(n)+ngrp(j)
	   enddo
c	   print *,n,n1(n),n2(n)
	   if(n.lt.ngroup) n1(n+1)=n2(n)+1		!first in next group
	enddo
c Find guesses between ends of groups in x1(n), x2(n).
c If the group has an even number of roots then both guesses must
c both give detw() with same sign, so can't be used for bisection to
c find one root in the group, but should be OK if there is an odd
c number of roots in the group.
c In former case, start at midpoint of group (from last iteration) and
c take med size steps (going out as far as x1(n), x2(n) if nec) to find
c a change of sign in detw(), then use bisection to find one root in the
c group.
	x1(1)=3.d0*rootX(1)	!lower lim for most negative root
	if(kX.eq.1) then
	   x2(1)=-0.00001d0	 !10000 sec
	else		!kX >1
	   x2(1)=-dsqrt(rootX(n2(1))*rootX(n1(2)))	!between last in group 1 and first in group 2
	endif
	if(kX.gt.1) then
	   do n=2,ngroup
		x1(n)=-dsqrt(rootX(n2(n-1))*rootX(n1(n)))
		if(n.lt.ngroup) then
		   x2(n)=-dsqrt(rootX(n2(n))*rootX(n1(n+1)))
		 else
		   x2(n)=-0.00001d0		!10000 sec
		 endif
	   enddo
	endif
c   Now find one root in each group using these guesses -keep in rootG(n)
	Y=0.0d0
	ndisp=-1
	if(debug) ndisp=1
	epsy=-1.d0	  !so ignored
	do n=1,ngroup
	   ng=ngrp(n)
c
	   if(mod(ng,2).ne.0) then	!odd number of roots in group
c
56		continue	!return to try again after bisection error
		epsx=1.d-8*dabs(x1(n))	!1.d-10 seems much the same
		x1sav=x1(n)
		x2sav=x2(n)
		call BISECd(DETWX,x1(n),x2(n),Y,xout,yout,epsx,epsy,
     &     nerr1,ndisp,.false.)
		if(iabs(nerr1).eq.1.or.iabs(nerr1).eq.2) then
		   print 41,n,ngroup,nerr1,x1sav,x2sav,
     &	     DETWX(x1sav),DETWX(x2sav)
		   if(dprt) write(8,41) n,ngroup,nerr1,x1sav,x2sav,
     &	     DETWX(x1sav),DETWX(x2sav)
c=======seems to give up here
c=======if fail to get one eigenvalue in each group better go to 5
c=======with grouped=F and do crude search?
c=======nerr=5 needs to be kept sep for each group?
41	         format(
     & ' Error in finding a root in group ',i3,' out of ',i3,
     & '; nerr = ',i3,/,
     & '  guesses, s1, s2 = ',2g13.6,/,
     & ' f(s1) = ',g13.6,',  f(s2) = ',g13.6,/,
     & '  Searching for roots again')
c		   newroot=.true.
c		   goto 56			!start again
		   print 207,n,ng
		   if(discprt) write(8,207) n,ng
207		   format(
     & ' ROOTSCH can''t find a root for group ',i3,' (',i2,' roots)')
		   nerr=5
		   goto 403		!next group
c		   goto 999		!deallocate and RETURN
		endif
		rootG(n)=xout
	   else	!even number of roots in group
c   start search at geometric mean of group and look for a sign change
c===try using actual range (from last iter) rather than max range?
		ired=0
	   	range=rcrit**(ngrp(n)-1)
		slo=gmean(n)*range
		shi=slo/range 	!(shi less negative)
		if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
		if(shi.gt.-0.00001d0) shi=-0.00001d0	!100 000 sec
		np=1 + int4((dlog10(-slo)-dlog10(-shi))/dlog10(rgmin(n)))
		if(np.lt.50) np=50	!for safety
		if(np.gt.npmax) np=npmax 	!prevent 10^9 points
c          calculate increment in log(-s) needed to achieve this
		dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)
		if(ired.gt.0) then
c===           print 72, slo,shi,np
c===            if(dprt) write(8,72) slo,shi,np
c72		format(
c     & ' Search from s = ',g13.6,' to ',g13.6,' # of points = ',i8)
		endif
55		continue		!return here to restart with smaller steps
		s=slo
		slast=s
		istep=0			!to count steps
		det=DETWX(s)
		if(nerr.eq.10) then	!determinant overflowed
		   goto 999		!deallocate and RETURN
		endif
c get initial sign of DETW()
		if(det.gt.0.0d0) then
		   isign=1
		else if(det.eq.0.0d0) then
		   isign=0
		else if(det.lt.0.0d0) then
		   isign=-1
		endif
c Now start loop
		debug=caplock()	!not function -set on entry to ROOTSCH
c
402		continue		!return here for next step in s value
c
		istep=istep+1
		slast=s
		s=slo/(10**(dfloat(istep)*dlogs))
		det=DETWX(s)
		if(nerr.eq.10) then	!determinant overflowed
		   goto 999		!deallocate and RETURN
		endif
		if(det.gt.0.0d0) then
		   isign1=1
		else if(det.eq.0.0d0) then
		   isign1=0
		else if(det.lt.0.0d0) then
		   isign1=-1
		endif
c if sign has changed then one root has been found in current group
c so bisect, put value in rootG(n) and jump out to locate rest of roots
c in this group. If get through all np points without finding a root
c then reduce step size and/or increase range
		if(isign.ne.isign1.or.isign1.eq.0) then	!crossed, or hit, a root
		   xlo=slast
		   xhi=s
		   epsx=1.d-8*dabs(xlo)		!1.d-10 seems much the same
		   x1sav=xlo
		   x2sav=xhi
		   call BISECd(DETWX,xlo,xhi,Y,xout,yout,epsx,epsy,
     &     		nerr1,ndisp,.false.)
		   if(iabs(nerr1).eq.1.or.iabs(nerr1).eq.2) then
			print 41,n,ngroup,nerr1,x1sav,x2sav,
     &	       DETWX(x1sav),DETWX(x2sav)
			if(dprt) write(8,41) n,ngroup,nerr1,x1sav,x2sav,
     &	        DETWX(x1sav),DETWX(x2sav)
c41	      	format(
c     & ' Error in finding a root in groups ',i3,' out of ',i3,
c     & '; nerr = ',i3,/,
c     & '  guesses, s1, s2 = ',2g13.6,/,
c     & ' f(s1) = ',g13.6,',  f(s2) = ',g13.6,/,
c     & '  Searching for roots again')
c			newroot=.true.
c		      goto 56			!start again
		      print 204,n
		      if(discprt) write(8,204) n
204		      format(
     &     		' ROOTSCH can''t find a root for group ',i3)
		      nerr=5
		      goto 403		!next group
c		      goto 999		!deallocate and RETURN
		   endif
		   rootG(n)=xout
		   goto 403		!go on to next group
		endif		!end of bisection
c   carry on with search until ONE root found for current group
		if(istep.eq.np) then	!last step in s for current step length
      	   print 206,n,ired+1
	         if(dprt) write(8,206) n,ired+1

206		   format(
     &	 	' One root not yet found for group ',i2,/,
     & 		' found -step size reduction # ',i2)
		   ired=ired+1
		   if(ired.gt.nredmax) then
		      print 74,nredmax
		      write(8,74) nredmax
c74			format(' Gave up after ',i3,' reductions')
			goto 999		!deallocate and RETURN
		   endif
		   np=np*npfac		!more points
		   if(np.gt.npmax) np=npmax
		   dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)	!smaller steps
		   slo=fac2*slo	!start MORE negative if restart with smaller steps
		   goto 55			!restart from beginning (s=slo)
		endif
		isign=isign1			!prepare for next root
		goto 402		!next step in s
	   endif	!end of odd/even number of roots
403	   continue
	enddo		!end of do n=1,ngroup
c At this point should have one root from each group in rootG(n)
c If a group has only one root, then it has already been found!
c If a group has more than one root, then look for the rest of them
c
c Next look for rest of the roots in the group using small steps
c Width of group (from last iteration is at most rcrit^(n-1)
c Look over fine mesh over this distance in
c both directions from the one root found in this group, rootG(n)
c use initial step size that is say, 1/5 of the smallest ratio between
c roots in the group at the last iteration, rgmin(n)
c   Search from (rootG(n) - facr*range) to (rootG(n) + facr*range)
c Jump out if all the roots expected in this group are found
c If there is only one rrot in a group it has already been found and is
c in rootG(n) so just copy to rootX(nroot) and go on to next group
	do n=1,ngroup
	   ng=ngrp(n)
	   if(ng.eq.1) then	!root already found
c===already found but this may not be right pos for it?  But if all roots
c already found in prev groups then should be OK
		nroot=nroot+1
		rootX(nroot)=rootG(n)
c         Define guesses for HJCASYMP (but bisection there should not be needed)
		s1(nroot)=rootX(nroot)-1.d-3
		s2(nroot)=rootX(nroot)+1.d-3
		print 77,n
77		format(
     &	' Guesses for the one root in group ',i2,' found')
		if(nroot.eq.kX) then
		   print 76,kX
c76		   format(
c     &	' Guesses for all ',i2,' roots found')
		   goto 1	!all roots found
		else
		   goto 202			!next group
		endif
	   endif
	   ngf=0		!number of roots found in current qroup
	   range=rcrit**(ngrp(n)-1)
	   step=rgmin(n)/facg
	   slo=rootG(n)*(facr*range)
	   shi=rootG(n) / (facr*range)
	   if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
c	   if(shi.gt.-0.00001d0) shi=-0.00001d0	!100 000 sec
c shi (least neg) must be below next group boundary
	   d=-dsqrt(rootX(n2(n))*rootX(n1(n+1))) !between last in group n and first in group n+1
	   if(shi.gt.d) shi=d
	   if(n.gt.1) then
		if(slo.lt.shilast) slo=shilast	!prevent rages overlapping
	   endif
c  Record limits of the range that is searched to avoid searching
c overlapping range for nexr group
c	   slolast=slo
	   shilast=shi
c      number of points needed
	   np=1 + int4((dlog10(-slo)-dlog10(-shi))/dlog10(rgmin(n)))
c===	   np=1 + int4(dlog10(-slo)-dlog10(-shi))/rgmin(n)
c	   if(np.lt.50) np=50	!for safety
c      calculate increment in log(-s) needed to achieve this
	   dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)
51	   continue		!return here to restart with smaller steps
	   if(ired.gt.0) then
		print 203,n, slo,shi,np
		if(dprt) write(8,203) n,slo,shi,np
203		format(' group ',i2,
     & ' Search from s = ',g13.6,' to ',g13.6,' # of points = ',i8)
	   endif
	   s=slo
	   istep=0			!to count steps
	   det=DETWX(s)
	   if(nerr.eq.10) then	!determinant overflowed
		goto 999		!deallocate and RETURN
	   endif
	   if(det.gt.0.0d0) then
		isign=1
	   else if(det.eq.0.0d0) then
		isign=0
	   else if(det.lt.0.0d0) then
		isign=-1
	   endif
	   slast=s
	   detlast=det
	   istep=istep+1
	   s=slo/(10**(dfloat(istep)*dlogs))
	   det=DETWX(s)
	   if(nerr.eq.10) then	!determinant overflowed
		goto 999		!deallocate and RETURN
	   endif
	   slop=(det-detlast)/(s-slast)
c Now start search loop
	   last=.false.
	   debug=caplock()	!not function -set on entry to ROOTSCH
	   ired=0
200	   continue		!return here for next step in s value
	   slast=s		!record value of previous s value
	   sloplast=slop		!record value of previous slope
	   detlast=det		!record value of previous determinant
c
	   istep=istep+1
	   s=slo/(10**(dfloat(istep)*dlogs))
	   if(last) then	!last step in s for current step length
	      print 201,ngf,ngrp(n),n,ired+1
      	if(dprt) write(8,201) ngf,ngrp(n),n,ired+1
201		format(
     & 	' Only ',i2,' roots out of ',i3,
     & 	' found in group ',i3,': step size reduction # ',i2)
		ired=ired+1
		if(ired.gt.nredmax) then
	         print 74,nredmax
	         write(8,74) nredmax
C74		   format(' Gave up after ',i3,' reductions')
		   if(n.lt.ngroup) then
			goto 202		!next group
		   else
			goto 999		!deallocate and RETURN
		   endif
		endif
	      np=np*npfac		!more points
		if(np.gt.npmax) np=npmax
	      dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)	!smaller steps
	      slo=fac2*slo	!start MORE negative if restart with smaller steps
c		nroot=0
	      goto 51			!restart from beginning (s=slo)
	   endif	!end of if(last)
	   if(istep.eq.np.or.s.gt.0.d0) then
		last=.true.
	   endif
	   det=DETWX(s)
	   if(nerr.eq.10) then	!determinant overflowed
		goto 999		!deallocate and RETURN
	   endif
	   slop=(det-detlast)/(s-slast)
	   if(debug.and.ired.gt.1) then
		print 8,istep,s,det,slop
c8		format(' step = ',i8,' s, det(s), slope = ',3g13.6)
	   endif
	   if(det.gt.0.0d0) then
		isign1=1
	   else if(det.eq.0.0d0) then
		isign1=0
	   else if(det.lt.0.0d0) then
		isign1=-1
	   endif
c slop*sloplast can overflow in extreme cases so use DSIGN(1.,slop)=-1 if slop
c  s is negative, +1 if slop is 0 or positive
	   sl1=dsign(1.d0,slop)
	   sl2=dsign(1.d0,sloplast)
	   if(.false..and.sl1*sl2.lt.0.0d0) then	!suppress print of min/max
c=	   if(.false..and.slop*sloplast.lt.0.0d0) then	!suppress print of min/max
c===	   if(dprt.and.slop*sloplast.lt.0.0d0) then
		if(sloplast.lt.0.d0.and.slop.gt.0.d0) then
      	   print 731
      	   write(8,731)
c731		   format(/,' Minimum found')
		else
      	   print 732
      	   write(8,732)
c732		   format(/,' Maximum found')
		endif
		print 73,istep-1,slast,sloplast,detlast,
     &	 istep,s,slop,det
		write(8,73) istep-1,slast,sloplast,detlast,
     &	 istep,s,slop,det
c73		format(
c     &	' step # ',i8,': s, slope, det(s) = ',3g13.6,/,
c     &	' step # ',i8,': s, slope, det(s) = ',3g13.6)
c	   	pause
	   endif
c
	   if(isign.ne.isign1.or.isign1.eq.0) then	!crossed, or hit, a root
		nroot=nroot+1	!total number found
		ngf=ngf+1		!number found in current group
		s1(nroot)=slast
		s2(nroot)=s
		if(isign1.eq.0) then
		   s2(nroot)=s+step        !move past correct root, and bisect anyway!
c===TOO BIG		s2(nroot)=s + 0.05d0*dabs(s)
	         if(s2(nroot).gt.0.d0) s2(nroot)=-1.0d-8	!reset to near-zero
		endif
c
		if(ngf.eq.ngrp(n)) then
		   print 75,ngf,n
75		   format(
     &	' Guesses for all ',i2,' roots in group ',i2,' found')
		   goto 202 	!all in group found -next group
		endif
c
		if(nroot.eq.kX) then
		   print 76,kX
76		   format(
     &	' Guesses for all ',i2,' roots found')
		   goto 1	!all roots found
		endif
c
		isign=isign1			!prepare for next root
c
	   endif
	   goto 200		!next step in s
c
202	   continue		!jump here for next group
	   if(nroot.eq.kX) then
	      print 76,kX
c76		format(
c     &	' Guesses for all ',i2,' roots found')
		goto 1	!all roots found
	   endif
	enddo		!end of do n=1,ngroup
c Should not get here -if we do then revert to crude ungrouped search
	print 78
	if(dprt) write(8,78)
78	format(
     & ' Failed to locate a root in each group -revert to crude search')
	grouped=.false.
	goto 5
c
c END of code for grouped searchlog-spaced s values
c
c Original code the arithmetic increments in s for search
100	continue
c Define step size
	fac=20.d0	!try these values!
	redfac=4.d0
	fac1=dble(float(kX))*fac	!used if eigenvalues equal
	if(kX.ge.2) then
	   step=dabs(rootX(1)-rootX(2))/fac
	   if(rootX(1)/rootX(2).lt.1.0001d0) then	!in case eigenvalues 'equal'
	      step=dabs(rootX(1)/fac1)
	   endif
	else
	   step=dabs(rootX(1)/fac)
	   fac1=fac
	endif
	if(step.lt.1.d-7) then
	   print 741
	   write(8,741)
741	   format(' Gave up: step less than 0.1 microsecond')
	   goto 999		!deallocate and RETURN
	endif
c	if(debug.and.ired.gt.1) then
	if(ired.gt.0) then
         print 721, slo,step
         if(dprt) write(8,721) slo,step
721	   format(' Start search at s = ',g13.6,'  step size = ',g13.6)
	endif
52	continue		!return here to restart with smaller steps
	s=slo
	istep=0			!to count steps for debug
	det=DETWX(s)
	if(nerr.eq.10) then	!determinant overflowed
	   goto 999		!deallocate and RETURN
c	   RETURN
	endif
	if(det.gt.0.0d0) then
	   isign=1
	else if(det.eq.0.0d0) then
	   isign=0
	else if(det.lt.0.0d0) then
	   isign=-1
	endif
c For debug (only?) calculate slope also to look for max/min.  For this
c purpose take one step more before starting the loop, to get an initial
c value for the slope
	slast=s
	detlast=det
	s=s + step		!work towards s=0
	istep=istep+1	!for debug
	det=DETWX(s)
	if(nerr.eq.10) then	!determinant overflowed
	   goto 999		!deallocate and RETURN
	endif
	slop=(det-detlast)/(s-slast)
c Now start loop
	last=.false.
	debug=caplock()	!not function -set on entry to ROOTSCH

c
21	continue		!return here for next step in s value
	slast=s		!record value of previous s value
	sloplast=slop		!record value of previous slope
	detlast=det		!record value of previous determinant
c
	s=s + step		!work towards s=0
	istep=istep+1	!for debug
	if(last) then	!last step in s for current step length
         print 4,nroot,kX
         write(8,4) nroot,kX
c4	   format(
c     & ' Only ',i2,' roots out of ',i3,' found -step size reduced')
	   do m=1,nroot
          if(dprt) print 7,m,s1(m),s2(m),DETWX(s1(m)),DETWX(s2(m))
          if(dprt) write(8,7) m,s1(m),s2(m),DETWX(s1(m)),DETWX(s2(m))
	   enddo
	   ired=ired+1
	   if(ired.gt.nredmax) then
	      print 74,nredmax
	      write(8,74) nredmax
c74		format(' Gave up after ',i3,' reductions')
		goto 999		!deallocate and RETURN
	   endif
	   fac=fac*redfac		!make steps smaller
	   if(ired.eq.1) then
		slo=eigen(1)		!use eigs rather than prev root after 1st try
		if(slo.lt.-1.0d9) slo=-1.d9		!1 nanosecond
	   endif
	   slo=1.5d0*slo	!start MORE negative if restart with smaller steps
	   goto 52			!restart from beginning (s=slo)
	endif
	if(s.gt.0.d0) then
	   s=-1.0d-50	!reset to near-zero
	   last=.true.
	endif
c
c Calculate the determinant
	det=DETWX(s)
	if(nerr.eq.10) then	!determinant overflowed
	   goto 999		!deallocate and RETURN
	endif
	slop=(det-detlast)/(s-slast)
	if(debug.and.ired.gt.1) then
	   print 8,istep,s,det,slop
c8	   format(i8,' s, det(s), slope = ',3g13.6)
	endif
	if(det.gt.0.0d0) then
	   isign1=1
	else if(det.eq.0.0d0) then
	   isign1=0
	else if(det.lt.0.0d0) then
	   isign1=-1
	endif
c slop*sloplast can overflow in extreme cases so use DSIGN(1.,slop)=-1 if slop
c  s is negative, +1 if slop is 0 or positive
      sl1=dsign(1.d0,slop)
	sl2=dsign(1.d0,sloplast)
	if(sl1*sl2.lt.0.0d0) then
	   if(sloplast.lt.0.d0.and.slop.gt.0.d0) then
      	if(dprt) print 731
      	if(dprt) write(8,731)
c731		format(/,' Minimum found')
	   else
      	if(dprt) print 732
      	if(dprt) write(8,732)
c732		format(/,' Maximum found')
	   endif
         if(dprt) print 73,istep-1,slast,sloplast,detlast,
     &	istep,s,slop,det
         if(dprt) write(8,73) istep-1,slast,sloplast,detlast,
     &	istep,s,slop,det
c73	   format(
c     &	' step # ',i8,': s, slope, det(s) = ',3g13.6,/,
c     &	' step # ',i8,': s, slope, det(s) = ',3g13.6)
c	   pause
	endif
	if(isign.ne.isign1.or.isign1.eq.0) then	!crossed, or hit, a root
	   nroot=nroot+1
	   s1(nroot)=slast
	   s2(nroot)=s
	   if(isign1.eq.0) then
		s2(nroot)=s+step        !move past correct root, and bisect anyway!
	      if(s2(nroot).gt.0.d0) s2(nroot)=-1.0d-30	!reset to near-zero
	   endif
	   if(nroot.eq.kX) goto 1	!all roots found
	   isign=isign1			!prepare for next root
	   if(nroot.gt.1) then		!change step size
		step=dabs(rootX(nroot+1)-rootX(nroot))/fac
		if(rootX(nroot+1)/rootX(nroot).lt.1.0001d0) then !in case eigs'equal'
		   step=dabs(rootX(1)/fac1)
		endif
	   endif
	endif
	goto 21		!next step in s
c end of code for arith-spaced s values
c
c Get here when succesful
1	continue
      print 3,kX
      if(dprt) write(8,3) kX
3	format(' Guesses for all ',i2,' roots located in ROOTSCH')
	if(debug) then
	 do 6 m=1,kX
        if(dprt) print 7,m,s1(m),s2(m),DETWX(s1(m)),DETWX(s2(m))
        if(dprt) write(8,7) m,s1(m),s2(m),DETWX(s1(m)),DETWX(s2(m))
7	  format(' #',i2,' guesses: s1,s2 = ',2g13.6,' det(s) = ',2g13.6)
6	 continue
	endif
c
999	continue
	if(grouped) DEALLOCATE(x1,x2,n1,n2,rootG)
	RETURN
	end

