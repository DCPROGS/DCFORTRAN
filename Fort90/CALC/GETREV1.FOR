	subroutine GETREV1(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini)
c To alter, if req, the state in each cycle that is calculated by
c micro ref (see also GETCYC, in QDEF)
c
c Getrev1 is version with npar, irate,jrate in params for calcmr
c
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
c

	ndim=50	!for call to ivecrev
	allmr=.false.	!local
	do i=1,50
	   obeymr(i)=.false.
	enddo
	print 1,ncyc
1	format(/,' Number of cycles in mechanism [',i2,'] = ')
	call INPUTi(ncyc)
	if(ncyc.eq.0) then
	   RETURN
	endif
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
25	   continue		!jump here to try again if invalif values given
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
		   if(nOK.eq.0) then
			call BELL(3)
			print 21,i,i
21			format(
     & ' All rates in cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  cycle ',i3,' set not to obey microscopic reversibility.')
			mrcalc=.true.	!one or more with nOK=0 so use CALCMR
c			obeymr(i)=.false. 	!set  false and go to next cycle
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

