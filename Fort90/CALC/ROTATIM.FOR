	subroutine ROTATIM(nsc,icyc,im,jm,isetmr,obeymr)
c Called by getrev to get the MR rates into first two places in cycle
	integer nsc(50),im(50,100),jm(50,100)
	logical obeymr(50)
	integer*4 icyc(50)
	integer isetmr(50)
c  Local arrays to get valid MR routes if selected one is not valid
	integer iOK(10),jOK(10)

===new subroutine ROTATIM
	ndim=50
c Must now get i,j to the 1st two places in the cycle, so look through
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
15	continue
c now bring state in element m2 of icyc into element #1
	call IVECROT(icyc,nsc(i),1-m2,ndim)
	goto 18
151	continue
c now bring state in element m2 of icyc into element #1
	call IVECROT(icyc,nsc(i),1,ndim)
c and redefine im,jm in correct order
18	continue
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
20		format(
     & ' Q(',i2,',',i2,') is a member of cycle ',i3,
     & ' so cannot be reset in cycle ',i3)
		if(nOK.eq.0.and.i.eq.ncyc) then
		   print 23,i
23		   format(
     & ' All rates in last cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  set not to obey MR but should do so anyway, if others do.')
		   pause
		   obeymr(i)=.false. 	!set false
		else if(nOK.eq.0.and.i.lt.ncyc) then
		   call BELL(3)
		   print 21,i,i
21		   format(
     & ' All rates in cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  cycle ',i3,' MR set globally for all rates.')
===		   mrcalc=.true.	!one or more with nOK=0 so use CALCMR
		else
		   print 22, (iOK(n),jOK(n),n=1,nOK)
22		   format(
     & ' The following routes are NOT a member of any previous cycle,',
     & '  so OK ',/, 5(i3,1x,i3,',  '))
		   i1=iOK(1)		!default=first good route
		   j1=jOK(1)
		   goto 25
		endif
	   endif
	endif
