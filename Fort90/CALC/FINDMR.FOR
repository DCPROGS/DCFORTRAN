	subroutine FINDMR(i1,j1,ncyc,nsc,icy,icyc,im,jm,obeymr,done,idest)
c Called by getrev to check whether i1,j1 is a valid MR route, and if not,
c to find a route that can be set by MR and to
c get the MR rates into first two places in cycle
c Now called with icy=index, such that actual cycle number=isetmr(is)
c
	integer nsc(50),im(50,100),jm(50,100)
	logical obeymr(50)
	logical done(50)
	integer*4 icyc(50)
c  Local arrays to get valid MR routes if selected one is not valid
	integer iOK(10),jOK(10)
c
	integer isetmr(50)
	common/mr1/isetmr
c
	logical discprt
	common/dp/discprt
c
	ndim=50
	idest=0
c Must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j

c	i=icy			!cycle # of current cycle
	i=isetmr(icy)
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
c	goto 17
	idest=17		!start mr setting again in GETREV
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
	if(i.ge.2) then
	   i11=im(i,1)
	   j11=jm(i,1)
c	   call TESTMR(i,i11,j11,done,ncycp,nOK,iOK,jOK,ndim)
c call testmr with icy=index, such that actual cycle number=isetmr(is)
	   call TESTMR(icy,i11,j11,done,ncycp,nOK,iOK,jOK,ndim)
	   if(ncycp.ne.0) then
		call BELL(1)
		print 20,i11,j11,ncycp,i
20		format(
     & '    Q(',i2,',',i2,') is a member of cycle ',i3,
     & ' so cannot be reset in cycle ',i3)
		if(nOK.eq.0.and.i.eq.ncyc) then
		   print 23,i
		   if(discprt) write(8,23) i
23		   format(
     & ' All rates in last cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  tell DC that there is a problem.')
		   call BELL(3)
		   pause
		   obeymr(i)=.false. 	!set false
		else if(nOK.eq.0.and.i.lt.ncyc) then
		   call BELL(2)
		   print 21,i
21		   format(
     & ' All rates in cycle ',i3,'  are part of previous cycles',/,
     & '  -NONE available to set by microscopic reversibility, so',/,
     & '  cycle ',i3,' MR set globally for all rates.')
		else
		   print 22, (iOK(n),jOK(n),n=1,nOK)
22		   format(
     & ' The following routes are NOT a member of any previous cycle,',
     & '  so OK ',/, 5(i3,1x,i3,',  '))
		   i1=iOK(1)		!default=first good route
		   j1=jOK(1)
c		   goto 25
		   idest=25		!try again if invalid values given
		endif
	   endif
	endif
c
	RETURN
	end

