	subroutine INSTAT(peq,k,kA,kB,kC,kD,i1,pcum,ist,k1,fixed)
c To choose initial state for simulation at random, in proportion
c to equilibrium occupancies. K=no of states, i1= # of init state.
c If some (eg 'absorbing') states to be omitted from choice of
c init state, the corresp elements of iexc() are set to 1
c  For re-entry to INSTAT with PICUM,IST,k1 already set jump straight to 200
c -do this if called with k1>0. To set values call with k1<0 initially.
	real*4 peq(100),peq1(100),pcum(101)
	integer iexc(100),ist(100)
	character*1 ans,UC
	logical fixed
	common/rand/ix,iy,iz
	SAVE iexc
c
	if(k1.gt.0) goto 200
	k1=k		!no of states from which init state chosen
	do i=1,k
	   iexc(i)=0
	   ist(i)=i
	   peq1(i)=peq(i)
	enddo
	print 55
55	format(
     &' Choose initial state at random from initial vector [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') goto 10
c	print 56
c56	format('&Exclude any states as starting state [N] ? ')
c	ans='N'
c	call INPUTa(ans)
c	if(UC(ans).ne.'Y') goto 57
	print 56
56	format(' Choose the initial state:',/,
     & ' (1) Start in a long shut state (C state)',/,
     & ' (2) Start in any shut state',/,
     & ' (3) Start in any open state',/,
     & ' (4) Exclude specified states as initial state',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
c
	if(iopt.eq.1) then	!exclude A,B states
	   nexc=kA+kB
	   do i=1,nexc
		iexc(i)=1
	   enddo
	else if(iopt.eq.2) then		!exclude open states
	   nexc=kA
	   do i=1,nexc
		iexc(i)=1
	   enddo
	else if(iopt.eq.3) then		!exclude shut states
	   nexc=kB+kC
	   do i=kA+1,k
		iexc(i)=1
	   enddo
	else if(iopt.eq.4) then
	   nexc=0
	   print 58
58	   format('&   number to be excluded = ')
	   call INPUTi(nexc)
	   k1=k-nexc
	   do i=1,nexc
		print 61,i
61		format('&',1x,i3,' : state no = ')
		call INPUTi(j)
		iexc(j)=1
	   enddo
	endif
c
	j=0
	s=0.
	do 63 i=1,k
	if(iexc(i).ne.0) goto 63
	j=j+1
	peq1(j)=peq(i)
	ist(j)=i	!state # for peq1(j)
	s=s+peq(i)
c	print 68,i,s
c68	format(' i,s= ',i8,g13.6)
63	continue
	do 64 i=1,k1
	peq1(i)=peq1(i)/s	!so sum to 1
c	print 67,i,peq1(i),s
64	continue
c67	format(2x,' i,peq1(i),s= ',i4,2g13.6)
c
57	pcum(1)=0.
	do 11 i=2,k1+1
	pcum(i)=pcum(i-1) + peq1(i-1)	!so pcum(k+1)=1
c	print 12,i,pcum(i)
c12	format(2x,' i,pcum(i)= ',i4,g13.6)
11	continue
c
c  For re-entry to INSTAT with PICUM,IST,k1 already set jump straight to 200
200	continue
	u=random()		!uniform(0,1)
c	print 65,u
c65	format(' Random= ',g13.6)
	do i=1,k1
	   i1=ist(i)
	   if(u.gt.pcum(i).and.u.le.pcum(i+1)) goto 14	!jump out with i1
	enddo
14	continue
c	print 571,i1
c571	format(' Starting state #= ',i4)
	RETURN
c
10	continue
	print 561
561	format(' Starting state #= ')
	call INPUTi(i1)
	fixed=.true.
	u=random()	 !not used (but leaves ix,iy,iz same as if i1 chosen randomly)
	u=random()	 !not used (but leaves ix,iy,iz same as if i1 chosen randomly)
	RETURN
	end


