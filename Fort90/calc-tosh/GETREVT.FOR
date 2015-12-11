	subroutine GETREVT(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,k,
     &	titlep,npar,irate,jrate,useini)
c To alter, if req, the state in each cycle that is calculated by
c micro ref (see also GETCYC, in QDEF)
c
c GETREVT is version of GETREV that uses the spanning
c tree method 11/05/03 05:03pm. Both automr() and isetmr() are redundant
c with this method, but leave in while both methods still in prog
c
c NB cannot ask for ALL the MR rates to be specified in advance, because if all
c are specified, this also specifies the whole spanning tree -so PRIM not
c needed at all!  All we can do is perhaps specify a few MR routes, and get tree
c For each of the connections not in tree (i-j say), decide whether it is
c q(i,j) of q(j,i) that is to be set (as in getrevh).
c
c  Modif 06/19/03 09:53am by adding TESTMR
c
c Modif 11/22/01 09:43am so ncyc,nsc,im,jm are parameters, rather than
c in common/mpar, so can be called either with values from prog.ini or values
c from qmechs.mec.  Now sets obeymr(i)=true if cycle #i is constrained by
c MR, and moves the constrained rate to first postion in im(),jm(). If no
c MR constraint then obeymr(i)=false and IM(), JM() returned unchanged
c (im1 etc are used only for defaults)
c im() etc are values for QMECHS.mec; im1() etc are values
c from prog.ini
c
c Modif 11/16/01 09:41am so that if im(L,1)=0 then microscopic reversibility
c check is omitted for cycle #L -so this way can have some cycles that
c do not obey MR and some that do.
c
	character*1 ans
cc  Local arrays to get valid MR routes if selected one is not valid
c
	integer nsc(50),im(50,100),jm(50,100)
	integer nsc1(50),im1(50,100),jm1(50,100)
c=	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	logical discprt
	common/dp/discprt
	logical allmr,useini
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c	common/np1/npar
c	integer irate(200),jrate(200)
c	common/ir/irate,jrate
c For PRIM
	character*10 titlep(200)
	allocatable::ICin,ICout,iedge,Jcon,Jtree,incirc
	integer*4 ICin(:,:)	!IC for connections to be IN tree
	integer*4 ICout(:,:)	!IC for connections to be NOT IN tree
	integer*4 iedge(:,:)	!IC for specified connections
	integer*4 jtree(:,:)
	integer*4 incirc(:)
c
	integer*4 JCON(:,:)
	COMMON/QPAR/NCON,IC(2,200)

c
	allocate(ICin(2,200),ICout(2,200),iedge(2,200),
     &    JCON(100,100),Jtree(100,100),incirc(100))
c Make 'adjacency' matrix corresponding to IC()
	nd1=200
	nd2=100
	call IC_JCON(IC,ncon,nd1,JCON,k,nd2)
c
	allmr=.false.	!local
	ndim=100 		!size of JCON array
	ncmax=50		!max no of cycles
	do i=1,ncmax
	   automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
	   isetmr(i)=i		!order irrelevant here
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
c For each cycle may have
c OBEYMR=true or false
c AUTOMR=true or false (if true, OBEYMR is ignored)
c A cycle is set to obey MR in the usual way IF obeymr=true AND automr=false
c
c
	if(ncyc.gt.0.and.useini) then
	   call BELL(1)
	   print 143
143	   format(/,
     & ' Last run set the following rates by microscopic reversibility')
	   do n=1,ncyc
		i1=im1(n,1)		!default
		j1=jm1(n,1)
		call GETM(i1,j1,m,npar,irate,jrate)
		if(obeymr(n)) then
		   call GETM(im(n,1),jm(n,1),m,npar,irate,jrate)
      	   print 142,n,im(n,1),jm(n,1),nsc(n),
     &    	   titlep(m),im(n,1),jm(n,1)
142		   format(1x,i3,
     &    ': Microscopic reversibility route ',i3,'-',i3,
     &    ' found from ',i3,'-state cycle',/,
     &    ' ',a10,' = ',' q(',i3,',',i3,') calculated by micro rev')
		   print 128,(im(n,m),jm(n,m),m=2,nsc(n))
c128		   format(20(5(i3,'-',i3,3x)))
		else
		   print 145,n,(im(n,m),jm(n,m),m=1,nsc(n))
145		   format(' Cycle ',i3,' NOT set by micro rev',/,
     &		20(5(i3,'-',i3,3x)),/)
		endif
	   enddo
	   ans='N'
	   call DCASK('Use same again',ans,ans)
	   if(ans.eq.'Y') then
		do n=1,ncyc
		   nsc(n)=nsc1(n)
		   do m=1,nsc(n)
			im(n,m)=im1(n,m)
			jm(n,m)=jm1(n,m)
		   enddo
		enddo
		goto 99	!skip asking
	   endif
	endif

	print 108			!new line
	if(discprt) write(8,108)

c set default for allmr and iopt
	if(useini) then
	   allmr=.true.
	   n=0
	   do i=1,ncyc
		if(.not.obeymr(i)) n=n+1
	   enddo
	   if(n.gt.1) then
		allmr=.false.
		if(n.lt.ncyc) then
		   iopt=3
		else if(n.eq.ncyc) then
		   iopt=2
		endif
	   endif
	else
	   allmr=.true.
	   iopt=1
	endif

7	continue
	if(allmr) then
	   iopt=1
	else
	   iopt=3	!default
	endif
	print 2,iopt
2	format(
     & ' (1) ALL cycles to be constrained by microscopic rev',/,
     & ' (2) NO cycles to be constrained by microscopic rev',/,
     & ' (3) Choose which cycles are to be constrained by mic rev',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   allmr=.true.
	   do i=1,ncmax
		obeymr(i)=.true.
		automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
		isetmr(i)=i		!order irrelevant here
	   enddo
	else if(iopt.eq.2) then
c	   ncyc=0		!should not be needed -do not want to save ncyc=0
	   allmr=.false.
	   do i=1,ncmax
		obeymr(i)=.false.
		automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
		isetmr(i)=i		!order irrelevant here
	   enddo
	   print 6
	   if(discprt) write(8,6)
6	   format(' NO rate constants set by microscopic reversibility')
	   goto 99
	else if(iopt.eq.3) then
	   allmr=.false.
	   do i=1,ncmax
		automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
		isetmr(i)=i		!order irrelevant here
	   enddo
	endif
c
c
c Now define those to be NOT in tree
c NB it is a connection (2 rates) rather than a rate constant that is
c chosen to be set by MR so the RATE that is set could be q(i,j) or q(j,i)
c but simplest to specify this here
c NB already know how many mr routes there are, ncyc=nmr=ncon-k+1, and ALL of
c them should now be specified here
c
c	print 132,ncout
c132	format(/,
c     &' No. of routes that should NOT be in the tree (MR) [',i3,'] = ')
c	call INPUTi(ncout)
c1321	print 132,ncyc
c132	format(/,
c     &' Specify the ',i3,' routes to be set by micro rev ')
c	ncout=ncyc
c
	print 163
163	format(
     &' It is possible (but not necessary) to specify some or all of',/,
     &' the rate constants that are to be calculated by microscopic',/,
     &' reversibility (edges not in the tree)',/,/,
     &' Do you wish to specify some/all of the rates that are to be',/,
     &' calculated by microscopic reversibility [N] ? ')
	ans='N'
	call INPUTa(ans)
	ncout=0
	if(ans.eq.'Y') then
1321	 print 164,ncyc,ncout
164	 format(
     &' How many routes (up to ',i2,') to be specified: n [',i2,'] = ')
	 call INPUTi(ncout)
	 if(ncout.gt.0) then
c=	   do n=1,ncyc
	   do n=1,ncout
		if(useini) then
		   i1=im1(n,1)		!default
		   j1=jm1(n,1)
		else
		   i1=im(n,1)		!default
		   j1=jm(n,1)
		endif
242		print 14,n,i1,j1
14		format(' Route ',i2,': specify i,j [',i3,',',i3,'] = ')
		call INPUT2i(i1,j1)
		if(jcon(i1,j1).eq.0) then
		   call BELL(2)
		   print 70,i1,j1
70		   format(' There is no connection between ',i3,
     &		' and ',i3,'. Try again')
		   goto 1321
		endif
		call GETM(i1,j1,m,npar,irate,jrate)
		print 141,i1,j1,titlep(m)
141		format(
     &	' Rate q(',i3,',',i3,
     &	') = ',a10,' is set by micro rev OK [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.ne.'Y') then
		   goto 242
		endif
c
		im1(n,1)=i1		!keep mr rate in im1,jm1 to compare with PRIM output
		jm1(n,1)=j1		!keep mr rate
		ICout(1,n)=i1	!for input to PRIM
		ICout(2,n)=j1
		print 151,n,titlep(m),i1,j1
		write (8,151) n,titlep(m),i1,j1
151		format(' ',i2,': ',a10,' = rate q(',i3,',',i3,
     &	') is to be set by MR')
	   enddo
	 endif
	endif
c
c For theory progs, the next bit is surely superfluous
	ncin=0
c131	print 13,ncin
c13	format(/,
c     &' Number of routes that should NOT be set by MR (in tree) [',
c     & i3,'] = ')
c	call INPUTi(ncin)
c	if(ncin.gt.0) then
c	   do n=1,ncin
c		print 14,n,i,j
c14		format(' Route ',i2,': specify i,j [',i3,',',i3,'] = ')
c		call INPUT2i(i,j)
c		ICin(1,n)=i
c		ICin(2,n)=j
c		if(jcon(i,j).eq.0) then
c		   call BELL(2)
c		   print 70,i,j
c70		   format(' There is no connection between ',i3,
c     &		' and ',i3,'. Try again')
c		   goto 131
c		endif
c		print 15,n,i,j
c		write (8,15) n,i,j
c15		format(' ',i2,': route ',i3,',',i3,' must not be set by MR')
c	   enddo
cc	   Alternative (much simpler) check
cc 		-convert ICin to jtree form and use NCONS
c	   call IC_JCON(ICin,ncin,nd1,Jtree,k,nd2)
c	   call NCONS(Jtree,k,ncon1,nstate,ndim)  !returns number of connections
c	   print 5,nstate,ncon1
c	   write(8,5) nstate,ncon1
c5	   format(
c     &	' Specified routes have ',i3,' states, ',i3,' connections')
c	   if(ncon1.gt.nstate-1) then
c		print 161
c	 	write(8,161)
c161		format(' These routes contain cycle(s): invalid, try again')
cc		goto 131
c	   endif
cc
c	   ncmax=k	!max cycle size to be found
c	   call CYCQ1(k,ncin,ICin,ncyc,nsc0,im1,jm1,ncmax)
c	   if(ncyc.ne.0) then
c		call BELL(1)
c		print 16,ncyc
c		write(8,16) ncyc
c16		format(
c     &	' From CYCQ1, these routes contain ',i3,
c     &	' cycle(s): invalid, try again')
c		goto 131
c	   endif
c	endif
c
      call PRIM3(JCON,ICin,ncin,ICout,ncout,Iedge,nedge,
     &	nerr1,nerr2,nerr3,nerr4,k,ndim)
c
c Convert iedge to kxk 'adjacency matrix' form in Jtree
	nd1=200
	nd2=100
	nc=nedge
	call IC_JCON(Iedge,nc,nd1,Jtree,k,nd2)
c
c Check the result
c
c Convert iedge to k x k 'adjacency matrix' form in Jtree
	nd1=200
	nd2=100
	nc=nedge
	call IC_JCON(Iedge,nc,nd1,Jtree,k,nd2)
	call NCONS(Jtree,k,ncon1,nstate,ndim)  !returns ncon1
c
c Check for errors
	if(ncon1.ne.nstate-1) then
	   call BELL(2)
	   print 19,ncon1,nstate
	   if(discprt) write(8,19) ncon1,nstate
19	   format(' ERROR: number of connections in spanning tree = ',i3,
     &	' is not number of states - 1')
	   pause
	endif
c
	if(nerr2.ne.0) then
	   call BELL(2)
	   print 191
	   if(discprt) write(8,191)
191	   format(' ERROR:',/,
     &'  not all states occur at least once as termination of an edge')
	endif
C
	if(nerr3.ne.0) then
	   call BELL(2)
	   print 20
	   if(discprt) write(8,20)
20	   format(' WARNING:',/,
     &' The routes that are specified to be in the tree are NOT all in')
	endif
C
	if(nerr4.ne.0) then
	   call BELL(2)
	   print 21
	   if(discprt) write(8,21)
21	   format(' WARNING:',/,
     &' The routes that are specified to be not in the tree are NOT all',
     & ' excluded')
	endif
c
	print 121
	write(8,121)
121	format(/,' Edges in spanning tree')
	do j=1,nedge
	   print 110,j,iedge(1,j),iedge(2,j)
	   if(discprt) write(8,110) j,iedge(1,j),iedge(2,j)
110	   format(' ',i3,': ',i3,' - ',i3)
	enddo
c
c NB it will not usually be sensible to pre-specify ALL of the MR rates
c (see above), so all that can be done here is to check that
c the right number of MR edges has been found, and to choose between
c q(i,j) and q(j,i) for each of them, contrary to following note.
c
c NB the rate (i.e. connection + direction) to be set by mr has
c already been specified in im(n,1),jm(n,1) -this route should
c be one of those identified by (not in) spanning tree if PRIM3 worked
c properly, and a tree was in fact found that did not include this
c route -need to check this here.
c NB incirc() is defined so that if MR route   1: i =   6   j =   2
c then incirc()=   2   1   5   6
c In this case the MR rate constant could be 6,2  or 2,6
c so of im(n,1)=6,jm(n,1)=2 (set above) then look in incirc() for the 6,2
c sequence, and set the rest of that cycle in im(n,j),jm(n,j), j=2,3,...,nsc(n)
c NB in general, the order in which mr cycles are defined above may not be the
c same as those generated in PRIM (though if model is defined with PRIM they
c should be). But if the mr routes are succesfully excluded from the spanning
c tree, all the routes should be present in the output from PRIM, though they
c may not be in the same order. So compare the mr rates defined above in
c im1(n,1), jm1(n,1) with the output from PRIM
c
	nm=0
	do i=2,k	!check lower triangle only
	   do j=1,i-1
		if(jcon(i,j).eq.1.and.jtree(i,j).eq.0) then
		   nm=nm+1
c
c Use Dowsland's circuit routine
		   call CIRCUIT(i,j,incirc,ncirc)
c
c            check incirc
		   if(incirc(ncirc).ne.i.or.incirc(1).ne.j) then
			print 144,i,j,(incirc(n),n=1,ncirc)
			if(discprt) write(8,144) i,j,(incirc(n),n=1,ncirc)
144			format(' Error in circuit: i, j = ',i3,',',i3,/,
     &		5(20i3,/))
		   else
			if(ncirc.le.20) then
			   print 41,nm,(incirc(n),n=ncirc,1,-1)
41			   format(/,' Cycle ',i3,':',20i3)
			else
			   print 42,nm,(incirc(n),n=ncirc,1,-1)
42			   format(/,' Cycle ',i3,':',5(20i3,/))
			endif
		   endif
c
		   nsc(nm)=ncirc
		   call GETM(i,j,m1,npar,irate,jrate)
		   call GETM(j,i,m2,npar,irate,jrate)
		   if(allmr) then
			iopt=1		!default
			if(im1(nm,1).eq.j.and.jm1(nm,i).eq.j) iopt=2
			print 4,nm,i,j,titlep(m1),j,i,titlep(m2),iopt
4			format(' Micro rev connection ',i3,': ',/,
     &		' (1) set q(',i3,',',i3,') = ',a10,/,
     &		' (2) set q(',i3,',',i3,') = ',a10,/,
     &		' Option number [',i2,'] = ')
		   else 	!not allmr
			iopt=1		!default
			if(im1(nm,1).eq.j.and.jm1(nm,i).eq.j) iopt=2
			if(.not.obeymr(nm)) iopt=3
			print 9,nm,i,j,titlep(m1),j,i,titlep(m2),iopt
9			format(' Micro rev connection ',i3,': ',/,
     &		' (1) set q(',i3,',',i3,') = ',a10,/,
     &		' (2) set q(',i3,',',i3,') = ',a10,/,
     &		' (3) Do NOT set this cycle by micro rev',/,
     &		' Option number [',i2,'] = ')
		   endif
		   call INPUTi(iopt)
		   if(iopt.eq.3) then
			obeymr(nm)=.false.
			im(nm,1)=i		!define im,jm anyway, for print
			jm(nm,1)=j
			do m=2,nsc(nm)
			   im(nm,m)=jm(nm,m-1)
			   jm(nm,m)=incirc(m)
			enddo
			goto 91	!next cycle
		   endif
c
		   if(iopt.eq.1) then
			im(nm,1)=i
			jm(nm,1)=j
			do m=2,nsc(nm)
			   im(nm,m)=jm(nm,m-1)
			   jm(nm,m)=incirc(m)
			enddo
		   else if(iopt.eq.2) then
			im(nm,1)=j
			jm(nm,1)=i
			do m=2,nsc(nm)
			   im(nm,m)=jm(nm,m-1)
			   jm(nm,m)=incirc(ncirc-m+1)
			enddo
		   endif
c
91		   continue	!current cycle done
		endif
	   enddo
	enddo
c	nmr=nm
c
c Following bit replaced by above which is same as in GETREVh
c	nmr=0
c	do i=2,k	!check lower triangle only
c	   do j=1,i-1
c		if(jcon(i,j).eq.1.and.jtree(i,j).eq.0) then
c		   nmr=nmr+1
cc		   print 4,nmr,i,j
cc		   write(8,4) nmr,i,j
cc4		   format(/,' MR route ',i3,': i = ',i3,'   j = ',i3)
cc Use Dowsland's circuit routine
c		   call CIRCUIT(i,j,incirc,ncirc)
c		   nsc(nmr)=ncirc
cc		which of the im1,jm1 does current incirc correspond with?
c		   do n=1,ncyc
c			if((im1(n,1).eq.incirc(1)).and.
c     &			(jm1(n,1).eq.incirc(ncirc))) then
c			   im(nmr,1)=incirc(1)
c			   jm(nmr,1)=incirc(ncirc)
c			   do m=2,nsc(nmr)
c				im(nmr,m)=jm(nmr,m-1)
c				jm(nmr,m)=incirc(ncirc-m+1)
c			   enddo
c			   goto 91
c			else if((im1(n,1).eq.incirc(ncirc)).and.
c     &			(jm1(n,1).eq.incirc(1))) then
c			   im(nmr,1)=incirc(ncirc)
c			   jm(nmr,1)=incirc(1)
c			   do m=2,nsc(nmr)
c				im(nmr,m)=jm(nmr,m-1)
c				jm(nmr,m)=incirc(m)
c			   enddo
c			   goto 91
c			endif
c		   enddo
cc route not found (should be skipped via goto 91)
c		   call BELL(1)
c		   print 92,im1(nmr,1),jm1(nmr,1)
c92		   format(
c     &	   ' MR route ',i3,',',i3,' not found by CIRCUIT')
c		   PAUSE
cc
c91		   continue	!current cycle done
c		   n=nmr
c		   if(iopt.eq.3) then
c		      print 911,n,(im(n,m),jm(n,m),m=1,nsc(n))
c911		      format(' Cycle ',i3,': ',20(5(i3,'-',i3,3x)),/)
c			ans='N'
c			if(obeymr(n)) ans='Y'
c			call DCASK('Set micro rev in this cycle',ans,ans)
c			obeymr(n)=ans.eq.'Y'
c		   endif
c		endif
c	   enddo
c	enddo
c
c Print results
99	do n=1,ncyc
	   if(obeymr(n)) then
		call GETM(im(n,1),jm(n,1),m,npar,irate,jrate)
      	print 1311,n,im(n,1),jm(n,1),nsc(n),
     &    	  titlep(m),im(n,1),jm(n,1)
	   	if(discprt) write(8,1311) n,im(n,1),jm(n,1),nsc(n),
     &		titlep(m),im(n,1),jm(n,1)
1311		format(1x,i3,
     &    ': Microscopic reversibility route ',i3,'-',i3,
     &    ' found from ',i3,'-state cycle',/,
     &    ' ',a10,' = ',' q(',i3,',',i3,') calc by micro rev')
		print 128,(im(n,m),jm(n,m),m=2,nsc(n))
		if(discprt) write(8,128)(im(n,m),jm(n,m),m=2,nsc(n))
128		format(20(5(i3,'-',i3,3x)))
	   else
		print 129,n,(im(n,m),jm(n,m),m=1,nsc(n))
		if(discprt) write(8,129)n,(im(n,m),jm(n,m),m=1,nsc(n))
129		format(' Cycle ',i3,' NOT set by micro rev',/,
     &		20(5(i3,'-',i3,3x)),/)
	   endif
	enddo
c
	deallocate(ICin,ICout,iedge,
     &    JCON,Jtree,incirc)
	RETURN
	end

