	subroutine GETREVH(JCON,ICin,ncin,ICout,ncout,allmr,ncyc,nsc,im,jm,
     &	ncyc1,nsc1,im1,jm1,k,
     &     titlep,npar,irate,jrate,useini,nmr,jmic,idest)
	 use menu_f90

c To alter, if req, the state in each cycle that is calculated by
c micro ref (see also GETCYC, in QDEF)
c
c GETREVH is version of GETREVT for HJCFIT where constraints are set first
c and can therefore be included in the tree without typing them again
c NB cannot ask for all MR rates to be specified in advance, because if all
c are specified, this also specified the spanning tree which may well not
c be a valid tree.  Not much option but to
c (a) ensure constrained rates, or rates for which errors are particularly
c	wanted are in the tree (and chack that tree so found is valid)
c (b) Find any tree that fulfils these criteria -this gives a set
c	of connections that are to be set by MR
c (c) For each of these connections (i-j say), decide whether it is
c	q(i,j) of q(j,i) that is to be set
c
c NB the constraints apply to RATE constants, but spanning tree deals only
c with connections (edges). If both q(2,3) and q(3,2) are constrained (as when
c ensuring independence in 'Milone' for example), they are both part of the
c same edge so two constraints require addition of only one edge to the tree.
c
c GETREVT is version of GETREV that uses the spanning
c tree method 11/05/03 05:03pm. Both automr() and isetmr() are redundant
c with this method, but leave in while both methods still in prog
c
c Modif 06/19/03 09:53am by adding TESTMR
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
	character*1 ans
cc  Local arrays to get valid MR routes if selected one is not valid
c
	integer nsc(50),im(50,100),jm(50,100)
	integer nsc1(50),im1(50,100),jm1(50,100)
c=	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer*4 IQ(100,100),jmic(200)
	COMMON/qblk/IQ
	logical discprt
	common/dp/discprt
	logical allmr,useini,samedge
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
	integer irate(200),jrate(200)
c constraints
	
c For PRIM
	character*10 titlep(200)
	allocatable::iedge,Jtree,incirc
	integer*4 ICin(2,200)	!IC for connections to be IN tree
	integer*4 ICout(2,200)	!IC for connections to be NOT IN tree
	integer*4 iedge(:,:)	!IC for specified connections
	integer*4 jtree(:,:)
	integer*4 incirc(:)
	integer*4 JCON(100,100)
	COMMON/QPAR/NCON,IC(2,200)

c
	idest=0	!set to 1 to return to SETCONS if errors found
c
	allocate(iedge(2,200),
     &    Jtree(100,100),incirc(100))
c Make 'adjacency' matrix corresponding to IC()
	nd1=200
	nd2=100
	call IC_JCON(IC,ncon,nd1,JCON,k,nd2)
c
	
	ndim=100 		!size of JCON array
	ncmax=50		!max no of cycles

	
c
	if(ncin.gt.0) then
c	   Alternative (much simpler) check
c 		-convert ICin to jtree form and use NCONS
	   call IC_JCON(ICin,ncin,nd1,Jtree,k,nd2)
	   call NCONS(Jtree,k,ncon1,nstate,ndim)  !returns ncon1
	   
	   write(8,5) nstate,ncon1
5	   format(
     &	' Specified routes have ',i3,' states, ',i3,' connections')
	   if(ncon1.gt.nstate-1) then
	
		if(allmr) then
		   imes=gmdisplaymessagebox('',
     &	   ' The constraints contain cycle(s): invalid, try again',
     &		gstop,gok)
		   write(8,161)
161		   format(
     &	 ' The constraints contain cycle(s): invalid, try again')
c		   goto 131
		   idest=1	!return to SETCONS
		   
		   goto 99
		else
		   imes=gmdisplaymessagebox('',
     &' The constraints contain cycle(s):cannot set all cycles by MR ',
     &		gstop,gok)
		   write(8,162)
162		   format(
     & ' The constraints contain cycle(s): cannot set all cycles by MR')
		   
		endif
	   endif
c
	   ncmax=k	!max cycle size to be found
	   call CYCQ1(k,ncin,ICin,ncyc2,nsc0,im1,jm1,ncmax)
	   if(ncyc2.ne.0.and.allmr) then
		imes=gmdisplaymessagebox('',
     &' From CYCQ1, these routes contain cycle(s): invalid, try again',
     &		gstop,gok)
		write(8,16) ncyc2
16		format(
     &	' From CYCQ1, these routes contain ',i3,
     &	' cycle(s): invalid, try again')
		idest=1	!return to SETCONS
	
		goto 99
	   endif
	endif
c
c
      call PRIM3(JCON,ICin,ncin,ICout,ncout,Iedge,nedge,nerr,k,ndim)
c
c Check the result
c
c Convert iedge to k x k 'adjacency matrix' form in Jtree
	nd1=200
	nd2=100
	nc=nedge
	call IC_JCON(Iedge,nc,nd1,Jtree,k,nd2)
	call NCONS(Jtree,k,ncon1,nstate,ndim)  !returns ncon1
	if(ncon1.ne.nstate-1) then
	  imes=gmdisplaymessagebox('',
     &' ERROR: number of connections in spanning tree is not number of 
     &states - 1',ginformation,gok)  
	   if(discprt) write(8,19) ncon1,nstate
19	   format(' ERROR: number of connections in spanning tree = ',i3,
     &	' is not number of states - 1')
	   
	endif
c
	if(nerr.eq.1) then
	   	imes=gmdisplaymessagebox('',
     &' The routes that are specified to be in the tree are NOT all in',
     &			ginformation,gok)
	   if(discprt) write(8,20)
20	   format(
     &' The routes that are specified to be in the tree are NOT all in')
	else if(nerr.eq.2) then
	imes=gmdisplaymessagebox('',
     &' The routes that are specified to not be in the tree are NOT all 
     &excluded',ginformation,gok)   
	   if(discprt) write(8,21)
21	   format(
     &' The routes that are specified to be not in the tree are NOT all',
     & ' excluded')
	endif
c
	
	write(8,121)
121	format(/,' Edges in spanning tree')
	do j=1,nedge
	 
	   if(discprt) write(8,110) j,iedge(1,j),iedge(2,j)
110	   format(' ',i3,': ',i3,' - ',i3)
	enddo
c
c NB it is not possible to pre-specify all of the MR rates
c (see above), so all that can be done here is to check that
c the right number of MR edges has been found, and to choose between
c q(i,j) and q(j,i) for each of them
c
c The rate (i.e. connection + direction) to be set by mr has
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
	if(ncyc.gt.0.and.useini) then
	   
143	   format(/,
     & ' Last run set the following rates by microscopic reversibility')
	   do n=1,ncyc
		i1=im1(n,1)		!default
		j1=jm1(n,1)
		call GETM(i1,j1,m,npar,irate,jrate)
		if(obeymr(n)) then
c		   print 142,n,i1,j1,titlep(m)
142		   format(
     &  ' Route ',i2,': q(',i3,',',i3,') = ',a10,' set by MR')
		else
c		   print 145,n,i1,j1,titlep(m)
145		   format(
     &  ' Route ',i2,': q(',i3,',',i3,') = ',a10,' NOT set by MR')
		endif
	   enddo
	  imes=gmdisplaymessagebox('','Use old rates set by MR',
     &gquestion,gyesno)
	   if(imes.eq.gyesbutton) then
		do n=1,ncyc
		   nsc(n)=nsc1(n)
		   do m=1,nsc(n)
			im(n,m)=im1(n,m)
			jm(n,m)=jm1(n,m)
		   enddo
		enddo
		goto 92	!skip asking
	   endif
	endif

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
c			print 144,i,j,(incirc(n),n=1,ncirc)
			if(discprt) write(8,144) i,j,(incirc(n),n=1,ncirc)
144			format(' Error in circuit: i, j = ',i3,',',i3,/,
     &		5(20i3,/))
		   else
			if(ncirc.le.20) then
c			   print 41,nm,(incirc(n),n=ncirc,1,-1)
41			   format(/,' Cycle ',i3,':',20i3)
			else
c			   print 42,nm,(incirc(n),n=ncirc,1,-1)
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
c Print results
92	do n=1,ncyc
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
99	continue
c
	if(allmr.and.nerr.ne.0) idest=1	!return to SETCONS
c
	deallocate(iedge,
     &    Jtree,incirc)
c Now set jmic() here
	do i=1,200
	   jmic(i)=0	!zero in case mic rev parameter has been changed
	enddo
	nmr=0			!number of cycles that obey m.r. (.le.ncyc)
	if(ncyc.gt.0) then
	   do i1=1,ncyc
		i=isetmr(i1)	!actual cycle number
		if(obeymr(i).and.(.not.automr(i))) then	!if cycle #i does not obey mr (see GETREV)
		   m=IQ(im(i,1),jm(i,1))
		   jmic(m)=1		!fixed by micro rev
		   nmr=nmr+1
		endif
	   enddo
	endif
	RETURN
	end


