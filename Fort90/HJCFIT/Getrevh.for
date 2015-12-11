	subroutine GETREVH(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,k,
     &     titlep,npar,irate,jrate,useini,nmr,jmic,idest)
c To set MR rates with spanning tree method
c
c GETREVH is version of GETREVT for HJCFIT where constraints are set first
c and can therefore be included in the tree without typing them again
c NB cannot ask for all MR rates to be specified in advance, because if all
c are specified, this also specifies the whole spanning tree, so PRIM not
c needed at all -user would have to specify a valid tree. Not much option but to
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
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
	real*8 ec501,xqlo,xqhi
	common/ec/fixec50,nmod,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel
c nmod=imod0 (already in commom/model)
	real*8 ec502,xqlo2,xqhi2
	real*4 conc_ec1(10),conc_ec2(10)
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
c For PRIM
	character*10 titlep(200)
	allocatable::ICin,ICout,iedge,Jcon,Jtree,incirc
	integer*4 ICin(:,:)	!IC for connections to be IN tree
	integer*4 ICout(:,:)	!IC for connections to be NOT IN tree
	integer*4 iedge(:,:)	!IC for specified connections
	integer*4 jtree(:,:)
	integer*4 incirc(:)
	integer*4 JCON(:,:)
	COMMON/QPAR/NCON,IC(2,200)

c
	idest=0	!set to 1 to return to SETCONS if errors found
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
c need to set things below in getqd1, before setcons, setec50
c	do i=1,ncmax
c	   obeymr(i)=.true.
c	   automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
c	   isetmr(i)=i		!order irrelevant here
c	enddo
108	format(/)
	print 1,ncyc
1	format(/,' Number of cycles in mechanism [',i2,'] = ')
	call INPUTi(ncyc)
	if(ncyc.eq.0) then
	   RETURN
	endif
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
	   nmr=0
	   do n=1,ncyc
		i1=im1(n,1)		!default
		j1=jm1(n,1)
		call GETM(i1,j1,m,npar,irate,jrate)
		if(obeymr(n)) then
		   nmr=nmr+1
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
	   ans='Y'
	   call DCASK('Use same again',ans,ans)
	   if(ans.eq.'Y') then
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
c
	print 108			!new line
	if(discprt) write(8,108)

c
7	iopt=1			!default
	if(.not.allmr) iopt=3	!default
	print 2,iopt
2	format(
     & ' (1) ALL cycles to be constrained by microscopic rev',/,
     & ' (2) NO cycles to be constrained by microscopic rev',/,
     & ' (3) Choose which cycles are to be constrained by mic rev',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   allmr=.true.
	   nmr=ncyc
	else if(iopt.eq.2) then
c	   ncyc=0		!should not be needed -do not want to save ncyc=0
	   nmr=0
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
c	   print 3
c3	   format(' Not yet implemented for spanning tree method')
c	   goto 7
	endif
c NB number of edges to be included will be less than number of constraints
c if, say q(2,3) and q(3,2) are both constrained.
131	continue
c
c First the usual constraints
c==	ncin=neq+nfixec50		!number already constrained
	ncin=0	!count the number of edges that must be in the tree

	do j=1,neq
	   samedge=.false.
	   ie1=ie(j)
	   je1=je(j)
	   do j1=1,j-1	!look at earlier constraints to see if edge repeated
		if((ie(j1).eq.ie1.and.je(j1).eq.je1).or.
     &	   (je(j1).eq.ie1.and.ie(j1).eq.je1)) then	!2nd cond may be true
		   samedge=.true.
		endif
	   enddo
	   if(.not.samedge) then
		ncin=ncin+1
		ICin(1,ncin)=ie(j)
		ICin(2,ncin)=je(j)
	   endif
	enddo
c Next rates constrained by ec50 -check that edges that contain rates i50->j50
c (and i502->j502 if two fixed) are not already in tree
	if(nfixec50.ge.1) then
	   samedge=.false.
	   do j1=1,ncin	!already in
		if((i50.eq.ICin(1,j1).and.j50.eq.ICin(2,j1)).or.
     &	   (j50.eq.ICin(1,j1).and.i50.eq.ICin(2,j1))) then
		   samedge=.true.
		endif
	   enddo
	   if(.not.samedge) then
		ncin=ncin+1
		ICin(1,ncin)=i50
		ICin(2,ncin)=j50
	   endif
	endif
	if(nfixec50.eq.2) then
	   samedge=.false.
	   do j1=1,ncin	!already in
		if((i502.eq.ICin(1,j1).and.j502.eq.ICin(2,j1)).or.
     &	   (j502.eq.ICin(1,j1).and.i502.eq.ICin(2,j1))) then
		   samedge=.true.
		endif
	   enddo
	   if(.not.samedge) then
		ncin=ncin+1
		ICin(1,ncin)=i502
		ICin(2,ncin)=j502
	   endif
	endif
c
c Print those already constrained, and ask if any more

	if(ncin.gt.0) then
	   print 12,ncin,(icin(1,j),icin(2,j),j=1,ncin)
12	   format(/,
     &' The following ',i3,' connections have one (or 2) constraints',/,
     & ' and are therefore already set to be NOT found by MR',/,
     & '  (and so must be in the tree) ',/,
     &  5(10(i3,'-',i3)/))
	endif
c
	ncin1=0
	print 13,ncin1
13	format(/,
     &' Number of additional routes that should NOT be set by MR [',
     & i3,'] = ')
	call INPUTi(ncin1)
c
	if(ncin1.gt.0) then
	   ncin=ncin+ncin1
	   do n=ncin+1,ncin
		print 14,n,i,j
14		format(' Route ',i2,': specify i,j [',i3,',',i3,'] = ')
		call INPUT2i(i,j)
		ICin(1,n)=i
		ICin(2,n)=j
		if(jcon(i,j).eq.0) then
		   call BELL(2)
		   print 70,i,j
70		   format(' There is no connection between ',i3,
     &		' and ',i3,'. Try again')
		   pause
		   goto 131
		endif
		print 15,n,i,j
		write (8,15) n,i,j
15		format(' ',i2,': route ',i3,',',i3,' must not be set by MR')
	   enddo
	endif
c
	if(ncin.gt.0) then
c	   Alternative (much simpler) check
c 		-convert ICin to jtree form and use NCONS
	   call IC_JCON(ICin,ncin,nd1,Jtree,k,nd2)
	   call NCONS(Jtree,k,ncon1,nstate,ndim)  !returns ncon1
	   print 5,nstate,ncon1
	   write(8,5) nstate,ncon1
5	   format(
     &	' Specified routes have ',i3,' states, ',i3,' connections')
	   if(ncon1.gt.nstate-1) then
		call BELL(1)
		if(allmr) then
		   print 161
		   write(8,161)
161		   format(
     &	 ' The constraints contain cycle(s): invalid, try again')
c		   goto 131
		   idest=1	!return to SETCONS
		   pause
		   goto 99
		else
		   print 162
		   write(8,162)
162		   format(' WARNING: ',/,
     & ' the constraints contain cycle(s) -cannot set ALL cycles by MR')
		   pause
		endif
	   endif
c
	   ncmax=k	!max cycle size to be found
	   call CYCQ1(k,ncin,ICin,ncyc2,nsc0,im1,jm1,ncmax)
	   if(ncyc2.ne.0.and.allmr) then
		call BELL(1)
		print 16,ncyc2
		write(8,16) ncyc2
16		format(
     &	' From CYCQ1, these routes contain ',i3,
     &	' cycle(s): invalid, try again')
		idest=1	!return to SETCONS
		pause
		goto 99
	   endif
	endif
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
c	   do n=1,ncyc
	   do n=1,ncout
		if(useini) then
		   i1=im1(n,1)		!default
		   j1=jm1(n,1)
		else
		   i1=im(n,1)		!default
		   j1=jm(n,1)
		endif
242		print 14,n,i1,j1
c14		format(' Route ',i2,': specify i,j [',i3,',',i3,'] = ')
		call INPUT2i(i1,j1)
		if(jcon(i1,j1).eq.0) then
		   call BELL(2)
		   print 70,i1,j1
c70		   format(' There is no connection between ',i3,
c     &		' and ',i3,'. Try again')
		   goto 1321
		endif
		call GETM(i1,j1,m,npar,irate,jrate)
		print 141,i1,j1,titlep(m)
141		format(
     &	' Rate q(',i3,',',i3,
     &	') = ',a10,' is to be set by micro rev OK [Y] ? ')
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
c		write (8,151) n,titlep(m),i1,j1
151		format(' ',i2,': ',a10,' = rate q(',i3,',',i3,
     &	') is to be set by MR')
	   enddo
	 endif
	endif
c
      call PRIM3(JCON,ICin,ncin,ICout,ncout,Iedge,nedge,
     &	nerr1,nerr2,nerr3,nerr4,k,ndim)
c
c Convert iedge to k x k 'adjacency matrix' form in Jtree
	nd1=200
	nd2=100
	nc=nedge
	call IC_JCON(Iedge,nc,nd1,Jtree,k,nd2)
	call NCONS(Jtree,k,ncon1,nstate,ndim)  !returns ncon1
c
c Check the result
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
	if(allmr.and.(nerr1.ne.0.or.nerr2.ne.0)) idest=1	!return to SETCONS
c
	deallocate(ICin,ICout,iedge,
     &    JCON,Jtree,incirc)
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


