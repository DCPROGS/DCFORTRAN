	program TSPAN
c VERSION USING PRIM.FOR
c If states i and j are connected then IC(1,m)=i, IC(2,m)=j, m=1,ncon
c IC is IC(2,ncon).
c May be easier to work with JCON(i,j)=0 if i,j not connected, =1 if connected
c where JCON(k,k), k=number of states (symmetric -only lower triangle needed)
	integer IC(2,200),JCON(100,100)
	integer ict(2,200)	!IC for tree
	integer icspec(2,200)	!IC for specified connections
	integer iedge(2,200)	!IC for specified connections
	integer jtree(100,100)
	integer incirc(100)	!for Dowsland's circuit
	character*1,ans
	integer NSC(50),IM(50,100),JM(50,100)
	integer NSC1(50),IM1(50,100),JM1(50,100)
c  for printing
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	logical discprt
	common/dp/discprt
c
c
	filnam='tspan.prt'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
c
	print 30,imod
30	format(' (1) cube; (2) 4x4 state grid. Model number [',i2,'] = ')
	call INPUTi(imod)

	SELECT CASE(imod)
	CASE(1)
c For cube
	k=8
	ncon=12
c front face =1,2,3,4
	ic(1,1)=1
	ic(2,1)=2
	ic(1,2)=2
	ic(2,2)=3
	ic(1,3)=3
	ic(2,3)=4
	ic(1,4)=4
	ic(2,4)=1
c back face =5,6,7,8
	ic(1,5)=5
	ic(2,5)=6
	ic(1,6)=6
	ic(2,6)=7
	ic(1,7)=7
	ic(2,7)=8
	ic(1,8)=8
	ic(2,8)=5
c front to back connections
	ic(1,9)=1
	ic(2,9)=6
	ic(1,10)=2
	ic(2,10)=7
	ic(1,11)=3
	ic(2,11)=8
	ic(1,12)=4
	ic(2,12)=5
c
	CASE(2)
c FOR 4x4=16 STATES
	k=16
	ncon=24
	ic(1,1)=1
	ic(2,1)=2
	ic(1,2)=1
	ic(2,2)=5
	ic(1,3)=2
	ic(2,3)=3
	ic(1,4)=2
	ic(2,4)=6
	ic(1,5)=3
	ic(2,5)=4
	ic(1,6)=3
	ic(2,6)=7
	ic(1,7)=4
	ic(2,7)=8

	ic(1,8)=5
	ic(2,8)=6
	ic(1,9)=5
	ic(2,9)=9
	ic(1,10)=6
	ic(2,10)=7
	ic(1,11)=6
	ic(2,11)=10
	ic(1,12)=7
	ic(2,12)=8
	ic(1,13)=7
	ic(2,13)=11
	ic(1,14)=8
	ic(2,14)=12

	ic(1,15)=9
	ic(2,15)=10
	ic(1,16)=9
	ic(2,16)=13
	ic(1,17)=10
	ic(2,17)=11
	ic(1,18)=10
	ic(2,18)=14
	ic(1,19)=11
	ic(2,19)=12
	ic(1,20)=11
	ic(2,20)=15
	ic(1,21)=12
	ic(2,21)=16

	ic(1,22)=13
	ic(2,22)=14
	ic(1,23)=14
	ic(2,23)=15
	ic(1,24)=15
	ic(2,24)=16

	END SELECT
c ===test subroutines
c	do j=1,ncon
c	   print 30,j,ic(1,j),ic(2,j)
c	   write(8,30) j,ic(1,j),ic(2,j)
c30	   format(1x,i2,3x,2i3)
c	enddo
c	nd1=200
c	nd2=100
c	call IC_JCON(IC,ncon,nd1,JCON,k,nd2)
c	do i=1,k
c	   print 1,(jcon(i,j),j=1,k)
c	    if(discprt) write(8,1) (jcon(i,j),j=1,k)
cc1	   format(20i3)
c	enddo
c	call JCON_IC(IC,ncon,nd1,JCON,k,nd2)
c	do j=1,ncon
c	   print 30,j,ic(1,j),ic(2,j)
c	   write(8,30) j,ic(1,j),ic(2,j)
cc30	   format(1x,i2,3x,2i3)
c	enddo
c===end test
c
	do i=1,k
	   do j=1,k
		JCON(i,j)=0
c		Jtree(i,j)=0
		do m=1,ncon
		   if((IC(1,m).eq.i.and.IC(2,m).eq.j).or.
     &	      (IC(1,m).eq.j.and.IC(2,m).eq.i)) JCON(i,j)=1
		enddo
	   enddo
	enddo
	if(imod.eq.1) then
	   print 11
	   write(8,11)
11	   format(' Connections for cube')
	else if(imod.eq.2) then
	   print 31
	   write(8,31)
31	   format(' Connections for 4x4 state grid')
	endif
	do i=1,k
	   print 1,(jcon(i,j),j=1,k)
	    if(discprt) write(8,1) (jcon(i,j),j=1,k)
1	   format(20i3)
	enddo
c
	ndim=100
	is1=1
20	continue
c
c Specify nc routes that must be in the tree
c======debug
c	goto 70
c======
131	print 13,nc
13	format(/,
     &' Number of routes that MUST be in the tree (not MR) [',i3,'] = ')
	call INPUTi(nc)
	if(nc.gt.0) then
	   do n=1,nc
		print 14,n
14		format(' Route ',i2,': specify i,j = ')
		call INPUT2i(i,j)
		icspec(1,n)=i
		icspec(2,n)=j
		print 15,n,i,j
		write (8,15) n,i,j
15		format(' ',i2,': route ',i3,',',i3,' must not be set by MR')
	   enddo
c	   Alternative (much simpler) check
c 		-convert icspec to jtree form and use NCONS
	   nd1=200
	   nd2=100
	   call IC_JCON(ICspec,ncon,nd1,Jtree,k,nd2)
	   call NCONS(Jtree,k,ncon1,nstate,ndim)
	   print 5,nstate,ncon1
	   write(8,5) nstate,ncon1
5	   format(
     &	' Specified routes have ',i3,' states, ',i3,' connections')
	   if(ncon1.gt.nstate-1) then
		print 161
	 	write(8,161)
161		format(' These routes contain cycle(s): invalid, try again')
c		goto 131
	   endif
c
	   ncmax=k	!max cycle size to be found
	   call CYCQ1(k,nc,icspec,ncyc,nsc0,im1,jm1,ncmax)
	   if(ncyc.ne.0) then
		call BELL(1)
		print 16,ncyc
		write(8,16) ncyc
16		format(
     &	' From CYCQ1, these routes contain ',i3,
     &	' cycle(s): invalid, try again')
		goto 131
	   endif
	else
	   print 10,k,is1
10	   format(
     & ' Starting state in search for tree (1 to ',i2,') [',i2,'] = ')
	   call INPUTi(is1)
	endif
c======test debug
c70	continue
c	nc=4
c	icspec(1,1)=1
c	icspec(2,1)=2
c	icspec(1,2)=6
c	icspec(2,2)=7
c	icspec(1,3)=5
c	icspec(2,3)=8
c	icspec(1,4)=4
c	icspec(2,4)=3
cc	nc=3
c	icspec(1,1)=2
c	icspec(2,1)=3
c	icspec(1,2)=6
c	icspec(2,2)=7
c	icspec(1,3)=4
c	icspec(2,3)=3
      call PRIM(JCON,ICspec,nc,Iedge,nedge,k,ndim)

	print 121
	write(8,121)
121	format(/,' Edges in spanning tree')
	do j=1,nedge
	   print 110,j,iedge(1,j),iedge(2,j)
	   if(discprt) write(8,110) j,iedge(1,j),iedge(2,j)
110	   format(' ',i3,': ',i3,' - ',i3)
	enddo
	pause
c
	ncon=nedge
c Convert iedge to kxk form in Jtree
	nd1=200
	nd2=100
	ncon=nedge
	call IC_JCON(Iedge,ncon,nd1,Jtree,k,nd2)
c==	call SPANTREE(JCON,Jtree,is1,k,ncon,ICspec,nc,ndim)
	print 2
	write(8,2)
2	format(/,/)
	print 12
	write(8,12)
12	format(' Connections for spanning tree ')
	do i=1,k
	   print 1,(jtree(i,j),j=1,k)
	    if(discprt) write(8,1) (jtree(i,j),j=1,k)
c1	   format(20i3)
	enddo
	pause
c check which links are missing in the tree
	nmr=0
	ncont=0	!ncon for tree connections
c initialise ic() array for tree
	do i=1,2
	   do j=1,100
		ict(i,j)=0
	   enddo
	enddo
c
	do i=2,k	!check lower triangle only
	   do j=1,i-1
		if(jtree(i,j).eq.1) then
		   ncont=ncont+1
		   ict(1,ncont)=i
		   ict(2,ncont)=j
		endif
	   enddo
	enddo
c
	nd1=200
	nd2=100
	call NCONS(Jtree,k,ncon1,nstate,ndim)
	print 51,nstate,ncon1
	write(8,51) nstate,ncon1
51	format(
     &	' Final tree has ',i3,' states, ',i3,' connections')
c double check with cycq1.
	ncmax=k	!max cycle size to be found
	call CYCQ1(k,ncont,ict,ncyc,nsc0,im1,jm1,ncmax)
	print 21,ncyc
	write(8,21) ncyc
21	format(' Number of cycles (from cycq) in spanning tree = ',i3)
	print 3
	write(8,3)
3	format(/,' Links missing in tree (=MR routes)',/)
c
c To find cycle for each MR route (a) add that route (only) to JTREE(), and
c call CYCQ1 to find cycles in resulting structure -with luck get only one
c cycle -if more than one take the smallest
c To use original CYCQ need to convert the (modified) JTREE it the IC() form
c and find ncon for it.
c
	do i=2,k	!check lower triangle only
	   do j=1,i-1
		if(jcon(i,j).eq.1.and.jtree(i,j).eq.0) then
		   nmr=nmr+1
c		   im(nmr,1)=i	!set first im() to mr parameter
c		   jm(nmr,1)=j
		   print 4,nmr,i,j
		   write(8,4) nmr,i,j
4		   format(/,' MR route ',i3,': i = ',i3,'   j = ',i3)
c         add the i,j route (only) to ICT() -the tree connections
		   ncont1=ncont+1
		   ict(1,ncont1)=i
		   ict(2,ncont1)=j
		   call CYCQ1(k,ncont1,ict,ncyc,nsc,im,jm,ncmax)
c		   print 22,ncyc		!must be 1
c		   write(8,22) ncyc
c22		   format(
c     &		' Number of cycles when this route added to tree = ',i3)
		   do n=1,ncyc
	      	print 1311,im(n,1),jm(n,1)
			if(discprt) write(8,1311)im(n,1),jm(n,1)
1311			format(2i3,'  (calc by micro rev)')
			print 128,(im(n,m),jm(n,m),m=2,nsc(n))
			if(discprt) write(8,128)(im(n,m),jm(n,m),m=2,nsc(n))
128			format(2(5(2i3,4x)))
		   enddo
c now test Dowsland's circuit routine
		   call CIRCUIT(i,j,incirc,ncirc)
		   print 41,(incirc(n),n=1,ncirc)
		   write(8,41) (incirc(n),n=1,ncirc)
41		   format(' MR circuit: ',10i4)
		endif
	   enddo
	enddo
c
c
	ans='Y'
	call DCASK(' Try another ',ans,ans)
	if(ans.eq.'Y') goto 20
c
	end

	subroutine IC_JCON(IC,ncon,nd1,JCON,k,nd2)
c Converts connections from IC(2,ncon) form to JCON(k,k) form
c Input IC, ncon,k
c Output JCON
	integer*4 IC(2,nd1),JCON(nd2,nd2)
c
	do i=1,nd2
	   do j=1,nd2
		JCON(i,j)=0
	   enddo
	enddo
c
	do i=1,k
	   do j=1,k
		do m=1,ncon
		   if((IC(1,m).eq.i.and.IC(2,m).eq.j).or.
     &	      (IC(1,m).eq.j.and.IC(2,m).eq.i)) JCON(i,j)=1
		enddo
	   enddo
	enddo
	RETURN
	end


	subroutine JCON_IC(IC,ncon,nd1,JCON,k,nd2)
c Converts connections from JCON(k,k) form to IC(2,ncon) form
c Input JCON,k
c Output IC, ncon

	integer*4 IC(2,nd1),JCON(nd2,nd2)
c
	do i=1,2
	   do j=1,nd1
		IC(i,j)=0
	   enddo
	enddo
c
	ncon=0
	do i=1,k-1	!check upper triangle only
	   do j=i+1,k
		if(jcon(i,j).eq.1) then
		   ncon=ncon+1
		   ic(1,ncon)=i
		   ic(2,ncon)=j
		endif
	   enddo
	enddo
c	do i=2,k	!check lower triangle only
c	   do j=1,i-1
c		if(jcon(i,j).eq.1) then
c		   ncon=ncon+1
c		   ic(1,ncon)=i
c		   ic(2,ncon)=j
c		endif
c	   enddo
c	enddo
	RETURN
	end
