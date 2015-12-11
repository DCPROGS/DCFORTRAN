	subroutine RENUMB(inew,dgamma,statname,nbound,nlig,kmax)
c To renumber states, according to inew() in
c conductances, c-dep rates, v-dep rates, connections and cycles
c
c Modified 04/15/01 08:53pm for 100 states, 10 ligands
c Need to renumber also nbound(), statname(), irate(), jrate()
c
c This version is like MSWAP but definition of inew is reversed (as it is
c in SCJUMP and NEWMOD) compared with original definition used
c in MSWAP and HJCFIT.
c kmax=dimension of Q = kA+kB+kC+kD
	real*8 dgamma(100),dtemp(100)
	integer inew(100),itemp(100),jtemp(100)
	logical discprt
	integer im1(50,100),jm1(50,100)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
c
	character*10 statname(100),sntemp(100)
	integer nbound(100,10),nbtemp(100,10)
c
	common/dp/discprt
c
c
c Renumber conductances
	do i=1,kmax
	   dtemp(i)=dgamma(inew(i))
	enddo
	do i=1,kmax
	   dgamma(i)=dtemp(i)
	enddo
c
c Renumber statname()
	do i=1,kmax
	   sntemp(i)=statname(inew(i))
	enddo
	do i=1,kmax
	   statname(i)=sntemp(i)
	enddo
c
c Renumber nbound
c	((nbound(i,j),i=1,k),j=1,nlig)					4*k*nlig
	if(nlig.gt.0) then
	   do i=1,kmax
		do j=1,nlig
		   nbtemp(i,j)=nbound(inew(i),j)
		enddo
	   enddo
	   do i=1,kmax
		do j=1,nlig
		   nbound(i,j)=nbtemp(i,j)
		enddo
	   enddo
	endif
c
c Renumber conc-dep states
	if(ncdep.gt.0) then
	   do n=1,ncdep
		do i=1,kmax
		   if(ix(n).eq.i) itemp(n)=inew(i)
		   if(jx(n).eq.i) jtemp(n)=inew(i)
		enddo
		ix(n)=itemp(n)
		jx(n)=jtemp(n)
	   enddo
	endif
c
c Renumber V-dep states
	if(nvdep.gt.0) then
	   do n=1,nvdep
		do i=1,kmax
		   if(iv(n).eq.i) itemp(n)=inew(i)
		   if(jv(n).eq.i) jtemp(n)=inew(i)
		enddo
		iv(n)=itemp(n)
		jv(n)=jtemp(n)
	   enddo
	endif
c
c Renumber connections
	do m=1,ncon
	   do i=1,kmax
		if(IC(1,m).eq.i) itemp(m)=inew(i)
		if(IC(2,m).eq.i) jtemp(m)=inew(i)
	   enddo
	   ic(1,m)=itemp(m)
	   ic(2,m)=jtemp(m)
	enddo
c
c Now renumber cycles
	if(ncyc.eq.0) goto 1
	do L=1,ncyc
	   do m=1,nsc(L)
		do i=1,kmax
		   if(im(L,m).eq.i) im1(L,m)=inew(i)
	  	   if(jm(L,m).eq.i) jm1(L,m)=inew(i)
		enddo
		im(L,m)=im1(L,m)
		jm(L,m)=jm1(L,m)
	   enddo
	enddo
c
      print 40
      if(discprt) write(8,40)
40	format(/,' After renumbering of states the cycles are:')
	do L=1,ncyc
	   print 126, L
	   print 1311,im(L,1),jm(L,1)
	   print 127,(im(L,m),jm(L,m),m=2,nsc(L))
	   if(discprt) then
		write(8,126)L
		write(8,1311)im(L,1),jm(L,1)
		write(8,127)(im(L,m),jm(L,m),m=2,nsc(L))
	   endif
126	   format(' Cycle # ',i3)
1311	   format(2i3,'  (calc by micro rev)')
127	   format(2(5(2i3,4x),/))
	enddo
1	continue
c
	RETURN
	end

