	subroutine GETnLIG(k,npar)
c To find automatically the number of ligands bound to each state
c Returns nbound(i)=mumber og ligands bound to state i (in common)
	allocatable:: nto,nfrom,assrate,dissrate,used
	integer nto(:),nfrom(:)
	logical assrate(:,:),dissrate(:,:),used(:,:)
	integer nbound(10)
	integer IQ(100,100)
	COMMON/QPAR/NCON,IC(2,200)
	logical alter
	character*1 ans
	common/nbnd/nbound
	common/qblk/IQ
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
c
	if(ncdep.eq.0) then
	   do i=1,k
		nbound(i)=0
	   enddo
	   RETURN
	endif
c
	ALLOCATE(nto(10),nfrom(10),assrate(20,20),dissrate(20,20),
     &	used(20,20))
c Will save time to define an array for each rate constant to indicate
c directly whether it is an association rate constant
	do i=1,k
	   do j=1,k
	      assrate(i,j)=.false.
	      dissrate(i,j)=.false.
	      used(i,j)=.false.
	   enddo
	enddo
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   do n=1,ncdep
		if(ix(n).eq.i.and.jx(n).eq.j) then
		   assrate(i,j)=.true.
		   dissrate(j,i)=.true.
		endif
	   enddo
	enddo
c
	do i=1,k
	   nto(i)=0
	   nfrom(i)=0
	   nbound(i)=-1	!not yet defined
	enddo
c
c First find the state(s) that have NO ligands bound
c Any state that has no assoc rate leading TO it is either unliganded,
c or fully-liganded.  The former has at least one assoc rate leading FROM it,
c but the latter has not,  so the two cases are distinguishable
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   if(assrate(i,j)) then
		nfrom(i)=nfrom(i)+1	!assoc rate leads FROM state i TO state j
	   else if(dissrate(i,j)) then
	     nto(i)=nto(i)+1	!assoc rate leads TO state i, FROM state j
	   endif
	enddo
c
	do i=1,k
	   if(nto(i).eq.0.and.nfrom(i).gt.0) then
		nbound(i)=0			!state i is unliganded
	   endif
	enddo
c Usually highest numbered state will have none bound
	if(nbound(k).eq.0) then
	   print 41,k
41	   format(/,' State ',i2,' has no ligand bound -O.K. [Y] ?')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') then
		izero=k
	   else
		nbound(k)=-1	!not defined yet
		print 42
42		format(
     &	' Specify a state number that has no ligand bound: i = ')
		call INPUTi(i)
		nbound(i)=0
		izero=i
	   endif
	endif
	do i=1,k
	   if(i.ne.izero) nbound(i)=-1
	enddo
c Really need to locate ALL of those with 0 (nb) bound first
c then ALL of those with 1 (nb+1) bound, etc
c	i=izero		!first time
	nb=0			!first time
4	continue
c
2	alter=.false.
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
cc	   if(i.eq.5.or.j.eq.5) pause
	   if(used(i,j)) goto 3
	   if((nbound(i).eq.nb).and.nbound(j).ne.nb) then
		if(.not.assrate(i,j).and.(.not.dissrate(i,j))) then
		   nbound(j)=nb
		   alter=.true.
		   used(i,j)=.true.	!so each assoc rate used only once
		endif
	   endif
	enddo
	if(alter) goto 2	!any more to be set
c Once ALL states with nb (0 first time) are set then look at all of them
c and see whether any are connected to another state via an assoc rate, and if
c so set nbound(j) to nb+1
3	alter=.false.
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   if(used(i,j)) goto 31
	   if((nbound(i).eq.nb).and.(nbound(j).ne.nb+1)) then
		if(assrate(i,j)) then
		   nbound(j)=nb+1
		   alter=.true.
		   used(i,j)=.true.	!so each assoc rate used only once
		endif
	   endif
	enddo
	if(alter) goto 3
c No repeat for next number bound, up to maximum of k-1 bound
31	if(nb.lt.k-1) then
	   nb=nb+1
	   goto 4
	endif
c
c Find max number bound
	nmax=0
	do i=1,k
	   if(nbound(i).gt.nmax) nmax=nbound(i)
	enddo
c Above bit does not deal with fully-liganded states that have no assoc rate
c leading to them.  All states that have no assoc rates leading either to OR
c from them must be fully liganded
	do i=1,k
	   if(nto(i).eq.0.and.nfrom(i).eq.0) then
		nbound(i)=nmax
	   endif
	enddo
c
	DEALLOCATE(nto,nfrom,assrate,dissrate,used)
	RETURN
	end


