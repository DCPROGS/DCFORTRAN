	subroutine GETBOUND(k,npar)
c To find automatically the number of ligands bound to each state
c Returns nbound(i)=number of ligands bound to state i (in common)
c Modif 11/13/99 03:07pm
c Returns nbound(i,j)=number of ligands if type j (j=1 or 2 at present)
c that are bound to state i (in common) -added common/LIG/
	allocatable:: assrate,dissrate
	logical assrate(:,:,:),dissrate(:,:,:)
	integer nbound(100,10)
	integer IQ(100,100)
	COMMON/QPAR/NCON,IC(2,200)
	character*1 ans
	common/nbnd/nbound
	common/qblk/IQ
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/LIG/nlig,IL(100)
c
	if(ncdep.eq.0) then
	   do i=1,k
		do j=1,2
		   nbound(i,j)=0
		enddo
	   enddo
	   RETURN
	endif
c
	print 123,(IC(1,j),ic(2,j),j=1,ncon)
123	format(' Connections= ',/,4(5(2i3,4x),/))
c	ALLOCATE(assrate(20,20,2),dissrate(20,20,2))
	ALLOCATE(assrate(k,k,2),dissrate(k,k,2))
c Will save time to define an array for each rate constant to indicate
c directly whether it is an association rate constant
	do i=1,k
	   do j=1,k
		do n=1,2
	   	   assrate(i,j,n)=.false.
	         dissrate(i,j,n)=.false.
		enddo
	   enddo
	enddo
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   do n=1,ncdep
		if(ix(n).eq.i.and.jx(n).eq.j) then
		   assrate(i,j,IL(n))=.true.
		   dissrate(j,i,IL(n))=.true.
		endif
	   enddo
	enddo
c
	do i=1,k
	   do j=1,2
		nbound(i,j)=-1	!not yet defined
	   enddo
	enddo
c
c First find a state that has NONE bound
c Usually highest numbered state will have none bound
	print 41,k
41	format(/,' State ',i2,' has no ligand(s) bound -O.K. [Y] ?')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   izero=k
	   nbound(izero,1)=0
	   nbound(izero,2)=0
	else
	   nbound(k,1)=-1	!not defined yet
	   nbound(k,2)=-1	!not defined yet
	   print 42
42	   format(
     &	' Specify a state number that has no ligand bound: i = ')
	   call INPUTi(i)
	   nbound(i,1)=0
	   nbound(i,2)=0
	   izero=i
	endif
c Now have a state with NONE bound.  Look at all states connected to it: any
c that are connected via an assoc rate must have ONE bound
c
	do j=1,k
	   i1=IC(1,j)
	   j1=IC(2,j)
	   if(i1.eq.izero) then		!connection FROM state izero
		do n=1,nlig
		   nbound(j1,n)=0
		   if(assrate(i1,j1,n)) then	!assoc of ligand n
			nbound(j1,n)=1
		   else
			nbound(j1,n)=0
		   endif
		enddo
	   else if(j1.eq.izero) then		!connection FROM state izero
	      j1=IC(1,j)
	      i1=IC(2,j)
		do n=1,nlig
		   nbound(j1,n)=0
		   if(assrate(i1,j1,n)) then	!assoc of ligand n
			nbound(j1,n)=1
		   else
			nbound(j1,n)=0
		   endif
		enddo
	   endif
	enddo
c At this point should have all the states connected to izero that have
c 0 or 1 bound.  Now must look at every state connected to each of these -they
c are identifiable by fact that nbound=0 or 1 (not -1)
c
1	nleft=0		!number of values still left to be defined
	do istart=1,k
	  do n1=1,nlig
	   if(nbound(istart,n1).eq.-1) then
		nleft=nleft+1
	   else
		do j=1,ncon
		   i1=IC(1,j)
		   j1=IC(2,j)
		   if(i1.eq.istart) then		!connection FROM state istart
			do n=1,nlig
			   nbound(j1,n)=0
			   if(assrate(i1,j1,n)) then	!assoc of ligand n
				nbound(j1,n)=nbound(i1,n) + 1
			   else if(dissrate(i1,j1,n)) then
				nbound(j1,n)=nbound(i1,n) - 1
			   else
				nbound(j1,n)=nbound(i1,n)
			   endif
			enddo
		   else if(j1.eq.istart) then		!connection FROM state istart
			j1=IC(1,j)
			i1=IC(2,j)
			do n=1,nlig
			   nbound(j1,n)=0
			   if(assrate(i1,j1,n)) then	!assoc of ligand n
				nbound(j1,n)=nbound(i1,n) + 1
			   else if(dissrate(i1,j1,n)) then
				nbound(j1,n)=nbound(i1,n) - 1
			   else
				nbound(j1,n)=nbound(i1,n)
			   endif
			enddo
c			if(assrate(i1,j1)) then
c			   nbound(j1)=nbound(i1) + 1
c			else if(dissrate(i1,j1)) then
c			   nbound(j1)=nbound(i1) - 1
c			else
c			   nbound(j1)=nbound(i1)
c			endif
		   endif
		enddo
	   endif
	  enddo
	enddo
c  Now need to repeat this for all the states newly defined
	if(nleft.gt.0) goto 1
c
	DEALLOCATE(assrate,dissrate)
	RETURN
	end


