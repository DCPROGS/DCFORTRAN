	subroutine find_cubes(ncyc0,im0,jm0,nsc0)

c	find cubes by "peeling" the corners until reaches the middle
c	
	
	PARAMETER  N=200
	
	type subunit
		integer vertex(8)
		integer	facet(6)
		integer edge(6,4,2)
		integer	position
	end type subunit
	
	TYPE cube
		integer nmax
		integer	ix
		integer iy
		integer iz
		type (subunit) units(n)
	END TYPE cube
	type(cube) cubes
	integer IM0(50,100),JM0(50,100),ncube(100),imark(100),jmark(100)
	integer ic(100,4),isub(100),subcube(100,6),poscube(100)
	integer	vertx(24),vc(8),vertex(50,8),vex(8)
	logical discprt,apr
		integer nsc0(50)

	common/dp/discprt
c	cubes:
	
c	position=1	corner  
c	position=2	external 
c	position=3	external-middle  1 cube
c	position=4	internal
c	

c	Cycles belonging to:
c	jmark(icyc) =1 corner 1 cube
c	jmark(icyc) =2 second loop
c	jmark(icyc) =3 third loop
c	jmark(icyc) =4 internal 
c	jmark(icyc) =5 external top 1 cube	
c	jmark(icyc) =6 internal middle 2 cubes

c
	if(ncyc0.gt.0) then
			write(7,103)
103			format(' Microscopic reversibility')
			do i=1,ncyc0
				write(7,104) i,nsc0(i),(im0(i,j), j=1,nsc0(i))
104				format(i4,i4,5(5x,i3))
			enddo
	else
	goto 99
	endif
	if(ix.le.2.and.iy.le.2.and.iz.le.2) then
		nloop=1
	else if((ix.eq.3.and.iy.lt.3.and.iz.lt.3).or.
     &		(iy.eq.3.and.ix.lt.3.and.iz.lt.3).or.
     &		(iz.eq.3.and.iy.lt.3.and.ix.lt.3)) then
		nloop=2
	else if((ix.eq.3.and.iy.eq.3.and.iz.lt.3).or.
     &		(iy.eq.3.and.iz.eq.3.and.ix.lt.3).or.
     &		(iz.eq.3.and.ix.lt.3.and.iy.lt.3)) then
		nloop=3
	else if(ix.eq.3.and.iy.eq.3.and.iz.eq.3) then
		nloop=4
	endif   	
	
	indunit=0
	do i=1,ncyc0	
	do j=1,4

		do k=1,ncyc0
			imark(k)=0
		enddo
	
	
		do k=1,ncyc0
		
		do l=1,4
			if(im0(i,j).eq.im0(k,l)) then 
				imark(k)=1
				imark(i)=1
			endif
		enddo		 
		enddo
		
		icount=0
		do m=1,ncyc0
		if(imark(m).eq.1) then
			icount=icount+1
		endif
		enddo
		if(icount.eq.3) then ! cycle belongs to corner cubes
		ncube(i)=1
		l=0
			indunit=indunit+1
		cubes%units(indunit)%position=1
		do m=1,ncyc0
			if(imark(m).eq.1) then
				l=l+1
				cubes%units(indunit)%facet(l)=m
				
				cubes%units(indunit)%edge(l,1,1)=im0(m,1)
				cubes%units(indunit)%edge(l,1,2)=im0(m,2)
				cubes%units(indunit)%edge(l,2,1)=im0(m,2)
				cubes%units(indunit)%edge(l,2,2)=im0(m,3)
				cubes%units(indunit)%edge(l,3,1)=im0(m,3)
				cubes%units(indunit)%edge(l,3,2)=im0(m,4)
				cubes%units(indunit)%edge(l,4,1)=im0(m,4)
				cubes%units(indunit)%edge(l,4,2)=im0(m,1)
			
				
				jmark(m)=1
			endif
		enddo
		lf =3
	! now search for the other 3 cycles belonging to the cube:
		do k=1,4
			do l=2,3 
				do m=1,4
				if(cubes%units(indunit)%edge(1,k,1).eq.
     &			cubes%units(indunit)%edge(l,m,1) )then

     				IF(cubes%units(indunit)%edge(1,k,2).ne.
     &			cubes%units(indunit)%edge(l,m,2)) then
				
				do mn=1,ncyc0
				if (imark(mn).ne.1) then
				do jk=1,4
				if(cubes%units(indunit)%edge(l,m,1).eq.im0(mn,jk)) then
				do jl=1,4
				if(cubes%units(indunit)%edge(l,m,2).eq.im0(mn,jl)) then
				! check if valid:
				do j1=1,4
					if(j1.ne.jk.and.j1.ne.jl) then
					do i1=1,3
						do j2=1,4
							if(im0(mn,j1).eq.
     &						cubes%units(indunit)%edge(i1,j2,1).or.
     &						im0(mn,j1).eq.
     &						cubes%units(indunit)%edge(i1,j2,2))then
								lf=lf+1
								cubes%units(indunit)%facet(lf)=mn
								goto 6
								imark(mn)=1

							endif
						enddo
					enddo
					endif
				enddo
				endif
				enddo
6				continue
				endif
				enddo
				endif
				enddo
				
				goto 4
				endif
				endIF
				ENDDO
			enddo
4		CONTINUE
		enddo
		continue
	
		goto 1
		endif 
	enddo
1	continue
	enddo


	do i=1,ncyc0
		if(jmark(i).ne.1) then
		do j=1,4
		ic(i,j)=0
		do k=1,ncyc0
			imark(k)=0
		enddo
	
	
		do k=1,ncyc0
		
		do l=1,4
			if(im0(i,j).eq.im0(k,l)) then 
				imark(k)=1
				
				goto 5 
			endif
		enddo		 
5		continue
		enddo
		
		
		do m=1,ncyc0
			if(imark(m).eq.1) then
				ic(i,j)=ic(i,j)+1
			endif
		enddo
		
		enddo
		if(ic(i,1).eq.5.and.ic(i,2).eq.5.and.ic(i,3).eq.5.and.ic(i,4)
     &	.eq.5) jmark(i)=5
		if(ic(i,1).eq.12.and.ic(i,2).eq.12.and.ic(i,3).eq.12.and.ic(i,4)
     &	.eq.12) jmark(i)=6
		endif
2		continue
		
	enddo

c	Second loop for edge cubes
	do i=1,ncyc0
		if(jmark(i).ne.1) nloop=2
	enddo
	
	goto 71
	do i=1,indunit-1
		if(isub(i).ne.-1) then
		do j=i+1,indunit
			if(isub(j).ne.-1) then
			ij=0
			do k=1,6
			do l=1,6
			if(cubes%units(i)%facet(k).eq.cubes%units(j)%facet(l)) 
     &		ij=ij+1
			enddo
			enddo
			if(ij.gt.2) isub(j)=-1
			endif
		enddo
		endif
	enddo
	if(discprt) then 
	write(7,13) 
		do i=1,indunit
		if(isub(i).ne.-1) then
		write(7,200) cubes%units(i)%position
		do j=1,6
		k= cubes%units(i)%facet(j)
		if(k.ne.0) then
		write(7,201) k,(im0(k,l), l=1,nsc0(k))
		endif
		enddo
!200		format(/,' Position = ',i8,/)
!13		format(/,' =========================================',/)
		
!201		format(i4,5(5x,i3))	
		endif
		enddo
	endif	
71	if(nloop.eq.1) goto 99

c==================================================================
	
	do i=1,ncyc0
	if(jmark(i).ne.1) then	
	do j=1,4

		do k=1,ncyc0
			imark(k)=0
		enddo
	
	
		do k=1,ncyc0
		if(jmark(k).ne.1) then
		do l=1,4
			if(im0(i,j).eq.im0(k,l)) then 
				imark(k)=1
				imark(i)=1
			endif
		enddo	
		endif	 
		enddo
		
		icount=0
		do m=1,ncyc0
		if(imark(m).eq.1) then
			icount=icount+1
		endif
		enddo
		if(icount.eq.3) then ! cycle belongs to corner cubes
		ncube(i)=1
		l=0
		indunit=indunit+1
		cubes%units(indunit)%position=2
		do m=1,ncyc0
			if(imark(m).eq.1) then
				l=l+1
				cubes%units(indunit)%facet(l)=m
				
				cubes%units(indunit)%edge(l,1,1)=im0(m,1)
				cubes%units(indunit)%edge(l,1,2)=im0(m,2)
				cubes%units(indunit)%edge(l,2,1)=im0(m,2)
				cubes%units(indunit)%edge(l,2,2)=im0(m,3)
				cubes%units(indunit)%edge(l,3,1)=im0(m,3)
				cubes%units(indunit)%edge(l,3,2)=im0(m,4)
				cubes%units(indunit)%edge(l,4,1)=im0(m,4)
				cubes%units(indunit)%edge(l,4,2)=im0(m,1)
			
				
				jmark(m)=2
			endif
		enddo
		lf =3
	! now search for the other 3 cycles belonging to the cube:
		do k=1,4
			do l=2,3 
				do m=1,4
				if(cubes%units(indunit)%edge(1,k,1).eq.
     &			cubes%units(indunit)%edge(l,m,1) )then

     				IF(cubes%units(indunit)%edge(1,k,2).ne.
     &			cubes%units(indunit)%edge(l,m,2)) then
				
				do mn=1,ncyc0
				if (imark(mn).ne.1) then
				do jk=1,4
				if(cubes%units(indunit)%edge(l,m,1).eq.im0(mn,jk)) then
				do jl=1,4
				if(cubes%units(indunit)%edge(l,m,2).eq.im0(mn,jl)) then
				! check if valid:
				do j1=1,4
					if(j1.ne.jk.and.j1.ne.jl) then
					do i1=1,3
						do j2=1,4
							if(im0(mn,j1).eq.
     &						cubes%units(indunit)%edge(i1,j2,1).or.
     &						im0(mn,j1).eq.
     &						cubes%units(indunit)%edge(i1,j2,2))then
								lf=lf+1
								cubes%units(indunit)%facet(lf)=mn
								goto 61
								imark(mn)=1

							endif
						enddo
					enddo
					endif
				enddo
				endif
				enddo
61				continue
				endif
				enddo
				endif
				enddo
				
				goto 41
				endif
				endIF
				ENDDO
			enddo
41		CONTINUE
		enddo
		continue
	
		goto 11
		endif 
	
	enddo
11	continue
	endif
	enddo

c	Third loop for surface cubes
	do i=1,ncyc0
		if(jmark(i).ne.1.and.jmark(i).ne.2) nloop=3
	enddo
	
	goto 72
	do i=1,indunit-1
		if(isub(i).ne.-1) then
		do j=i+1,indunit
			if(isub(j).ne.-1) then
			ij=0
			do k=1,6
			do l=1,6
			if(cubes%units(i)%facet(k).eq.cubes%units(j)%facet(l)) 
     &		ij=ij+1
			enddo
			enddo
			if(ij.gt.2) isub(j)=-1
			endif
		enddo
		endif
	enddo
	if(discprt) then 
	write(7,13) 
		do i=1,indunit
		if(isub(i).ne.-1) then
		write(7,200) cubes%units(i)%position
		do j=1,6
		k= cubes%units(i)%facet(j)
		if(k.ne.0) then
		write(7,201) k,(im0(k,l), l=1,nsc0(k))
		endif
		enddo
	
		endif
		enddo
	endif	
	
72	if(nloop.eq.2) goto 99
	
	do i=1,ncyc0
	if(jmark(i).ne.1.and.jmark(i).ne.2) then	
	do j=1,4

		do k=1,ncyc0
			imark(k)=0
		enddo
	
	
		do k=1,ncyc0
		if(jmark(k).ne.1.and.jmark(k).ne.2) then
		do l=1,4
			if(im0(i,j).eq.im0(k,l)) then 
				imark(k)=1
				imark(i)=1
			endif
		enddo	
		endif	 
		enddo
		
		icount=0
		do m=1,ncyc0
		if(imark(m).eq.1) then
			icount=icount+1
		endif
		enddo
		if(icount.eq.3) then ! cycle belongs to corner cubes
		ncube(i)=1
		l=0
		indunit=indunit+1
		cubes%units(indunit)%position=3
		do m=1,ncyc0
			if(imark(m).eq.1) then
				l=l+1
				cubes%units(indunit)%facet(l)=m
				
				cubes%units(indunit)%edge(l,1,1)=im0(m,1)
				cubes%units(indunit)%edge(l,1,2)=im0(m,2)
				cubes%units(indunit)%edge(l,2,1)=im0(m,2)
				cubes%units(indunit)%edge(l,2,2)=im0(m,3)
				cubes%units(indunit)%edge(l,3,1)=im0(m,3)
				cubes%units(indunit)%edge(l,3,2)=im0(m,4)
				cubes%units(indunit)%edge(l,4,1)=im0(m,4)
				cubes%units(indunit)%edge(l,4,2)=im0(m,1)
			
				
				jmark(m)=3
			endif
		enddo
		lf =3
	! now search for the other 3 cycles belonging to the cube:
		do k=1,4
			do l=2,3 
				do m=1,4
				if(cubes%units(indunit)%edge(1,k,1).eq.
     &			cubes%units(indunit)%edge(l,m,1) )then

     				IF(cubes%units(indunit)%edge(1,k,2).ne.
     &			cubes%units(indunit)%edge(l,m,2)) then
				
				do mn=1,ncyc0
				if (imark(mn).ne.1) then
				do jk=1,4
				if(cubes%units(indunit)%edge(l,m,1).eq.im0(mn,jk)) then
				do jl=1,4
				if(cubes%units(indunit)%edge(l,m,2).eq.im0(mn,jl)) then
				! check if valid:
				do j1=1,4
					if(j1.ne.jk.and.j1.ne.jl) then
					do i1=1,3
						do j2=1,4
							if(im0(mn,j1).eq.
     &						cubes%units(indunit)%edge(i1,j2,1).or.
     &						im0(mn,j1).eq.
     &						cubes%units(indunit)%edge(i1,j2,2))then
								lf=lf+1
								cubes%units(indunit)%facet(lf)=mn
								goto 611
								imark(mn)=1

							endif
						enddo
					enddo
					endif
				enddo
				endif
				enddo
611				continue
				endif
				enddo
				endif
				enddo
				
				goto 411
				endif
				endIF
				ENDDO
			enddo
411		CONTINUE
		enddo
		continue
	
		goto 111
		endif 
	
	enddo
111	continue
	endif
	enddo
	  
c	Third loop for surface cubes
	do i=1,ncyc0
		if(jmark(i).ne.1.and.jmark(i).ne.2.and.jmark(i).ne.3) nloop=4
	enddo
	
	goto 73
		do i=1,indunit-1
		if(isub(i).ne.-1) then
		do j=i+1,indunit
			if(isub(j).ne.-1) then
			ij=0
			do k=1,6
			do l=1,6
			if(cubes%units(i)%facet(k).eq.cubes%units(j)%facet(l)) 
     &		ij=ij+1
			enddo
			enddo
			if(ij.gt.2) isub(j)=-1
			endif
		enddo
		endif
	enddo
	if(discprt) then 
		write(7,13) 
		do i=1,indunit
		if(isub(i).ne.-1) then
		write(7,200) cubes%units(i)%position
!200		format(/,' Position = ',i8,/)
		do j=1,6
		k= cubes%units(i)%facet(j)
		if(k.ne.0) then
		write(7,201) k,(im0(k,l), l=1,nsc0(k))
		endif
		enddo
!201		format(6i8)		
		endif
		enddo
	endif	
73	if(nloop.eq.3) goto 99
	

99	continue
	write(7,13)
15	format(i4,i4,6(5x,i3))	
	if(indunit.gt.0) then
		do i=1,indunit
		write(7,15) i,cubes%units(i)%position,
     &    (cubes%units(i)%facet(l),l=1,6)
		enddo
		do i=1,indunit-1
		if(isub(i).ne.-1) then
		do j=i+1,indunit
			if(isub(j).ne.-1) then
			ij=0
			do k=1,6
			do l=1,6
c	if(cubes%units(i)%facet(k).ne.0.and.
c     &	cubes%units(j)%facet(l).ne.0) then 
			if(cubes%units(i)%facet(k).eq.cubes%units(j)%facet(l)) 
     &		ij=ij+1
c			endif
			enddo
			enddo
			if(ij.gt.3) isub(j)=-1
			endif
		enddo
		endif
	enddo
	if(discprt) then 
		write(7,13) 
13		format(/'===========================================',/)
		do i=1,indunit
		if(isub(i).ne.-1) then
		write(7,200) cubes%units(i)%position
200		format(/,' Position = ',i8,/)
		inew=inew+1
		do j=1,6
		
		k= cubes%units(i)%facet(j)
		subcube(inew,j)=k
		poscube(inew)=cubes%units(inew)%position	
		if(k.ne.0) then
		write(7,201) k,(im0(k,l), l=1,nsc0(k))
		endif
		enddo
201		format(i4,5(5x,i3))			
		endif
		enddo
	endif
	goto 999 ! this part needs sorting

	do i=1,inew
		do j=1,6
			if(subcube(i,j).ne.0) then
			do k=1,4
			iv=iv+1
			vertx(iv)=im0(subcube(i,j),k)
		
			enddo
			endif
		enddo
		
		
		call find_vertex(iv,vertx,vc,l,vex)
		do imk=1,8
	! real vertices for cube i
		vertex(i,imk)=vex(imk)
		enddo
		
		if(l.eq.4) then
			apr=.false.
			call same_cycle(vc,ncyc0,im0,apr,m)
			if(apr) then
			do ip=1,6
				if(subcube(i,ip).eq.0) then
				subcube(i,ip)=m
				cubes%units(i)%facet(ip)=subcube(i,ip)
				goto 55
				endif
			enddo
			endif
			
			
55			continue
		endif
		
		
	enddo
	
		do i=1,inew
		write(7,205) i,(vertex(i,l), l=1,8)
		
		enddo
205		format(i4,8(2x,i3))			
999	continue
	endif
	end


	subroutine find_vertex(n,vin,vc,l,vex)

	integer v(24),vin(24),vex(8),nv(8),vc(8)
	
		do i=1,n
			if(v(i).ne.-1) then
			k=k+1
			vex(k)=vin(i)
			nv(k)=0
			do j=1,n
			if(vin(j).ne.-1) then
			if(vin(i).eq.vin(j)) then
		
			nv(k)=nv(k)+1
			v(j)=-1 
			
			endif
			endif
			enddo
			endif
		enddo
		do i=1,k
			if(nv(k).lt.3) then
			l=l+1
			vc(l)=vex(k)
			endif
		enddo
	
	end

	subroutine same_cycle(vc,ncyc0,im0,apr,m)
	integer vc(8),im0(50,100)
	logical apr

	
		do j=1,ncyc0
			do k=1,4
				if(vc(1).eq.im0(j,k)) then
					do k1=1,4
					if(vc(2).eq.im0(j,k1)) then
						do k2=1,4
						if(vc(3).eq.im0(j,k2) )then
						apr=.true.
						m=j
	goto 33
						endif
						enddo
					endif
					enddo
				endif
			enddo
		enddo
	

33	end