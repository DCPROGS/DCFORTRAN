	subroutine find_cubes(ncyc1,im1,jm1,nsc1)


c	find cubes by "peeling" the corners until reaches the middle
c	
	
	PARAMETER  N=200
	
	type subunit
		integer vertex(8)
		integer	facet(6)
		integer edge(6,4,2)
		integer	position
		integer sign
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
	integer	vertx(24),vc(8),vertex(50,8),vex(8),nv(8),vc3(8),mc(8)
	logical discprt,apr
	integer nsc0(50),nsc1(50),im1(50,100),jm1(50,100)

	common/dp/discprt

c Added 16-03-07 DC
	common/nc0/ncyc0

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


c
	if(ncyc1.gt.0) then
			write(7,13)
			write(7,13)
			write(7,103)
103			format(' Microscopic reversibility')
			do i=1,ncyc1
				write(7,104) i,nsc1(i),(im1(i,j), j=1,nsc1(i))
104				format(i4,i4,5(5x,i3))
			enddo
	else
		goto 909
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
	k=0
	do i=1,ncyc1
	if(nsc1(i).eq.4) then
		k=k+1
		nsc0(k)=nsc1(i)
		do j=1,4
			im0(k,j)=im1(i,j)
			jm0(k,j)=jm1(i,j)
		enddo
		endif
	enddo
	ncyc0=k-1

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
	
	
73	if(nloop.eq.3) goto 99
	!--------------------
	do i=1,ncyc0
	if(jmark(i).ne.1.and.jmark(i).ne.2.and.jmark(i).ne.3) then	
	do j=1,4

		do k=1,ncyc0
			imark(k)=0
		enddo
	
	
		do k=1,ncyc0
		if(jmark(k).ne.1.and.jmark(k).ne.2.and.jmark(i).ne.3) then
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
		cubes%units(indunit)%position=4
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
			
				
				jmark(m)=4
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
								goto 6111
								imark(mn)=1

							endif
						enddo
					enddo
					endif
				enddo
				endif
				enddo
6111				continue
				endif
				enddo
				endif
				enddo
				
				goto 4111
				endif
				endIF
				ENDDO
			enddo
4111		CONTINUE
		enddo
		continue
	
		goto 1111
		endif 
	
	enddo
1111	continue
	endif
	enddo

99	continue
c	write(7,13)

c	eliminate dublicate cubes (first run)
	if(indunit.gt.0) then
		do i=1,indunit
c			write(7,15) i,cubes%units(i)%position,
c     &		(cubes%units(i)%facet(l),l=1,6)
		enddo
		do i=1,indunit-1
		if(isub(i).ne.-1) then
		do j=i+1,indunit
			if(isub(j).ne.-1) then
			ij=0
			do k=1,6
			do l=1,6
			if(cubes%units(i)%facet(k).ne.0.and.
     &		cubes%units(j)%facet(l).ne.0) then 
			if(cubes%units(i)%facet(k).eq.cubes%units(j)%facet(l)) 
     &		ij=ij+1
			endif
			enddo
			enddo
			if(ij.gt.2) isub(j)=-1
			endif
		enddo
		endif
		enddo
	endif

c	write(7,13) 


c	now try to find the other surfaces/cycles

	do i=1,indunit
	if(isub(i).ne.-1) then
c		write(7,200) cubes%units(i)%position

		inew=inew+1
		do j=1,6
			cubes%units(inew)%facet(j)=cubes%units(i)%facet(j)
		enddo
		ifc=0
		iv=0
		cubes%units(inew)%position=cubes%units(i)%position
		do j=1,24
			vertx(j)=0
		enddo 
		do j=1,6
		
			k= cubes%units(i)%facet(j)
		
			subcube(inew,j)=k
			poscube(inew)=cubes%units(inew)%position	
			if(k.ne.0) then
			ifc=ifc+1
c			write(7,201) k,(im0(k,l), l=1,nsc0(k))
			cubes%units(inew)%facet(j)=cubes%units(i)%facet(j)
			do jk=1,4
				iv=iv+1
				vertx(iv)=im0(k,jk)
		
			enddo
			endif
		
		enddo
		do i8=1,8
			nv(i8)=0
			vex(i8)=0
			vc(i8)=0
			vc3(i8)=0
		enddo
		
		call find_vertex(iv,vertx,kv,nv,vex,vc,vc3)
		lc=0
		if(ifc.eq.5) then
			DO L=1,8
				cubes%units(inew)%vertex(l)=vex(l)
				IF (NV(L).LT.3) THEN
				LC=LC+1
				VC(LC)=VEX(L)
				ENDIF
			ENDDO
			apr=.false.
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
				goto 555
				endif
			enddo
			endif
		else if(ifc.eq.4) then
			DO L=1,8
				
				cubes%units(inew)%vertex(l)=vex(l)
			
				IF (NV(L).LT.2) THEN
				N2=N2+1
				LC=LC+1
				VC(LC)=VEX(L)
				ELSE IF (NV(L).LT.3) THEN
				N3=N3+1
				lC=lC+1
				VC(LC)=VEX(L)
				ENDIF
			ENDDO
			apr=.false.
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
			
				goto 57
				endif
			enddo
			endif
57			apr=.false.
			A=VC(1)
			b=VC(2)
			vc(1)=vc(3)
			vc(2)=vc(4)
			vc(3)=vc(5)
			vc(4)=vc(6)
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
			
				goto 58
				endif
			enddo
			endif
58			apr=.false.
			vc(1)=a
			vc(2)=b
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
			
				goto 555
				endif
			enddo
			endif
		else if(ifc.eq.3) then
	
			m1=cubes%units(inew)%facet(1)
			m2=cubes%units(inew)%facet(2)
			m3=cubes%units(inew)%facet(3)
			apr=.false.
			call same_cycle(vc,ncyc0,im0,apr,ma)
			if(apr.and.ma.ne.m1.and.ma.ne.m2.and.ma.ne.m3) then
				m=ma
			else
				ia=vc(3)
				vc(3)=vc(4)
				apr=.false.
				ma=0
				call same_cycle(vc,ncyc0,im0,apr,ma)
				if(apr.and.ma.ne.m1.and.ma.ne.m2.and.ma.ne.m3) then
					m=ma
				else
					vc(3)=vc(5)
					apr=.false.
					ma=0
					call same_cycle(vc,ncyc0,im0,apr,ma)
					if(apr.and.ma.ne.m1.and.ma.ne.m2.and.ma.ne.m3)then
						m=ma
					else
						vc(3)=vc(6)
						apr=.false.
						ma=0
						call same_cycle(vc,ncyc0,im0,apr,ma)
						if(apr.and.ma.ne.m1.and.ma.ne.m2.and.ma.ne.m3) 
     &						m=ma
					endif
				endif
			endif
					
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
				do jk=1,4
					iv=iv+1
					vertx(iv)=im0(m,jk)
		
				enddo
			
				goto 551
				endif
			enddo
			endif

551			continue
			call find_vertex(iv,vertx,kv,nv,vex,vc,vc3)
			do l=1,8
			cubes%units(inew)%vertex(l)=vex(l)
			enddo
			continue
			apr=.false.
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
			
				goto 573
				endif
			enddo
			endif
573			apr=.false.
			A=VC(1)
			b=VC(2)
			vc(1)=vc(3)
			vc(2)=vc(4)
			vc(3)=vc(5)
			vc(4)=vc(6)
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
			
				goto 583
				endif
			enddo
			endif
583			apr=.false.
			vc(1)=a
			vc(2)=b
			call same_cycle(vc,ncyc0,im0,apr,m)	
			if(apr) then
			do ip=1,6
				if(subcube(inew,ip).eq.0) then
				subcube(inew,ip)=m
				cubes%units(inew)%facet(ip)=subcube(inew,ip)
c				write(7,201) M,(im0(M,l), l=1,nsc0(M))
			
				goto 555
				endif
			enddo
			endif
		endif
	endif
555	CONTINUE
	enddo

c	eliminate dublicate cubes:
	
	do i=1,inew-1
	if(cubes%units(i)%sign.ne.-1) then
		do j=i+1,inew
		if(cubes%units(j)%sign.ne.-1) then
			indf=0
			do k=1,6
				do l=1,6
					if(cubes%units(i)%facet(k).eq.
     &				cubes%units(j)%facet(l)) then
						indf=indf+1
						if(cubes%units(i)%facet(k).eq.0.and.
     &					cubes%units(j)%facet(l).eq.0) indf=indf-1
					endif
				enddo
			enddo
			if(indf.ge.3) then
				indi=0
				indj=0
				do k=1,6
					if(cubes%units(i)%facet(k).ne.0) indi=indi+1
					if(cubes%units(j)%facet(k).ne.0) indj=indj+1
				enddo
				if(indi.gt.indj) then
					cubes%units(j)%sign=-1
				else
					cubes%units(i)%sign=-1
				endif
			endif
		endif
		enddo
	endif
	enddo


			
999	continue
	write(7,13)
	write(7,203)

	do i=1,inew
		if(cubes%units(I)%sign.ne.-1)then
			jnew=jnew+1
			cubes%units(jnew)%position=cubes%units(i)%position
			write(7,200) jnew,cubes%units(jnew)%position
			do k=1,6
				cubes%units(jnew)%facet(k)=cubes%units(i)%facet(k)
				m=cubes%units(jnew)%facet(k)
				if(m.ne.0) write(7,201) M,(im0(M,l), l=1,nsc0(M))
			enddo
			do l=1,8
			cubes%units(jnew)%vertex(l)=cubes%units(i)%vertex(l)
			
			enddo
			write(7,202)
			write(7,205) (cubes%units(jnew)%vertex(l),l=1,8)
		endif	
	enddo
	
201	format(i4,5(5x,i3))		
203	format(/,'-------   Last run -----------',/)	
15	format(i4,i4,6(5x,i3))
13	format(/'===========================================',/)
200	format(/,' Cube = ',i4,' Position = ',i8,/)
202	format(/,' Vertices:',/)
205	format(9(i4))

909	end


	subroutine find_vertex(n,vin,k,nv,vex,vc,vc3)

	integer v(24),vin(24),vex(8),nv(8),vc(8),vc3(8)
		do i=1,n
			v(i)=0
	
		enddo
		k=0
		do i=1,n
		
			if(v(i).ne.-1.and.k.lt.8) then
			k=k+1
			vex(k)=vin(i)
			nv(k)=0
			do j=1,n
				if(vin(j).ne.-1) then
				if(vin(i).eq.vin(j)) then
				NV(K)=NV(K)+1
			
				v(j)=-1 
			
				endif
				endif
			enddo
			endif
		enddo

		l=0
		m=0
		do i=1,k
			if(nv(i).lt.3) then
			l=l+1
			vc(l)=vex(i)
			else
			m=m+1
			vc3(m)=vex(i)
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
						if(vc(3).eq.im0(j,k2))then
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

	subroutine same_cycle1(vc,ncyc0,im0,apr,mc)
	integer vc(8),im0(50,100),mc(8)
	logical apr

		l1=0
		do j=1,ncyc0
			do k=1,4
				if(vc(1).eq.im0(j,k)) then
					do k1=1,4
					if(vc(2).eq.im0(j,k1)) then
						do k2=1,4
					if(vc(3).eq.im0(j,k2).or.vc(4).eq.im0(j,k2))then
							apr=.true.
							l1=l1+1
							mc(l1)=j
							goto 34
						endif
					
						enddo
					endif
					enddo
				endif
			enddo
34		continue
		enddo
	

33	end