	subroutine findcyc(n,link,ncyc0,im0,jm0,nsc0,ncmax)

c	for cubes of dimension x*y*z
c	units=x*y*z					or	x**3 (x=y=z)	
c	states=(x+1)*(y+1)*(z+1)	or	(x+1)**3
c	cycles=x*y+y*z+z*x+3*x*y*z	or	3*x**2*(x+1)
c	connections=x*(y+1)*(z+1)+y*(x+1)*(z+1)+z*(x+1)*(y+1)	or	3*x*(x+1)**2

c	inserted for 5 and 6 states cycles
  
	integer IM0(50,100),JM0(50,100)	!for cycles (final output)
	integer nsc(1000)
	integer IM(1000,100),JM(1000,100),imk(1000,100)	!for cycles (final output)
	integer nsc0(50)
	integer link(100,100),icycsav(1000,1000),icyc(1000)
	logical same,apr
	integer vc(8)
	
	ncyc0=0
	NCYC=0
	if(ncmax.eq.0) ncmax=4
	if(ncmax.gt.n) ncmax=n
	
	do i=1,50
		imk(i,1)=0
		nsc0(i)=0
		nsc(i)=0
		do j1=1,100
		  icycsav(i,j1)=0
		  icyc(i)=0
		enddo
		do j=1,4
		im0(i,j)=0
		im(i,j)=0
		jm0(i,j)=0
		jm(i,j)=0
		enddo
	
	enddo

	
	n0=1000
	

	do i=1,n
		do j=i+1,n
			if((link(i,j).eq.1).and.(i.ne.j)) then
		
			call A(i,j,n,link,ncyc,im,nsc,ncmax)
			endif
		enddo
	enddo
	


	end

	recursive subroutine A(i,j,n,link,ncyc,im,nsc,ncmax)
	integer nsc(1000)
	integer IM(1000,100),JM(1000,100)
	integer link(100,100),icycsav(1000,1000),icyc(1000)
	logical same,apr
	
	if(j.lt.ncmax) then
		istart=j+1
		ncyc1=0
		do k=i,istart-2
			if(link(istart,k).eq.1) then
				m=i
				do l=1,istart
					icyc(l)=m
					m=m+1
				enddo
				mc=istart+1
				call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
				if(.not.same) then
					if(ncyc.gt.1000) goto 99
					ncyc=ncyc+1
					nsc(ncyc)=istart
					do j2=1,istart
						im(ncyc,j2)=icyc(j2)
					enddo
					do j1=1,mc
						icycsav(ncyc,j1)=icyc(j1)
					enddo
				endif
			endif
		enddo
		if(ncyc1.eq.0) then
			call A(i,j,n,link,ncyc,im,nsc,ncmax)
		endif
	endif

99	end



