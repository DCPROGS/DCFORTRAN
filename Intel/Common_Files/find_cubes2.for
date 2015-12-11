	subroutine find_cubes2(ncyc0,im0,jm0)
	
	
	PARAMETER  N=200
	
	type subunit
		integer vertex(8)
		integer	facet(12)
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
	integer ic(100,4)

	indunit=1
	do i=1,ncyc0
		if(jmark(i).ne.1) then
		imark(i)=1
c	    jmark(i)=1
		lf=1
		cubes%units(indunit)%facet(lf)=i
		do i1=1,ncyc0
		if(i.ne.i1) then
		do j1=1,4 
			if(im0(i,3).eq.im0(i1,j1)) then
					imark(i1)=2
					k1=j1+2
					if(k1.eq.5) k1=1
					if(k1.eq.6) k1=2
					do i2=1,ncyc0
				!	if(jmark(i2).ne.1) then
					do j2=1,4
					k2=j2+2
					if(k2.eq.5) k2=1
					if(k2.eq.6) k2=2
					if(im0(i1,k1).eq.im0(i2,j2).and.
     &					im0(i2,k2).eq.im0(i,1)) then
						imark(i2)=1
					!	jmark(i2)=1
						lf=lf+1
						cubes%units(indunit)%facet(lf)=i2
					endif
					enddo
				!	endif
					enddo
				!	goto 1
				
			endif
		enddo
		endif
		enddo
1		continue
		do i1=1,ncyc0
	    
		if(imark(i1).ne.0) then
			
			do j1=1,4
				ink=0
				do i2=1,ncyc0
					if(imark(i2).ne.0.and.i2.ne.i1) then
					do j2=1,4
						if(im0(i1,j1).eq.im0(i2,j2)) ink=ink+1
							
					enddo
					endif

				enddo
				if(ink.lt.1) then
						imark(i1)=0
					!	jmark(i1)=0
				else
					!jmark(i1)=1
				endif
				continue
			enddo
	    	
		endif
		
		enddo
		do jfk=1,ncyc0
			if(imark(jfk).eq.1) jmark(jfk)=1
			if(imark(jfk).eq.2) then
			jmark(jfk)=1
			lf=lf+1
			cubes%units(indunit)%facet(lf)=jfk
			endif
		enddo
		
		do ima=1,100
			imark(ima)=0
		enddo
		lf=0
		indunit=indunit+1
	endif
	enddo
	
			

	end
