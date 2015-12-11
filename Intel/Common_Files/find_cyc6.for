	subroutine find_cyc(n,link,ncyc0,im0,jm0,nsc0,ncmax)

c	for cubes of dimension x*y*z
c	units=x*y*z					or	x**3 (x=y=z)	
c	states=(x+1)*(y+1)*(z+1)	or	(x+1)**3
c	cycles=x*y+y*z+z*x+3*x*y*z	or	3*x**2*(x+1)
c	connections=x*(y+1)*(z+1)+y*(x+1)*(z+1)+z*(x+1)*(y+1)	or	3*x*(x+1)**2


  
	integer IM0(50,100),JM0(50,100)	!for cycles (final output)
	integer nsc(1000)
	integer IM(1000,100),JM(1000,100),imk(1000,100)	!for cycles (final output)
	integer nsc0(50)
	integer link(100,100),icycsav(1000,1000),icyc(1000)
	logical same

	
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
	do j=1,n
	if((link(i,j).eq.1).and.(i.ne.j)) then
	 do k=1,n
	 if((link(j,k).eq.1).and.(k.ne.j).and.(k.ne.i)) then
		do l=1,n
			if(link(k,l).eq.1.and.(k.ne.l).and.(l.ne.j)) then
				if(l.eq.i) then
					icyc(1)=i
					icyc(2)=j
					icyc(3)=k
					icyc(4)=0
					mc=4
					call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
					if(.not.same) then
						if(ncyc.gt.1000) goto 99
						ncyc=ncyc+1
						nsc(ncyc)=3
						im(ncyc,1)=i
						im(ncyc,2)=j
						im(ncyc,3)=k
						im(ncyc,4)=0
						jm(ncyc,1)=j
						jm(ncyc,2)=k
						jm(ncyc,3)=i
						jm(ncyc,4)=0
						do j1=1,mc
							icycsav(ncyc,j1)=icyc(j1)
						enddo
					endif
				else ! check for 4
					do m=1,n
					if(link(l,m).eq.1.and.(m.ne.l).and.(m.ne.k)) then
						if(m.eq.j) then
							icyc(1)=j
							icyc(2)=k
							icyc(3)=l
							icyc(4)=0
							mc=4
					call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
							if(.not.same) then
								if(ncyc.gt.1000) goto 99
								ncyc=ncyc+1
								nsc(ncyc)=3
								im(ncyc,1)=j
								im(ncyc,2)=k
								im(ncyc,3)=l
								im(ncyc,4)=0
								jm(ncyc,1)=k
								jm(ncyc,2)=l
								jm(ncyc,3)=j
								jm(ncyc,4)=0
								do j1=1,mc
									icycsav(ncyc,j1)=icyc(j1)
								enddo
							endif
							!exit 
						else 
						if(ncmax.gt.3) then
							if(m.eq.i) then
								icyc(1)=i
								icyc(2)=j
								icyc(3)=k
								icyc(4)=l
								mc=5
					call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
								if(.not.same) then
									if(ncyc.gt.1000) goto 99
									ncyc=ncyc+1

									nsc(ncyc)=4
									im(ncyc,1)=i
									im(ncyc,2)=j
									im(ncyc,3)=k
									im(ncyc,4)=l
									jm(ncyc,1)=j
									jm(ncyc,2)=k
									jm(ncyc,3)=l
									jm(ncyc,4)=i
								do j1=1,mc
									icycsav(ncyc,j1)=icyc(j1)
								enddo
								endif
							else ! search for 5
							if(ncmax.gt.4) then
							do m5=1,n
							if(link(m,m5).eq.1.and.m5.ne.l.
     &							and.m5.ne.m) then
							if(m5.eq.j) then
								icyc(1)=j		
								icyc(2)=k
								icyc(3)=l
								icyc(4)=m
								mc=5
					call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
								if(.not.same) then
									if(ncyc.gt.1000) goto 99
									ncyc=ncyc+1

									nsc(ncyc)=4
									im(ncyc,1)=j
									im(ncyc,2)=k
									im(ncyc,3)=l
									im(ncyc,4)=m
									jm(ncyc,1)=k
									jm(ncyc,2)=l
									jm(ncyc,3)=m
									jm(ncyc,4)=j
								do j1=1,mc
									icycsav(ncyc,j1)=icyc(j1)
								enddo
								endif
							else if(m5.eq.k) then
							icyc(1)=k
							icyc(2)=l
							icyc(3)=m
							icyc(4)=0
							mc=4
					call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
							if(.not.same) then
								if(ncyc.gt.1000) goto 99
								ncyc=ncyc+1
								nsc(ncyc)=3
								im(ncyc,1)=k
								im(ncyc,2)=l
								im(ncyc,3)=m
								im(ncyc,4)=0
								jm(ncyc,1)=l
								jm(ncyc,2)=m
								jm(ncyc,3)=k
								jm(ncyc,4)=0
								do j1=1,mc
									icycsav(ncyc,j1)=icyc(j1)
								enddo
							endif
							!exit 

							else if(m5.eq.i) then
							icyc(1)=i
							icyc(2)=j
							icyc(3)=k
							icyc(4)=l
							icyc(5)=m
							mc=6
					   call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
								if(.not.same) then
									if(ncyc.gt.1000) goto 99
									ncyc=ncyc+1

									nsc(ncyc)=5
									im(ncyc,1)=i
									im(ncyc,2)=j
									im(ncyc,3)=k
									im(ncyc,4)=l
									im(ncyc,5)=m
									jm(ncyc,1)=j
									jm(ncyc,2)=k
									jm(ncyc,3)=l
									jm(ncyc,4)=m
									jm(ncyc,5)=i
									do j1=1,mc
									icycsav(ncyc,j1)=icyc(j1)
									enddo
								endif
							else
							if(ncmax.gt.5)then
							do m6=1,n
							if(link(m6,m5).eq.1.and.(m6.ne.j).and.
     &						(m6.ne.k).and.m6.ne.l.and.m6.ne.m.and.
     &						m6.ne.m5) then
							if(m6.eq.i) then
							icyc(1)=i
							icyc(2)=j
							icyc(3)=k
							icyc(4)=l
							icyc(5)=m
							icyc(6)=m5
							mc=7
					   call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
							if(.not.same) then
									if(ncyc.gt.1000) goto 99
									ncyc=ncyc+1

									nsc(ncyc)=6
									im(ncyc,1)=i
									im(ncyc,2)=j
									im(ncyc,3)=k
									im(ncyc,4)=l
									im(ncyc,5)=m
									im(ncyc,6)=m5
									jm(ncyc,1)=j
									jm(ncyc,2)=k
									jm(ncyc,3)=l
									jm(ncyc,4)=m
									jm(ncyc,5)=m5
									jm(ncyc,6)=i
									do j1=1,mc
									icycsav(ncyc,j1)=icyc(j1)
									enddo
								endif
							endif
						    endif
							enddo
							endif ! end ncmax>5
							endif !end of m5=i
							
							endif
							enddo
							endif
							endif !end of m=i)
						!endif
						!enddo
						ENDIF !end of ncmax.3)
						endif
					endif
					enddo
				endif
			endif
		enddo
	 endif
	 enddo
	endif
	enddo
	enddo

c	eliminates 4 states cycles containing 3 states cycles
	do i=1,ncyc
		if(nsc(i).eq.4) then
			if(link(im(i,1),im(i,3)).eq.1.or.
     &	link(im(i,2),im(i,4)).eq.1) imk(i,1)=-1
		endif
	enddo
c

	
99	continue
	ncyc0=ncyc
	if(ncyc0.gt.50) ncyc0=50
	k=1
	do i=1,ncyc0
		if(imk(i,1).ne.-1) then
			nsc0(k)=nsc(i)
			do j=1,4
				im0(k,j)=im(i,j)
				jm0(k,j)=jm(i,j)
			enddo
			k=k+1
	   endif
	   	
	enddo

	ncyc0=k-1
	end
