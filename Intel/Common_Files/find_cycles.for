	subroutine find_cycles(n,link,ncyc0,im0,jm0,nsc0,ncmax)


	integer IM0(50,100),JM0(50,100)	!for cycles (final output)
	integer nsc(100)
	integer IM(100,100),JM(100,100)	!for cycles (final output)
	integer nsc0(50),nc(100)
	integer link(200,200),icycsav(100,100),icyc(100)
	logical same

!	allocatable:: im,jm,icyc,icycsav,nsc

	ncyc0=0
	NCYC=0
	if(ncmax.eq.0) ncmax=4
	if(ncmax.gt.n) ncmax=n
	
	n0=100
!	allocate(im(n0,n0),jm(n0,n0),icycsav(n0,n0),icyc(n0),nsc(n0))
	do i=1,50
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

	
	do i=1,n
		nc(1)=i
		do j=1,n
			nc(2)=j
			if((link(i,j).eq.1).and.(i.ne.j)) then
			do k=1,n
				nc(3)=k	  
				if(link(k,j).eq.1.and.k.ne.j.and.k.ne.i) then
				ind=3		  
     			call cycles (n,ind,nc,ncyc,nsc,im,jm,ncmax,link,icycsav)
					  
				 if(ncyc.gt.100) go to 99 
				 endif  
			enddo
			endif
		enddo
	enddo

	
99	continue
	ncyc0=ncyc
	if(ncyc0.gt.50) ncyc0=50
	do i=1,ncyc0
		nsc0(i)=nsc(i)
		do j=1,4
		im0(i,j)=im(i,j)
		jm0(i,j)=jm(i,j)
		enddo
	enddo

!	deallocate(im,jm,icycsav,icyc,nsc)
	end
				
				
	recursive subroutine cycles(n,ind,nc,ncyc,nsc,im,jm,ncmax,link,
     &	icycsav)

	integer nsc(100)
	integer IM(100,100),JM(100,100)	!for cycles (final output)
	integer nc(100)
	integer link(100,100),icycsav(100,100),icyc(100)
	logical same
	n0=100
	
	
	if(ncyc.gt.n0) goto 99
	
	if(ind.le.ncmax) then
	
	ind=ind+1
	
	do l=1,n
		nc(ind)=l
		if(link(l,nc(ind-1)).eq.1.and.(l.ne.nc(ind-1))
     &	.and.(l.ne.nc(ind-2))) then
			
			if(l.eq.nc(1)) then
				do i=1,ind-1
					icyc(i)=nc(i)
				enddo
				do i=ind,n
					icyc(i)=0
				enddo
			
				mc=ind
				call CYCHECK(ncyc,nsc,icyc,mc,icycsav,same,n0)
				if(.not.same) then
					ncyc=ncyc+1
							
					nsc(ncyc)=mc-1
					do i=1,nsc(ncyc)
						
							
							im(ncyc,i)=nc(i)
							jm(ncyc,i)=nc(i+1)
						
					enddo
					jm(ncyc,nsc(ncyc))=nc(1)
					do i=ind,n
						im(ncyc,i)=0
						jm(ncyc,i)=0
						icyc(i)=0
					enddo
					do j1=1,mc
						icycsav(ncyc,j1)=icyc(j1)
					enddo
				endif
			else
				
     			call cycles(n,ind,nc,ncyc,nsc,im,jm,ncmax,link,icycsav)

			endif
		endif
	enddo				
	endif
99	end			
				
