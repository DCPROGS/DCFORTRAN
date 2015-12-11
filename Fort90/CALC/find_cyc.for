	subroutine find_cyc(n,link,ncyc0,im0,jm0,nsc0)


	integer IM0(50,100),JM0(50,100)	!for cycles (final output)
	integer nsc(1000)
	integer IM(1000,100),JM(1000,100)	!for cycles (final output)
	integer nsc0(50)
	integer link(100,100),icycsav(1000,1000),icyc(1000)
	logical same

	ncyc0=0
	do i=1,50
	nsc0(i)=0
	   do j=1,4
		im0(i,j)=0
		jm0(i,j)=0
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
				   else
					do m=1,n
					   if(link(l,m).eq.1.and.(m.ne.l).and.(m.ne.k)) then
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


	end
