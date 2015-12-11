	program TDEF
c Tests for DEF_ONE
	real*4 stepamp(50),dt(49),theta(50)
	real*4 delt(0:50)
	integer jfix(50),kfix(0:50),itrigm(50)
	integer*2 iampl(1024)
	logical shut,shutprev,opendown,refit
c
	refit=.false.
	opendown=.true.
	ntmax=1024
	base=-3.
	scrit=15.
	prevlevel=-105.
	t0=10.
	do i=0,50
	   kfix(i)=0
	enddo
c
	iampl(20)=0
	iampl(21)=-100
	iampl(22)=-50
	iampl(23)=0
	iampl(24)=0
c==	iampl(24)=-100
	iampl(25)=-150
	iampl(26)=-50
	iampl(27)=0
	iampl(28)=-150
	iampl(29)=0
c
	itrigm(1)=-110
	itrigm(2)=-40
	itrigm(3)=-10
	itrigm(4)=5
c==	itrigm(4)=-90
	itrigm(5)=-140
	itrigm(6)=-60
	itrigm(7)=5
	itrigm(8)=-140
	itrigm(9)=10
c
	delt(1)=1.
	delt(2)=2.
	delt(3)=120.
	delt(4)=1.
	delt(5)=2.
	delt(6)=3.
	delt(7)=90.
	delt(8)=4.
c
	print 1
1	format(' itype = ')
	read*,itype

	if(itype.eq.1) then
c (1) shut at each end
c should give  nper=3 open periods; ntrans=6
c and delt1(i)=3,120,6,90,4 (i=1,ntrans-1)
	  kt=20
	  ntrans=9
	  shutprev=.true.
	  shut=.true.
	else if(itype.eq.2) then
c
c (2) shut at start, open at end
c should give  nper=3 open periods; ntrans=5
c and delt1(i)=3,120,6,90 (i=1,ntrans-1)
	  kt=20
	  ntrans=8
	  shutprev=.true.
	  shut=.false.
	else if(itype.eq.3) then
c (3) open at start, shut at end
 	  kt=21
	  ntrans=8
	  shutprev=.false.
	  shut=.true.
	  do i=1,8
	   delt(i)=delt(i+1)
	   itrigm(i)=itrigm(i+1)
	  enddo
c should give  nper=3 open periods; ntrans=5
c and delt1(i)=120,6,90,4 (i=1,ntrans-1)
c
	else if(itype.eq.4) then
c (4) open at start, open at end
	  kt=21
	  ntrans=7
	  shutprev=.false.
	  shut=.false.
	  do i=1,8
	    delt(i)=delt(i+1)
	    itrigm(i)=itrigm(i+1)
	  enddo
c should give  nper=3 open periods; ntrans=4
c and delt1(i)=120,6,90 (i=1,ntrans-1)
	endif
c
	call DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
     & scrit,base,prevlevel,theta,itype,y0,stepamp,kmax,opendown,
     & refit,iampl,ntmax,itrigm)
c
c	print*,' nt, nper, base, s = ',nt,nper,base,s
	do i=1,ntrans-1
	   print*,' i, delt(i), dt(i)= ',i,delt(i),dt(i)
	enddo
	pause
	do i=1,kmax
	   print*,' i,theta(i)= ',i,theta(i)
	enddo
	do i=1,ntrans
	   print*,' i,stepamp(i)= ',i,stepamp(i)
	enddo
c
	end

