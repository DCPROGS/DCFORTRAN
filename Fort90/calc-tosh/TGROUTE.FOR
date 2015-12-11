c To test getroute
	integer IX(10),JX(10)
	integer IL(10)		!for ligand type
	common/QPAR/ncon,IC(2,20)     !for connections
	logical is_route
c States:
c      9--10--11--12
c      |  |   |   |
c      5--6 --7 --8
c      |  |   |   |
c      1--2 --3 --4
c	ka=4
c	kf=8
	ka=1
	kf=11
	k=12
c
	ncon=17
	ic(1,1)=1
	ic(2,1)=2
	ic(1,2)=1
	ic(2,2)=5
	ic(1,3)=2
	ic(2,3)=6
	ic(1,4)=2
	ic(2,4)=3
	ic(1,5)=3
	ic(2,5)=7
	ic(1,6)=3
	ic(2,6)=4
	ic(1,7)=4
	ic(2,7)=8
	ic(1,8)=8
	ic(2,8)=7
	ic(1,9)=8
	ic(2,9)=12
	ic(1,10)=7
	ic(2,10)=11
	ic(1,11)=7
	ic(2,11)=6
	ic(1,12)=6
	ic(2,12)=5
	ic(1,13)=6
	ic(2,13)=10
	ic(1,14)=5
	ic(2,14)=9
	ic(1,15)=9
	ic(2,15)=10
	ic(1,16)=10
	ic(2,16)=11
	ic(1,17)=11
	ic(2,17)=12
c
c	ncdep=0
	ncdep=4
c Isolate states 9, 10 as closed set
	ix(1)=9
	jx(1)=10
	ix(2)=9
	jx(2)=5
	ix(3)=10
	jx(3)=11
	ix(4)=10
	jx(4)=6
c also isolate state 8
	ncdep=7
	ix(5)=8
	jx(5)=4
	ix(6)=8
	jx(6)=7
c leave one escape route from 8
	ncdep=6
c	ix(7)=8
c	jx(7)=12
c
	do i=1,ncdep
	   IL(i)=1		!all ligand #1
	enddo
c
	do i=ka+1,k
	   call GETROUTE(i,kA,kF,ix,jx,il,ncdep,
     &	.true.,.false.,is_route)
	   print 1,i,is_route
1	   format('state # ',i3,'  route = ',l4)
	enddo
	end
