	subroutine PRTMOD(il,ligname)
c To print connections etc to the print file; things fixed by the model

	character*20 ligname(10)
	REAL*4 EFAC(200)	!declare for disc-write-not used now
c	integer IS(10),JS(10)	!declare for disc-write-not used now
	integer NEQ,IE(200),JE(200),IF(200),JF(200)	!ditto
c	INTEGER IC(2,200)
	integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer IX(100),JX(100)
	integer IL(100)		!for ligand type
	logical discprt
	common/dp/discprt
	COMMON/CPAR/NCDEP,IX,JX,X
c	COMMON/QPAR/NCON,IC
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/SPAR/NSPEC,IS,JS		!special parameters
	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	COMMON/indblk/nqset,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr		!true if microscopic reversibility to be obeyed
c
	integer isetmr(50)
	common/mr1/isetmr
c Print of connections not needed now have diagram (and IC not
c at present available when sameq=true)
c	write(7,123)(IC(1,j),ic(2,j),j=1,ncon)
c	print 123,(IC(1,j),ic(2,j),j=1,ncon)
c
	if(.not.discprt) RETURN
c
c123	format(/,' Connections= ',/,4(5(2i3,4x),/))
c
	if(ncdep.gt.0) then
	   write(7,113)
113	   format(/' Concentration-dependent elements:',/,
     &   '   i   j     ligand #   Ligand name')
	   do L=1,ncdep
		write(7,133) IX(L),JX(L),IL(L),ligname(IL(L))
133		format(1x,2i3,5x,i3,12x,a20)
	   enddo
	endif
c
	if(ncyc.gt.0) then
	 do L1=1,ncyc
	   L=isetmr(L1)
c	   print 126, L
	   if(discprt) write(7,126)L
126	   format(' Cycle # ',i3)
	   if(obeymr(L).and.(.not.automr(L))) then
c		print 1311,im(L,1),jm(L,1)
		if(discprt) write(7,1311)im(L,1),jm(L,1)
1311		format(2i3,'  (calc by micro rev)')
	   else if(automr(L)) then
c		print 1312,im(L,1),jm(L,1)
		if(discprt) write(7,1312)im(L,1),jm(L,1)
1312		format(2i3,'  (obeys micro rev automatically)')
	   else
c		print 1313,im(L,1),jm(L,1)
		if(discprt) write(7,1313)im(L,1),jm(L,1)
1313		format(2i3,'  (no micro rev constraint)')
	   endif
c	   print 127,(im(L,m),jm(L,m),m=2,nsc(L))
127	   format(2(5(2i3,4x),/))
	   if(discprt) write(7,127)(im(L,m),jm(L,m),m=2,nsc(L))
	 enddo
	endif
c
c135	if(nspec.gt.0) write(7,130) (IS(L),JS(L),L=1,nspec)
c130	format(' Forward desens rates= ',/,2(5(2i3,4x),/))
c
c
	if(neq.gt.0) then
	   write(7,128)
128	   format(' Constrained elements (for HJCFIT)')
	   do L=1,neq
		if(efac(L).gt.0.) then
		   write(7,129) IE(L),JE(L),EFAC(L),IF(L),JF(L)
129		   format(1x,2i3,'= ',f9.3,' times ',2i3)
		else
		   write(7,130) IE(L),JE(L),IF(L),JF(L),-EFAC(L)
130		   format(1x,2i3,' + ',2i3,' = ',f9.3)
		endif
	   enddo
	endif
	if(nqset.gt.0) then
	   write(7,228)
228	   format(' Fixed q(i,j), set for independent models:')
	   do L=1,nqset
		write(7,229) ieq(L),jeq(L),efacq(L),ifq(L),jfq(L)
229		format(1x,2i3,'= ',f9.3,' times ',2i3)
	   enddo
	endif
c
	RETURN
	end





