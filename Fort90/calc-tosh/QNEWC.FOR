	subroutine QNEWC(QD,xA1,xB1,QNEW,xA2,xB2,ncdep,nlig,IL,IX,JX,k,km)
c To alter an input Q matrix, QD, defined for concentrations xA1,xB1, to
c an output Q matrix, QNEW, with concentrations xA2,xB2 (eg used in SCALCS
c and SCJUMP).
c NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	real*8 QD(km,km),QNEW(km,km),r,rA,rB,s
	integer il(km),ix(km),jx(km)
c
	do 1 i=1,k
	do 1 j=1,k
1	QNEW(i,j)=QD(i,j)
c
	if(ncdep.eq.0) RETURN
c
	rA=dble(xA2/xA1)
	if(nlig.eq.2) rB=dble(xB2/xB1)
c
	do 5 L=1,ncdep
	r=rA
	if(nlig.gt.1.and.IL(L).eq.2) r=rB
	i=IX(L)
	j=JX(L)
	QNEW(i,j)=QD(i,j)*r
5	continue
c
c Recalculate the diagonals
	do i=1,k
	   s=0.d0
	   do j=1,k
		if(i.ne.j) s=s+qnew(i,j)
	   enddo
	   qnew(i,i)=-s
	enddo
	RETURN
	end



