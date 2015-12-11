	subroutine QSETC(QT,conc,QNEW,ncdep,nlig,IL,IX,JX,k,km)
c To take an input matrix, QT, and mutliply assoc rates by the concentrations
c in conc(10),  calculate diagonals and output Q matrix, QNEW.
c NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	real*8 QT(km,km),QNEW(km,km),S
	real*4 conc(10)
	integer il(km),ix(km),jx(km)
c
	do i=1,k
	   do j=1,k
		QNEW(i,j)=QT(i,j)
	   enddo
	enddo
c
	if(ncdep.gt.0) then
	   do L=1,ncdep
		i=IX(L)
      	j=JX(L)
	  	QNEW(i,j)=QT(i,j)*dble(conc(IL(L)))
	   enddo
	endif
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



