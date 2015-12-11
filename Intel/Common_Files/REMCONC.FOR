	subroutine REMCONC(QD,QT,k,jset,conc,kdim)

	real*4 conc(10,10)
	real*8 QD(kdim,kdim),QT(kdim,kdim)
	common/LIG/nligsav,IL(100)
	integer IX(100),JX(100)
	common/cpar/ncdep,IX,JX,x
c
	logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb,mfb
c
c To remove concentrations from QD and output QT
c
	do i=1,k
	   do j=1,k
	      QT(i,j)=QD(i,j)
	   enddo
	enddo
c Start with QT same as QD but then keep QT on RHS, in case
c a rate is both an assoc rate (divide by x1) AND a fasblock rate
c (multiply by (1+cB) etc.
c
	if(ncdep.gt.0) then
	   do L=1,ncdep
	      i=ix(L)
	      j=jx(L)
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	      x1=conc(IL(L),jset)
	      QT(i,j)=QT(i,j)/dble(x1)
	   enddo
	endif
c Must also remove conc dep in case of fast block correction
	if(fastblk) then
	   do n=1,nfblock
		i=ifb(n)
		j=jfb(n)
		x1=dble(conc(iflig,jset)/aKB)
		if(mfb(n).gt.0) then
		   QT(i,j)=QT(i,j)*(1.0d0 + x1)
		else
		   QT(i,j)=QT(i,j)*(1.0d0 + x1)/x1
		endif
	   enddo
	endif
c
	RETURN
	end
