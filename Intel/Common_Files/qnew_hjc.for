	subroutine QNEW_HJC(QT,jset,conc,QNEW,ncdep,nlig,IL,IX,JX,k,km,kn)
c New subroutine for HJCFIT, like QNEWC in \calc
c Input = QT with assoc rate constants init
c Output = QD with assoc rate constants multiplied by appropriate conc for
c   data set #jset
c
c NB it is ONLY concentration that is used here.  Assumed that all constraints,
c  etc already set in QT by call to QSET_HJC, BUT this routine
c differs from QNEWC in that micro-rev is recalculated for QNEW before
c setting the diagonals (qste_hjc sets micro rev only for QD, not for QT)
c
c Modif 01/28/03 11:31am for common/indblk, so when nsetq>0 (for indep
c models only) the rest of Q(i,j) are set at the end.
c
c Modified 05/23/02 05:00pm so QT and QNEW can have different dimensions
c
c 01/30/02 04:55pm Changing conc cannot change micro-rev so SETMR
c removed
c
c To take an input matrix, QT, and mutliply assoc rates by the concentrations
c in conc(10),  calculate diagonals and output Q matrix, QNEW.
c NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	real*8 QT(km,km),QNEW(kn,kn),S
	real*4 conc(10,10),x1
	integer il(km),ix(km),jx(km)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
c
      logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb,mfb
c
	do i=1,k
	   do j=1,k
		QNEW(i,j)=0.d0
	   enddo
	enddo
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
		x1=conc(IL(L),jset)
		QNEW(i,j)=QT(i,j)*dble(x1)
	   enddo
	endif
c
c Reset micro rev for QNEW (subroutine SETMR is in QSET_HJC)
c	call SETMR(QNEW,jset,conc,kn)
c
c Lastly, for independent models, set all the other Q(i,j) that are
      if(fastblk) then
	   do n=1,nfblock
		i=ifb(n)
		j=jfb(n)
		x1=conc(iflig,jset)/aKB
		if(mfb(n).gt.0) then
		   QNEW(i,j)=QNEW(i,j)/dble(1.0 + x1)
		else
		   QNEW(i,j)=QNEW(i,j)*dble(x1/(1.0 + x1))
		endif
	   enddo
	endif
c the same basic npar rate constants
	if(nsetq.gt.0) then
	   do L=1,nsetq
		QNEW(ieq(L),jeq(L))=dble(efacq(L))*QNEW(ifq(L),jfq(L))
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



