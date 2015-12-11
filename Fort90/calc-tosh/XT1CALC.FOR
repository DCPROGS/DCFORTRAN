	subroutine XT1CALC(Q1,Xt,time,ncdep,IX,JX,k,km)
c DEBUG routine to calc X(t') where t'=t-tdec -for this concint=0
c To alter an input Q matrix, Q1, which has had conc terms removed, to
c the output matrix, X(t) needed in SCALCS (called by VCONC).  All
c conc-dep element (including those in diagonals) by integral of c(t).dt,
c and all other elements by integral dt=t
c NLIG=1 assumed (nlig=number of ligands and IL(i)=ligand type for ith conc-dep)
c time=time in msec
	real*8 Q1(km,km),Xt(km,km)
	real*8 time1,rA
c	integer il(km),ix(km),jx(km)
	integer ix(km),jx(km)
c
c      c1=CONCINT(time)*1.e-9	!molar*sec
	c1=0.0
      time1=dble(time*1.e-3)		!sec
	rA=dble(c1)/time1
c
c Can't remove time below when time=0, but in thus case Xt=0, so catch here
	if(time.eq.0.) then
	   call QZERO(Xt,k)
	   RETURN
	endif
c
c Simplest first to multiply ALL elements by time (remove from conc dep below)
	do 1 i=1,k
	do 1 j=1,k
1	Xt(i,j)=Q1(i,j)*time1
c
	if(ncdep.eq.0) goto 2	!recalc diagonals
c
	do L=1,ncdep
	   i=IX(L)
	   j=JX(L)
	   Xt(i,j)=Xt(i,j)*rA  !*concint/time (time cancels)
	end do
c
c Recalculate the diagonals
2	continue
	do i=1,k
	   s=0.d0
	   do 3 j=1,k
		if(i.eq.j) goto 3
		s=s+Xt(i,j)
3	   continue
	   Xt(i,i)=-s
	end do
	RETURN
	end



