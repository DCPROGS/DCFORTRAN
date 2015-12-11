	subroutine checkrW(W,p,k,kw,kp,pcalc,imode,root,m,nerr3)
c To check that the solution, p, found by eqoc_hjc or by eqoc_red, does indeed
c satisfy r*W=0
c Modif 02/14/06 03:14pm to print, when pcalc=true in hjcasymp, a warning
c if the solution is 'poor'.
c pcalc is now an argument, and imode=1,2,3,4 indicates which of the
c places in hjcasymp this call to checkrw came (row and cole eigenvectors
c for open time, ditto for shut time). rout(m)=mth root
c
c common/deb3/idebug3 for advanced options. If idebug3=1 all values printed
c otherwise printed only at the final call to hjclik, after convergence, when
c pcalc=true
c
c At present, quality of the inverse is checked, but may be best to take no action
c even when it is poor, in hope that it improves as fir converges.
	logical discprt,pcalc
	common/dp/discprt
	real*8 W(kw,kw),p(kp),one
	allocatable::row
	real*8 row(:,:),root
c
	one=1.d0
c
	nerr3=0
	allocate(row(1,k))
	call MATMUL(p,W,row,1,k,k,one,
     &	1,kp,kw,kw,1,k)
c result should be all zeros in row(1,j)
	do j=1,k
	   if(dabs(row(1,j)).gt.1.d-6) nerr3=1
	   if(dabs(row(1,j)).gt.1.d-2) then
		nerr3=2
	   endif
	enddo
	if(idebug3.eq.1.or.(pcalc.and.nerr3.eq.2)) then
	   if(idebug3.ne.1) then
c***		print 10
		if(discprt) write(7,10)
10		format(/,
     & ' WARNING: error in solving rW=0. If result not close to ',/,
     & ' zero there may be errors in amplitudes of components',/,
     & ' of the asymptotic HJC distribution.')
	   endif

	   if(imode.eq.1) then
c***		print 1,m,root,m
		if(discprt) write(7,1) m,root,m
1		format(' Root(',i3,') = ',g13.6,': row*WA(s(',i2,')) = ')
	   else if(imode.eq.2) then
c***		print 2,m,root,m
		if(discprt) write(7,2) m,root,m
2		format(
     &    ' Root(',i3,') = ',g13.6,': col''*WA(s(',i2,'))'' = ')
	   else if(imode.eq.3) then
c***		print 3,m,root,m
		if(discprt) write(7,3) m,root,m
3		format(' Root(',i3,') = ',g13.6,': row*WF(s(',i2,')) = ')
	   else if(imode.eq.4) then
c***		print 4,m,root,m
		if(discprt) write(7,4) m,root,m
4		format(
     &     ' Root(',i3,') = ',g13.6,': col''*WF(s(',i2,'))'' = ')
	   endif
c
c***	   print 5, (row(1,j),j=1,k)
	   if(discprt) write(7,5) (row(1,j),j=1,k)
5	   format(10(1x,g13.6))
	endif
	deallocate(row)
	return
	end

