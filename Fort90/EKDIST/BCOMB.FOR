	subroutine BCOMB(ibcomb,n,ir)
c  For up to n=5 amplitude levels, define array ibcomb(n,ndim) which contains
c  n rows and ndim columns where ndim=max value of binom_coeff(ir,n)
c The first ir columns only are defined
c The first binom_coeff(ir,n) rows ONLY are defined
c For n up to 5, the max value of binom_coeff(ir,n) is 10 so
c ibcomb(5,10) is always big enough
c
c ncr=binom(r,n)
	integer ibcomb(5,10)
c
	if(ir.eq.1.or.ir.eq.n) then
	 if(ir.eq.1) then
	   do i=1,n
		ibcomb(i,1)=i		!first col only defined
	   enddo

	 else if(ir.eq.n) then
	   do i=1,n
		ibcomb(1,i)=i		!first row only defined
	   enddo
	 endif
	 RETURN
	endif
c Now the more complicated cases (n=1, 2 included in above)
	if(n.eq.3) then
	   if(ir.eq.2) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(2,1)=1
		ibcomb(2,2)=3
		ibcomb(3,1)=2
		ibcomb(3,2)=3
	   endif
	else if(n.eq.4) then
	   if(ir.eq.2) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(2,1)=1
		ibcomb(2,2)=3
		ibcomb(3,1)=1
		ibcomb(3,2)=4
		ibcomb(4,1)=2
		ibcomb(4,2)=3
		ibcomb(5,1)=2
		ibcomb(5,2)=4
		ibcomb(6,1)=3
		ibcomb(6,2)=4
	   else if(ir.eq.3) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(1,3)=3
		ibcomb(2,1)=1
		ibcomb(2,2)=2
		ibcomb(2,3)=4
		ibcomb(3,1)=1
		ibcomb(3,2)=3
		ibcomb(3,3)=4
		ibcomb(4,1)=2
		ibcomb(4,2)=3
		ibcomb(4,3)=4
	   endif
	else if(n.eq.5) then
	   if(ir.eq.2) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(2,1)=1
		ibcomb(2,2)=3
		ibcomb(3,1)=1
		ibcomb(3,2)=4
		ibcomb(4,1)=1
		ibcomb(4,2)=5
		ibcomb(5,1)=2
		ibcomb(5,2)=3
		ibcomb(6,1)=2
		ibcomb(6,2)=4
		ibcomb(7,1)=2
		ibcomb(7,2)=5
		ibcomb(8,1)=3
		ibcomb(8,2)=4
		ibcomb(9,1)=3
		ibcomb(9,2)=5
		ibcomb(10,1)=4
		ibcomb(10,2)=5
	   else if(ir.eq.3) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(1,3)=3
		ibcomb(2,1)=1
		ibcomb(2,2)=2
		ibcomb(2,3)=4
		ibcomb(3,1)=1
		ibcomb(3,2)=2
		ibcomb(3,3)=5
		ibcomb(4,1)=1
		ibcomb(4,2)=3
		ibcomb(4,3)=4
		ibcomb(5,1)=1
		ibcomb(5,2)=3
		ibcomb(5,3)=5
		ibcomb(6,1)=1
		ibcomb(6,2)=4
		ibcomb(6,3)=5
		ibcomb(7,1)=2
		ibcomb(7,2)=3
		ibcomb(7,3)=4
		ibcomb(8,1)=2
		ibcomb(8,2)=3
		ibcomb(8,3)=5
		ibcomb(9,1)=2
		ibcomb(9,2)=4
		ibcomb(9,3)=5
		ibcomb(10,1)=3
		ibcomb(10,2)=4
		ibcomb(10,3)=5
	   else if(ir.eq.4) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(1,3)=3
		ibcomb(1,4)=4
		ibcomb(2,1)=1
		ibcomb(2,2)=2
		ibcomb(2,3)=3
		ibcomb(2,4)=5
		ibcomb(3,1)=1
		ibcomb(3,2)=2
		ibcomb(3,3)=4
		ibcomb(3,4)=5
		ibcomb(4,1)=1
		ibcomb(4,2)=3
		ibcomb(4,3)=4
		ibcomb(4,4)=5
		ibcomb(5,1)=2
		ibcomb(5,2)=3
		ibcomb(5,3)=4
		ibcomb(5,4)=5
	   else
		print 1,n
1		format('n = ',i4,' is INVALID')
	   endif
	endif
c
	RETURN
	end

