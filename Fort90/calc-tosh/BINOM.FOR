	function BINOM(ir,n,P)
c To calculate binomial distribution (for small n) -result is real*4
	real*8 bcoeff,bin,p2
	real*4 p

c
	if(ir.eq.0.or.ir.eq.n) then
	   bcoeff=1.0d0
	else
	   m=n
	   do i=n-1,n-ir+1,-1
		m=m*i
	   enddo
	   m1=1
	   do i=2,ir
		m1=m1*i
	   enddo
	   bcoeff=dfloat(m)/dfloat(m1)
	endif
c
	p2=dble(p)
	bin=bcoeff*(P2**ir)*((1.0d0-P2)**(n-ir))
	binom=sngl(bin)
	RETURN
	end



