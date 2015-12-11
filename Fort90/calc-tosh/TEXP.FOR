	program TEXP
c To test overflow
c OK up to exp(709)=0.82 E+308
c  and 10**309 overflows, 10^308 OK
	real*8 x,ye,yd
c
c Misc tests
	nd=8
	x=1.d308
	do i=1,nd
	   x=x*1.d-10
	enddo
	ss=dsign(1.d0,x)
	is=int4(ss)
	ss=dsign(1.d0,-x)
	is=int4(ss)
c restore x
	x=x*10.d0**(10*nd)

c
c
c Test overflow
	print 3
3	format(
     & ' (1) exp(x)',/,' (2) 10^x ',/,' Option = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   do i=400,800
		x=dfloat(i)
		yE=dexp1(x)
		print 1,x,ye
1		format(' x = ',g13.6,'  y=exp(x) = ',g13.6)
	   enddo
	else
	   do i=200,400
		x=dfloat(i)
		yd=10.d0**x
		print 2,x,yd
2		format(' x = ',g13.6,'  y = 10^x = ',g13.6)
	   enddo
	endif
	end
