	integer*2 iampl(10)
	real*4 ampl(10)
c
	do i=1,10,2
	  iampl(i)=0
	  iampl(i+1)=1
	enddo
c
	do i=1,10
	  ampl(i)=1.e-30*float(iampl(i))
	  if(ampl(i).eq.0) print *,i
	enddo
c
	do i=1,10
	  if(ampl(i).ne.0) print *,i
	enddo
	end

