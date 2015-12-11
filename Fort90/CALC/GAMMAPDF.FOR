	real function GAMMApdf(t,rate,n)
c
	fact=1.0
	do i=1,n-1
	   fact=fact*float(i)
	enddo
c
	gammapdf=((rate**n)*(t**(n-1))*exp1(-rate*t))/fact
	RETURN
	end


