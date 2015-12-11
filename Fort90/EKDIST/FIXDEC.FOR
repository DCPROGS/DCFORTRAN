	subroutine FIXDEC(x,nxf,nx1)
c To fix number (NX=output) of places after the decimal point so that
c non-significant trailing zeroes not drawn (also X is rounded
c correctly for the specified number of decimal places)
c 	If FXD(nxf,nxy) then nxf values after the decimal point will be
c displayed
c eg x=15.3445 then 15.34 is shown if nxd=2.
c Multiply by 10**nxf gives 1534.45
c Round this correctly i=ifixr(1534.45)=1534; float(i)=1534.
c Is last digit zero? amod(1534.,10.)=4. so NO
c			but amod(1530.,10.)=0. so YES- reduce nxf by one
c Are last 2 digits zero ? amod(1534.,100.)=34. so NO
c			but amod(1500.,100.)=0. so YES- reduce nxf by two
c	 At present assumes x=0.01,0.1,1,10,100,1000 for use with log scales
c with numbers not in exponent form
	nx1=nxf
	if(nxf.eq.0) RETURN	!no action needed
c	a=10.**nxf
c	x1=x*a
c	x2=ifixr(x1)
c If nxf=2 so 10**nxf=100 then cannot use IFIXR for x=1000 say, without
c getting integer overflow
	x2=afixr(x*10.**nxf)
c	print 2,x2
c2	format(' x2= ',g13.6)
	do 1 i=1,nxf
c	b=10.**i
c	x3=amod(x2,b)
c	if(abs(x3).lt.0.5) nx1=nx1-1
	if(abs(amod(x2,10.**i)).lt.0.5) nx1=nx1-1
c	print 3,b,x3,nx1
c3	format(' b,x3,nx1= ',2g13.6,i8)
c	print 3,nx1
c3	format(' nx1= ',i8)
c So now display using nx1 decimal places, and round correctly for
c this number
	x=afixr(x*10.**nx1)*10.**(-nx1)
1	continue
	RETURN
	end


