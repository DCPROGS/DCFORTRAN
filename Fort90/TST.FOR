	character str*30,st1*30

	i=24
	j=50
	k=100
	x=101.5
	str='ADCMEM,25,'
	call tst1(str,i,j,k,st1)
	print*,st1
	end

	subroutine tst1(str,i,j,k,st1)
	character st1*(*),fmt*30,comma*1,str*(*)
c	n=NBLANK(str)
c	comma=','
	fmt='(a15,i4,a1,i4,a1,i4)'
	write(st1,fmt) str,i,',',j,',',k
	end

