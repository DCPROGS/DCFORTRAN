	integer*2 idata(100)
c
	do i=1,100
	  idata(i)=int2(i)
	enddo
c
2	print 1
1	format(' j = ')
	read *,j
	ndim=100
	call DC1(idata(j),ndim)
	goto 2
	end

	subroutine DC1(idat,n)
	integer*2 idat(n)
c
	pause
	return
	end

