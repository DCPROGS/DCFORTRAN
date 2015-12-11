	subroutine GETBITS(ip1,ip4)
	integer*1 ip1
	integer*4 ip4
	logical btest
c
	print 2,ip1
2	format(' ip1 = ',i4,/)
	print 5
5	format('  0 1 2 3 4 5 6 7',/)
	do i=0,7
	   if(BTEST(ip1,i)) then
		print 1,1
1		format('& ',i1)
	   else
		print 1,0
	   endif
	enddo
c
	print 3,ip4
3	format(' ip4 = ',i4,/)
	print 6
6	format('  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 ...',/)
	do i=0,31
	   if(BTEST(ip4,i)) then
		print 1,1
	   else
		print 1,0
	   endif
	enddo
c
	RETURN
	end


