	print 1
1	format(' P = ')
	call INPUTr(p)
	print 2
2	format(' n =')
	call INPUTi(n)
	s=0.
	do i=0,n
	   p1=binom(i,n,p)
	   s=s+p1
	   print 3,i,p1
3	   format(' P(',i3,') = ',g13.6)
	enddo
	print 4,s
4	format(' Total = ',g13.6)
	end


