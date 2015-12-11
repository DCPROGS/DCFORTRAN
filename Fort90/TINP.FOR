c	logical defolt
c	common/inp/defolt
c
3	x=999		!default
	print 1
1	format(' x = ')
	call INPUTr(x)
	print 2,x
2	format(g13.6)
	x1=9999	!default
	x2=-9999	!default
	print 11
11	format(' x1,x2 = ')
	call INPUT2r(x1,x2)
	print 12,x1,x2
12	format(2g13.6)
	goto 3
	end


