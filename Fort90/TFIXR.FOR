	program TFIXR
c to test ifixr
	print 1
1	format(' x = ')
	call INPUTr (x)
	x1=x+SIGN(0.5,x)
	print 3, x1
3	format(' x1 = ',g13.6)
	print 5, round(x)
5	format(' round(x) = ',i15)
	print 2, ifixr(x)
2	format(' ifixr(x) = ',i15)
	end

