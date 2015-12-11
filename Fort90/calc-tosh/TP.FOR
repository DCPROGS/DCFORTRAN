	character ch*1
c
	OPEN(unit=7,file='PRN',iostat=nerr)
	print 1
1	format(' ASCII code (integer) = ')
	read *,ival
	ch=char(ival)
	write(7,2) ch//ch//ch//ch
2	format(a4)
	end
