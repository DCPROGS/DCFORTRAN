	OPEN(unit=7,file='PRN',iostat=nerr)
      print 1
      write(7,1)
1	format(' testing')
	call flush(7)
	end

