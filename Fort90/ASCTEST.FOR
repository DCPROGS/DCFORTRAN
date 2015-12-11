	program asctest
	character*1 ch
c to check ascii characters in Win 95

      OPEN(unit=8,file='asctest.prt',status='UNKNOWN',
     & access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	REWIND(unit=8)
	do i=1,255
	   if(i.ne.26) then
		ch=char(i)
		print 1,i,ch
		write(8,1) i,ch
1		format(i4,2x,a1)
	  endif
	enddo
	end

