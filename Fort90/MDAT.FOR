	integer*2 idata(1000)
	character*40 datfil
c
	do i=1,1000
	   idata(i)=i
	enddo
	datfil='datfil.dat'
c
      OPEN(unit=14,file=datfil,status='UNKNOWN',access='TRANSPARENT')
	write(unit=14,rec=1) idata
	close(unit=14)
	end

