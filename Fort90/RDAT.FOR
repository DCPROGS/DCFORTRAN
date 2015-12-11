	integer*2 idata(1000)
	character*40 datfil
c
	datfil='TEST.SSD'
c
      OPEN(unit=14,file=datfil,status='UNKNOWN',access='TRANSPARENT')
	READ(unit=14,rec=513) (idata(i),i=1,1000)
	close(unit=14)
	end
