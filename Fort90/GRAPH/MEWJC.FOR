	program newjc

	character*30 alpha
	OPEN(unit=14,file=m2c,status='UNKNOWN',
     &		 access='TRANSPARENT')

	read(14,rec=1) alpha,x,y,z
	print*, alpha,x,y,z
	end
