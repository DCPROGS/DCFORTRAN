	integer nprime(1900)
      OPEN(unit=17,file='961016-1cs.dat',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=7600)
c NB recl is in bytes (7600 bytes=1900 integer*4)
	write(17,rec=1) nprime
      close(unit=17)
	end

