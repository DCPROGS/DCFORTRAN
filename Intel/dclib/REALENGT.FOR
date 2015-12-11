	subroutine realength(str,nr)

	character*(*) str

	nl = nblank1(str)
	if(nl.ge.2) then
	if(ichar(str(nl:nl)).eq.0) nl=nl-1
	endif
	nr = nl
	i=1
 	do while(i.lt.nl)
	   if(str(i:i).eq.'*') then
		if (str(i+1:i+1).eq.'N') nr=nr-2
		if (str(i+1:i+1).eq.'E') nr=nr-2
		if (str(i+1:i+1).eq.'I') nr=nr-2
		if (str(i+1:i+1).eq.'A') nr=nr-2
		if (str(i+1:i+1).eq.'S') nr=nr-2
		if (str(i+1:i+1).eq.'/') nr=nr-2
		if (str(i+1:i+1).eq.'l') nr=nr-2
		if (str(i+1:i+1).eq.'F'.and.(i+2).le.nl) then
		   if (str(i+2:i+2).eq.'R') nr=nr-3
		   if (str(i+2:i+2).eq.'0') nr=nr-5
		   if (str(i+2:i+2).eq.'1') nr=nr-5
		endif
	   endif
	   i=i+1
	enddo
	end
