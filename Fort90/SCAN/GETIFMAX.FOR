	subroutine GETIFMAX(imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax)
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)

c Find largest and smallest non-zero entries in ifreq()
	do i=imax,imin,-1
	   if(ifreq1(i).ne.0) then
		ifmax(1)=i
		goto 5
	   endif
	enddo
5	do i=imax,imin,-1
	   if(ifreq2(i).ne.0) then
		ifmax(2)=i
		goto 6
	   endif
	enddo
6	do i=imax,imin,-1
	   if(ifreq3(i).ne.0) then
		ifmax(3)=i
		goto 7
	   endif
	enddo
7	do i=imin,imax
	   if(ifreq1(i).ne.0) then
		ifmin(1)=i
		goto 8
	   endif
	enddo
8	do i=imin,imax
	   if(ifreq2(i).ne.0) then
		ifmin(2)=i
		goto 9
	   endif
	enddo
9	do i=imin,imax
	   if(ifreq3(i).ne.0) then
		ifmin(3)=i
		goto 10
	   endif
	enddo
10	continue
c
	RETURN
	end


