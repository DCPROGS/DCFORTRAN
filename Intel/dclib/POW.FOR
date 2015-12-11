	subroutine pow(z,a1,ipow)
	k=0
	if(abs(z).gt.1.) then
	   y=abs(z)
	   do while(y.gt.10.)
		y=y/10.
		k=k+1
	   enddo
	   a1=y*10.
	   if (z.lt.0.) a1=-y*10.
	   b1= k-1
	   goto 1
	else if(abs(z).gt.0.and.abs(z).lt.1) then
	   y=abs(z)
	   do while(y.lt.10.)
		y=y*10.
		k=k+1
	   enddo
	   a1=y
	   if (z.lt.0.) a1=-y
	   b1=-k
	   goto 1
	endif
1     ipow=int(b1)
	end
