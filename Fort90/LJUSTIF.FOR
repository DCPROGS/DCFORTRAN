 	subroutine LJUSTIF(string,n)
	character string*(*)
c To left-justify the characters in a string (eg for use in ASCIO)
c (prev called LJUST, but this clashes with SPINATT.HDR)
	call GBLANK(string,n,n1,n2)
	if(n1.eq.1) RETURN	!already left-justified
	nc=n2-n1+1		!number of characters to be shifted
	do i=1,nc
	   j=i+n1-1
	   string(i:i)=string(j:j)
	enddo
c Fill right end with blanks
	do i=nc+1,n
	   string(i:i)=' '
	enddo
c
	RETURN
	end

