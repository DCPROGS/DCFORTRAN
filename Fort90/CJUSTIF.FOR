	subroutine CJUSTIF(string,n)
	character string*(*)
	common/rj/nj
c To centre the characters in a string (modif from RJUSTIF, LJUSTIF)
c n in call is equal to or less than declared dimension of string = last
c position for purposes of centering.
	call GBLANK(string,n,n1,n2)
	nc=n2-n1+1		!number of characters in string
	n0=n-nc		!number of blanks
	nj=n0/2		!number of blanks before first character (rounded down)
	nj=nj+1		!position of first character
	if(nj.eq.n1) then
	   RETURN	!already centred
	else if(nj.gt.n1) then	!shift right
	   ns=nj-n1       !number of places to shift right
	   do i=n2,n1,-1
		j=i+ns
		string(j:j)=string(i:i)
	   enddo
c      -fill left end with blanks
	   do i=1,nj-1
		string(i:i)=' '
	   enddo
	else if(nj.lt.n1) then	!shift left
	   ns=n1-nj       !number of places to shift left
	   do i=n1,n2
		j=i-ns
		string(j:j)=string(i:i)
	   enddo
c      -fill right end with blanks
	   do i=nj+nc,n
		string(i:i)=' '
	   enddo
	endif
c
	RETURN
	end
