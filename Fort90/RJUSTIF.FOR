	subroutine RJUSTIF(string,n)
	character string*(*)
	common/rj/nj
c To right-justify the characters in a string (eg for use in ASCIO)
c (prev called RJUST, but this clashes with SPINATT.HDR)
c n in call is equal to or less than declared dimension of string = position
c of last character when right justified.
	call GBLANK(string,n,n1,n2)
	nc=n2-n1+1		!number of characters in string
	nj=n-nc+1	!posn of 1st char after right justif, for COMMON
	if(n2.eq.n) RETURN	!already right-justified
	nr=n-n2		!number of places to shift right
	nc=n2-n1+1		!number of characters to be shifted
	do 1 i=n2,n1,-1
	   j=i+nr
	   string(j:j)=string(i:i)
1     continue
c Fill left end with blanks
	do 2 i=1,n-nc
2     string(i:i)=' '
c
	RETURN
	end
