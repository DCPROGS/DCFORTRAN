	subroutine CHtoINT(cnum,int)
c Character to integer*4 conversion. Returns int=0 if string is blank
	character*11 cnum
	logical neg
c
	int=0
	call GBLANK(cnum,11,n1,n2)
	if(n1.eq.0.and.n2.eq.0) RETURN	!with int=0
	neg=.false.
	if(ichar(cnum(n1:n1)).eq.45) then
	   neg=.true.
	   n1=n1+1
	endif
	ifac=1
	do i=n2,n1,-1
	   j=ichar(cnum(i:i))-48
	   if(j.ge.0.and.j.le.9) then
		if(ifac.eq.1000000000.and.j.gt.2) then	!too big
		   int=2147483647
		   goto 9
		endif
		inext=ifac*j
		if(inext.gt.2147483647-int) then	!too big
		   int=2147483647
		   goto 9
		else
		   int=int + inext
		endif
		ifac=ifac*10
	   endif
	enddo
9	if(neg) int=-int
	RETURN
	end


