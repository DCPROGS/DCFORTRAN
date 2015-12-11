subroutine chatonos(cnum0,num,k)

	character*(*) cnum0
	character*5 cnum5,cnum51


	integer num(10)

	nlc=len_trim(cnum0)
	k=1
	l=1
	ij=1

	do m=ij,nlc
		if(cnum0(m:m).eq.','.or.cnum0(m:m).eq.';') then
			cnum5=' '
			cnum5=cnum0(l:m-1)
			call chtoint(cnum5,num(k))
			k=k+1
			l=m+1
		endif
	enddo		 
	cnum5=cnum0(l:nlc)
	call chtoint(cnum5,num(k))
		
end