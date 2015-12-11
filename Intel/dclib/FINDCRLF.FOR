	subroutine FINDCRLF(nfind,string,len,ioff)
c Finds the nfind'th occurrence of CR-LF, and returns ioff=byte # for
c character following it.
c Modified 05/01/98 06:11pm sso also returns ioff=byte # for character
c following LF by itself
	character*1 string(len)
c
	n=0
	do i=1,len
	   if(ichar(string(i)).eq.13.and.ichar(string(i+1)).eq.10) then !<CR><LF> found
		n=n+1
		if(n.eq.nfind) goto 1
	   endif
	   if(ichar(string(i)).eq.10) then !<LF> found
		n=n+1
		if(n.eq.nfind) goto 2
	   endif
	enddo
	ioff=0	!NOT FOUND!
1	continue
	ioff=i+2
	RETURN
2	continue
	ioff=i+1
	RETURN
	end
