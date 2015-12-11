	subroutine FINDSEP1(i1,string,len,isep,iend,inext)
c To locate the next separator, starting at string(i1)
c NB may have several spaces between columns, so when space is found
c keep looking at next char until non-space character is found or
c each space will be taken as a separator
c Returns isep=1 if tab, space or comma found
c Returns isep=2 if <CR><LF> found
c Returns isep=2 if <LF> found only (modif 05/01/98 05:52pm)
c Returns isep=0 if neither found
c Returns iend such that string(iend) is last character preceding the separator
c inext=position of next valid character
c NB if the last character is <LF>, then must ensure that inext is set anyway
c In this case set inext=len+1, and must check in calling prog that no attempt
c is made to refer to string(inext)
	character*1 string(len)
c
	i=i1
	do while(i.lt.len)
	   ival=ichar(string(i))
	   ival1=ichar(string(i+1))
	   if(ival.eq.9.or.ival.eq.32.or.ival.eq.44) then  !tab,space, comma found
		iend=i-1
1		i=i+1
		ival2=ichar(string(i))
		if(ival2.eq.9.or.ival2.eq.32) goto 1   !look for consec tabs,spaces
	      ival=ichar(string(i))
	      ival1=ichar(string(i+1))
		if((ival.eq.13.and.ival1.eq.10).or.
     &	  (ival.eq.10.or.ival1.eq.10)) then	!found spaces followed by CRLF (or LF alone)
		   if(ival.eq.13.and.ival1.eq.10) then
			inext=i+2		!CR/LF
		   else if(ival1.eq.10) then
			inext=i+2		!LF only
		   else if(ival.eq.10) then
			inext=i+1		!LF only
		   endif
		   isep=2
c If there are blanks following CR/LF count them as part of the
c same separator
		   if(inext.le.len) then
			ival2=ichar(string(inext))
			do while(ival2.eq.32.and.inext.le.len)
			   inext=inext+1
		         ival2=ichar(string(inext))
			enddo
		   endif
		else
		   inext=i	!non-blank char is start of next number
		   isep=1	!=space or comma found (same row)
		endif
		RETURN
c	   else if(ival.eq.13.and.ival1.eq.10) then
	   else if((ival.eq.13.and.ival1.eq.10).or.
     &	  (ival.eq.10.or.ival1.eq.10)) then	!found spaces followed by CRLF (or LF alone)
		isep=2	 !<CR><LF>, or <LF> found
		iend=i-1
		if(ival.eq.13.and.ival1.eq.10) then
		   inext=i+2		!<CR><LF>
		else if(ival1.eq.10) then
			inext=i+2		!LF only
		else if(ival.eq.10) then
		   inext=i+1		!<LF>
		endif
c If there are blanks or tabs following CR/LF count them as part of the
c same separator
		if(inext.le.len) then
		   ival2=ichar(string(inext))
		   do while((ival2.eq.32.or.ival2.eq.9).and.inext.le.len)
			inext=inext+1
			ival2=ichar(string(inext))
		   enddo
		endif
		RETURN
	   endif
	   i=i+1
	enddo
	isep=0
	RETURN
	end
