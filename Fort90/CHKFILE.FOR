	subroutine CHKFILE(filnam,nerr,ndim)
c
c To check that filnam is a valid DOS file name
c	   nerr=1		!too long
c	   nerr=2		!more than one dot
c	   nerr=3		!character bad
c	   nerr=4		!no dot and more than 8 characters
c	   nerr=5		!one dot and prefix more than 8 characters
c	   nerr=6		!suffix longer than 3 char
	character filnam*(*)
c	character path*30,pname*8,suffix*3,ndev*2,UC*1
c	logical nopath,nosuf
	logical valid,nodot
c
	nerr=0	!if valid
	nodot=.false.
c	call PARSNAME(filnam,path,ndev,pname,suffix,nopath,30)
c Define first and last non-blank characters in FILNAM
	n1=1		!in case filnam is all blank
	n2=1		!in case filnam is all blank
	do i=1,ndim
	   ic=ichar(filnam(i:i))
	   if(ic.ne.0.and.ic.ne.32) then
		n1=i
		goto 11		!first non-blank character found
	   endif
	enddo
11	do i=ndim,1,-1
	   ic=ichar(filnam(i:i))
	   if(ic.ne.0.and.ic.ne.32) then
		n2=i
		goto 13		!last non-blank character found
	   endif
	enddo
13	continue
c Look for last '\' character
	i1=0
	do i=n2,n1,-1
	   i1=i
	   if(filnam(i:i).eq.'\') goto 5
	enddo
c If reach here no \ found so no path given
c	nopath=.true.
	i1=0		!if no '\' found
5	continue
c If path specified then alter first character of filename to the
c character that follows the last '\'
	n1=n1+i1
	n=n2-n1+1
c
	if(n.gt.12) then
	   nerr=1		!too long
	   valid=.false.
	   goto 99
	endif
c
c Look for the dot
	nd=0
	do i=n1,n2
	  ival=ICHAR(filnam(i:i))
	  if(ival.eq.46) then
		nd=nd+1
		ndot=i
	  endif
	enddo
	if(nd.gt.1) then
	   nerr=2		!more than one dot
	   valid=.false.
	   goto 99
	else if(nd.eq.0) then
	   nodot=.true.
	endif
c
	if(nodot.and.n.gt.8) then
	   nerr=1		!too long
	   valid=.false.
	   goto 99
	endif
c
c Check the characters
	valid=.false.
	do i=n1,n2
	  ival=ICHAR(filnam(i:i))
	  if(ival.ge.48.and.ival.le.57) valid=.true.	!digit
	  if(ival.ge.65.and.ival.le.90) valid=.true.	!UC letter
	  if(ival.ge.97.and.ival.le.122) valid=.true.	!LC letter
	  if(ival.eq.46) valid=.true.				!dot
	  if(ival.eq.45) valid=.true.				!hyphen
	  if(ival.eq.92) valid=.true.				!\
	  if(ival.eq.95) valid=.true.				!underline
	  if(.not.valid) then
		nerr=3		!character bad
		valid=.false.
		goto 99
	   else
		valid=.false.
	   endif
	enddo
c
	if(nodot.and.n.gt.8) then
	   nerr=4		!no dot and more than 8 characters
	   valid=.false.
	   goto 99
	endif
c
	if(nd.eq.1) then
	   npre=ndot-n1	!no of char in filname before dot
	   if(npre.gt.8) then
		nerr=5		!one dot and prefix more than 8 characters
		valid=.false.
		goto 99
	   endif
	endif
c Check length of suffix
	if(nd.eq.1.and.(n2-ndot.gt.3)) then
	   nerr=6		!suffix longer than 3 char
	   valid=.false.
	   goto 99
	endif
c
99	if(valid) nerr=0
	RETURN
	end

