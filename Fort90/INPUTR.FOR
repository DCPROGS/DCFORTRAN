	subroutine INPUTR(real)
c To input a real number (with or without decimal point!)
c NB if more than 12 digits entered without decimal point
c the point is assumed to be after the 12th digit!
c Modified 01/20/93 08:59pm to distinguish between input of 0 and
c input of <ENTER>. If no valid characters entered (e.g. if ENTER is hit) then
c input (default) value of REAL is returned unchanged.
c Invalid characters and blanks are ignored
c Mofif 01/11/95 03:41pm so switches numlock on, and exits with numlock
c in same state as when entered
	character CH*12,ch1*12
	logical digchar,isnum,numon
c
	numon=isnum()	!num lock is on already on
	if(.not.numon) call NUMSET	!Spindrift library
c
	do i=1,12
	  ch(i:i)=char(32)
	  ch1(i:i)=char(32)
	enddo
c
	read 1,ch
1	format(a12)
c
c Define ch1 to exclude blanks and non-digit characters
	n=0
	do i=1,12
	   ival=ichar(ch(i:i))
	   if(digchar(ival).and.(ival.ne.32)) then
		n=n+1
		ch1(n:n)=ch(i:i)
	   endif
	enddo		!end of i loop
c if all blank, and/or non-digits, then n=0 so return with input value of REAL
	if(n.ne.0) call CHTOREAL(ch1(1:n),real)
c
	if(.not.numon) call NUMCLR	!Spindrift library
	RETURN
	end

