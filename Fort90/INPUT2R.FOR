	subroutine INPUT2R(real1,real2)
c To input two real numbers (with or without decimal point!) separated
c by a comma.  If invalid character entered the bell rung and both
c outputs set to 1.e35.  If no comma entered the bell rung and both
c outputs set to the number entered (if it was a valid one)
c Modified 01/20/93 11:24pm to distinguish between input of 0 and
c input of <ENTER>.
c (1) Invalid characters and blanks are ignored
c (2) If no valid characters entered (eg ENTER is hit) then the input (default)
c	values of real1,real2 are returned unchanged
c (3) Entry of a single value, followed by comma or not, returns that value
c	as real1, and the default real2
c (4) If the first valid character is a comma, any valid characters following
c	it are returned as real2, with the default value of real1
	character ch2*25,ch21*25,ch*12
	logical digchar,isnum,numon
c
	numon=isnum()	!num lock is on already on
	if(.not.numon) call NUMSET	!Spindrift library
c
	do i=1,25
	  ch2(i:i)=char(32)
	  ch21(i:i)=char(32)
	enddo
	do i=1,12
	  ch(i:i)=char(32)
	enddo
c
	read 1,ch2
1	format(a25)
c
c Return with input values of real1, real2 if ENTER hit, so ch2 blank
	if(ch2.eq.'                         ') RETURN
c
c Search for comma, and define CH21 as like CH2 but remove blanks and
c non-digits (but include the comma, if present, in ch21)
	icom=0		!not changed if no comma present
	n=0			!count valid characters
	do i=1,25
	   ival=ichar(ch2(i:i))
	   if((digchar(ival).or.ival.eq.44).and.(ival.ne.32)) then
		n=n+1
		ch21(n:n)=ch2(i:i)
		if(ival.eq.44) icom=n	!comma found at position icom in ch21
	   endif
	enddo		!end of i loop
c
c Blanks excluded from ch21 already, so if ch2 had all blanks before comma
c then ch21 has comma in position 1, and if ch2 had all blanks after comma
c then ch21 has comma in position n.
c If no comma found, or comma is at end (posn n in ch21) then treat
c all characters as REAL1
	if(icom.eq.0.or.icom.eq.n) then
	   ch=ch21(1:12)
	   if(ch.ne.'            ') call CHTOREAL(ch,real1) !convert unless blank
c If first character is comma then treat characters 2 to n in ch21 as REAL2
	else if(icom.eq.1) then
	   ch=ch21(2:n)
	   call CHTOREAL(ch,real2)
c If comma is elsewhere, convert to get real1, real2 (unless blank)
	else
	   ch=ch21(1:icom-1)		!exclude comma
	   call CHTOREAL(ch,real1)
	   ch=ch21(icom+1:n)		!exclude comma
	   call CHTOREAL(ch,real2)
	endif
c
c	j=0
c	n=0
c	do 3 i=1,25
c	   ival=ichar(ch2(i:i))
c	   if(n.eq.0.and.ival.eq.44) then		!1st number finished
c 		n=n+1
c		if(i.eq.1) goto 6		!first character=comma so real1=default
c		do k=1,i-1
c		   if(ch(k:k).ne.' ') then    !ch is not all blank, so convert
c			call CHTOREAL(ch(1:i-1),real1)	!exclude comma
c			goto 6		!jump out of k loop
c		   endif
c		enddo
c6		j=0			!start next number
c		do 22 k=1,12
c22		ch(k:k)=char(32)
c	   else
c		if(.not.digchar(ival)) then
c		   call BELL(1)
cc		   real1=1.e35
cc		   real2=1.e35
c		   RETURN
c		endif
c		j=j+1
c		if(j.gt.12) goto 33	!ch() filled -convert it
c		ch(j:j)=ch2(i:i)
c	   endif
c3	continue
c33	continue
c	if(ch.ne.'            ') call CHTOREAL(ch,real2)     !convert unless blank
cc Ring bell if no comma found, unless ch=blank still as when CR hit
cc with no input (so real1=real2=0.)
c	if(n.eq.0) then
c	   if(ch.ne.'            ') call BELL(1)
cc	   print 5
cc5	   format(' No comma')
c	   real1=real2
c	endif
c
	if(.not.numon) call NUMCLR	!Spindrift library
	RETURN
	end

